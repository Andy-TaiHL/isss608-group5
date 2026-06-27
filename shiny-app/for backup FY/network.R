# ============================================================
# Network Analysis Module (Base R version - no tidyverse)
# Author: FY
# ============================================================

library(shiny)
library(bslib)
library(jsonlite)
library(igraph)
library(DT)
library(visNetwork)

# =========================
# Data preparation
# =========================

mc1_raw <- fromJSON("data/MC1_final_00.json", flatten = TRUE)

# --- equivalent of: rounds %>% select(hour, communications) %>% unnest(communications) ---
rounds_df <- mc1_raw$rounds[, c("hour", "communications")]

communications_tbl <- do.call(rbind, lapply(seq_len(nrow(rounds_df)), function(i) {
  comms <- rounds_df$communications[[i]]
  if (is.null(comms) || nrow(comms) == 0) return(NULL)
  comms$hour <- rounds_df$hour[i]
  comms
}))
row.names(communications_tbl) <- NULL

# --- mutate(timestamp, date, period) ---
communications_tbl$timestamp <- as.POSIXct(
  communications_tbl$timestamp, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"
)
communications_tbl$date <- as.Date(communications_tbl$timestamp)

communications_tbl$period <- with(communications_tbl, ifelse(
  date < as.Date("2046-05-23"), "Before Embargo Declaration",
  ifelse(date >= as.Date("2046-05-23") & date < as.Date("2046-06-05"), "After Embargo Declaration",
         ifelse(date == as.Date("2046-06-05"), "Breach Day", "Other"))
))

# =========================
# Reply edges
# =========================

# --- filter(!is.na(responding_to)) ---
has_reply <- communications_tbl[!is.na(communications_tbl$responding_to), ]

# --- left_join lookup: message_id -> replied_to_agent ---
lookup <- communications_tbl[, c("message_id", "agent_label")]
names(lookup) <- c("responding_to", "replied_to_agent")

reply_edges <- merge(has_reply, lookup, by = "responding_to", all.x = TRUE)
reply_edges <- reply_edges[!is.na(reply_edges$replied_to_agent), ]

# --- transmute(from, to, timestamp, date, period, channel, message_type) ---
reply_edges <- data.frame(
  from        = reply_edges$agent_label,
  to          = reply_edges$replied_to_agent,
  timestamp   = reply_edges$timestamp,
  date        = reply_edges$date,
  period      = reply_edges$period,
  channel     = reply_edges$channel,
  message_type = reply_edges$message_type,
  stringsAsFactors = FALSE
)

# =========================
# Centrality calculation
# =========================

calculate_metrics <- function(edge_data) {
  
  if (is.null(edge_data) || nrow(edge_data) == 0) {
    return(data.frame(
      Agent = character(),
      Betweenness = numeric(),
      Degree = numeric(),
      Closeness = numeric(),
      Eigenvector = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  
  # --- count(from, to, name = "weight") ---
  network_edges <- aggregate(
    list(weight = rep(1, nrow(edge_data))),
    by = list(from = edge_data$from, to = edge_data$to),
    FUN = sum
  )
  
  network_nodes <- data.frame(
    name = unique(c(network_edges$from, network_edges$to)),
    stringsAsFactors = FALSE
  )
  
  if (nrow(network_edges) == 0 || nrow(network_nodes) == 0) {
    return(data.frame(
      Agent = character(),
      Betweenness = numeric(),
      Degree = numeric(),
      Closeness = numeric(),
      Eigenvector = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  
  agent_network <- graph_from_data_frame(
    d = network_edges,
    vertices = network_nodes,
    directed = TRUE
  )
  
  metrics <- data.frame(
    Agent       = V(agent_network)$name,
    Degree      = degree(agent_network, mode = "all"),
    Betweenness = round(betweenness(agent_network, directed = TRUE), 2),
    Closeness   = round(closeness(agent_network, mode = "all"), 2),
    Eigenvector = round(eigen_centrality(agent_network, directed = TRUE)$vector, 2),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  
  metrics[, c("Agent", "Betweenness", "Degree", "Closeness", "Eigenvector")]
}

# =========================
# UI
# =========================

networkUI <- function(id) {
  
  ns <- NS(id)
  
  layout_sidebar(
    sidebar = sidebar(
      title = "Network Controls",
      width = 360,
      
      selectInput(
        ns("centrality_metric"),
        "Centrality Metric",
        choices = c("Betweenness", "Degree", "Closeness", "Eigenvector"),
        selected = "Betweenness"
      ),
      
      selectInput(
        ns("time_period"),
        "Time Period",
        choices = c(
          "Entire Investigation",
          "Before Embargo Declaration",
          "After Embargo Declaration",
          "Breach Day"
        ),
        selected = "Entire Investigation"
      ),
      
      dateRangeInput(
        ns("timeline_range"),
        "Timeline Range",
        start = min(communications_tbl$date),
        end = max(communications_tbl$date),
        min = min(communications_tbl$date),
        max = max(communications_tbl$date)
      ),
      
      selectInput(
        ns("agent_type"),
        "Agent Type",
        choices = c("All", unique(communications_tbl$agent_role)),
        selected = "All"
      ),
      
      hr(),
      
      checkboxInput(ns("show_labels"), "Show Node Labels", value = TRUE),
      checkboxInput(ns("highlight_hubs"), "Highlight Key Hubs", value = TRUE)
    ),
    
    tags$style(HTML("
      .nav-tabs .nav-link.active {
        background-color: #163D77 !important;
        color: white !important;
      }

      .nav-tabs .nav-link {
        color: #2F80ED !important;
      }
    ")),
    
    navset_tab(
      nav_panel(
        "Network Graph",
        h3("Interactive Network Graph"),
        visNetworkOutput(ns("network_graph"), height = "650px"),
        
        br(),
        h4("Top Communication Links"),
        tableOutput(ns("edge_preview"))
      ),
      
      nav_panel(
        "Pre vs Post Embargo",
        h3("Pre vs Post Embargo Comparison"),
        p("This panel can be used to compare network structure before and after the embargo declaration.")
      ),
      
      nav_panel(
        "Centrality Table",
        h3("Centrality Table"),
        DTOutput(ns("centrality_table")),
        
        br(),
        h4("Supporting Communication Records"),
        p("These records show the communication links used to calculate the selected network metrics."),
        DTOutput(ns("supporting_records"))
      )
    )
  )
}

# =========================
# Server
# =========================

networkServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    filtered_edges <- reactive({
      
      edge_data <- reply_edges
      
      if (input$time_period != "Entire Investigation") {
        edge_data <- edge_data[edge_data$period == input$time_period, ]
      }
      
      edge_data <- edge_data[
        edge_data$date >= input$timeline_range[1] &
          edge_data$date <= input$timeline_range[2],
      ]
      
      if (input$agent_type != "All") {
        selected_agents <- unique(
          communications_tbl$agent_label[communications_tbl$agent_role == input$agent_type]
        )
        
        edge_data <- edge_data[
          edge_data$from %in% selected_agents | edge_data$to %in% selected_agents,
        ]
      }
      
      edge_data
    })
    
    filtered_metrics <- reactive({
      metrics <- calculate_metrics(filtered_edges())
      metrics[order(-metrics[[input$centrality_metric]]), ]
    })
    
    output$network_graph <- renderVisNetwork({
      
      edge_data <- aggregate(
        list(weight = rep(1, nrow(filtered_edges()))),
        by = list(from = filtered_edges()$from, to = filtered_edges()$to),
        FUN = sum
      )
      edge_data <- edge_data[edge_data$weight > 0, ]
      
      req(nrow(edge_data) > 0)
      
      metric_data <- calculate_metrics(filtered_edges())
      
      node_ids <- unique(c(edge_data$from, edge_data$to))
      node_data <- data.frame(id = node_ids, label = node_ids, stringsAsFactors = FALSE)
      
      node_data <- merge(
        node_data,
        metric_data[, c("Agent", "Degree", "Betweenness")],
        by.x = "id", by.y = "Agent", all.x = TRUE
      )
      
      node_data$value <- node_data$Degree
      node_data$title <- paste0(
        "<b>", node_data$id, "</b><br>",
        "Degree: ", node_data$Degree, "<br>",
        "Betweenness: ", node_data$Betweenness
      )
      
      hub_threshold <- quantile(node_data$Betweenness, 0.75, na.rm = TRUE)
      node_data$group <- ifelse(
        input$highlight_hubs & node_data$Betweenness >= hub_threshold,
        "Key Hub",
        "Other Agent"
      )
      
      if (!input$show_labels) {
        node_data$label <- ""
      }
      
      visNetwork(
        nodes = node_data,
        edges = edge_data,
        height = "650px",
        background = "#0a1628"
      ) %>%
        visNodes(
          shape = "dot",
          scaling = list(min = 15, max = 45),
          font = list(color = "white", size = 18)
        ) %>%
        visEdges(
          arrows = "to",
          smooth = TRUE,
          color = list(color = "#BFC7D5", highlight = "#56CCF2"),
          scaling = list(min = 1, max = 8)
        ) %>%
        visGroups(
          groupname = "Key Hub",
          color = list(background = "#2F80ED", border = "#56CCF2")
        ) %>%
        visGroups(
          groupname = "Other Agent",
          color = list(background = "#163D77", border = "#BFC7D5")
        ) %>%
        visOptions(
          highlightNearest = TRUE,
          nodesIdSelection = TRUE
        ) %>%
        visPhysics(
          solver = "forceAtlas2Based",
          stabilization = TRUE
        )
    })
    
    output$centrality_table <- renderDT({
      
      metric <- input$centrality_metric
      table_data <- filtered_metrics()
      
      # table_data is already sorted desc by the selected metric in filtered_metrics()
      n_top <- min(3, nrow(table_data))
      table_data$Rank <- seq_len(nrow(table_data))
      table_data$Top <- ifelse(table_data$Rank <= n_top, "Top Agent", "Other")
      
      metric_vals <- table_data[[metric]]
      val_range <- range(metric_vals, na.rm = TRUE)
      
      sorted_vals <- sort(unique(metric_vals))
      n_unique <- length(sorted_vals)
      
      top10_threshold <- quantile(metric_vals, 0.9, na.rm = TRUE)
      
      if (n_unique <= 1) {
        # only one distinct value - no need to distinguish anything
        datatable(
          table_data,
          rownames = FALSE,
          options = list(
            pageLength = 10,
            dom = "tip",
            columnDefs = list(
              list(visible = FALSE, targets = c("Rank", "Top"))
            )
          )
        ) %>%
          formatStyle(
            "Top",
            target = "row",
            backgroundColor = styleEqual(
              c("Top Agent", "Other"),
              c("#1B5E20", NA)
            )
          )
        
      } else {
        
        # mark the top 10% of values in red, bold, rest stays default text color
        is_top10 <- sorted_vals >= top10_threshold
        text_colors <- ifelse(is_top10, "#D32F2F", "#1e293b")
        font_weights <- ifelse(is_top10, "bold", "normal")
        
        # breakpoints sit at the midpoints between consecutive distinct values
        breaks <- (sorted_vals[-n_unique] + sorted_vals[-1]) / 2
        
        datatable(
          table_data,
          rownames = FALSE,
          options = list(
            pageLength = 10,
            dom = "tip",
            columnDefs = list(
              list(visible = FALSE, targets = c("Rank", "Top"))
            )
          )
        ) %>%
          formatStyle(
            "Top",
            target = "row",
            backgroundColor = styleEqual(
              c("Top Agent", "Other"),
              c("#1B5E20", NA)
            )
          ) %>%
          formatStyle(
            metric,
            color = styleInterval(breaks, text_colors),
            fontWeight = styleInterval(breaks, font_weights)
          )
      }
    })
    
    output$edge_preview <- renderTable({
      edge_data <- aggregate(
        list(weight = rep(1, nrow(filtered_edges()))),
        by = list(from = filtered_edges()$from, to = filtered_edges()$to),
        FUN = sum
      )
      edge_data <- edge_data[order(-edge_data$weight), ]
      head(edge_data, 10)
    })
    
    output$supporting_records <- renderDT({
      
      records <- filtered_edges()
      records <- records[order(-as.numeric(records$timestamp)), ]
      records <- data.frame(
        Timestamp = records$timestamp,
        From = records$from,
        To = records$to,
        Channel = records$channel,
        `Message Type` = records$message_type,
        check.names = FALSE
      )
      
      datatable(
        records,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = "tip"
        )
      )
    })
  })
}