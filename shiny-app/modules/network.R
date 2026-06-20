# ============================================================
# Network Analysis Module
# ============================================================

library(shiny)
library(bslib)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(tidygraph)
library(igraph)
library(DT)
library(visNetwork)

# =========================
# Data preparation
# =========================

mc1_raw <- fromJSON("data/MC1_final_00.json", flatten = TRUE)

communications_tbl <- mc1_raw$rounds %>%
  select(hour, communications) %>%
  unnest(communications) %>%
  mutate(
    timestamp = ymd_hms(timestamp),
    date = as_date(timestamp),
    period = case_when(
      date < as.Date("2046-05-23") ~ "Before Embargo Declaration",
      date >= as.Date("2046-05-23") & date < as.Date("2046-06-05") ~ "After Embargo Declaration",
      date == as.Date("2046-06-05") ~ "Breach Day",
      TRUE ~ "Other"
    )
  )

reply_edges <- communications_tbl %>%
  filter(!is.na(responding_to)) %>%
  left_join(
    communications_tbl %>%
      select(message_id, replied_to_agent = agent_label),
    by = c("responding_to" = "message_id")
  ) %>%
  filter(!is.na(replied_to_agent)) %>%
  transmute(
    from = agent_label,
    to = replied_to_agent,
    timestamp,
    date,
    period,
    channel,
    message_type
  )

calculate_metrics <- function(edge_data) {
  
  network_edges <- edge_data %>%
    count(from, to, name = "weight")
  
  network_nodes <- tibble(
    name = unique(c(network_edges$from, network_edges$to))
  )
  
  if (nrow(network_edges) == 0 || nrow(network_nodes) == 0) {
    return(tibble(
      Agent = character(),
      Betweenness = numeric(),
      Degree = numeric(),
      Closeness = numeric(),
      Eigenvector = numeric()
    ))
  }
  
  agent_network <- tbl_graph(
    nodes = network_nodes,
    edges = network_edges,
    directed = TRUE
  )
  
  agent_network %>%
    activate(nodes) %>%
    mutate(
      Degree = centrality_degree(),
      Betweenness = round(centrality_betweenness(), 2),
      Closeness = round(centrality_closeness(), 2),
      Eigenvector = round(centrality_eigen(), 2)
    ) %>%
    as_tibble() %>%
    rename(Agent = name) %>%
    select(Agent, Betweenness, Degree, Closeness, Eigenvector)
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
        edge_data <- edge_data %>%
          filter(period == input$time_period)
      }
      
      edge_data <- edge_data %>%
        filter(
          date >= input$timeline_range[1],
          date <= input$timeline_range[2]
        )
      
      if (input$agent_type != "All") {
        selected_agents <- communications_tbl %>%
          filter(agent_role == input$agent_type) %>%
          pull(agent_label) %>%
          unique()
        
        edge_data <- edge_data %>%
          filter(from %in% selected_agents | to %in% selected_agents)
      }
      
      edge_data
    })
    
    filtered_metrics <- reactive({
      calculate_metrics(filtered_edges()) %>%
        arrange(desc(.data[[input$centrality_metric]]))
    })
    
    output$network_graph <- renderVisNetwork({
      
      edge_data <- filtered_edges() %>%
        count(from, to, name = "weight") %>%
        filter(weight > 0)
      
      req(nrow(edge_data) > 0)
      
      metric_data <- calculate_metrics(filtered_edges())
      
      node_data <- tibble(
        id = unique(c(edge_data$from, edge_data$to)),
        label = id
      ) %>%
        left_join(
          metric_data %>%
            select(Agent, Degree, Betweenness),
          by = c("id" = "Agent")
        ) %>%
        mutate(
          value = Degree,
          title = paste0(
            "<b>", id, "</b><br>",
            "Degree: ", Degree, "<br>",
            "Betweenness: ", Betweenness
          ),
          group = ifelse(
            input$highlight_hubs &
              Betweenness >= quantile(Betweenness, 0.75, na.rm = TRUE),
            "Key Hub",
            "Other Agent"
          )
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
      
      datatable(
        table_data,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          dom = "tip"
        )
      ) %>%
        formatStyle(
          metric,
          backgroundColor = styleInterval(
            quantile(table_data[[metric]], c(0.7, 0.9), na.rm = TRUE),
            c(NA, "#163D77", "#2F80ED")
          ),
          color = "white",
          fontWeight = "bold"
        )
    })
    
    output$edge_preview <- renderTable({
      filtered_edges() %>%
        count(from, to, name = "weight") %>%
        arrange(desc(weight)) %>%
        head(10)
    })
    
    output$supporting_records <- renderDT({
      
      records <- filtered_edges() %>%
        arrange(desc(timestamp)) %>%
        select(
          Timestamp = timestamp,
          From = from,
          To = to,
          Channel = channel,
          `Message Type` = message_type
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