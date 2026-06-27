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
      width = 280,
      
      tags$style(HTML("
        /* Sidebar font sizes */
        .bslib-sidebar-layout > .sidebar { font-size: 13px !important; border-right: 1px solid #ffffff !important; }
        .bslib-sidebar-layout > .sidebar .control-label { font-size: 12px !important; margin-bottom: 3px !important; }
        .bslib-sidebar-layout > .sidebar .selectize-input { font-size: 12px !important; min-height: 30px !important; padding: 4px 28px 4px 8px !important; position: relative !important; }
        .bslib-sidebar-layout > .sidebar .selectize-input::after {
          content: '' !important;
          position: absolute !important;
          right: 10px !important;
          top: 50% !important;
          transform: translateY(-50%) !important;
          width: 0 !important;
          height: 0 !important;
          border-left: 5px solid transparent !important;
          border-right: 5px solid transparent !important;
          border-top: 6px solid #64748b !important;
          pointer-events: none !important;
        }
        .bslib-sidebar-layout > .sidebar .form-control { font-size: 12px !important; padding: 4px 8px !important; height: 30px !important; }
        .bslib-sidebar-layout > .sidebar .checkbox label { font-size: 12px !important; }
        .bslib-sidebar-layout > .sidebar .sidebar-title { font-size: 14px !important; }
        .bslib-sidebar-layout > .sidebar .form-group { margin-bottom: 8px !important; }
        .bslib-sidebar-layout > .sidebar hr { margin: 8px 0 !important; }
        /* Fix dateRangeInput: linked boxes, uniform height, flush 'to' separator */
        .bslib-sidebar-layout > .sidebar .input-daterange {
          display: flex !important;
          align-items: stretch !important;
          height: 30px !important;
          border: 1px solid #ccc !important;
          border-radius: 4px !important;
          overflow: hidden !important;
        }
        .bslib-sidebar-layout > .sidebar .input-daterange input {
          flex: 1 !important;
          min-width: 0 !important;
          height: 30px !important;
          font-size: 12px !important;
          padding: 4px 6px !important;
          border: none !important;
          border-radius: 0 !important;
          text-align: center !important;
          box-sizing: border-box !important;
          background: white !important;
          color: #1e293b !important;
        }
        .bslib-sidebar-layout > .sidebar .input-daterange .input-group-addon {
          display: flex !important;
          align-items: center !important;
          justify-content: center !important;
          height: 30px !important;
          width: 32px !important;
          font-size: 12px !important;
          background: #1e3a5f !important;
          color: #BFC7D5 !important;
          border: none !important;
          padding: 0 !important;
          margin: 0 !important;
          box-sizing: border-box !important;
        }
      ")),
      
      selectInput(ns("centrality_metric"), "Centrality Metric",
                  choices  = c("Betweenness", "Degree", "Closeness", "Eigenvector"),
                  selected = "Betweenness"),
      
      selectInput(ns("time_period"), "Time Period",
                  choices  = c("Entire Investigation", "Before Embargo Declaration",
                               "After Embargo Declaration", "Breach Day"),
                  selected = "Entire Investigation"),
      
      dateRangeInput(ns("timeline_range"), "Timeline Range",
                     start = min(communications_tbl$date),
                     end   = max(communications_tbl$date),
                     min   = min(communications_tbl$date),
                     max   = max(communications_tbl$date)),
      
      selectInput(ns("agent_type"), "Agent Type",
                  choices  = c("All", unique(communications_tbl$agent_role)),
                  selected = "All"),
      
      hr(),
      
      checkboxInput(ns("show_labels"),    "Show Node Labels",  value = TRUE),
      checkboxInput(ns("highlight_hubs"), "Highlight Key Hubs", value = TRUE)
    ),
    
    tags$style(HTML("
      .nav-tabs .nav-link.active { background-color: #163D77 !important; color: white !important; }
      .nav-tabs .nav-link        { color: #2F80ED !important; }
      /* visNetwork dropdowns side by side */
      #network-network_graph .vis-configuration-wrapper {
        display: flex !important;
        flex-direction: row !important;
        gap: 8px !important;
        align-items: center !important;
        flex-wrap: nowrap !important;
        margin-bottom: 4px !important;
      }
      /* Style visNetwork dropdowns to match sidebar */
      #network-network_graph select {
        font-size: 12px !important;
        font-family: Inter, sans-serif !important;
        background: white url(\"data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' width='10' height='6'><path d='M0 0l5 6 5-6z' fill='%2364748b'/></svg>\") no-repeat right 8px center !important;
        background-size: 10px !important;
        color: #1e293b !important;
        border: 1px solid #ccc !important;
        border-radius: 4px !important;
        padding: 4px 28px 4px 8px !important;
        height: 30px !important;
        appearance: none !important;
        -webkit-appearance: none !important;
        margin-bottom: 0 !important;
        width: auto !important;
        min-width: 140px !important;
      }
      /* Prevent horizontal page overflow */
      .bslib-sidebar-layout > .main { overflow-x: hidden !important; }
      /* Navigation buttons - greyscale, hover turns blue */
      .vis-navigation .vis-button {
        filter: grayscale(1) brightness(1.8) !important;
        opacity: 0.85 !important;
      }
      .vis-navigation .vis-button:hover {
        filter: grayscale(0) hue-rotate(120deg) brightness(1.2) !important;
        opacity: 1 !important;
      }
      /* Edge/node tooltip — use ID-scoped selector to beat vis-network.css specificity */
      #network-network_graph .vis-tooltip {
        background-color: #1a2a45 !important;
        color: white !important;
        border: 1px solid #2F80ED !important;
        border-radius: 6px !important;
        font-size: 12px !important;
        font-family: Inter, sans-serif !important;
        padding: 6px 10px !important;
        box-shadow: 0 2px 8px rgba(0,0,0,0.5) !important;
      }
    ")),
    
    # MutationObserver watching document.body (not the vis container, which may not
    # exist yet at ready time) — forces dark tooltip style via setProperty('important')
    tags$script(HTML("
      $(document).ready(function() {
        var observer = new MutationObserver(function(mutations) {
          mutations.forEach(function(m) {
            if (m.target.classList && m.target.classList.contains('vis-tooltip')) {
              var el = m.target;
              el.style.setProperty('background-color', '#1a2a45', 'important');
              el.style.setProperty('color', 'white', 'important');
              el.style.setProperty('border', '1px solid #2F80ED', 'important');
              el.style.setProperty('border-radius', '6px', 'important');
              el.style.setProperty('font-size', '12px', 'important');
              el.style.setProperty('font-family', 'Inter, sans-serif', 'important');
              el.style.setProperty('padding', '6px 10px', 'important');
              el.style.setProperty('box-shadow', '0 2px 8px rgba(0,0,0,0.5)', 'important');
            }
          });
        });
        observer.observe(document.body, { subtree: true, attributes: true, attributeFilter: ['style'] });
      });
    ")),
    
    navset_tab(
      
      nav_panel(
        "Network Graph",
        div(style = "display:flex; gap:0; align-items:flex-start; padding:8px 0;
                     overflow:hidden;",
            
            div(style = "flex:1 1 0; min-width:0; position:relative;",
                visNetworkOutput(ns("network_graph"), height = "440px"),
                div(style = "position:absolute; top:8px; right:8px; z-index:10;
                         color:#BFC7D5; font-size:12px; font-family:Inter,sans-serif;
                         padding:3px 8px; background:rgba(10,22,40,0.85);
                         border-radius:4px; border:1px solid #2F80ED;
                         pointer-events:none;",
                    textOutput(ns("node_size_label"), inline = TRUE)
                )
            ),
            
            # (divider removed per user request)
            
            div(style = "flex:0 0 280px; width:280px; overflow-x:auto;",
                tags$p("Top Communication Links",
                       style = "color:white; font-size:13px; font-family:Inter,sans-serif;
                       font-weight:600; margin:0 0 8px 0; letter-spacing:0.02em;"),
                tags$style(HTML("
              #network-edge_preview table { font-size:11px !important; }
              #network-edge_preview td, #network-edge_preview th {
                white-space: nowrap !important;
                padding: 4px 5px !important;
              }
            ")),
                DTOutput(ns("edge_preview"))
            )
        )
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
      edge_data <- edge_data[edge_data$weight > 0 & edge_data$from != edge_data$to, ]
      
      req(nrow(edge_data) > 0)
      
      metric_data <- calculate_metrics(filtered_edges())
      metric      <- input$centrality_metric
      
      node_ids  <- unique(c(edge_data$from, edge_data$to))
      node_data <- data.frame(id = node_ids, label = node_ids, stringsAsFactors = FALSE)
      
      node_data <- merge(
        node_data,
        metric_data[, c("Agent", "Betweenness", "Degree", "Closeness", "Eigenvector")],
        by.x = "id", by.y = "Agent", all.x = TRUE
      )
      
      # node SIZE driven by the selected centrality metric
      metric_vals      <- node_data[[metric]]
      metric_range     <- range(metric_vals, na.rm = TRUE)
      metric_spread    <- diff(metric_range)
      
      if (metric_spread == 0 || is.na(metric_spread)) {
        node_data$value <- 20
      } else {
        node_data$value <- 10 + 40 * (metric_vals - metric_range[1]) / metric_spread
      }
      
      # node COLOR: blue gradient from dim (low) to bright (high) on the selected metric
      ramp <- colorRampPalette(c("#163D77", "#2F80ED", "#56CCF2"))(100)
      if (metric_spread == 0 || is.na(metric_spread)) {
        node_data$color_bg <- ramp[50]
      } else {
        scaled_idx        <- pmax(1, pmin(100, round(
          (metric_vals - metric_range[1]) / metric_spread * 99
        ) + 1))
        node_data$color_bg <- ramp[scaled_idx]
      }
      
      # brighten the top hub (highest metric value) with a distinct color
      hub_threshold      <- quantile(metric_vals, 0.75, na.rm = TRUE)
      is_hub             <- !is.na(metric_vals) & metric_vals >= hub_threshold & input$highlight_hubs
      node_data$color_bg[is_hub] <- "#56CCF2"
      node_data$color_border     <- ifelse(is_hub, "#ffffff", "#BFC7D5")
      node_data$group            <- ifelse(is_hub, "Key Hub", "Other Agent")
      
      # full tooltip showing all 4 metrics
      node_data$title <- paste0(
        "<div style='background:#1a2a45;color:white;padding:8px;border-radius:6px;",
        "font-family:Inter,sans-serif;min-width:160px;'>",
        "<b style='font-size:13px;'>", node_data$id, "</b><br/>",
        "<hr style='border-color:#2F80ED;margin:4px 0;'/>",
        "Betweenness: <b>", node_data$Betweenness, "</b><br/>",
        "Degree: <b>",      node_data$Degree,      "</b><br/>",
        "Closeness: <b>",   node_data$Closeness,   "</b><br/>",
        "Eigenvector: <b>", node_data$Eigenvector, "</b>",
        "</div>"
      )
      
      if (!input$show_labels) node_data$label <- ""
      
      # edge width scaled by communication frequency
      edge_data$width <- 1 + 5 * (edge_data$weight - min(edge_data$weight)) /
        max(1, diff(range(edge_data$weight)))
      edge_data$title <- paste0(
        "<div style='background:#1a2a45;color:white;padding:6px 10px;",
        "border-radius:6px;font-family:Inter,sans-serif;font-size:12px;'>",
        "Messages: <b>", edge_data$weight, "</b>",
        "</div>"
      )
      
      visNetwork(
        nodes = node_data,
        edges = edge_data,
        height = "650px",
        background = "#0a1628"
      ) %>%
        visNodes(
          shape  = "dot",
          color  = list(
            background = node_data$color_bg,
            border     = node_data$color_border,
            highlight  = list(background = "#F2994A", border = "#ffffff")
          ),
          font   = list(color = "white", size = 16, bold = TRUE),
          shadow = list(enabled = TRUE, size = 8)
        ) %>%
        visEdges(
          arrows = "to",
          smooth = list(enabled = TRUE, type = "curvedCW", roundness = 0.2),
          color  = list(color = "#4A5568", highlight = "#F2994A", hover = "#56CCF2"),
          font   = list(color = "white", size = 11)
        ) %>%
        visOptions(
          highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
          nodesIdSelection = list(
            enabled = TRUE,
            style   = "background:#163D77;color:white;border:1px solid #2F80ED;padding:4px;"
          ),
          selectedBy = list(
            variable = "group",
            style    = "background:#163D77;color:white;border:1px solid #2F80ED;padding:4px;"
          )
        ) %>%
        visPhysics(
          solver            = "forceAtlas2Based",
          forceAtlas2Based  = list(
            gravitationalConstant = -60,
            centralGravity        = 0.01,
            springLength          = 120,
            springConstant        = 0.08,
            damping               = 0.6
          ),
          stabilization = list(enabled = TRUE, iterations = 200)
        ) %>%
        visInteraction(
          navigationButtons = TRUE,
          keyboard          = TRUE,
          tooltipDelay      = 100
        )
    })
    
    output$centrality_table <- renderDT({
      
      metric     <- input$centrality_metric
      table_data <- filtered_metrics()
      
      # add hidden rank/top columns for top-3 row tracking
      n_top <- min(3, nrow(table_data))
      table_data$Rank <- seq_len(nrow(table_data))
      table_data$Top  <- ifelse(table_data$Rank <= n_top, "Top Agent", "Other")
      
      # 0-based column index for JS (Agent=0, Betweenness=1, Degree=2, Closeness=3, Eigenvector=4)
      metric_col_idx <- which(names(table_data) == metric) - 1L
      
      # JS drawCallback: runs 50ms after draw.dt handler has reset all colors,
      # then uses setProperty('important') to force orange on the max-value cell
      # — inline !important beats any stylesheet !important rule
      draw_js <- sprintf("
        function() {
          var api = this.api();
          var colIdx = %d;
          setTimeout(function() {
            // reset metric column to normal first
            api.column(colIdx).nodes().each(function(cell) {
              cell.style.setProperty('background-color', 'transparent', 'important');
              cell.style.setProperty('color', '#1e293b', 'important');
              cell.style.setProperty('font-weight', 'normal', 'important');
            });
            // highlight only the max-value cell (always row 0, sorted desc)
            var maxCell = api.cell(0, colIdx).node();
            if (maxCell) {
              maxCell.style.setProperty('background-color', '#2F80ED', 'important');
              maxCell.style.setProperty('color', '#000000', 'important');
              maxCell.style.setProperty('font-weight', 'bold', 'important');
            }
          }, 50);
        }
      ", metric_col_idx)
      
      datatable(
        table_data,
        rownames = FALSE,
        options = list(
          pageLength  = 10,
          dom         = "tip",
          columnDefs  = list(list(visible = FALSE, targets = c("Rank", "Top"))),
          drawCallback = JS(draw_js)
        )
      )
    })
    
    output$node_size_label <- renderText({
      paste("Node size =", input$centrality_metric)
    })
    
    output$edge_preview <- renderDT({
      edges <- filtered_edges()
      req(nrow(edges) > 0)
      
      edge_data <- aggregate(
        list(msgs = rep(1, nrow(edges))),
        by = list(From = edges$from, To = edges$to),
        FUN = sum
      )
      # remove self-loops (agent messaging themselves)
      edge_data <- edge_data[edge_data$From != edge_data$To, ]
      edge_data <- edge_data[order(-edge_data$msgs), ]
      edge_data <- head(edge_data, 10)
      names(edge_data)[names(edge_data) == "msgs"] <- "Messages"
      edge_data$From <- gsub("-Agent$", "", edge_data$From)
      edge_data$To   <- gsub("-Agent$", "", edge_data$To)
      
      draw_js <- "
        function() {
          var api = this.api();
          setTimeout(function() {
            api.column(2).nodes().each(function(cell) {
              cell.style.setProperty('background-color', 'transparent', 'important');
              cell.style.setProperty('color', '#1e293b', 'important');
              cell.style.setProperty('font-weight', 'normal', 'important');
            });
            var maxCell = api.cell(0, 2).node();
            if (maxCell) {
              maxCell.style.setProperty('background-color', '#2F80ED', 'important');
              maxCell.style.setProperty('color', '#000000', 'important');
              maxCell.style.setProperty('font-weight', 'bold', 'important');
            }
          }, 50);
        }
      "
      
      datatable(
        edge_data,
        rownames = FALSE,
        options = list(
          pageLength   = 10,
          dom          = "t",
          scrollX      = FALSE,
          columnDefs   = list(
            list(width = "42%", targets = 0),
            list(width = "42%", targets = 1),
            list(width = "16%", targets = 2, className = "dt-right")
          ),
          drawCallback = JS(draw_js)
        )
      )
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