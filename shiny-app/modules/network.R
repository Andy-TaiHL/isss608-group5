# ============================================================
# Network Analysis Module (Base R version - no tidyverse)
# Author: Fang Yu
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
  date <= as.Date("2046-05-22"), "Pre-Embargo",
  ifelse(date >= as.Date("2046-05-23") & date <= as.Date("2046-05-28"), "Embargo\u2192Leak",
         ifelse(date >= as.Date("2046-05-29") & date <= as.Date("2046-06-04"), "Post-Leak",
                ifelse(date == as.Date("2046-06-05"), "Crisis Day", "Other")))
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
        .bslib-sidebar-layout > .sidebar { font-size: 13px !important; }
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
        .bslib-sidebar-layout > .sidebar hr,
        .bslib-sidebar-layout .sidebar hr,
        .sidebar hr,
        aside hr { display: none !important; }
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
      
      tags$label(
        "Centrality Metric",
        style = "font-size:12px; margin-bottom:0; margin-top:4px; display:block;"
      ),
      div(style = "margin-top:-10px;",
          selectInput(ns("centrality_metric"), label = NULL,
                      choices  = c("Betweenness", "Degree", "Closeness", "Eigenvector"),
                      selected = "Betweenness")
      ),
      selectInput(ns("time_period"), "Time Period",
                  choices  = c("Entire Investigation", "Pre-Embargo",
                               "Embargo\u2192Leak", "Post-Leak", "Crisis Day"),
                  selected = "Entire Investigation"),
      
      selectInput(ns("agent_type"), "Agent Type",
                  choices  = c("All",
                               "Intern"          = "intern",
                               "Judge"           = "judge",
                               "Legal"           = "legal",
                               "Platform Trust"  = "platform_trust",
                               "PR"              = "pr",
                               "PR Intern"       = "pr_intern",
                               "Social Manager"  = "social_media"),
                  selected = "All"),
      
      checkboxInput(ns("show_labels"),    "Show Node Labels",  value = TRUE),
      checkboxInput(ns("highlight_hubs"), "Highlight Key Hubs", value = TRUE)
    ),
    
    tags$style(HTML("
      .nav-tabs .nav-link.active { background-color: #163D77 !important; color: white !important; }
      .nav-tabs .nav-link        { color: #2F80ED !important; }
      /* Sidebar right border (visible on Network Graph and Centrality Table tabs) */
      .bslib-sidebar-layout > .sidebar { border-right: 1px solid rgba(255,255,255,0.2) !important; }
      /* Hide sidebar, toggle AND resize handle on Pre vs Post Embargo tab */
      body.pp-tab-active .bslib-sidebar-layout > .sidebar,
      body.pp-tab-active .bslib-sidebar-toggle,
      body.pp-tab-active .collapse-toggle,
      body.pp-tab-active .sidebar-toggle,
      body.pp-tab-active button[aria-label*='sidebar'],
      body.pp-tab-active button[aria-label*='Sidebar'],
      body.pp-tab-active button[aria-label*='collapse'],
      body.pp-tab-active button[aria-label*='Collapse'],
      body.pp-tab-active .bslib-sidebar-layout > .sidebar-resizer,
      body.pp-tab-active [class*='resizer'],
      body.pp-tab-active [class*='resize-handle'] {
        display: none !important;
      }
      body.pp-tab-active .bslib-sidebar-layout > .main {
        grid-column: 1 / -1 !important;
        max-width: 100% !important;
      }
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
      /* Zoom buttons - white, pinned inside graph canvas */
      .vis-navigation {
        right: 120px !important;
        bottom: 10px !important;
        position: absolute !important;
      }
      .vis-navigation .vis-button {
        filter: grayscale(1) brightness(3) !important;
        opacity: 0.85 !important;
      }
      .vis-navigation .vis-button:hover {
        filter: grayscale(1) brightness(5) !important;
        opacity: 1 !important;
      }
      /* Hide directional nav buttons (up/down/left/right), keep zoom +/- */
      .vis-navigation .vis-button.vis-up,
      .vis-navigation .vis-button.vis-down,
      .vis-navigation .vis-button.vis-left,
      .vis-navigation .vis-button.vis-right {
        display: none !important;
      }
      /* Edge preview pagination - small font matching table */
      #network-edge_preview_wrapper .dataTables_paginate,
      #network-edge_preview_wrapper .dataTables_paginate * {
        font-size: 10px !important;
        font-family: Inter, sans-serif !important;
      }
      #network-edge_preview_wrapper .dataTables_paginate .paginate_button {
        padding: 2px 5px !important;
        min-width: 20px !important;
      }
      /* Suppress default margins inside pp tab flex column */
      #network-pp_main_interpretation,
      #network-pp_kpis {
        margin: 0 !important;
      }
      #network-pp_main_interpretation > *,
      #network-pp_kpis > * {
        margin-bottom: 0 !important;
      }
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
      /* Centrality table: uniform 12px Inter, compact padding, no-wrap agent column */
      #network-centrality_table_wrapper,
      #network-centrality_table_wrapper * {
        font-size: 12px !important;
        font-family: Inter, sans-serif !important;
      }
      #network-centrality_table td,
      #network-centrality_table th { padding: 4px 8px !important; }
      #network-centrality_table th { padding-right: 20px !important; }
      #network-centrality_table th.sorting::before,
      #network-centrality_table th.sorting::after,
      #network-centrality_table th.sorting_asc::before,
      #network-centrality_table th.sorting_asc::after,
      #network-centrality_table th.sorting_desc::before,
      #network-centrality_table th.sorting_desc::after {
        font-size: 9px !important;
        line-height: 1 !important;
      }
      #network-centrality_table td:first-child { white-space: nowrap !important; }
    ")),
    
    # MutationObserver watching document.body (not the vis container, which may not
    # exist yet at ready time) — forces dark tooltip style via setProperty('important')
    tags$script(HTML("
      // Register BEFORE document.ready so Shiny message is never missed
      Shiny.addCustomMessageHandler('toggleSidebar', function(msg) {
        if (msg.collapse) {
          document.body.classList.add('pp-tab-active');
        } else {
          document.body.classList.remove('pp-tab-active');
        }
        // Directly hide/show any toggle button regardless of bslib version
        ['.bslib-sidebar-toggle','.collapse-toggle','.sidebar-toggle'].forEach(function(sel) {
          document.querySelectorAll(sel).forEach(function(el) {
            el.style.display = msg.collapse ? 'none' : '';
          });
        });
      });

      $(document).ready(function() {
        // Permanently remove bslib-injected HR from sidebar using MutationObserver
        function removeSidebarHR() {
          document.querySelectorAll('.bslib-sidebar-layout hr, .sidebar hr, aside hr').forEach(function(el) {
            el.parentNode.removeChild(el);
          });
        }
        removeSidebarHR();
        var hrObserver = new MutationObserver(removeSidebarHR);
        hrObserver.observe(document.body, { childList: true, subtree: true });

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
      id = ns("main_tabs"),
      nav_panel(
        "Network Graph",
        # Agent highlight control + selection summary — one row
        div(style = "display:flex; flex-wrap:wrap; gap:8px; align-items:center; padding:6px 0 4px 0;",
            tags$label("Highlight Agent Path:",
                       style = "color:#BFC7D5; font-size:13px; font-family:Inter,sans-serif;
                                font-weight:600; white-space:nowrap; margin:0;"),
            div(style = "width:140px; margin-bottom:-15px;",
                selectizeInput(ns("sel_node_id"), NULL,
                               choices  = c("(all agents)" = "",
                                            setNames(
                                              sort(unique(c(reply_edges$from, reply_edges$to))),
                                              gsub("-Agent$", "", sort(unique(c(reply_edges$from, reply_edges$to))))
                                            )),
                               selected = "",
                               options  = list(placeholder = "(all agents)")
                )
            ),
            tags$button("Reset", id = ns("reset_selection"),
                        class = "action-button",
                        style = "font-size:13px; font-family:Inter,sans-serif;
                                 padding:0 10px; height:34px; line-height:34px;
                                 background:#1e3a5f; color:#BFC7D5;
                                 border:1px solid #2F80ED; border-radius:4px;
                                 cursor:pointer; flex-shrink:0;
                                 margin-right:20px;"),
            # Summary inline on same row
            uiOutput(ns("selection_summary"))
        ),
        div(style = "display:flex; gap:0; align-items:flex-start; overflow:hidden; margin-top:8px;",
            
            div(style = "flex:1 1 0; min-width:0; position:relative;",
                visNetworkOutput(ns("network_graph"), height = "420px")
            ),
            
            div(style = "flex:0 0 280px; width:280px; overflow-x:auto; align-self:flex-start;",
                tags$p("Communication Links",
                       style = "color:white; font-size:13px; font-family:Inter,sans-serif;
                       font-weight:600; margin:0 0 6px 0; letter-spacing:0.02em;"),
                tags$style(HTML("
              #network-edge_preview table { font-size:11px !important; }
              #network-edge_preview td, #network-edge_preview th {
                white-space: nowrap !important;
                padding: 4px 5px !important;
              }
              #network-edge_preview th:last-child {
                padding-right: 18px !important;
              }
              #network-edge_preview tr.selected td,
              #network-edge_preview tr.selected td:hover,
              #network-edge_preview_wrapper table tbody tr.selected td,
              #network-edge_preview_wrapper table tbody tr.selected > td {
                background-color: #56CCF2 !important;
                color: #0a1628 !important;
                box-shadow: none !important;
              }
              /* Centrality table row selection */
              #network-centrality_table tr.selected td,
              #network-centrality_table tr.selected td:hover,
              #network-centrality_table_wrapper table tbody tr.selected td,
              #network-centrality_table_wrapper table tbody tr.selected > td {
                background-color: #56CCF2 !important;
                color: #0a1628 !important;
                box-shadow: none !important;
              }
              #network-edge_preview_wrapper .dataTables_paginate,
              #network-edge_preview_wrapper .dataTables_paginate * {
                font-size: 10px !important;
                font-family: Inter, sans-serif !important;
              }
              #network-edge_preview_wrapper .dataTables_paginate .paginate_button {
                padding: 1px 5px !important;
                min-width: 18px !important;
              }
            ")),
                DTOutput(ns("edge_preview"))
            )
        )  # end inner flex
      ),  # end nav_panel Network Graph
      
      nav_panel(
        "Centrality Table",
        
        tags$style(HTML("
          #network-ct_summary_bar, #network-ct_interpretation {
            margin-bottom: 0 !important;
            margin-top: 0 !important;
          }
          #network-ct_summary_bar > *, #network-ct_interpretation > * {
            margin-bottom: 0 !important;
          }
        ")),
        # Full-width interpretation panel
        uiOutput(ns("ct_interpretation")),
        
        div(style = "display:flex; gap:16px; align-items:flex-start; padding:2px 0 0 0;
                     overflow:hidden;",
            
            # Left column: centrality table
            div(style = "flex:1 1 0; min-width:0; overflow:hidden;",
                DTOutput(ns("centrality_table"))
            ),
            
            # Right column: supporting records
            div(style = "flex:1 1 0; min-width:0; overflow:hidden;",
                tags$p("Supporting Communication Records",
                       style = "font-size:14px; font-family:Inter,sans-serif; font-weight:600;
                       color:white; margin:0 0 4px 0;"),
                uiOutput(ns("supporting_label")),
                DTOutput(ns("supporting_records"))
            )
        )
      ),
      
      nav_panel(
        "Pre vs Post Embargo",
        
        div(style = "display:flex; flex-direction:column; gap:10px; padding-top:8px;",
            
            # ── Main interpretation banner ──────────────────────────────
            uiOutput(ns("pp_main_interpretation")),
            
            # ── Agent Type selector ─────────────────────────────────────
            div(style="display:flex; align-items:center; gap:12px;",
                tags$label("Agent Type:",
                           style="color:#BFC7D5;font-size:12px;font-family:Inter,sans-serif;
                     font-weight:600;white-space:nowrap;margin:0;"),
                selectInput(ns("pp_agent_type"), NULL,
                            choices  = c("All",
                                         "Intern"         = "intern",
                                         "Judge"          = "judge",
                                         "Legal"          = "legal",
                                         "Platform Trust" = "platform_trust",
                                         "PR"             = "pr",
                                         "PR Intern"      = "pr_intern",
                                         "Social Manager" = "social_media"),
                            selected = "All", width = "180px"),
                tags$span("Node size = Degree centrality",
                          style = "color:#7A8FA6; font-size:11px; font-family:Inter,sans-serif;
                                 font-style:italic;")
            ),
            
            # ── KPI Cards ──────────────────────────────────────────────
            uiOutput(ns("pp_kpis")),
            
            # ── Networks + Observations ─────────────────────────────────
            div(style = "display:flex; gap:8px; align-items:flex-start;",
                
                # PRE network
                div(style = "flex:1; min-width:0;",
                    tags$p("PRE-EMBARGO  (Before 23 May 2046)",
                           style = "color:#56CCF2; font-size:11px; font-family:Inter,sans-serif;
                       font-weight:700; text-align:center; margin:0 0 2px 0;"),
                    div(style = "width:100%; height:270px; overflow:hidden; pointer-events:auto;",
                        visNetworkOutput(ns("pp_pre_net"), height = "270px")
                    )
                ),
                
                # POST network
                div(style = "flex:1; min-width:0;",
                    tags$p("POST-EMBARGO  (After 23 May 2046)",
                           style = "color:#F2994A; font-size:11px; font-family:Inter,sans-serif;
                       font-weight:700; text-align:center; margin:0 0 2px 0;"),
                    div(style = "width:100%; height:270px; overflow:hidden; pointer-events:auto;",
                        visNetworkOutput(ns("pp_post_net"), height = "270px")
                    )
                ),
                
                # Observations
                div(style = "flex:1; min-width:0;",
                    tags$p("Overall Observations",
                           style = "color:white; font-size:12px; font-family:Inter,sans-serif;
                       font-weight:600; margin:0 0 6px 0;"),
                    uiOutput(ns("pp_observations"))
                )
            )  # end networks flex
        )  # end outer flex column
      )  # end nav_panel Pre vs Post Embargo
    )  # end navset_tab
  )  # end layout_sidebar
}  # end networkUI

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
    
    # ── helpers to build node_data from edge + metric data ──────────────
    build_node_data <- function(edge_data, metric_data, metric, highlight_hubs, show_labels) {
      node_ids  <- unique(c(edge_data$from, edge_data$to))
      nd <- data.frame(id = node_ids, label = node_ids, stringsAsFactors = FALSE)
      nd <- merge(nd, metric_data[, c("Agent","Betweenness","Degree","Closeness","Eigenvector")],
                  by.x = "id", by.y = "Agent", all.x = TRUE)
      
      metric_vals  <- nd[[metric]]
      metric_range <- range(metric_vals, na.rm = TRUE)
      metric_spread <- diff(metric_range)
      
      nd$value <- if (metric_spread == 0 || is.na(metric_spread)) 20 else
        10 + 40 * (metric_vals - metric_range[1]) / metric_spread
      
      ramp <- colorRampPalette(c("#163D77", "#2F80ED", "#56CCF2"))(100)
      nd$color_bg <- if (metric_spread == 0 || is.na(metric_spread)) ramp[50] else {
        idx <- pmax(1, pmin(100, round((metric_vals - metric_range[1]) / metric_spread * 99) + 1))
        ramp[idx]
      }
      
      hub_threshold      <- quantile(metric_vals, 0.75, na.rm = TRUE)
      is_hub             <- !is.na(metric_vals) & metric_vals >= hub_threshold & highlight_hubs
      nd$color_bg[is_hub]  <- "#56CCF2"
      nd$color_border       <- ifelse(is_hub, "#ffffff", "#BFC7D5")
      nd$group              <- ifelse(is_hub, "Key Hub", "Other Agent")
      
      nd$title <- paste0(
        "<div style='background:#1a2a45;color:white;padding:8px;border-radius:6px;",
        "font-family:Inter,sans-serif;min-width:160px;'>",
        "<b style='font-size:13px;'>", nd$id, "</b><br/>",
        "<hr style='border-color:#2F80ED;margin:4px 0;'/>",
        "Betweenness: <b>", nd$Betweenness, "</b><br/>",
        "Degree: <b>",      nd$Degree,      "</b><br/>",
        "Closeness: <b>",   nd$Closeness,   "</b><br/>",
        "Eigenvector: <b>", nd$Eigenvector, "</b>",
        "</div>"
      )
      
      nd$label <- if (!show_labels) "" else nd$id
      nd
    }
    
    # ── INITIAL full render — only rebuilds when edges change (time/agent filter) ──
    output$network_graph <- renderVisNetwork({
      
      edge_agg <- aggregate(
        list(weight = rep(1, nrow(filtered_edges()))),
        by = list(from = filtered_edges()$from, to = filtered_edges()$to),
        FUN = sum
      )
      edge_agg <- edge_agg[edge_agg$weight > 0 & edge_agg$from != edge_agg$to, ]
      req(nrow(edge_agg) > 0)
      
      metric_data <- calculate_metrics(filtered_edges())
      nd <- build_node_data(edge_agg, metric_data,
                            input$centrality_metric, input$highlight_hubs, input$show_labels)
      
      edge_agg$width <- 1 + 5 * (edge_agg$weight - min(edge_agg$weight)) /
        max(1, diff(range(edge_agg$weight)))
      edge_agg$title <- paste0(
        "<div style='background:#1a2a45;color:white;padding:6px 10px;",
        "border-radius:6px;font-family:Inter,sans-serif;font-size:12px;'>",
        "Messages: <b>", edge_agg$weight, "</b></div>"
      )
      
      visNetwork(nodes = nd, edges = edge_agg, height = "420px", background = "#0a1628") %>%
        visNodes(
          shape  = "dot",
          color  = list(background = nd$color_bg, border = nd$color_border,
                        highlight  = list(background = "#F2994A", border = "#ffffff")),
          font   = list(color = "white", size = 13, bold = FALSE,
                        strokeWidth = 3, strokeColor = "#0a1628"),
          shadow = list(enabled = TRUE, size = 8)
        ) %>%
        visEdges(
          arrows = "to",
          smooth = list(enabled = TRUE, type = "curvedCW", roundness = 0.2),
          color  = list(color = "#4A5568", highlight = "#F2994A", hover = "#56CCF2"),
          font   = list(color = "white", size = 11)
        ) %>%
        visOptions(
          highlightNearest = list(enabled = TRUE, degree = 1, hover = FALSE)
        ) %>%
        visPhysics(
          solver           = "forceAtlas2Based",
          forceAtlas2Based = list(gravitationalConstant = -80, centralGravity = 0.005,
                                  springLength = 180, springConstant = 0.05, damping = 0.6),
          stabilization    = list(enabled = TRUE, iterations = 300)
        ) %>%
        visInteraction(navigationButtons = FALSE, keyboard = FALSE,
                       tooltipDelay = 100, zoomView = FALSE,
                       dragView = TRUE, dragNodes = TRUE)
    })
    
    # ── Selection summary bar ──────────────────────────────────────────
    output$selection_summary <- renderUI({
      metric      <- input$centrality_metric %||% "Betweenness"
      time_period <- input$time_period %||% "Entire Investigation"
      agent_type  <- input$agent_type  %||% "All"
      
      div(style = "display:inline-flex; gap:12px; align-items:center;
                   padding:3px 10px;
                   background:#0f2035; border-radius:4px;
                   border:1px solid #1e3a5f; font-family:Inter,sans-serif;
                   width:fit-content; white-space:nowrap;",
          tags$span(
            tags$span("Node Size: ", style = "color:#7A8FA6; font-size:13px;"),
            tags$span(metric, style = "color:#56CCF2; font-size:13px; font-weight:600;")
          ),
          tags$span(style = "color:#1e3a5f;", "|"),
          tags$span(
            tags$span("Time Period: ", style = "color:#7A8FA6; font-size:13px;"),
            tags$span(time_period, style = "color:#56CCF2; font-size:13px; font-weight:600;")
          ),
          tags$span(style = "color:#1e3a5f;", "|"),
          tags$span(
            tags$span("Agent Type: ", style = "color:#7A8FA6; font-size:13px;"),
            tags$span(agent_type, style = "color:#56CCF2; font-size:13px; font-weight:600;")
          )
      )
    })
    
    # ── Agent highlight via proxy ──────────────────────────────────────
    observeEvent(input$sel_node_id, {
      proxy <- visNetworkProxy(session$ns("network_graph"))
      if (is.null(input$sel_node_id) || input$sel_node_id == "") {
        visSelectNodes(proxy, id = list())
      } else {
        visSelectNodes(proxy, id = input$sel_node_id, highlightEdges = TRUE)
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$reset_selection, {
      updateSelectizeInput(session, "sel_node_id", selected = "")
      visNetworkProxy(session$ns("network_graph")) %>%
        visSelectNodes(id = list())
    })
    
    observeEvent(input$sel_group, {
      proxy <- visNetworkProxy(session$ns("network_graph"))
      if (is.null(input$sel_group) || input$sel_group == "") {
        visSelectNodes(proxy, id = list())
      } else {
        edge_agg <- aggregate(
          list(weight = rep(1, nrow(filtered_edges()))),
          by = list(from = filtered_edges()$from, to = filtered_edges()$to),
          FUN = sum
        )
        edge_agg <- edge_agg[edge_agg$weight > 0 & edge_agg$from != edge_agg$to, ]
        metric_data <- calculate_metrics(filtered_edges())
        nd <- build_node_data(edge_agg, metric_data,
                              input$centrality_metric, input$highlight_hubs, input$show_labels)
        ids_in_group <- nd$id[nd$group == input$sel_group]
        visSelectNodes(proxy, id = ids_in_group)
      }
    }, ignoreInit = TRUE)
    
    # ── PROXY update — fires when only metric/hubs/labels change ──
    observeEvent(list(input$centrality_metric, input$highlight_hubs, input$show_labels), {
      edge_agg <- aggregate(
        list(weight = rep(1, nrow(filtered_edges()))),
        by = list(from = filtered_edges()$from, to = filtered_edges()$to),
        FUN = sum
      )
      edge_agg <- edge_agg[edge_agg$weight > 0 & edge_agg$from != edge_agg$to, ]
      if (nrow(edge_agg) == 0) return()
      
      metric_data <- calculate_metrics(filtered_edges())
      nd <- build_node_data(edge_agg, metric_data,
                            input$centrality_metric, input$highlight_hubs, input$show_labels)
      
      visNetworkProxy(session$ns("network_graph")) %>%
        visUpdateNodes(nodes = nd[, c("id","label","value","color_bg","color_border","title","group")])
    }, ignoreInit = TRUE)
    
    
    # ── Centrality Table summary bar ───────────────────────────────────
    output$ct_summary_bar <- renderUI({
      metric      <- input$centrality_metric %||% "Betweenness"
      time_period <- input$time_period       %||% "Entire Investigation"
      agent_type  <- input$agent_type        %||% "All"
      
      agent_label <- switch(agent_type,
                            "intern"         = "Intern",
                            "judge"          = "Judge",
                            "legal"          = "Legal",
                            "platform_trust" = "Platform Trust",
                            "pr"             = "PR",
                            "pr_intern"      = "PR Intern",
                            "social_media"   = "Social Manager",
                            agent_type
      )
      
      div(style = "display:inline-flex; gap:12px; align-items:center;
                   padding:3px 10px;
                   background:#0f2035; border-radius:4px;
                   border:1px solid #1e3a5f; font-family:Inter,sans-serif;
                   width:fit-content; white-space:nowrap;",
          tags$span(
            tags$span("Centrality Metric: ", style = "color:#7A8FA6; font-size:13px;"),
            tags$span(metric, style = "color:#56CCF2; font-size:13px; font-weight:600;")
          ),
          tags$span(style = "color:#2a4a6b;", "|"),
          tags$span(
            tags$span("Time Period: ", style = "color:#7A8FA6; font-size:13px;"),
            tags$span(time_period, style = "color:#56CCF2; font-size:13px; font-weight:600;")
          ),
          tags$span(style = "color:#2a4a6b;", "|"),
          tags$span(
            tags$span("Agent Type: ", style = "color:#7A8FA6; font-size:13px;"),
            tags$span(agent_label, style = "color:#56CCF2; font-size:13px; font-weight:600;")
          )
      )
    })
    
    # ── Centrality Table interpretation — rule-based summary ──────────
    output$ct_interpretation <- renderUI({
      metrics     <- filtered_metrics()
      time_period <- input$time_period
      agent_type  <- input$agent_type
      metric      <- input$centrality_metric
      if (nrow(metrics) == 0) return(NULL)
      
      m <- metrics
      m$Agent <- gsub("-Agent$", "", m$Agent)
      
      # Sort by selected metric to get correct top agent
      m <- m[order(-m[[metric]]), ]
      
      top_btwn <- m$Agent[which.max(m$Betweenness)]
      top_deg  <- m$Agent[which.max(m$Degree)]
      top_eig  <- m$Agent[which.max(m$Eigenvector)]
      top_met  <- m$Agent[1]   # top for currently selected metric
      
      val_btwn <- max(m$Betweenness, na.rm = TRUE)
      val_deg  <- max(m$Degree,      na.rm = TRUE)
      val_eig  <- round(max(m$Eigenvector, na.rm = TRUE), 2)
      val_met  <- m[[metric]][1]
      
      # Metric-specific labels and sentences
      metric_label <- switch(metric,
                             Betweenness  = "Primary Bridge",
                             Degree       = "Most Connected",
                             Closeness    = "Most Reachable",
                             Eigenvector  = "Most Influential"
      )
      metric_sentence <- switch(metric,
                                Betweenness = paste0(top_met, " (Betweenness = ", val_btwn, ") served as the main intermediary connecting communication paths and is a key candidate for tracing information flow."),
                                Degree      = paste0(top_met, " (Degree = ", val_deg, ") interacted directly with the largest number of agents, indicating a strong coordination role."),
                                Closeness   = paste0(top_met, " (Closeness = ", round(val_met,2), ") can reach all other agents most efficiently, making it a fast information disseminator."),
                                Eigenvector = paste0(top_met, " (Eigenvector = ", val_eig, ") was closely connected to other influential agents, suggesting strategic involvement within the communication network.")
      )
      
      # Period context
      period_note <- switch(time_period,
                            "Pre-Embargo"          = "During normal pre-embargo operations, communication patterns reflect routine coordination.",
                            "Embargo\u2192Leak"    = "With the embargo active, communication intensified as agents coordinated around information control.",
                            "Post-Leak"            = "Following the @Elena leak, agents shifted focus to damage control and crisis response.",
                            "Crisis Day"           = "On the crisis day (5 Jun), communication was at its most concentrated as the formal breach unfolded.",
                            "Entire Investigation" = "Across the full investigation period, patterns reflect both routine and crisis-driven communication."
      )
      
      # Decide which bullets to show:
      # If only metric changed (time = Entire Investigation, agent = All) → show only selected metric bullet
      # Otherwise → show all three bullets
      only_metric <- (time_period == "Entire Investigation" && agent_type == "All")
      
      bullet <- function(label, text) {
        tags$p(
          tags$span("\u25C6 ", style = "color:#56CCF2; font-weight:bold;"),
          tags$span(paste0(label, ": "), style = "color:#E2E8F0; font-weight:bold; font-size:12px;"),
          tags$span(text, style = "color:#E2E8F0; font-size:12px; font-family:Inter,sans-serif;"),
          style = "margin:0 0 8px 0; line-height:1.6; font-family:Inter,sans-serif;"
        )
      }
      
      bullets <- if (only_metric) {
        list(bullet(metric_label, metric_sentence))
      } else {
        list(
          bullet("Primary Bridge",   paste0(top_btwn, " (Betweenness = ", val_btwn, ") served as the main intermediary connecting communication paths and is a key candidate for tracing information flow.")),
          bullet("Most Connected",   paste0(top_deg,  " (Degree = ", val_deg, ") interacted directly with the largest number of agents, indicating a coordination role.")),
          bullet("Most Influential", paste0(top_eig,  " (Eigenvector = ", val_eig, ") was closely connected to other influential agents, suggesting strategic involvement within the communication network."))
        )
      }
      
      div(style = "background:#0f1f3d; border-left:3px solid #56CCF2;
                   border-radius:4px; padding:8px 14px; margin-bottom:0;",
          tags$p("Centrality Table Interpretation",
                 style = "color:#56CCF2; font-size:11px; font-weight:700;
                          font-family:Inter,sans-serif; margin:0 0 8px 0;
                          text-transform:uppercase; letter-spacing:0.06em;"),
          tags$p(period_note,
                 style = "color:#7A8FA6; font-size:11px; font-family:Inter,sans-serif;
                          margin:0 0 8px 0; font-style:italic;"),
          tagList(bullets)
      )
    })
    
    # ── KPI cards (unused — replaced by ct_interpretation) ────────────
    output$kpi_cards <- renderUI({
      metrics <- filtered_metrics()
      if (nrow(metrics) == 0) return(NULL)
      
      metric_cols  <- c("Betweenness", "Degree", "Closeness", "Eigenvector")
      metric_color <- c("#56CCF2", "#2F80ED", "#F2994A", "#27AE60")
      
      cards <- lapply(seq_along(metric_cols), function(i) {
        m         <- metric_cols[i]
        top_idx   <- which.max(metrics[[m]])
        top_agent <- gsub("-Agent$", "", metrics$Agent[top_idx])
        top_val   <- metrics[[m]][top_idx]
        
        div(style = paste0(
          "flex:1; background:#0f1f3d; border-top: 3px solid ", metric_color[i], ";",
          "border-left:1px solid #1e3a5f; border-right:1px solid #1e3a5f;",
          "border-bottom:1px solid #1e3a5f; border-radius:0 0 6px 6px;",
          "padding:10px 14px; min-width:0;"
        ),
        tags$p(m,
               style = paste0("color:", metric_color[i], "; font-size:10px;",
                              "font-family:Inter,sans-serif; font-weight:700; margin:0 0 4px 0;",
                              "text-transform:uppercase; letter-spacing:0.08em;")),
        tags$p(top_agent,
               style = "color:white; font-size:13px; font-family:Inter,sans-serif;
                     font-weight:700; margin:0 0 3px 0; line-height:1.3;"),
        tags$p(paste("Score:", top_val),
               style = "color:#BFC7D5; font-size:11px; font-family:Inter,sans-serif; margin:0;")
        )
      })
      
      div(style = "display:flex; gap:8px; margin-bottom:4px;",
          cards[[1]], cards[[2]], cards[[3]], cards[[4]])
    })
    
    output$centrality_table <- renderDT({
      
      metric     <- input$centrality_metric
      table_data <- filtered_metrics()
      
      # add hidden rank/top columns for top-3 row tracking
      n_top <- min(3, nrow(table_data))
      table_data$Rank <- seq_len(nrow(table_data))
      table_data$Top  <- ifelse(table_data$Rank <= n_top, "Top Agent", "Other")
      # strip -Agent suffix for cleaner display
      table_data$Agent <- gsub("-Agent$", "", table_data$Agent)
      
      # 0-based column index for JS (Agent=0, Betweenness=1, Degree=2, Closeness=3, Eigenvector=4)
      metric_col_idx <- which(names(table_data) == metric) - 1L
      
      # JS drawCallback: runs 50ms after draw.dt handler has reset all colors,
      # then uses setProperty('important') to force orange on the max-value cell
      # — inline !important beats any stylesheet !important rule
      # Only highlight column when on Entire Investigation + All (i.e. default state)
      # When time_period or agent_type is filtered, skip all cell highlighting
      time_period <- input$time_period
      agent_type  <- input$agent_type
      do_highlight <- (time_period == "Entire Investigation" && agent_type == "All")
      
      draw_js <- sprintf("
        function() {
          var api = this.api();
          var colIdx = %d;
          var doHighlight = %s;
          setTimeout(function() {
            var wrapper = api.table().container();
            wrapper.querySelectorAll('*').forEach(function(el) {
              el.style.setProperty('font-size', '12px', 'important');
              el.style.setProperty('font-family', 'Inter, sans-serif', 'important');
            });
            // reset all metric columns
            [1,2,3,4].forEach(function(ci) {
              api.column(ci).nodes().each(function(cell) {
                cell.style.setProperty('background-color', 'transparent', 'important');
                cell.style.setProperty('color', '#1e293b', 'important');
                cell.style.setProperty('font-weight', 'normal', 'important');
              });
            });
            // clear inline bg/color on ALL rows first, then re-apply to selected only
            api.rows().nodes().each(function(row) {
              Array.from(row.cells).forEach(function(cell) {
                cell.style.removeProperty('background-color');
                cell.style.removeProperty('color');
              });
            });
            // re-apply column highlight after clear
            if (doHighlight) {
              api.column(colIdx).nodes().each(function(cell) {
                cell.style.setProperty('background-color', '#DBEAFE', 'important');
                cell.style.setProperty('color', '#1e293b', 'important');
              });
              var maxCell = api.cell(0, colIdx).node();
              if (maxCell) {
                maxCell.style.setProperty('font-weight', 'bold', 'important');
              }
            }
            // apply selection colour to currently selected row only
            api.rows('.selected').nodes().each(function(row) {
              Array.from(row.cells).forEach(function(cell) {
                cell.style.setProperty('background-color', '#56CCF2', 'important');
                cell.style.setProperty('color', '#0a1628', 'important');
              });
            });
          }, 50);
        }
      ", metric_col_idx, tolower(as.character(do_highlight)))
      
      datatable(
        table_data,
        rownames  = FALSE,
        selection = list(mode = "single", target = "row"),
        options   = list(
          pageLength   = 10,
          dom          = "tip",
          autoWidth    = FALSE,
          columnDefs   = list(
            list(visible = FALSE, targets = c("Rank", "Top")),
            list(width = "30%", targets = 0),
            list(width = "18%", targets = 1),
            list(width = "14%", targets = 2),
            list(width = "18%", targets = 3),
            list(width = "18%", targets = 4)
          ),
          drawCallback = JS(draw_js),
          rowCallback  = JS("
            function(row, data, index) {
              $(row).on('click', function() {
                setTimeout(function() {
                  // trigger a draw so drawCallback re-runs and clears stale highlights
                  $(row).closest('table').DataTable().draw(false);
                }, 20);
              });
            }
          ")
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
      names(edge_data)[names(edge_data) == "msgs"] <- "Messages"
      edge_data$From <- gsub("-Agent$", "", edge_data$From)
      edge_data$To   <- gsub("-Agent$", "", edge_data$To)
      
      draw_js <- "
        function() {
          var api = this.api();
          setTimeout(function() {
            // uniform font across all cells and pagination
            var wrapper = api.table().container();
            wrapper.querySelectorAll('*').forEach(function(el) {
              el.style.setProperty('font-size', '11px', 'important');
              el.style.setProperty('font-family', 'Inter, sans-serif', 'important');
            });
            // re-apply selected row colour after redraw
            api.rows('.selected').nodes().each(function(row) {
              Array.from(row.cells).forEach(function(cell) {
                cell.style.setProperty('background-color', '#56CCF2', 'important');
                cell.style.setProperty('color', '#0a1628', 'important');
              });
            });
          }, 50);
        }
      "
      
      datatable(
        edge_data,
        rownames  = FALSE,
        selection = list(mode = "single", style = "background-color: #56CCF2 !important; color: #0a1628 !important;"),
        options = list(
          pageLength   = 15,
          dom          = "tp",
          scrollX      = FALSE,
          columnDefs   = list(
            list(width = "42%", targets = 0),
            list(width = "42%", targets = 1),
            list(width = "16%", targets = 2, className = "dt-right")
          ),
          drawCallback = JS(draw_js),
          initComplete = JS("
            function(settings) {
              var api = this.api();
              var GREEN = '#56CCF2'; var TEXT = '#0a1628';
              function applyGreen() {
                $(api.rows('.selected').nodes()).find('td').each(function() {
                  this.style.setProperty('background-color', GREEN, 'important');
                  this.style.setProperty('color', 'white', 'important');
                });
              }
              $(api.table().body()).on('click', 'tr', function() {
                var $tr = $(this);
                // if already selected, deselect
                if ($tr.hasClass('selected')) {
                  api.rows($tr).deselect();
                  $tr.find('td').each(function() {
                    this.style.removeProperty('background-color');
                    this.style.removeProperty('color');
                  });
                } else {
                  api.rows().deselect();
                  api.rows($tr).select();
                  setTimeout(applyGreen, 10);
                }
              });
            }
          ")
        )
      )
    })
    
    output$supporting_label <- renderUI({
      metrics <- filtered_metrics()
      sel_row <- input$centrality_table_rows_selected
      
      if (length(sel_row) > 0 && nrow(metrics) >= sel_row) {
        agent <- metrics$Agent[sel_row]
        tags$p(
          paste0("Showing communications involving ", agent,
                 " — click a different row to change."),
          style = "font-size:12px; font-family:Inter,sans-serif; color:#56CCF2; margin-bottom:6px;"
        )
      } else {
        top_agent <- metrics$Agent[1]
        tags$p(
          paste0("Showing communications for top agent: ", top_agent,
                 " — click any row in the table on the left to filter by that agent."),
          style = "font-size:12px; font-family:Inter,sans-serif; color:#BFC7D5; margin-bottom:6px;"
        )
      }
    })
    
    output$supporting_records <- renderDT({
      metrics <- filtered_metrics()
      sel_row <- input$centrality_table_rows_selected
      
      # use clicked agent, or default to top agent by selected metric
      if (length(sel_row) > 0 && nrow(metrics) >= sel_row) {
        selected_agent <- metrics$Agent[sel_row]
      } else {
        selected_agent <- metrics$Agent[1]
      }
      
      records <- filtered_edges()
      records <- records[records$from == selected_agent | records$to == selected_agent, ]
      records <- records[order(-as.numeric(records$timestamp)), ]
      
      records <- data.frame(
        Timestamp    = format(as.POSIXct(records$timestamp, tz = "UTC"), "%Y-%m-%d %H:%M"),
        From         = records$from,
        To           = records$to,
        Channel      = records$channel,
        `Message Type` = records$message_type,
        check.names  = FALSE
      )
      
      font_js <- "
        function() {
          var wrapper = this.api().table().container();
          wrapper.querySelectorAll('*').forEach(function(el) {
            el.style.setProperty('font-size', '10px', 'important');
            el.style.setProperty('font-family', 'Inter, sans-serif', 'important');
          });
        }
      "
      
      datatable(
        records,
        rownames = FALSE,
        options  = list(
          pageLength   = 5,
          dom          = "tip",
          drawCallback = JS(font_js)
        )
      )
    })
    # Auto-collapse sidebar on Pre vs Post tab, restore on others
    observeEvent(input$main_tabs, {
      if (isTRUE(input$main_tabs == "Pre vs Post Embargo")) {
        session$sendCustomMessage("toggleSidebar", list(collapse = TRUE))
      } else {
        session$sendCustomMessage("toggleSidebar", list(collapse = FALSE))
      }
    }, ignoreInit = TRUE)
    
    # base_edges: applies pp_agent_type (inline control) but NOT Time Period
    base_edges <- reactive({
      edge_data <- reply_edges
      if (!is.null(input$pp_agent_type) && input$pp_agent_type != "All") {
        selected_agents <- unique(
          communications_tbl$agent_label[communications_tbl$agent_role == input$pp_agent_type])
        edge_data <- edge_data[
          edge_data$from %in% selected_agents | edge_data$to %in% selected_agents, ]
      }
      edge_data
    })
    
    # ── PRE vs POST EMBARGO ────────────────────────────────────────────
    
    EMBARGO_DATE <- as.Date("2046-05-23")
    
    pre_edges_data <- reactive({
      e <- base_edges()
      e[!is.na(e$date) & e$date < EMBARGO_DATE, ]
    })
    
    post_edges_data <- reactive({
      e <- base_edges()
      e[!is.na(e$date) & e$date >= EMBARGO_DATE, ]
    })
    
    compute_net_kpis <- function(edge_data) {
      if (is.null(edge_data) || nrow(edge_data) == 0)
        return(list(agents=0, messages=0, density=0, avg_deg=0))
      net_e <- aggregate(list(w=rep(1,nrow(edge_data))),
                         by=list(from=edge_data$from, to=edge_data$to), FUN=sum)
      net_e <- net_e[net_e$from != net_e$to, ]
      if (nrow(net_e)==0)
        return(list(agents=0, messages=nrow(edge_data), density=0, avg_deg=0))
      nodes <- unique(c(net_e$from, net_e$to))
      g <- graph_from_data_frame(net_e, vertices=data.frame(name=nodes), directed=TRUE)
      list(agents=vcount(g), messages=nrow(edge_data),
           density=round(edge_density(g),2), avg_deg=round(mean(degree(g,mode="all")),1))
    }
    
    pp_kpi_card <- function(label, pre, post) {
      delta <- post - pre
      # when PRE=0, show "NEW" instead of NA%
      if (pre == 0 && post > 0) {
        ptxt <- "\u25b2 NEW"; acol <- "#27AE60"
      } else if (pre == 0 && post == 0) {
        ptxt <- "\u2500"; acol <- "#BFC7D5"
      } else {
        pct  <- round(abs(delta)/pre*100)
        arrow <- if (delta>0) "\u25b2" else if (delta<0) "\u25bc" else "\u2500"
        acol  <- if (delta>0) "#27AE60" else if (delta<0) "#E74C3C" else "#BFC7D5"
        ptxt  <- paste0(arrow," ",pct,"%")
      }
      div(style="flex:1;background:#0f1f3d;border:1px solid #1e3a5f;border-radius:6px;padding:6px 10px;",
          tags$p(label, style="color:#56CCF2;font-size:10px;font-family:Inter,sans-serif;
                              font-weight:700;text-transform:uppercase;letter-spacing:0.06em;
                              margin:0 0 4px 0;text-align:center;"),
          div(style="display:flex;justify-content:space-around;align-items:center;",
              div(style="text-align:center;",
                  tags$p("PRE",style="color:#BFC7D5;font-size:10px;font-family:Inter,sans-serif;margin:0;"),
                  tags$p(as.character(pre),style="color:white;font-size:15px;font-family:Inter,sans-serif;font-weight:700;margin:0;")),
              tags$p(ptxt,style=paste0("color:",acol,";font-size:12px;font-family:Inter,sans-serif;font-weight:700;margin:0;")),
              div(style="text-align:center;",
                  tags$p("POST",style="color:#BFC7D5;font-size:10px;font-family:Inter,sans-serif;margin:0;"),
                  tags$p(as.character(post),style="color:white;font-size:15px;font-family:Inter,sans-serif;font-weight:700;margin:0;"))
          )
      )
    }
    
    output$pp_kpis <- renderUI({
      pre  <- compute_net_kpis(pre_edges_data())
      post <- compute_net_kpis(post_edges_data())
      div(style="display:flex;gap:6px;",
          pp_kpi_card("Active Agents",   pre$agents,   post$agents),
          pp_kpi_card("Total Messages",  pre$messages, post$messages),
          pp_kpi_card("Network Density", pre$density,  post$density),
          pp_kpi_card("Avg Degree",      pre$avg_deg,  post$avg_deg)
      )
    })
    
    make_pp_net <- function(edge_data, node_color) {
      if (is.null(edge_data) || nrow(edge_data) == 0)
        return(visNetwork(nodes=data.frame(id=1,label="No data"),
                          edges=data.frame(), background="#0a1628"))
      net_e <- aggregate(list(weight=rep(1,nrow(edge_data))),
                         by=list(from=edge_data$from,to=edge_data$to), FUN=sum)
      net_e <- net_e[net_e$from != net_e$to, ]
      m_data <- calculate_metrics(edge_data)
      nodes  <- unique(c(net_e$from, net_e$to))
      nd <- data.frame(id=nodes, label=gsub("-Agent$","",nodes), stringsAsFactors=FALSE)
      nd <- merge(nd, m_data[,c("Agent","Degree")], by.x="id", by.y="Agent", all.x=TRUE)
      nd$value <- nd$Degree
      nd$color <- node_color
      nd$title <- paste0(
        "<div style='background:#1a2a45;color:white;padding:6px 10px;border-radius:6px;font-size:11px;'>",
        "<b>",nd$label,"</b><br>Degree: <b>",nd$Degree,"</b></div>")
      net_e$width <- 1+3*(net_e$weight-min(net_e$weight))/max(1,diff(range(net_e$weight)))
      visNetwork(nodes=nd, edges=net_e, background="#0a1628") |>
        visNodes(shape="dot", font=list(color="white",size=12), scaling=list(min=12,max=35)) |>
        visEdges(arrows="to", color=list(color="#4A5568"),
                 smooth=list(enabled=TRUE,type="curvedCW")) |>
        visOptions(highlightNearest=TRUE) |>
        visPhysics(solver="forceAtlas2Based", stabilization=TRUE) |>
        visInteraction(tooltipDelay=80, navigationButtons=FALSE, zoomView=FALSE)
    }
    
    output$pp_pre_net  <- renderVisNetwork(make_pp_net(pre_edges_data(),  "#163D77"))
    output$pp_post_net <- renderVisNetwork(make_pp_net(post_edges_data(), "#C0410A"))
    
    # ── Pre vs Post main interpretation banner ─────────────────────────
    output$pp_main_interpretation <- renderUI({
      pre_m    <- calculate_metrics(pre_edges_data())
      post_m   <- calculate_metrics(post_edges_data())
      pre_k    <- compute_net_kpis(pre_edges_data())
      post_k   <- compute_net_kpis(post_edges_data())
      agent_type <- input$pp_agent_type
      
      new_agents <- gsub("-Agent$", "", setdiff(post_m$Agent, pre_m$Agent))
      top_btw    <- if (nrow(post_m) > 0) gsub("-Agent$", "", post_m$Agent[which.max(post_m$Betweenness)]) else "unknown"
      top_deg    <- if (nrow(post_m) > 0) gsub("-Agent$", "", post_m$Agent[which.max(post_m$Degree)])      else "unknown"
      cohesion   <- if (post_k$density < pre_k$density) "reducing overall network cohesion" else "increasing overall network cohesion"
      new_part   <- if (length(new_agents) > 0)
        paste0("introducing new participants (", paste(new_agents, collapse = ", "), ")")
      else "activating agents across all roles"
      
      insight <- if (agent_type == "All") {
        paste0(
          "The embargo fundamentally reshaped communication patterns, ", new_part,
          " while shifting ", top_btw, " into the central coordination role and ", cohesion, "."
        )
      } else {
        agent_label <- switch(agent_type,
                              "intern"         = "Intern",
                              "judge"          = "Judge",
                              "legal"          = "Legal",
                              "platform_trust" = "Platform Trust",
                              "pr"             = "PR",
                              "pr_intern"      = "PR Intern",
                              "social_media"   = "Social Manager",
                              agent_type
        )
        paste0(
          "Filtering to ", agent_label, "-connected communications, the embargo still drove notable change: ",
          new_part, ", with ", top_btw, " acting as the primary bridge and ", cohesion, "."
        )
      }
      
      div(style = "background:#0f1f3d; border-left:3px solid #56CCF2;
                   border-radius:4px; padding:6px 14px; margin-bottom:2px;",
          tags$p("Key Insight",
                 style = "color:#56CCF2; font-size:11px; font-weight:700;
                          font-family:Inter,sans-serif; margin:0 0 4px 0;
                          text-transform:uppercase; letter-spacing:0.06em;"),
          tags$p(insight,
                 style = "color:#E2E8F0; font-size:13px; font-family:Inter,sans-serif;
                          margin:0; line-height:1.6;")
      )
    })
    
    output$pp_observations <- renderUI({
      pre_k  <- compute_net_kpis(pre_edges_data())
      post_k <- compute_net_kpis(post_edges_data())
      pre_m  <- calculate_metrics(pre_edges_data())
      post_m <- calculate_metrics(post_edges_data())
      
      # need at least post data to generate observations
      if (nrow(post_m) == 0) {
        return(div(style="background:#0f1f3d;border:1px solid #1e3a5f;border-radius:8px;padding:10px 14px;",
                   tags$p("No post-embargo data available for the selected filters.",
                          style="color:#BFC7D5;font-size:11px;font-family:Inter,sans-serif;margin:0;")))
      }
      
      obs_item <- function(text)
        tags$li(text, style="color:#BFC7D5;font-size:12px;font-family:Inter,sans-serif;
                              margin-bottom:6px;line-height:1.5;")
      
      items <- list(
        obs_item(paste0(
          "Messages: ", pre_k$messages, " pre-embargo → ", post_k$messages, " post-embargo",
          if (pre_k$messages > 0)
            paste0(" (+", round((post_k$messages-pre_k$messages)/pre_k$messages*100), "%)")
          else " (all activity post-embargo).")),
        obs_item(paste0(
          "Network density changed from ", pre_k$density, " to ", post_k$density,
          " — agents became ",
          if (post_k$density > pre_k$density) "more" else "less",
          " interconnected."))
      )
      
      # betweenness observations only if both pre and post have data
      if (nrow(pre_m) > 0) {
        merged     <- merge(pre_m, post_m, by="Agent", suffixes=c("_pre","_post"))
        if (nrow(merged) > 0) {
          merged$dbt <- merged$Betweenness_post - merged$Betweenness_pre
          merged$pbt <- ifelse(merged$Betweenness_pre==0, NA,
                               round(merged$dbt/merged$Betweenness_pre*100))
          top_bt      <- merged[which.max(abs(merged$dbt)), ]
          top_bt_name <- gsub("-Agent$","", top_bt$Agent)
          items <- c(items, list(obs_item(paste0(
            top_bt_name, " showed the largest betweenness shift (",
            top_bt$Betweenness_pre, " → ", top_bt$Betweenness_post,
            if (!is.na(top_bt$pbt)) paste0(", ", ifelse(top_bt$dbt>0,"+",""), top_bt$pbt, "%") else "",
            ")."))))
        }
        new_agents <- gsub("-Agent$","", setdiff(post_m$Agent, pre_m$Agent))
        if (length(new_agents) > 0)
          items <- c(items, list(obs_item(paste0(
            paste(new_agents, collapse=", "),
            if (length(new_agents)==1) " was" else " were",
            " absent pre-embargo, appearing only after the embargo."))))
      } else {
        new_agents <- gsub("-Agent$","", post_m$Agent)
        items <- c(items, list(obs_item(paste0(
          "All agents (", paste(new_agents, collapse=", "),
          ") were only active post-embargo."))))
      }
      
      top_deg_name <- gsub("-Agent$","", post_m$Agent[which.max(post_m$Degree)])
      top_deg_val  <- max(post_m$Degree)
      items <- c(items, list(obs_item(paste0(
        top_deg_name, " was the most connected agent post-embargo (", top_deg_val, " connections)."))))
      
      div(style="background:#0f1f3d;border:1px solid #1e3a5f;border-radius:8px;
                 padding:8px 12px; height:255px; overflow-y:auto;",
          tags$ul(style="margin:0;padding-left:16px;", do.call(tagList, items)))
    })
    
    
  })
}