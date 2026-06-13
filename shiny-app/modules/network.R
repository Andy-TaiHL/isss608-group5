# ============================================================
# VAST Challenge 2026 · Group 5
# Module: Network Metric Analysis
# Author: Andy Tai
# ============================================================
# This module examines how communication structures and agent
# relationships evolved before and after the embargo breach.
# Key features:
#   - Network centrality metrics (betweenness, degree, closeness)
#   - Pre vs post embargo comparison
#   - Key communication hub identification
# ============================================================

library(shiny)
library(bslib)

# ── UI ──────────────────────────────────────────────────────
networkUI <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      title = "Network Controls",
      width = 280,

      # ── Centrality Metric ──
      selectInput(
        ns("metric"),
        "Centrality Metric",
        choices = c(
          "Betweenness" = "betweenness",
          "Degree"      = "degree",
          "Closeness"   = "closeness",
          "Eigenvector" = "eigenvector"
        ),
        selected = "betweenness"
      ),

      # ── Degree Filter ──
      sliderInput(
        ns("degree_filter"),
        "Degree Filter",
        min = 0, max = 10, value = 1, step = 1
      ),

      # ── Timeline ──
      dateRangeInput(
        ns("timeline"),
        "Timeline Range",
        start = "2025-01-01",
        end   = "2026-01-01"
      ),

      # ── Agent Type ──
      selectInput(
        ns("agent_type"),
        "Agent Type",
        choices  = c("All", "Person", "Organisation"),
        selected = "All"
      ),

      hr(),

      checkboxInput(ns("show_labels"),    "Show Node Labels",   value = TRUE),
      checkboxInput(ns("highlight_hubs"), "Highlight Key Hubs", value = TRUE)
    ),

    # ── Main Panel ──
    div(
      class = "p-3",
      navset_card_tab(

        nav_panel(
          "Network Graph",
          card_body(
            # ── TODO: Replace with actual network plot (e.g. visNetwork, igraph) ──
            plotOutput(ns("network_plot"), height = "500px")
          )
        ),

        nav_panel(
          "Pre vs Post Embargo",
          card_body(
            # ── TODO: Side-by-side comparison of network before/after embargo ──
            plotOutput(ns("embargo_plot"), height = "500px")
          )
        ),

        nav_panel(
          "Centrality Table",
          card_body(
            # ── TODO: Replace with actual centrality data ──
            tableOutput(ns("centrality_table"))
          )
        )
      )
    )
  )
}

# ── Server ──────────────────────────────────────────────────
networkServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    # ── TODO: Load your network data here ──
    # Example:
    # edges <- read_csv("data/edges.csv")
    # nodes <- read_csv("data/nodes.csv")

    # ── Network Plot ──────────────────────────────────────
    output$network_plot <- renderPlot({
      # TODO: Replace with actual network visualisation
      # Suggested packages: visNetwork, igraph, ggraph
      plot(1, type = "n",
           main = paste("Network Graph —", input$metric, "centrality"),
           xlab = "", ylab = "", axes = FALSE)
      text(1, 1, "Connect your network data to render this plot", cex = 1.2, col = "grey50")
    })

    # ── Pre vs Post Embargo Plot ──────────────────────────
    output$embargo_plot <- renderPlot({
      # TODO: Replace with actual pre/post comparison
      par(mfrow = c(1, 2))
      plot(1, type = "n", main = "Pre-Embargo Network",  xlab = "", ylab = "", axes = FALSE)
      text(1, 1, "Pre-embargo data here", cex = 1.1, col = "grey50")
      plot(1, type = "n", main = "Post-Embargo Network", xlab = "", ylab = "", axes = FALSE)
      text(1, 1, "Post-embargo data here", cex = 1.1, col = "grey50")
    })

    # ── Centrality Table ──────────────────────────────────
    output$centrality_table <- renderTable({
      # TODO: Replace with actual centrality data
      data.frame(
        Agent        = c("Agent A", "Agent B", "Agent C"),
        Betweenness  = c(0.85, 0.62, 0.41),
        Degree       = c(12, 8, 5),
        Closeness    = c(0.72, 0.58, 0.43),
        Eigenvector  = c(0.91, 0.67, 0.38)
      )
    })

  })
}
