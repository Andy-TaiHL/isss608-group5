# ============================================================
# VAST Challenge 2026 · Group 5
# Module: Swimlane Plot
# Author: Jiang Yuxi
# ============================================================
# This module presents an interactive timeline showing each
# agent's declared behaviour versus actual communication
# behaviour over time.
# Key features:
#   - Swimlane timeline per agent
#   - Behaviour Mismatch Count
#   - Compliance Violations
#   - Agent Risk Ranking
#   - Behaviour Consistency Score
# ============================================================

library(shiny)
library(bslib)

# ── UI ──────────────────────────────────────────────────────
swimlaneUI <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      title = "Swimlane Controls",
      width = 280,

      # ── Agent Selection ──
      selectInput(
        ns("agents"),
        "Select Agent(s)",
        choices  = c("All Agents", "Agent A", "Agent B", "Agent C"),
        selected = "All Agents",
        multiple = TRUE
      ),

      # ── Timeline ──
      dateRangeInput(
        ns("timeline"),
        "Timeline Range",
        start = "2025-01-01",
        end   = "2026-01-01"
      ),

      hr(),

      # ── Indicators ──
      checkboxGroupInput(
        ns("indicators"),
        "Show Indicators",
        choices = c(
          "Behaviour Mismatch Count"   = "mismatch",
          "Compliance Violations"      = "compliance",
          "Agent Risk Ranking"         = "risk",
          "Behaviour Consistency Score" = "consistency"
        ),
        selected = c("mismatch", "compliance")
      ),

      hr(),

      # ── Risk Threshold ──
      sliderInput(
        ns("risk_threshold"),
        "Risk Threshold",
        min = 0, max = 100, value = 50, step = 5
      ),

      # ── Highlight Anomalies ──
      checkboxInput(
        ns("highlight_anomalies"),
        "Highlight Anomalies",
        value = TRUE
      )
    ),

    # ── Main Panel ──
    div(
      class = "p-3",
      navset_card_tab(

        nav_panel(
          "Swimlane Timeline",
          card_body(
            # ── TODO: Replace with actual swimlane plot ──
            # Suggested packages: ggplot2, plotly, timevis
            plotOutput(ns("swimlane_plot"), height = "500px")
          )
        ),

        nav_panel(
          "Behaviour Scores",
          card_body(
            # ── TODO: Line chart of behaviour consistency scores over time ──
            plotOutput(ns("behaviour_plot"), height = "500px")
          )
        ),

        nav_panel(
          "Risk Rankings",
          card_body(
            # ── TODO: Replace with actual agent risk rankings ──
            tableOutput(ns("risk_table"))
          )
        )
      )
    )
  )
}

# ── Server ──────────────────────────────────────────────────
swimlaneServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    # ── TODO: Load your agent behaviour data here ──
    # Example:
    # behaviour <- read_csv("data/behaviour.csv")

    # ── Swimlane Plot ─────────────────────────────────────
    output$swimlane_plot <- renderPlot({
      # TODO: Replace with actual swimlane visualisation
      # Suggested packages:
      #   ggplot2 with geom_segment / geom_tile
      #   plotly for interactivity
      #   timevis for timeline view
      plot(1, type = "n",
           main = "Swimlane: Declared vs Actual Behaviour",
           xlab = "Time", ylab = "Agent", axes = FALSE)
      text(1, 1, "Connect your agent behaviour data to render this plot",
           cex = 1.2, col = "grey50")
    })

    # ── Behaviour Consistency Plot ────────────────────────
    output$behaviour_plot <- renderPlot({
      # TODO: Replace with actual behaviour consistency scores
      plot(1, type = "n",
           main = "Behaviour Consistency Score Over Time",
           xlab = "Time", ylab = "Score", axes = FALSE)
      text(1, 1, "Behaviour score data will appear here", cex = 1.2, col = "grey50")
    })

    # ── Risk Rankings Table ───────────────────────────────
    output$risk_table <- renderTable({
      # TODO: Replace with actual risk rankings
      data.frame(
        Agent                      = c("Agent A", "Agent B", "Agent C"),
        `Risk Score`               = c(82, 65, 43),
        `Mismatch Count`           = c(7, 4, 2),
        `Compliance Violations`    = c(3, 1, 0),
        `Consistency Score`        = c(0.31, 0.58, 0.84)
      )
    })

  })
}
