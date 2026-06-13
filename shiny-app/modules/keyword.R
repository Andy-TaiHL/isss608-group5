# ============================================================
# VAST Challenge 2026 · Group 5
# Module: Keyword Embedding Visualisations
# Author: Goh Fangyu
# ============================================================
# This module explores communication topics through keyword-
# based visualisations in TenantThread's communication channels.
# Key features:
#   - Word Cloud
#   - TF-IDF Analysis
#   - Semantic Network
#   - Topic trend tracking over time
# ============================================================

library(shiny)
library(bslib)

# ── UI ──────────────────────────────────────────────────────
keywordUI <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      title = "Keyword Controls",
      width = 280,

      # ── Visualisation Type ──
      selectInput(
        ns("viz_type"),
        "Visualisation Type",
        choices = c(
          "Word Cloud"      = "wordcloud",
          "TF-IDF Analysis" = "tfidf",
          "Semantic Network" = "semantic"
        ),
        selected = "wordcloud"
      ),

      # ── Topic Filter ──
      selectInput(
        ns("topic"),
        "Topic Filter",
        choices  = c("All Topics", "Merger", "Risk", "Crisis", "Operations"),
        selected = "All Topics"
      ),

      # ── Distance Sensitivity ──
      sliderInput(
        ns("distance_sensitivity"),
        "Distance Sensitivity",
        min = 0.1, max = 1.0, value = 0.5, step = 0.1
      ),

      # ── Distance Metric ──
      selectInput(
        ns("distance_metric"),
        "Distance Metric",
        choices = c(
          "Cosine"     = "cosine",
          "Euclidean"  = "euclidean",
          "Manhattan"  = "manhattan"
        ),
        selected = "cosine"
      ),

      # ── Time Period ──
      dateRangeInput(
        ns("timeline"),
        "Time Period",
        start = "2025-01-01",
        end   = "2026-01-01"
      ),

      hr(),

      # ── Max Words (for Word Cloud) ──
      sliderInput(
        ns("max_words"),
        "Max Words Displayed",
        min = 10, max = 200, value = 100, step = 10
      )
    ),

    # ── Main Panel ──
    div(
      class = "p-3",
      navset_card_tab(

        nav_panel(
          "Visualisation",
          card_body(
            # ── TODO: Replace with actual keyword visualisation ──
            # Suggested packages: wordcloud2, ggplot2, igraph
            plotOutput(ns("keyword_plot"), height = "500px")
          )
        ),

        nav_panel(
          "Topic Trends",
          card_body(
            # ── TODO: Line/area chart of topic frequency over time ──
            plotOutput(ns("topic_trends"), height = "500px")
          )
        ),

        nav_panel(
          "TF-IDF Table",
          card_body(
            # ── TODO: Replace with actual TF-IDF scores ──
            tableOutput(ns("tfidf_table"))
          )
        )
      )
    )
  )
}

# ── Server ──────────────────────────────────────────────────
keywordServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    # ── TODO: Load your communication data here ──
    # Example:
    # messages <- read_csv("data/messages.csv")

    # ── Keyword Visualisation ─────────────────────────────
    output$keyword_plot <- renderPlot({
      # TODO: Replace with actual word cloud / TF-IDF / semantic network
      # Suggested packages:
      #   Word Cloud:      wordcloud, wordcloud2
      #   TF-IDF:          tidytext, ggplot2
      #   Semantic Network: igraph, ggraph
      plot(1, type = "n",
           main = paste(
             switch(input$viz_type,
                    wordcloud = "Word Cloud",
                    tfidf     = "TF-IDF Analysis",
                    semantic  = "Semantic Network"),
             "—", input$topic
           ),
           xlab = "", ylab = "", axes = FALSE)
      text(1, 1, "Connect your communication data to render this plot",
           cex = 1.2, col = "grey50")
    })

    # ── Topic Trends ──────────────────────────────────────
    output$topic_trends <- renderPlot({
      # TODO: Replace with actual topic trend over time
      plot(1, type = "n",
           main = "Topic Trends Over Time",
           xlab = "Time", ylab = "Frequency", axes = FALSE)
      text(1, 1, "Topic frequency data will appear here", cex = 1.2, col = "grey50")
    })

    # ── TF-IDF Table ──────────────────────────────────────
    output$tfidf_table <- renderTable({
      # TODO: Replace with actual TF-IDF scores
      data.frame(
        Term    = c("merger", "embargo", "risk", "compliance", "leak"),
        TF_IDF  = c(0.92, 0.87, 0.75, 0.63, 0.58),
        Topic   = c("Merger", "Crisis", "Risk", "Operations", "Crisis")
      )
    })

  })
}
