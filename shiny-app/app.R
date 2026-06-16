# ============================================================
# VAST Challenge 2026 · Group 5
# Main App — integrates all modules
# ============================================================

library(shiny)
library(bslib)

# Load modules
source("modules/network.R")
source("modules/keyword.R")
source("modules/swimlane.R")

# ── UI ──────────────────────────────────────────────────────
ui <- page_navbar(
  title = "VAST Challenge 2026",
  id = "navbar",
  theme = bs_theme(
    bg = "#0a1628",
    fg = "#ffffff",
    primary = "#378ADD",
    secondary = "#5bb3f0",
    base_font = font_google("Inter"),
    heading_font = font_google("Playfair Display"),
    navbar_bg = "#0a1628"
  ),

  # ── Tab 1: Overview ───────────────────────────────────────
  nav_panel(
    title = "Overview",
    icon = icon("house"),
    div(
      class = "container-fluid p-4",
      div(
        class = "row mb-4",
        div(
          class = "col-12",
          card(
            card_header("TenantThread Communication Analysis"),
            card_body(
              p("This application investigates whether TenantThread made a deliberate
                decision to leak merger information, or whether the system simply broke
                down under pressure."),
              p("Navigate the three analytical modules below:")
            )
          )
        )
      ),
      div(
        class = "row g-3",
        div(
          class = "col-md-4",
          card(
            card_header(icon("circle-nodes"), " Network Analysis"),
            card_body(
              p("Examine how communication structures and agent relationships evolved
                before and after the embargo breach."),
              actionButton("go_network", "Explore →", class = "btn-primary")
            )
          )
        ),
        div(
          class = "col-md-4",
          card(
            card_header(icon("cloud"), " Keyword Analysis"),
            card_body(
              p("Explore communication topics through word clouds, TF-IDF analysis,
                and semantic networks."),
              actionButton("go_keyword", "Explore →", class = "btn-primary")
            )
          )
        ),
        div(
          class = "col-md-4",
          card(
            card_header(icon("bars-staggered"), " Swimlane Plot"),
            card_body(
              p("Interactive timeline of each agent's declared versus actual
                communication behaviour over time."),
              actionButton("go_swimlane", "Explore →", class = "btn-primary")
            )
          )
        )
      )
    )
  ),

  # ── Tab 2: Network Analysis (Andy) ────────────────────────
  nav_panel(
    title = "Network Analysis",
    icon = icon("circle-nodes"),
    networkUI("network")
  ),

  # ── Tab 3: Keyword Analysis (Fangyu) ──────────────────────
  nav_panel(
    title = "Keyword Analysis",
    icon = icon("cloud"),
    keywordUI("keyword")
  ),

  # ── Tab 4: Swimlane Plot (Yuxi) ───────────────────────────
  nav_panel(
    title = "Swimlane Plot",
    icon = icon("bars-staggered"),
    swimlaneUI("swimlane")
  )
)

# ── Server ──────────────────────────────────────────────────
server <- function(input, output, session) {

  # Overview navigation buttons
  observeEvent(input$go_network,  nav_select("navbar", "Network Analysis"))
  observeEvent(input$go_keyword,  nav_select("navbar", "Keyword Analysis"))
  observeEvent(input$go_swimlane, nav_select("navbar", "Swimlane Plot"))

  # Load module servers
  networkServer("network")
  keywordServer("keyword")
  swimlaneServer("swimlane")
}

shinyApp(ui, server)
