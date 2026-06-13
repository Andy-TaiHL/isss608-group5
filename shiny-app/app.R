library(shiny)
library(bslib)

# ── UI ──────────────────────────────────────────────────────────────────────
ui <- page_navbar(
  title = "VAST Challenge 2026 · Group 5",
  theme = bs_theme(
    bg = "#0a1628",
    fg = "#ffffff",
    primary = "#378ADD",
    secondary = "#5bb3f0",
    base_font = font_google("Inter"),
    heading_font = font_google("Playfair Display"),
    navbar_bg = "#0a1628"
  ),
  
  # ── Tab 1: Overview ────────────────────────────────────────────────────────
  nav_panel(
    title = "Overview",
    icon = icon("house"),
    div(
      class = "container-fluid p-4",
      div(
        class = "row",
        div(
          class = "col-12 mb-4",
          card(
            card_header("TenantThread Communication Analysis"),
            card_body(
              p("This application investigates whether TenantThread made a deliberate decision 
                to leak merger information, or whether the system simply broke down under pressure."),
              p("Explore three analytical modules:"),
              tags$ul(
                tags$li(strong("Network Metric Analysis"), " — Communication structures and agent relationships"),
                tags$li(strong("Keyword Embedding Visualisations"), " — Topics and themes in communications"),
                tags$li(strong("Swimlane Plot"), " — Agent behaviour vs actual communication over time")
              )
            )
          )
        )
      ),
      div(
        class = "row g-3",
        div(
          class = "col-md-4",
          card(
            card_header(icon("circle-nodes"), " Network Metrics"),
            card_body(
              p("Identify key communication hubs, influential agents, and changes in network centrality."),
              actionButton("go_network", "Explore Network →", class = "btn-primary")
            )
          )
        ),
        div(
          class = "col-md-4",
          card(
            card_header(icon("cloud"), " Keyword Analysis"),
            card_body(
              p("Explore communication topics through word clouds, TF-IDF, and semantic networks."),
              actionButton("go_keyword", "Explore Keywords →", class = "btn-primary")
            )
          )
        ),
        div(
          class = "col-md-4",
          card(
            card_header(icon("timeline"), " Swimlane Plot"),
            card_body(
              p("Interactive timeline of each agent's declared vs actual communication behaviour."),
              actionButton("go_swimlane", "Explore Timeline →", class = "btn-primary")
            )
          )
        )
      )
    )
  ),
  
  # ── Tab 2: Network Metric Analysis ────────────────────────────────────────
  nav_panel(
    title = "Network Analysis",
    icon = icon("circle-nodes"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Controls",
        width = 280,
        
        selectInput(
          "network_metric",
          "Centrality Metric",
          choices = c("Betweenness" = "betweenness",
                      "Degree" = "degree",
                      "Closeness" = "closeness",
                      "Eigenvector" = "eigenvector"),
          selected = "betweenness"
        ),
        
        sliderInput(
          "degree_filter",
          "Degree Filter",
          min = 0, max = 10, value = 1, step = 1
        ),
        
        dateRangeInput(
          "timeline_range",
          "Timeline Range",
          start = "2025-01-01",
          end = "2026-01-01"
        ),
        
        selectInput(
          "agent_filter",
          "Filter by Agent Type",
          choices = c("All", "Person", "Organisation"),
          selected = "All"
        ),
        
        hr(),
        
        checkboxInput("show_labels", "Show Node Labels", value = TRUE),
        checkboxInput("highlight_hubs", "Highlight Key Hubs", value = TRUE)
      ),
      
      # Main panel
      div(
        class = "p-3",
        navset_card_tab(
          nav_panel(
            "Network Graph",
            card_body(
              p(em("Network graph will render here once data is loaded.")),
              plotOutput("network_plot", height = "500px")
            )
          ),
          nav_panel(
            "Centrality Table",
            card_body(
              tableOutput("centrality_table")
            )
          ),
          nav_panel(
            "Pre vs Post Embargo",
            card_body(
              p(em("Comparison of network structure before and after embargo breach.")),
              plotOutput("embargo_comparison", height = "500px")
            )
          )
        )
      )
    )
  ),
  
  # ── Tab 3: Keyword Embedding Visualisations ───────────────────────────────
  nav_panel(
    title = "Keyword Analysis",
    icon = icon("cloud"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Controls",
        width = 280,
        
        selectInput(
          "viz_type",
          "Visualisation Type",
          choices = c("Word Cloud" = "wordcloud",
                      "TF-IDF Analysis" = "tfidf",
                      "Semantic Network" = "semantic"),
          selected = "wordcloud"
        ),
        
        selectInput(
          "topic_filter",
          "Topic Filter",
          choices = c("All Topics", "Merger", "Risk", "Crisis", "Operations"),
          selected = "All Topics"
        ),
        
        sliderInput(
          "distance_sensitivity",
          "Distance Sensitivity",
          min = 0.1, max = 1.0, value = 0.5, step = 0.1
        ),
        
        selectInput(
          "distance_metric",
          "Distance Metric",
          choices = c("Cosine" = "cosine",
                      "Euclidean" = "euclidean",
                      "Manhattan" = "manhattan"),
          selected = "cosine"
        ),
        
        dateRangeInput(
          "keyword_timeline",
          "Time Period",
          start = "2025-01-01",
          end = "2026-01-01"
        )
      ),
      
      div(
        class = "p-3",
        navset_card_tab(
          nav_panel(
            "Visualisation",
            card_body(
              p(em("Keyword visualisation will render here once data is loaded.")),
              plotOutput("keyword_plot", height = "500px")
            )
          ),
          nav_panel(
            "Topic Trends",
            card_body(
              p(em("Topic trends over time will appear here.")),
              plotOutput("topic_trends", height = "500px")
            )
          ),
          nav_panel(
            "TF-IDF Table",
            card_body(
              tableOutput("tfidf_table")
            )
          )
        )
      )
    )
  ),
  
  # ── Tab 4: Swimlane Plot ──────────────────────────────────────────────────
  nav_panel(
    title = "Swimlane Plot",
    icon = icon("bars-staggered"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Controls",
        width = 280,
        
        selectInput(
          "agent_select",
          "Select Agent(s)",
          choices = c("All Agents", "Agent A", "Agent B", "Agent C"),
          selected = "All Agents",
          multiple = TRUE
        ),
        
        dateRangeInput(
          "swimlane_timeline",
          "Timeline Range",
          start = "2025-01-01",
          end = "2026-01-01"
        ),
        
        hr(),
        
        checkboxGroupInput(
          "indicators",
          "Show Indicators",
          choices = c("Behaviour Mismatch Count" = "mismatch",
                      "Compliance Violations" = "compliance",
                      "Agent Risk Ranking" = "risk",
                      "Behaviour Consistency Score" = "consistency"),
          selected = c("mismatch", "compliance")
        ),
        
        hr(),
        
        sliderInput(
          "risk_threshold",
          "Risk Threshold",
          min = 0, max = 100, value = 50, step = 5
        )
      ),
      
      div(
        class = "p-3",
        navset_card_tab(
          nav_panel(
            "Swimlane Timeline",
            card_body(
              p(em("Swimlane plot showing declared vs actual behaviour will render here.")),
              plotOutput("swimlane_plot", height = "500px")
            )
          ),
          nav_panel(
            "Risk Rankings",
            card_body(
              p(em("Agent risk rankings table will appear here.")),
              tableOutput("risk_table")
            )
          ),
          nav_panel(
            "Behaviour Scores",
            card_body(
              p(em("Behaviour consistency scores over time will appear here.")),
              plotOutput("behaviour_plot", height = "500px")
            )
          )
        )
      )
    )
  )
)

# ── Server ───────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # Navigation buttons on Overview tab
  observeEvent(input$go_network, {
    nav_select("navbar", "Network Analysis")
  })
  observeEvent(input$go_keyword, {
    nav_select("navbar", "Keyword Analysis")
  })
  observeEvent(input$go_swimlane, {
    nav_select("navbar", "Swimlane Plot")
  })
  
  # ── Placeholder outputs (replace with actual plots when data is ready) ──
  output$network_plot <- renderPlot({
    plot(1, type = "n", main = "Network Graph — connect your data here",
         xlab = "", ylab = "")
    text(1, 1, "Load your network data to render this plot", cex = 1.2)
  })
  
  output$keyword_plot <- renderPlot({
    plot(1, type = "n", main = "Keyword Visualisation — connect your data here",
         xlab = "", ylab = "")
    text(1, 1, "Load your communication data to render this plot", cex = 1.2)
  })
  
  output$swimlane_plot <- renderPlot({
    plot(1, type = "n", main = "Swimlane Plot — connect your data here",
         xlab = "", ylab = "")
    text(1, 1, "Load your agent behaviour data to render this plot", cex = 1.2)
  })
  
  output$centrality_table <- renderTable({
    data.frame(
      Agent = c("Agent A", "Agent B", "Agent C"),
      Betweenness = c(0.85, 0.62, 0.41),
      Degree = c(12, 8, 5),
      Closeness = c(0.72, 0.58, 0.43)
    )
  })
  
  output$risk_table <- renderTable({
    data.frame(
      Agent = c("Agent A", "Agent B", "Agent C"),
      `Risk Score` = c(82, 65, 43),
      `Mismatch Count` = c(7, 4, 2),
      `Compliance Violations` = c(3, 1, 0)
    )
  })
}

# ── Run ──────────────────────────────────────────────────────────────────────
shinyApp(ui, server)