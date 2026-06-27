# ============================================================
# VAST Challenge 2026 · Group 5
# Main App — integrates all modules
# ============================================================
source("packages.R")   # loads all packages
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
  header = tags$head(tags$style(HTML("
    /* Remove gap between rows inside tab panels */
    .tab-content > .tab-pane { padding:0 !important; }
    .tab-content > .tab-pane > * { margin-bottom:0 !important; }
    .tab-content > .tab-pane .row { margin-bottom:0 !important; }
    /* Force DT tables to white background */
    .dataTable { background:white !important; color:#1e293b !important; }
    .dataTable thead th { background:#f8fafc !important; color:#1e293b !important;
                          border-bottom:2px solid #e2e8f0 !important; }
    .dataTable tbody tr { background:white !important; }
    .dataTable tbody tr:nth-child(odd) { background:#f8fafc !important; }
    .dataTable tbody td { color:#1e293b !important; border-color:#e2e8f0 !important; }
    .dataTables_wrapper { color:#1e293b !important; }
    /* Centrality table: ID-based rules beat class-based !important rules */
    #network-centrality_table tbody td {
      color: inherit !important;
      background-color: inherit !important;
    }
    .dataTables_info, .dataTables_paginate { color:#64748b !important; }
    .paginate_button,
    .paginate_button.previous,
    .paginate_button.next { color:#334155 !important; background:white !important;
                             border:1px solid #e2e8f0 !important; border-radius:4px; }
    .paginate_button:hover,
    .paginate_button.previous:hover,
    .paginate_button.next:hover { background:#f1f5f9 !important; 
                                   color:#1e293b !important; }
    .paginate_button.current,
    .paginate_button.current:hover { background:#1d4ed8 !important;
                                      color:white !important;
                                      border-color:#1d4ed8 !important; }
    .paginate_button.disabled,
    .paginate_button.disabled:hover { color:#94a3b8 !important;
                                       background:white !important; }
    /* Force entire pagination row white */
    .dataTables_wrapper .dataTables_paginate { background:white !important;
      padding:8px 0 !important; }
    .dataTables_wrapper .dataTables_paginate span { background:white !important; }
    .dataTables_wrapper .dataTables_paginate span .paginate_button {
      background:white !important; color:#334155 !important; }
    .dataTables_paginate { background:white !important; padding:4px 0 !important; }
    .dataTables_info { background:white !important; padding:4px 0 !important; }
    /* Force all DT wrapper elements white */
    .dataTables_wrapper,
    .dataTables_wrapper * { color:#1e293b !important; }
    .dataTables_wrapper { background:white !important; }
    .dataTables_wrapper .dataTable tbody tr td { background-color:white !important; }
    .dataTables_wrapper .dataTable tbody tr:nth-child(odd) td { 
      background-color:#f8fafc !important; }
    /* Pagination - force all states */
    .dataTables_wrapper .dataTables_paginate,
    .dataTables_wrapper .dataTables_paginate * { background:white !important; }
    .dataTables_wrapper .dataTables_paginate .paginate_button {
      background:white !important; color:#334155 !important;
      border:1px solid #e2e8f0 !important; border-radius:4px !important; }
    .dataTables_wrapper .dataTables_paginate .paginate_button.current {
      background:#1d4ed8 !important; color:white !important;
      border-color:#1d4ed8 !important; }
    .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
      background:#f1f5f9 !important; color:#1e293b !important; }
    .dataTables_wrapper .dataTables_info { background:white !important; }
    /* Fix selectInput - both the input box and dropdown */
    .selectize-input { background:white !important; color:#1e293b !important;
                       border:1px solid #e2e8f0 !important; }
    .selectize-input * { color:#1e293b !important; }
    .selectize-dropdown, .selectize-dropdown-content {
      background:white !important; color:#1e293b !important; }
    .selectize-dropdown .option { color:#1e293b !important;
                                   background:white !important; }
    .selectize-dropdown .option:hover,
    .selectize-dropdown .option.active { background:#1d4ed8 !important;
                                          color:white !important; }
    select, select option { background:white !important; color:#1e293b !important; }
  ")),
                     tags$script(HTML("
      // Fix DT pagination after every table draw
      $(document).on('draw.dt', function(e) {
        var pag = $('.dataTables_paginate');
        pag.css('background','white');
        pag.find('.paginate_button').css({
          'background':'white',
          'color':'#334155',
          'border':'1px solid #e2e8f0',
          'border-radius':'4px'
        });
        pag.find('.paginate_button.current').css({
          'background':'#1d4ed8',
          'color':'white',
          'border-color':'#1d4ed8'
        });
        pag.find('.paginate_button.disabled').css({
          'color':'#94a3b8'
        });
        $('.dataTables_info').css({'background':'white','color':'#64748b'});
        // Fix table body — skip centrality table which uses custom cell coloring
        if (e.target.id !== 'network-centrality_table') {
          var $tbl = $(e.target);
          $tbl.find('tbody tr').css('background','white');
          $tbl.find('tbody tr:nth-child(odd)').css('background','#f8fafc');
          $tbl.find('tbody td').css({'color':'#1e293b','background-color':''});
        }
      });
    "))
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