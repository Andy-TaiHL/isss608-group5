# ============================================================
# keyword.R
# Integrator module for all keyword visualisations.
# Sources the 4 sub-scripts and exposes the Shiny module:
#   - keywordUI(id)
#   - keywordServer(id)
#
# Usage in app.R:
#   source("modules/keyword.R")
#   # in ui:  keywordUI("kw")
#   # in server: keywordServer("kw")
#
# Run standalone to test all sub-parts load cleanly:
#   source("modules/keyword.R")
# ============================================================

library(shiny)

# ── Resolve module directory ───────────────────────────────────────────────────
# Works whether called from app.R (cwd = shiny-app/) or run directly
MODULE_DIR <- tryCatch(
  dirname(sys.frame(1)$ofile),
  error = function(e) "modules"
)

# ── Source sub-modules in dependency order ─────────────────────────────────────
source(file.path(MODULE_DIR, "keyword_data_wrangling.R"))    # data + helpers
source(file.path(MODULE_DIR, "keyword_wordcloud.R"))          # plot_wordcloud()
source(file.path(MODULE_DIR, "keyword_semantic_network.R"))   # plot_network()
source(file.path(MODULE_DIR, "keyword_risk_timeline.R"))      # plot_risk_timeline()

DEFAULT_SEEDS <- "risk, embargo, leak, merger, legal, compliance, anonymous, monitor, breach, exposure"

# ── SHINY MODULE UI ────────────────────────────────────────────────────────────
keywordUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(tags$style(HTML("
      .kw-hdr    { background:#1a2940; color:white !important; padding:10px 18px;
                   margin-bottom:12px; }
      .kw-hdr h3 { margin:0; font-size:16px; font-weight:700; display:inline;
                   color:white !important; }
      .kw-hdr span { font-size:11px; opacity:.6; margin-left:12px; }
      /* Force white background + dark text on all cards regardless of bslib theme */
      .kw-card   { background:white !important; border-radius:8px; padding:10px 12px;
                   box-shadow:0 1px 4px rgba(0,0,0,.12); margin-bottom:4px; }
      .kw-card, .kw-card * { color:#1e293b !important; }
      .kw-card label,
      .kw-card .form-check-label,
      .kw-card .control-label { color:#334155 !important; font-size:13px !important; }
      .kw-card p, .kw-card small { color:#64748b !important; }
      .kw-card textarea  { color:#1e293b !important; background:white !important;
                           border:1px solid #e2e8f0 !important; }
      .kw-card select    { color:#1e293b !important; background:white !important; }
      .kw-card .btn-primary { color:white !important; }
      .kw-card .irs-min, .kw-card .irs-max,
      .kw-card .irs-from, .kw-card .irs-to,
      .kw-card .irs-single { color:#334155 !important;
                              background:white !important; }
      .kw-card-A { border-top:3px solid #1d4ed8; }
      .kw-card-B { border-top:3px solid #15803d; }
      .kw-ctrl   { font-size:11px !important; font-weight:700; color:#334155 !important;
                   text-transform:uppercase; letter-spacing:.04em;
                   margin:10px 0 3px; display:block; }
      .kw-hint   { font-size:11px !important; color:#64748b !important;
                   margin:-1px 0 5px; }
      .kw-tag-A  { background:#dbeafe; color:#1d4ed8 !important; padding:2px 8px;
                   border-radius:3px; font-size:10px; font-weight:700; }
      .kw-tag-B  { background:#dcfce7; color:#15803d !important; padding:2px 8px;
                   border-radius:3px; font-size:10px; font-weight:700; }
      hr.kw-hr   { margin:8px 0; border-color:#e2e8f0; }
      /* Compact top bar */
      .kw-topbar .form-check-label { font-size:11px !important; }
      .kw-topbar .form-check { margin-bottom:0 !important; margin-right:6px !important; }
      .kw-topbar .shiny-input-container { margin-bottom:0 !important; }
      .kw-topbar textarea { font-size:11px !important; padding:3px 6px !important; }
      .kw-topbar .irs { margin-top:0 !important; }
      .kw-topbar .form-check-inline { margin-right:4px !important; }
      /* Eliminate gap - target bslib column padding directly */
      .kw-row-charts > div { padding-bottom:0 !important; margin-bottom:0 !important; }
      .kw-row-charts > div > div { margin-bottom:0 !important; }
      .kw-row-table > div { padding-top:0 !important; margin-top:0 !important; }
      /* Shrink the dark area between rows */
      .container-fluid { row-gap:0 !important; }
      /* Fix DT table visibility in dark theme */
      .kw-card .dataTables_wrapper { color:#1e293b !important; }
      .kw-card table.dataTable thead th,
      .kw-card table.dataTable thead td { color:#1e293b !important;
        background:#f8fafc !important; border-bottom:2px solid #e2e8f0 !important; }
      .kw-card table.dataTable tbody tr { background:#ffffff !important;
        color:#1e293b !important; }
      .kw-card table.dataTable tbody tr:nth-child(odd) {
        background:#f8fafc !important; }
      .kw-card table.dataTable tbody td { color:#1e293b !important;
        border-color:#e2e8f0 !important; }
      .kw-card .dataTables_info,
      .kw-card .dataTables_paginate { color:#64748b !important; }
      .kw-card .paginate_button { color:#334155 !important; }
      .kw-card .paginate_button.current { background:#1d4ed8 !important;
        color:white !important; border-radius:4px; }
    "))),

    # ── TOP BAR: all shared controls in one compact row ─────────────────────
    div(class="kw-card kw-topbar", style="margin-bottom:4px; padding:6px 10px;",
      fluidRow(style="align-items:center;",
        column(2,
          tags$span("Channels", class="kw-ctrl", style="margin-top:0;"),
          checkboxGroupInput(ns("channels"), label=NULL, inline=TRUE,
            choices  = c("Group"  = "comms_huddle",
                         "1-on-1" = "private",
                         "Public" = "public_post"),
            selected = c("comms_huddle","private","public_post"))
        ),
        column(2,
          tags$span("Internal States", class="kw-ctrl", style="margin-top:0;"),
          checkboxGroupInput(ns("int_states"), label=NULL, inline=TRUE,
            choices  = c("Delib." = "deliberating",
                         "Ration."= "rationalizing",
                         "React." = "reacting"),
            selected = character(0))
        ),
        column(3,
          tags$span("Seed Words", class="kw-ctrl", style="margin-top:0;"),
          textAreaInput(ns("seeds_raw"), label=NULL,
                        value=DEFAULT_SEEDS, rows=1, width="100%",
                        resize="none")
        ),
        column(2,
          tags$span("Analysis Mode", class="kw-ctrl", style="margin-top:0;"),
          radioButtons(ns("nlp_mode"), label=NULL, inline=TRUE,
            choices  = c("Cosine" = "cosine", "Simple" = "freq"),
            selected = "cosine")
        ),
        column(2,
          conditionalPanel(
            condition = sprintf("input['%s'] == 'cosine'", ns("nlp_mode")),
            tags$span("Cosine Threshold", class="kw-ctrl", style="margin-top:0;"),
            sliderInput(ns("threshold"), label=NULL,
                        min=0.05, max=0.50, value=0.15, step=0.05,
                        ticks=FALSE)
          )
        ),
        column(1,
          tags$div(style="margin-top:18px;",
            actionButton(ns("go"), "Update", icon=icon("rotate"),
                         class="btn-primary", width="100%",
                         style="font-size:11px; padding:5px 4px;
                                line-height:1.2; white-space:nowrap;")
          )
        )
      )
    ),

    # PANEL A + PANEL B
    fluidRow(class="kw-row-charts",
      column(6,
        div(class="kw-card kw-card-A", style="margin-bottom:0; padding:10px;",
          div(style="display:flex; align-items:center; gap:6px; margin-bottom:3px;",
            span(class="kw-tag-A", "Panel A"),
            span(style="font-size:11px; color:#64748b;",
                 textOutput(ns("label_A"), inline=TRUE))
          ),
          div(style="margin-bottom:2px;",
            tags$span("PERIOD:", style="font-size:10px; font-weight:700;
                                         color:#334155; margin-right:6px;"),
            checkboxGroupInput(ns("periods_A"), label=NULL,
              choices  = c("Pre-Embargo"    = "Pre-Embargo",
                           "Embargo→Leak"   = "Embargo to Leak",
                           "Post-Leak"      = "Post-Leak"),
              selected = "Pre-Embargo", inline=TRUE)
          ),
          div(style="margin-bottom:6px;",
            tags$span("VIEW:", style="font-size:10px; font-weight:700;
                                       color:#334155; margin-right:6px;"),
            actionButton(ns("wc_A"),  "Word Cloud",
                         class="btn btn-sm btn-outline-secondary",
                         style="margin-right:3px;"),
            actionButton(ns("net_A"), "Semantic Network",
                         class="btn btn-sm btn-outline-secondary",
                         style="margin-right:3px;"),
            actionButton(ns("tl_A"),  "Risk Timeline",
                         class="btn btn-sm btn-outline-secondary")
          ),
          plotOutput(ns("chart_A"), height="300px", width="100%")
        )
      ),
      column(6,
        div(class="kw-card kw-card-B", style="margin-bottom:0; padding:10px;",
          div(style="display:flex; align-items:center; gap:6px; margin-bottom:3px;",
            span(class="kw-tag-B", "Panel B"),
            span(style="font-size:11px; color:#64748b;",
                 textOutput(ns("label_B"), inline=TRUE))
          ),
          div(style="margin-bottom:2px;",
            tags$span("PERIOD:", style="font-size:10px; font-weight:700;
                                         color:#334155; margin-right:6px;"),
            checkboxGroupInput(ns("periods_B"), label=NULL,
              choices  = c("Pre-Embargo"    = "Pre-Embargo",
                           "Embargo→Leak"   = "Embargo to Leak",
                           "Post-Leak"      = "Post-Leak"),
              selected = "Post-Leak", inline=TRUE)
          ),
          div(style="margin-bottom:6px;",
            tags$span("VIEW:", style="font-size:10px; font-weight:700;
                                       color:#334155; margin-right:6px;"),
            actionButton(ns("wc_B"),  "Word Cloud",
                         class="btn btn-sm btn-outline-secondary",
                         style="margin-right:3px;"),
            actionButton(ns("net_B"), "Semantic Network",
                         class="btn btn-sm btn-outline-secondary",
                         style="margin-right:3px;"),
            actionButton(ns("tl_B"),  "Risk Timeline",
                         class="btn btn-sm btn-outline-secondary")
          ),
          plotOutput(ns("chart_B"), height="300px", width="100%")
        )
      )
    ),

    

    # ── SHARED TABLE ─────────────────────────────────────────────────────────
    fluidRow(class="kw-row-table",
      column(12,
        div(class="kw-card", style="margin-top:0;",
          tags$b("Signal Log", style="font-size:11px; color:#334155;
                  text-transform:uppercase;"),
          tags$p("Filter by layer and risk band, then search within results.",
                 style="font-size:11px; color:#94a3b8; margin:4px 0 10px;"),
          # smart filter row
          fluidRow(
            column(5,
              tags$span("Layer", style="font-size:10px; font-weight:700;
                          color:#334155; text-transform:uppercase;"),
              checkboxGroupInput(ns("tbl_layer"), label=NULL, inline=TRUE,
                choices  = c("PT Risk Score"   = "PT Risk Score",
                             "Warning Signal"  = "Warning Signal",
                             "Off-script Post" = "Off-script Post"),
                selected = c("PT Risk Score","Warning Signal","Off-script Post"))
            ),
            column(4,
              tags$span("Risk band (PT only)", style="font-size:10px;
                          font-weight:700; color:#334155; text-transform:uppercase;"),
              checkboxGroupInput(ns("tbl_risk"), label=NULL, inline=TRUE,
                choices  = c("LOW","MODERATE","HIGH","CRITICAL"),
                selected = c("LOW","MODERATE","HIGH","CRITICAL"))
            ),
            column(3,
              tags$span("Sort by", style="font-size:10px; font-weight:700;
                          color:#334155; text-transform:uppercase;"),
              selectInput(ns("tbl_sort"), label=NULL, width="100%",
                choices = c("Date (newest first)" = "date_desc",
                            "Date (oldest first)" = "date_asc",
                            "Risk band"            = "risk"))
            )
          ),
          DTOutput(ns("msg_table"))
        )
      )
    )
  )
}

# ── SHINY MODULE SERVER ────────────────────────────────────────────────────────
keywordServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    chart_A <- reactiveVal("wc")
    chart_B <- reactiveVal("wc")

    observeEvent(input$wc_A,  chart_A("wc"))
    observeEvent(input$net_A, chart_A("net"))
    observeEvent(input$tl_A,  chart_A("tl"))
    observeEvent(input$wc_B,  chart_B("wc"))
    observeEvent(input$net_B, chart_B("net"))
    observeEvent(input$tl_B,  chart_B("tl"))

    # Parsed seeds (reactive, updates on Update button)
    seeds <- eventReactive(input$go, {
      str_split(input$seeds_raw, ",")[[1]] %>%
        str_trim() %>% str_to_lower() %>% keep(~nchar(.x) > 0)
    }, ignoreNULL = FALSE)

    # Filtered messages per panel (independent periods, shared channel/state filters)
    msgs_A <- eventReactive(input$go, {
      req(length(input$periods_A) > 0)
      filter_msgs(input$periods_A, input$channels, input$int_states)
    }, ignoreNULL = FALSE)

    msgs_B <- eventReactive(input$go, {
      req(length(input$periods_B) > 0)
      filter_msgs(input$periods_B, input$channels, input$int_states)
    }, ignoreNULL = FALSE)

    # Panel labels
    output$label_A <- renderText(paste(input$periods_A, collapse=" + "))
    output$label_B <- renderText(paste(input$periods_B, collapse=" + "))

    # Chart dispatcher
    make_chart <- function(msgs_fn, periods_fn, chart_rv) {
      renderPlot({
        switch(chart_rv(),
          wc  = plot_wordcloud(msgs_fn(), seeds(), input$nlp_mode,
                               input$threshold),
          net = plot_network(msgs_fn(), seeds(),
                             threshold = input$threshold,
                             mode      = input$nlp_mode),
          tl  = plot_risk_timeline(periods_fn(), seeds(),
                               input$nlp_mode, input$threshold)
        )
      }, res = 96)
    }

    output$chart_A <- make_chart(msgs_A, reactive(input$periods_A), chart_A)
    output$chart_B <- make_chart(msgs_B, reactive(input$periods_B), chart_B)

    # Shared table — built from all three timeline layers
    # reactive table data (recomputed on Update)
    tbl_data <- eventReactive(input$go, {
      req(input$periods_A, input$periods_B)
      periods_all <- unique(c(input$periods_A, input$periods_B))
      build_timeline_table(
        periods   = periods_all,
        seeds     = seeds(),
        mode      = input$nlp_mode,
        threshold = input$threshold
      )
    }, ignoreNULL = FALSE)

    output$msg_table <- renderDT({
      tbl <- tbl_data()
      if (is.null(tbl) || nrow(tbl) == 0) return(NULL)

      # safely add Date_sort if missing
      if (!"Date_sort" %in% names(tbl)) {
        tbl <- tbl %>% mutate(Date_sort = as.POSIXct(Date, format="%d %b %Y %H:%M"))
      }

      # apply layer filter
      tbl <- tbl %>% filter(Layer %in% input$tbl_layer)

      # apply risk band filter
      tbl <- tbl %>%
        filter(Layer != "PT Risk Score" | risk_band %in% input$tbl_risk)

      if (nrow(tbl) == 0) return(NULL)

      # sort using Date_sort (POSIXct)
      tbl <- switch(input$tbl_sort,
        date_desc = tbl %>% arrange(desc(Date_sort)),
        date_asc  = tbl %>% arrange(Date_sort),
        risk      = tbl %>% mutate(
          risk_order = case_when(
            risk_band == "CRITICAL" ~ 1,
            risk_band == "HIGH"     ~ 2,
            risk_band == "MODERATE" ~ 3,
            risk_band == "LOW"      ~ 4,
            TRUE                    ~ 5
          )) %>% arrange(risk_order, Date_sort) %>% select(-risk_order),
        tbl
      )

      # remove Date_sort before display
      tbl <- tbl %>% select(-Date_sort)

      # colour mapping
      bg_vals <- c(CRITICAL="#fee2e2", HIGH="#fecaca",
                   MODERATE="#fef3c7", LOW="#dcfce7", NONE="#ffffff")
      fg_vals <- c(CRITICAL="#7f1d1d", HIGH="#b91c1c",
                   MODERATE="#b45309", LOW="#15803d", NONE="#1e293b")

      # keep risk_band as last column for styling then hide it
      tbl_display <- tbl %>% select(-risk_band) %>%
        mutate(risk_band = tbl$risk_band)

      datatable(
        tbl_display,
        rownames = FALSE,
        filter   = "none",
        options  = list(
          pageLength = 5,
          scrollX    = TRUE,
          dom        = "tip",
          columnDefs = list(
            list(width="30%", targets=6),
            list(width="13%", targets=0),
            list(width="10%", targets=2),
            list(visible=FALSE, targets=7)
          )
        ),
        class = "compact"
      ) %>%
        formatStyle(
          "Signal",
          valueColumns    = "risk_band",
          backgroundColor = styleEqual(
            c("CRITICAL","HIGH","MODERATE","LOW","NONE"),
            c("#fee2e2","#fecaca","#fef3c7","#dcfce7","#ffffff")
          ),
          color = styleEqual(
            c("CRITICAL","HIGH","MODERATE","LOW","NONE"),
            c("#7f1d1d","#b91c1c","#b45309","#15803d","#1e293b")
          ),
          fontWeight = "bold"
        ) %>%
        formatStyle(
          "Layer",
          backgroundColor = styleEqual(
            c("PT Risk Score","Warning Signal","Off-script Post"),
            c("#f0fdf4","#fffbeb","#eff6ff")
          ),
          fontWeight = "bold"
        )
    })
  })
}

message("keyword.R loaded — all sub-modules sourced.")
