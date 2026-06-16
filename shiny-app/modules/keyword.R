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
      .kw-hdr    { background:#1a2940; color:white; padding:10px 18px;
                   margin-bottom:12px; }
      .kw-hdr h3 { margin:0; font-size:16px; font-weight:700; display:inline; }
      .kw-hdr span { font-size:11px; opacity:.6; margin-left:12px; }
      .kw-card   { background:white; border-radius:8px; padding:14px;
                   box-shadow:0 1px 4px rgba(0,0,0,.08); margin-bottom:10px; }
      .kw-card-A { border-top:3px solid #1d4ed8; }
      .kw-card-B { border-top:3px solid #15803d; }
      .kw-ctrl   { font-size:10px; font-weight:700; color:#334155;
                   text-transform:uppercase; letter-spacing:.04em;
                   margin:10px 0 3px; display:block; }
      .kw-hint   { font-size:10px; color:#94a3b8; margin:-1px 0 5px; }
      .kw-tag-A  { background:#dbeafe; color:#1d4ed8; padding:2px 8px;
                   border-radius:3px; font-size:10px; font-weight:700; }
      .kw-tag-B  { background:#dcfce7; color:#15803d; padding:2px 8px;
                   border-radius:3px; font-size:10px; font-weight:700; }
      hr.kw-hr   { margin:8px 0; border-color:#f1f5f9; }
    "))),

    div(class="kw-hdr",
      h3("Keyword Explorer"),
      span("Compare language & risk signals across embargo timeline periods")
    ),

    fluidRow(
      # ── SHARED CONTROLS ──────────────────────────────────────────────────────
      column(2,
        div(class="kw-card",
          tags$b("Shared Controls", style="font-size:11px; color:#334155;
                  text-transform:uppercase;"),

          tags$span("Channels", class="kw-ctrl"),
          checkboxGroupInput(ns("channels"), label=NULL,
            choices  = c("Group huddle"          = "comms_huddle",
                         "1-on-1 / side huddle"  = "private",
                         "Public posts"           = "public_post"),
            selected = c("comms_huddle","private","public_post")),

          hr(class="kw-hr"),
          tags$span("Internal States", class="kw-ctrl"),
          tags$p("Add agents' inner reasoning to corpus.", class="kw-hint"),
          checkboxGroupInput(ns("int_states"), label=NULL,
            choices  = c("Deliberating"  = "deliberating",
                         "Rationalizing" = "rationalizing",
                         "Reacting"      = "reacting"),
            selected = character(0)),

          hr(class="kw-hr"),
          tags$span("Seed Words", class="kw-ctrl"),
          tags$p("Comma-separated.", class="kw-hint"),
          textAreaInput(ns("seeds_raw"), label=NULL,
                        value=DEFAULT_SEEDS, rows=3, width="100%"),

          hr(class="kw-hr"),
          tags$span("Analysis Mode", class="kw-ctrl"),
          radioButtons(ns("nlp_mode"), label=NULL,
            choices  = c("Cosine (TF-IDF)"   = "cosine",
                         "Simple frequency"   = "freq"),
            selected = "cosine"),

          conditionalPanel(
            condition = sprintf("input['%s'] == 'cosine'", ns("nlp_mode")),
            tags$span("Cosine Threshold", class="kw-ctrl"),
            sliderInput(ns("threshold"), label=NULL,
                        min=0.05, max=0.50, value=0.15, step=0.01)
          ),

          hr(class="kw-hr"),
          actionButton(ns("go"), "Update", icon=icon("rotate"),
                       class="btn-primary", width="100%",
                       style="font-weight:600; font-size:12px;")
        )
      ),

      # ── PANEL A ──────────────────────────────────────────────────────────────
      column(5,
        div(class="kw-card kw-card-A",
          div(style="display:flex; align-items:center; gap:8px; margin-bottom:8px;",
            span(class="kw-tag-A", "Panel A"),
            span(style="font-size:11px; color:#64748b;",
                 textOutput(ns("label_A"), inline=TRUE))
          ),
          # Period selector
          div(style="margin-bottom:6px;",
            tags$span("PERIOD:", style="font-size:10px; font-weight:700;
                                         color:#334155; margin-right:6px;"),
            checkboxGroupInput(ns("periods_A"), label=NULL,
              choices  = c("Pre-Embargo"    = "Pre-Embargo",
                           "Embargo→Leak"   = "Embargo to Leak",
                           "Post-Leak"      = "Post-Leak"),
              selected = "Pre-Embargo", inline=TRUE)
          ),
          # Chart switcher
          div(style="margin-bottom:10px;",
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
          plotOutput(ns("chart_A"), height="400px")
        )
      ),

      # ── PANEL B ──────────────────────────────────────────────────────────────
      column(5,
        div(class="kw-card kw-card-B",
          div(style="display:flex; align-items:center; gap:8px; margin-bottom:8px;",
            span(class="kw-tag-B", "Panel B"),
            span(style="font-size:11px; color:#64748b;",
                 textOutput(ns("label_B"), inline=TRUE))
          ),
          div(style="margin-bottom:6px;",
            tags$span("PERIOD:", style="font-size:10px; font-weight:700;
                                         color:#334155; margin-right:6px;"),
            checkboxGroupInput(ns("periods_B"), label=NULL,
              choices  = c("Pre-Embargo"    = "Pre-Embargo",
                           "Embargo→Leak"   = "Embargo to Leak",
                           "Post-Leak"      = "Post-Leak"),
              selected = "Post-Leak", inline=TRUE)
          ),
          div(style="margin-bottom:10px;",
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
          plotOutput(ns("chart_B"), height="400px")
        )
      )
    ),

    # ── SHARED TABLE ─────────────────────────────────────────────────────────
    fluidRow(
      column(10, offset=2,
        div(class="kw-card",
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
          net = plot_network(msgs_fn(), seeds(), input$threshold),
          tl  = plot_risk_timeline(periods_fn(), seeds(),
                               input$nlp_mode, input$threshold)
        )
      })
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

      # apply layer filter
      tbl <- tbl %>% filter(Layer %in% input$tbl_layer)

      # apply risk band filter (only relevant for PT Risk Score rows)
      tbl <- tbl %>%
        filter(Layer != "PT Risk Score" | risk_band %in% input$tbl_risk)

      if (nrow(tbl) == 0) return(NULL)

      # apply sort
      tbl <- switch(input$tbl_sort,
        date_desc = tbl %>% arrange(desc(Date)),
        date_asc  = tbl %>% arrange(Date),
        risk      = tbl %>% mutate(
          risk_order = case_when(
            risk_band == "CRITICAL" ~ 1,
            risk_band == "HIGH"     ~ 2,
            risk_band == "MODERATE" ~ 3,
            risk_band == "LOW"      ~ 4,
            TRUE                    ~ 5
          )) %>% arrange(risk_order, desc(Date)) %>% select(-risk_order),
        tbl
      )

      # colour mapping
      bg_vals <- c(CRITICAL="#fee2e2", HIGH="#fecaca",
                   MODERATE="#fef3c7", LOW="#dcfce7", NONE="#ffffff")
      fg_vals <- c(CRITICAL="#7f1d1d", HIGH="#b91c1c",
                   MODERATE="#b45309", LOW="#15803d", NONE="#1e293b")

      datatable(
        tbl %>% select(-risk_band),
        rownames = FALSE,
        filter   = "none",   # using our own filters above DT
        options  = list(
          pageLength = 5,
          scrollX    = TRUE,
          dom        = "tip",   # no built-in search bar — we have our own
          columnDefs = list(
            list(width="30%", targets=6),  # Excerpt
            list(width="13%", targets=0),  # Date
            list(width="10%", targets=2)   # Layer
          )
        ),
        class = "compact stripe"
      ) %>%
        formatStyle(
          "Signal",
          backgroundColor = styleEqual(tbl$Signal, bg_vals[tbl$risk_band]),
          color           = styleEqual(tbl$Signal, fg_vals[tbl$risk_band]),
          fontWeight      = "bold"
        ) %>%
        formatStyle(
          "Layer",
          backgroundColor = styleEqual(
            c("PT Risk Score","Warning Signal","Off-script Post"),
            c("#f0fdf4",      "#fffbeb",       "#eff6ff")
          ),
          fontWeight = "bold"
        )
    })
  })
}

message("keyword.R loaded — all sub-modules sourced.")
