# ============================================================
# VAST Challenge 2026 · Group 5
# Module: Swimlane Plot
# Author: Jiang Yuxi
# ============================================================
# This module presents an interactive timeline showing each
# agent's declared behaviour versus actual communication
# behaviour over time.
# Key features:
#   - Agent Behaviour Heatmap (declared vs actual)
#   - Behaviour Mismatch / Divergence Table
#   - Compliance Violations
# ============================================================

pacman::p_load(
  tidyverse, patchwork, ggrepel, ggiraph, DT,
  gifski, plotly, gganimate, ggpp, ggtext, ggdist, ggridges,
  colorspace, geomtextpath, nord, nortest, seriation, dendextend,
  heatmaply, ggstatsplot, jsonlite, janitor, listviewer, gt,
  lubridate, scales, tidytext, proxy, cluster, wordcloud2,
  htmlwidgets, ggwordcloud, stringr, DiagrammeR, tidygraph,
  ggraph, igraph, kableExtra, widyr, text2vec
)

library(shiny)
library(bslib)

# ── Global: Data Loading (runs once on app start) ────────────
# Embargo was declared on May 23, 2046
embargo_date <- ymd_hms("2046-05-23 00:00:00")

raw <- fromJSON("data/MC1_final_00.json", simplifyVector = FALSE)

messages_tbl <- map_dfr(raw$rounds, function(r) {
  round_hour <- ymd_hms(r$hour)
  map_dfr(r$communications, function(m) {
    ist <- m$internal_state %||% list()
    tibble(
      round_hour    = round_hour,
      timestamp     = ymd_hms(m$timestamp),
      agent_id      = m$agent_id      %||% NA_character_,
      agent_label   = m$agent_label   %||% NA_character_,
      channel       = m$channel       %||% NA_character_,
      message_type  = m$message_type  %||% NA_character_,
      recipients    = list(unlist(m$recipients %||% list("ALL"))),
      content       = m$content       %||% NA_character_,
      deliberating  = ist$deliberating  %||% NA_character_,
      reacting      = ist$reacting      %||% NA_character_,
      rationalizing = ist$rationalizing %||% NA_character_
    )
  })
})

# ── Derive effective action per agent per round ──────────────
action_tbl <- messages_tbl %>%
  group_by(round_hour, agent_id, agent_label) %>%
  summarise(
    channels   = list(unique(channel)),
    n_messages = n(),
    .groups    = "drop"
  ) %>%
  mutate(
    effective_action = case_when(
      map_lgl(channels, ~any(.x == "anonymous_post")) ~ "Anonymous post",
      map_lgl(channels, ~any(.x == "personal_post"))  ~ "Personal post",
      map_lgl(channels, ~any(.x == "official_post"))  ~ "Official post",
      map_lgl(channels, ~any(.x == "side_huddle"))    ~ "Side huddle only",
      TRUE                                             ~ "Monitoring"
    ),
    period = case_when(
      round_hour < embargo_date                          ~ "Pre-Embargo",
      round_hour >= embargo_date &
        as.Date(round_hour) < as.Date("2046-06-05")     ~ "Post-Embargo",
      TRUE                                               ~ "Crisis Day"
    ),
    agent_label = coalesce(agent_label, agent_id)
  )

# ── Extract Crisis Day declared actions from participants ────
declared_crisis <- map_dfr(raw$rounds, function(r) {
  map_dfr(r$participants %||% list(), function(p) {
    da <- p$declared_action %||% NA_character_
    if (is.na(da)) return(NULL)
    tibble(
      round_hour    = ymd_hms(r$hour),
      agent_id      = p$agent_id %||% NA_character_,
      declared      = da,
      declared_type = case_when(
        str_detect(da, "POSTED_ANONYMOUS")   ~ "Anonymous post",
        str_detect(da, "POSTED_PERSONAL")    ~ "Personal post",
        str_detect(da, "POSTED_ON_FLEX")     ~ "Official post",
        str_detect(da, "COMPLIANCE_WARNING") ~ "Compliance warning",
        str_detect(da, "MONITORING")         ~ "Monitoring",
        TRUE                                 ~ "Other"
      )
    )
  })
})

# ── Merge declared + effective into final display table ──────
action_final_tbl <- action_tbl %>%
  left_join(
    declared_crisis %>% select(round_hour, agent_id, declared_type),
    by = c("round_hour", "agent_id")
  ) %>%
  mutate(
    display_action = coalesce(declared_type, effective_action),
    display_action = factor(display_action,
                            levels = c("Monitoring", "Side huddle only", "Official post",
                                       "Personal post", "Anonymous post",
                                       "Compliance warning", "Other")
    ),
    divergence = case_when(
      !is.na(declared_type) &
        declared_type == "Monitoring" &
        effective_action %in% c("Anonymous post", "Personal post",
                                "Side huddle only") ~ "DIVERGED",
      TRUE ~ "Consistent"
    ),
    round_label = format(round_hour, "%b %d %H:%M"),
    round_label = factor(round_label,
                         levels = unique(round_label[order(round_hour)]))
  )

# ── Colour palette ───────────────────────────────────────────
action_colours <- c(
  "Monitoring"         = "#BDE0F5",
  "Side huddle only"   = "#FFF3CD",
  "Official post"      = "#1565C0",
  "Personal post"      = "#E65100",
  "Anonymous post"     = "#C62828",
  "Compliance warning" = "#5F5E5A",
  "Other"              = "#BBBBBB"
)

# ── Period boundary labels for annotation ────────────────────
period_labels <- action_final_tbl %>%
  group_by(period) %>%
  slice_min(round_hour, n = 1) %>%
  ungroup() %>%
  distinct(period, round_label) %>%
  mutate(
    colour = case_when(
      period == "Pre-Embargo"  ~ "#1565C0",
      period == "Post-Embargo" ~ "#E65100",
      period == "Crisis Day"   ~ "#B71C1C"
    ),
    y_pos = length(unique(action_final_tbl$agent_label)) + 0.7
  )

# ── Divergence summary table (declared MONITORING but posted)
diverged_tbl <- action_final_tbl %>%
  filter(divergence == "DIVERGED") %>%
  mutate(
    Date              = format(round_hour, "%d %b %H:%M"),
    Agent             = agent_label,
    `Declared`        = coalesce(declared_type, "—"),
    `Actual channels` = map_chr(channels, ~paste(sort(.x), collapse = ", ")),
    `Messages sent`   = n_messages
  ) %>%
  select(Date, Agent, Declared, `Actual channels`, `Messages sent`)

# ── Risk Rankings calculations ───────────────────────────────
# Violation weights: side_huddle = 1 (Low), personal_post = 2 (Medium),
#                   anonymous_post = 3 (High)
# Risk Score     = sum of per-event violation weights across all DIVERGED rounds
# Mismatch Count = number of DIVERGED rounds per agent
# Compliance Violation = highest severity level observed (Low / Medium / High / None)
# Consistency Score    = consistent rounds / total active rounds

risk_tbl <- action_final_tbl %>%
  group_by(agent_label) %>%
  summarise(
    total_active_rounds  = n(),
    mismatch_count       = sum(divergence == "DIVERGED"),
    consistent_rounds    = sum(divergence == "Consistent"),
    risk_score           = sum(case_when(
      divergence == "DIVERGED" & effective_action == "Anonymous post"   ~ 3,
      divergence == "DIVERGED" & effective_action == "Personal post"    ~ 2,
      divergence == "DIVERGED" & effective_action == "Side huddle only" ~ 1,
      TRUE ~ 0
    )),
    max_weight           = max(case_when(
      divergence == "DIVERGED" & effective_action == "Anonymous post"   ~ 3L,
      divergence == "DIVERGED" & effective_action == "Personal post"    ~ 2L,
      divergence == "DIVERGED" & effective_action == "Side huddle only" ~ 1L,
      TRUE ~ 0L
    )),
    .groups = "drop"
  ) %>%
  mutate(
    compliance_violation = case_when(
      max_weight == 3 ~ "High",
      max_weight == 2 ~ "Medium",
      max_weight == 1 ~ "Low",
      TRUE            ~ "None"
    ),
    consistency_score = round(consistent_rounds / total_active_rounds, 2)
  ) %>%
  select(
    Agent                  = agent_label,
    `Mismatch Count`       = mismatch_count,
    `Compliance Violation` = compliance_violation,
    `Risk Score`           = risk_score,
    `Consistency Score`    = consistency_score
  ) %>%
  arrange(desc(`Risk Score`))

# ── Per-round violation score for behaviour scores heatmap ───
score_tbl <- action_final_tbl %>%
  mutate(
    violation_score = case_when(
      divergence == "DIVERGED" & effective_action == "Anonymous post"   ~ 3L,
      divergence == "DIVERGED" & effective_action == "Personal post"    ~ 2L,
      divergence == "DIVERGED" & effective_action == "Side huddle only" ~ 1L,
      TRUE ~ 0L
    ),
    score_label = case_when(
      violation_score == 3 ~ "High (3)",
      violation_score == 2 ~ "Medium (2)",
      violation_score == 1 ~ "Low (1)",
      TRUE                 ~ "Consistent (0)"
    ),
    score_label = factor(score_label,
                         levels = c("Consistent (0)", "Low (1)",
                                    "Medium (2)", "High (3)"))
  )

# ── Helper: build heatmap ggplot ─────────────────────────────
build_heatmap <- function(data, highlight_anomalies = TRUE) {
  n_agents <- length(unique(data$agent_label))
  
  embargo_idx <- which(
    levels(action_final_tbl$round_label) == format(embargo_date, "%b %d %H:%M")
  )
  
  p <- ggplot(data, aes(x = round_label, y = agent_label, fill = display_action)) +
    geom_tile(colour = "white", linewidth = 0.5)
  
  if (highlight_anomalies) {
    p <- p +
      geom_tile(
        data      = data %>% filter(divergence == "DIVERGED"),
        aes(x     = round_label, y = agent_label),
        fill      = NA,
        colour    = "#B71C1C",
        linewidth = 1.8
      )
  }
  
  if (length(embargo_idx) > 0) {
    p <- p +
      geom_vline(
        xintercept = embargo_idx,
        colour     = "#B71C1C",
        linewidth  = 1.2,
        linetype   = "solid"
      ) +
      annotate(
        "text",
        x        = embargo_idx + 0.15,
        y        = 2.0,
        label    = "Embargo declared",
        colour   = "#B71C1C",
        size     = 3,
        hjust    = 0,
        fontface = "italic"
      )
  }
  
  # Period labels — recompute y_pos from filtered data
  pl <- period_labels %>%
    filter(round_label %in% levels(data$round_label)) %>%
    mutate(y_pos = n_agents + 0.7)
  
  p +
    geom_text(
      data        = pl,
      aes(x = round_label, y = y_pos, label = period, colour = colour),
      hjust       = 0,
      size        = 3.5,
      fontface    = "bold",
      inherit.aes = FALSE
    ) +
    scale_fill_manual(values = action_colours, name = "Action") +
    scale_colour_identity() +
    scale_y_discrete(expand = expansion(add = c(0.5, 1.2))) +
    labs(
      title    = "Agent behaviour across all rounds — Pre-Embargo to Crisis Day",
      subtitle = paste0(
        "Colour = effective action (actual channels used). White = agent not active that round.\n",
        "On Crisis Day, declared action shown where available.\n",
        "Red border = declared MONITORING but used anonymous/personal/side-huddle channels.\n",
        "Red vertical line = embargo declared (May 23)."
      ),
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x   = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y   = element_text(size = 10),
      legend.position = "bottom",
      plot.title    = element_text(size = 13, face = "bold"),
      plot.subtitle = element_text(size = 9, colour = "grey40"),
      panel.grid    = element_blank()
    )
}


# ── Helper: build behaviour scores heatmap ───────────────────
build_score_heatmap <- function(data) {
  score_colours <- c(
    "Consistent (0)" = "#E8F5E9",
    "Low (1)"        = "#FFF176",
    "Medium (2)"     = "#FF8A65",
    "High (3)"       = "#C62828"
  )
  
  score_data <- score_tbl %>%
    filter(
      agent_label %in% unique(data$agent_label),
      round_label %in% levels(data$round_label)
    ) %>%
    mutate(round_label = factor(round_label, levels = levels(data$round_label)))
  
  n_agents <- length(unique(score_data$agent_label))
  
  embargo_idx <- which(
    levels(action_final_tbl$round_label) == format(embargo_date, "%b %d %H:%M")
  )
  
  pl <- period_labels %>%
    filter(round_label %in% levels(data$round_label)) %>%
    mutate(y_pos = n_agents + 0.7)
  
  p <- ggplot(score_data,
              aes(x = round_label, y = agent_label, fill = score_label)) +
    geom_tile(colour = "white", linewidth = 0.5)
  
  if (length(embargo_idx) > 0) {
    p <- p +
      geom_vline(xintercept = embargo_idx, colour = "#B71C1C",
                 linewidth = 1.2, linetype = "solid") +
      annotate("text", x = embargo_idx + 0.15, y = 2.0,
               label = "Embargo declared", colour = "#B71C1C",
               size = 3, hjust = 0, fontface = "italic")
  }
  
  p +
    geom_text(
      data        = pl,
      aes(x = round_label, y = y_pos, label = period, colour = colour),
      hjust       = 0, size = 3.5, fontface = "bold", inherit.aes = FALSE
    ) +
    scale_fill_manual(values = score_colours, name = "Violation Severity",
                      drop = FALSE) +
    scale_colour_identity() +
    scale_y_discrete(expand = expansion(add = c(0.5, 1.2))) +
    labs(
      title    = "Per-round violation severity — Behaviour Scores Heatmap",
      subtitle = paste0(
        "Fill = mismatch violation weight when declared MONITORING but posted.\n",
        "Green = consistent behaviour.  Yellow = Low (side huddle).  ",
        "Orange = Medium (personal post).  Red = High (anonymous post).\n",
        "Red vertical line = embargo declared (May 23)."
      ),
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x     = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y     = element_text(size = 10),
      legend.position = "bottom",
      plot.title      = element_text(size = 13, face = "bold"),
      plot.subtitle   = element_text(size = 9, colour = "grey40"),
      panel.grid      = element_blank()
    )
}

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
        choices  = c("All Agents", sort(unique(action_final_tbl$agent_label))),
        selected = "All Agents",
        multiple = TRUE
      ),
      
      # ── Timeline ──
      dateRangeInput(
        ns("timeline"),
        "Timeline Range",
        start = as.Date(min(action_final_tbl$round_hour)),
        end   = as.Date(max(action_final_tbl$round_hour))
      ),
      
      hr(),
      
      # ── Indicators ──
      checkboxGroupInput(
        ns("indicators"),
        "Show Indicators",
        choices = c(
          "Behaviour Mismatch Count"    = "mismatch",
          "Compliance Violations"       = "compliance",
          "Agent Risk Ranking"          = "risk",
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
            plotOutput(ns("swimlane_plot"), height = "500px"),
            hr(),
            tags$p(
              tags$em(
                "Rounds where agent declared ",
                tags$strong("MONITORING"),
                " but actually used anonymous, personal, or side-huddle channels:"
              )
            ),
            tags$style(HTML("
              .diverge-table thead tr th {
                background-color: #B71C1C !important;
                color: white;
                font-size: 12px;
              }
              .diverge-table tbody tr:hover {
                background-color: #fdecea !important;
              }
            ")),
            DTOutput(ns("diverge_table"))
          )
        ),
        
        nav_panel(
          "Behaviour Scores",
          card_body(
            plotOutput(ns("score_heatmap"), height = "500px")
          )
        ),
        
        nav_panel(
          "Risk Rankings",
          card_body(
            tags$style(HTML("
              .risk-table thead tr th {
                background-color: #1565C0 !important;
                color: white;
                font-size: 12px;
              }
              .risk-table tbody tr:hover { background-color: #e3f2fd !important; }
            ")),
            DTOutput(ns("risk_table"))
          )
        )
      )
    )
  )
}


# ── Server ──────────────────────────────────────────────────
swimlaneServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # ── Filtered data reactive ────────────────────────────
    filtered_data <- reactive({
      data <- action_final_tbl
      
      # Agent filter
      if (!is.null(input$agents) && !"All Agents" %in% input$agents) {
        data <- data %>% filter(agent_label %in% input$agents)
      }
      
      # Date filter
      data <- data %>%
        filter(
          as.Date(round_hour) >= input$timeline[1],
          as.Date(round_hour) <= input$timeline[2]
        )
      
      # Drop unused round_label factor levels so x-axis stays clean
      data <- data %>%
        mutate(round_label = droplevels(round_label))
      
      data
    })
    
    # ── Swimlane Heatmap ──────────────────────────────────
    output$swimlane_plot <- renderPlot({
      req(nrow(filtered_data()) > 0)
      build_heatmap(filtered_data(), highlight_anomalies = input$highlight_anomalies)
    }, height = 500)
    
    # ── Behaviour Scores Heatmap ──────────────────────────
    output$score_heatmap <- renderPlot({
      req(nrow(filtered_data()) > 0)
      build_score_heatmap(filtered_data())
    }, height = 500)
    
    # ── Risk Rankings Table ───────────────────────────────
    output$risk_table <- renderDT({
      # Filter risk_tbl to selected agents
      rt <- if (!is.null(input$agents) && !"All Agents" %in% input$agents) {
        risk_tbl %>% filter(Agent %in% input$agents)
      } else {
        risk_tbl
      }
      
      violation_colours <- c(
        "High"   = "#FFCDD2",
        "Medium" = "#FFE0B2",
        "Low"    = "#FFF9C4",
        "None"   = "#F1F8E9"
      )
      
      datatable(
        rt,
        class    = "risk-table",
        rownames = FALSE,
        options  = list(
          pageLength = 20,
          dom        = "tip",
          order      = list(list(3, "desc")),   # sort by Risk Score desc
          columnDefs = list(
            list(className = "dt-center",
                 targets   = c(1, 2, 3, 4))
          )
        )
      ) %>%
        formatStyle(
          columns         = names(rt),
          fontSize        = "12px"
        ) %>%
        formatStyle(
          "Compliance Violation",
          backgroundColor = styleEqual(
            names(violation_colours),
            unname(violation_colours)
          )
        ) %>%
        formatStyle(
          "Risk Score",
          background = styleColorBar(rt$`Risk Score`, "#1565C0"),
          backgroundSize   = "100% 80%",
          backgroundRepeat = "no-repeat",
          backgroundPosition = "center"
        ) %>%
        formatStyle(
          "Consistency Score",
          background = styleColorBar(c(0, 1), "#43A047"),
          backgroundSize   = "100% 80%",
          backgroundRepeat = "no-repeat",
          backgroundPosition = "center"
        )
    })
    
    # ── Divergence Table ──────────────────────────────────
    output$diverge_table <- renderDT({
      div_data <- filtered_data() %>%
        filter(divergence == "DIVERGED") %>%
        mutate(
          Date              = format(round_hour, "%d %b %H:%M"),
          Agent             = agent_label,
          Declared          = coalesce(declared_type, "—"),
          `Actual channels` = map_chr(channels, ~paste(sort(.x), collapse = ", ")),
          `Messages sent`   = n_messages
        ) %>%
        select(Date, Agent, Declared, `Actual channels`, `Messages sent`)
      
      datatable(
        div_data,
        class     = "diverge-table",
        rownames  = FALSE,
        options   = list(
          pageLength = 15,
          dom        = "tip",
          columnDefs = list(
            list(width = "10%",  targets = 0),
            list(width = "12%",  targets = 1),
            list(width = "15%",  targets = 2),
            list(width = "48%",  targets = 3),
            list(width = "10%",  className = "dt-center", targets = 4)
          )
        )
      ) %>%
        formatStyle(
          columns         = names(div_data),
          fontSize        = "11px"
        ) %>%
        formatStyle(
          columns         = 0,
          target          = "row",
          backgroundColor = styleEqual("DIVERGED", "#fdecea")
        )
    })
    
  })
}
