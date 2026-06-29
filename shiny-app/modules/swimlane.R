# ============================================================
# VAST Challenge 2026 · Group 5
# Module: Swimlane Plot
# Author: Jiang Yuxi
# ============================================================

library(shiny)
library(bslib)
library(dplyr)
library(purrr)
library(ggplot2)
library(stringr)
library(tibble)
library(lubridate)
library(jsonlite)
library(DT)

# ── Global: Data Loading ─────────────────────────────────────
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

# ── Effective action per agent per round ─────────────────────
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

# ── Crisis Day declared actions ──────────────────────────────
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

# ── Final merged table ───────────────────────────────────────
action_final_tbl <- action_tbl %>%
  left_join(
    declared_crisis %>% select(round_hour, agent_id, declared_type),
    by = c("round_hour", "agent_id")
  ) %>%
  mutate(
    display_action = coalesce(declared_type, effective_action),
    display_action = factor(display_action,
                            levels = c("Monitoring", "Side huddle only", "Official post",
                                       "Personal post", "Anonymous post", "Compliance warning", "Other")
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

period_fill <- c(
  "Pre-Embargo"  = "#E3F2FD",
  "Post-Embargo" = "#FFF8E1",
  "Crisis Day"   = "#FFEBEE"
)

# ── Period boundary labels ───────────────────────────────────
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

# ── Divergence trend per round ───────────────────────────────
divergence_trend_tbl <- action_final_tbl %>%
  group_by(round_hour, round_label, period) %>%
  summarise(
    n_diverged = sum(divergence == "DIVERGED"),
    n_active   = n(),
    .groups    = "drop"
  ) %>%
  arrange(round_hour) %>%
  mutate(
    period = factor(period, levels = c("Pre-Embargo", "Post-Embargo", "Crisis Day"))
  )

# ── Pre-breach zoom: last 5 rounds before June 5 ─────────────
prebreach_rounds <- action_final_tbl %>%
  filter(as.Date(round_hour) < as.Date("2046-06-05")) %>%
  pull(round_hour) %>% unique() %>% sort() %>% tail(5)

prebreach_tbl <- action_final_tbl %>%
  filter(round_hour %in% prebreach_rounds) %>%
  mutate(round_label = droplevels(round_label))

# ── Internal state per agent per round ───────────────────────
internal_state_tbl <- messages_tbl %>%
  mutate(
    agent_label  = coalesce(agent_label, agent_id),
    has_delib    = !is.na(deliberating)  & nchar(coalesce(deliberating,  "")) > 0,
    has_ration   = !is.na(rationalizing) & nchar(coalesce(rationalizing, "")) > 0
  ) %>%
  group_by(round_hour, agent_label) %>%
  summarise(
    deliberating_active  = any(has_delib),
    rationalizing_active = any(has_ration),
    .groups = "drop"
  ) %>%
  mutate(
    internal_label = case_when(
      rationalizing_active & deliberating_active ~ "Deliberating + Rationalizing",
      rationalizing_active                       ~ "Rationalizing only",
      deliberating_active                        ~ "Deliberating only",
      TRUE                                       ~ "No internal state"
    ),
    internal_label = factor(internal_label,
                            levels = c("No internal state", "Deliberating only",
                                       "Rationalizing only", "Deliberating + Rationalizing"))
  ) %>%
  left_join(
    action_final_tbl %>%
      select(round_hour, agent_label, round_label, period) %>% distinct(),
    by = c("round_hour", "agent_label")
  )

# ── Risk Rankings ────────────────────────────────────────────
calc_risk <- function(data) {
  data %>%
    group_by(agent_label) %>%
    summarise(
      total_active_rounds = n(),
      mismatch_count      = sum(divergence == "DIVERGED"),
      consistent_rounds   = sum(divergence == "Consistent"),
      risk_score          = sum(case_when(
        divergence == "DIVERGED" & effective_action == "Anonymous post"   ~ 3,
        divergence == "DIVERGED" & effective_action == "Personal post"    ~ 2,
        divergence == "DIVERGED" & effective_action == "Side huddle only" ~ 1,
        TRUE ~ 0
      )),
      max_weight = max(case_when(
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
    )
}

risk_base <- calc_risk(action_final_tbl)

# Period-level risk scores
risk_by_period <- action_final_tbl %>%
  group_by(agent_label, period) %>%
  summarise(
    period_risk = sum(case_when(
      divergence == "DIVERGED" & effective_action == "Anonymous post"   ~ 3,
      divergence == "DIVERGED" & effective_action == "Personal post"    ~ 2,
      divergence == "DIVERGED" & effective_action == "Side huddle only" ~ 1,
      TRUE ~ 0
    )),
    .groups = "drop"
  ) %>%
  group_by(agent_label) %>%
  summarise(
    `Pre-Embargo Score`  = sum(period_risk[period == "Pre-Embargo"],  na.rm = TRUE),
    `Post-Embargo Score` = sum(period_risk[period == "Post-Embargo"], na.rm = TRUE),
    `Crisis Score`       = sum(period_risk[period == "Crisis Day"],   na.rm = TRUE),
    .groups = "drop"
  )

risk_tbl <- risk_base %>%
  left_join(risk_by_period, by = "agent_label") %>%
  select(
    Agent                  = agent_label,
    `Mismatch Count`       = mismatch_count,
    `Compliance Violation` = compliance_violation,
    `Pre-Embargo Score`,
    `Post-Embargo Score`,
    `Crisis Score`,
    `Risk Score`           = risk_score,
    `Consistency Score`    = consistency_score
  ) %>%
  arrange(desc(`Risk Score`))

# ── Violation score tbl ──────────────────────────────────────
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

# ════════════════════════════════════════════════════════════
# HELPER FUNCTIONS
# ════════════════════════════════════════════════════════════

# ── Add period shading to any ggplot ─────────────────────────
add_period_bands <- function(p, data) {
  px <- data %>%
    group_by(period) %>%
    summarise(
      xmin = min(as.numeric(round_label)) - 0.5,
      xmax = max(as.numeric(round_label)) + 0.5,
      .groups = "drop"
    ) %>%
    left_join(
      tibble(period = names(period_fill), fill = unname(period_fill)),
      by = "period"
    )
  for (i in seq_len(nrow(px))) {
    p <- p + annotate("rect",
                      xmin = px$xmin[i], xmax = px$xmax[i],
                      ymin = -Inf, ymax = Inf,
                      fill = px$fill[i], alpha = 0.25)
  }
  p
}

# ── Swimlane heatmap ─────────────────────────────────────────
build_heatmap <- function(data, highlight_anomalies = TRUE) {
  n_agents <- length(unique(data$agent_label))
  
  embargo_idx <- which(
    levels(action_final_tbl$round_label) == format(embargo_date, "%b %d %H:%M")
  )
  
  p <- ggplot(data, aes(x = round_label, y = agent_label, fill = display_action)) +
    geom_tile(colour = "white", linewidth = 0.5)
  
  p <- add_period_bands(p, data)
  
  # re-draw tiles on top of bands
  p <- p + geom_tile(colour = "white", linewidth = 0.5)
  
  if (highlight_anomalies) {
    p <- p +
      geom_tile(
        data      = data %>% filter(divergence == "DIVERGED"),
        aes(x = round_label, y = agent_label),
        fill = NA, colour = "#B71C1C", linewidth = 1.8
      )
  }
  
  if (length(embargo_idx) > 0) {
    p <- p +
      geom_vline(xintercept = embargo_idx, colour = "#B71C1C",
                 linewidth = 1.2, linetype = "solid") +
      annotate("text", x = embargo_idx + 0.15, y = 2.0,
               label = "Embargo declared", colour = "#B71C1C",
               size = 3, hjust = 0, fontface = "italic")
  }
  
  pl <- period_labels %>%
    filter(round_label %in% levels(data$round_label)) %>%
    mutate(y_pos = n_agents + 0.7)
  
  p +
    geom_text(data = pl,
              aes(x = round_label, y = y_pos, label = period, colour = colour),
              hjust = 0, size = 3.5, fontface = "bold", inherit.aes = FALSE) +
    scale_fill_manual(values = action_colours, name = "Action") +
    scale_colour_identity() +
    scale_y_discrete(expand = expansion(add = c(0.5, 1.2))) +
    labs(
      title    = "Agent behaviour across all rounds — Pre-Embargo to Crisis Day",
      subtitle = paste0(
        "Shading = period (blue = Pre-Embargo, yellow = Post-Embargo, red = Crisis Day).\n",
        "Red border = declared MONITORING but used anonymous/personal/side-huddle channels.\n",
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

# ── Divergence trend line ────────────────────────────────────
build_divergence_trend <- function(data) {
  trend <- divergence_trend_tbl %>%
    filter(round_label %in% levels(data$round_label))
  
  period_colours <- c(
    "Pre-Embargo"  = "#1565C0",
    "Post-Embargo" = "#E65100",
    "Crisis Day"   = "#B71C1C"
  )
  
  embargo_idx <- which(
    levels(action_final_tbl$round_label) == format(embargo_date, "%b %d %H:%M")
  )
  
  p <- ggplot(trend, aes(x = round_label, y = n_diverged,
                         colour = period, group = 1)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2.5)
  
  p <- add_period_bands(p, data %>% mutate(round_label = droplevels(round_label)))
  
  p <- p +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2.5)
  
  if (length(embargo_idx) > 0) {
    p <- p +
      geom_vline(xintercept = embargo_idx, colour = "#B71C1C",
                 linewidth = 1, linetype = "dashed")
  }
  
  p +
    scale_colour_manual(values = period_colours, name = NULL) +
    labs(
      title = "Diverged agents per round",
      subtitle = "Count of agents who declared MONITORING but used a posting channel",
      x = NULL, y = "# Diverged agents"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x     = element_text(angle = 45, hjust = 1, size = 8),
      legend.position = "bottom",
      plot.title      = element_text(size = 12, face = "bold"),
      plot.subtitle   = element_text(size = 9, colour = "grey40"),
      panel.grid.minor = element_blank()
    )
}

# ── Behaviour score heatmap (faceted by period) ───────────────
build_score_heatmap <- function(data, severity = NULL) {
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
    )
  
  if (!is.null(severity) && !"All Severities" %in% severity) {
    score_data <- score_data %>%
      filter(as.character(score_label) %in% severity)
  }
  
  score_data <- score_data %>%
    mutate(
      round_label = factor(round_label, levels = levels(data$round_label)),
      period      = factor(period, levels = c("Pre-Embargo", "Post-Embargo", "Crisis Day"))
    )
  
  ggplot(score_data,
         aes(x = round_label, y = agent_label, fill = score_label)) +
    geom_tile(colour = "white", linewidth = 0.5) +
    facet_wrap(~period, scales = "free_x", nrow = 1) +
    scale_fill_manual(values = score_colours, name = "Violation Severity",
                      drop = FALSE) +
    labs(
      title    = "Per-round violation severity by period",
      subtitle = paste0(
        "Green = consistent.  Yellow = Low (side huddle).  ",
        "Orange = Medium (personal post).  Red = High (anonymous post)."
      ),
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x      = element_text(angle = 45, hjust = 1, size = 7),
      axis.text.y      = element_text(size = 9),
      legend.position  = "bottom",
      plot.title       = element_text(size = 13, face = "bold"),
      plot.subtitle    = element_text(size = 9, colour = "grey40"),
      strip.text       = element_text(face = "bold", size = 11),
      strip.background = element_rect(fill = "#F5F5F5", colour = NA),
      panel.grid       = element_blank(),
      panel.spacing    = unit(1, "lines")
    )
}

# ── Internal state heatmap ───────────────────────────────────
build_internal_heatmap <- function(data) {
  state_colours <- c(
    "No internal state"            = "#F5F5F5",
    "Deliberating only"            = "#BBDEFB",
    "Rationalizing only"           = "#FFCDD2",
    "Deliberating + Rationalizing" = "#CE93D8"
  )
  
  idata <- internal_state_tbl %>%
    filter(
      agent_label %in% unique(data$agent_label),
      round_label %in% levels(data$round_label)
    ) %>%
    mutate(
      round_label = factor(round_label, levels = levels(data$round_label)),
      period      = factor(period, levels = c("Pre-Embargo", "Post-Embargo", "Crisis Day"))
    )
  
  ggplot(idata,
         aes(x = round_label, y = agent_label, fill = internal_label)) +
    geom_tile(colour = "white", linewidth = 0.5) +
    facet_wrap(~period, scales = "free_x", nrow = 1) +
    scale_fill_manual(values = state_colours, name = "Internal State", drop = FALSE) +
    labs(
      title    = "Agent internal state activity by period",
      subtitle = paste0(
        "Blue = agent was deliberating (forward planning).  ",
        "Red = rationalising (post-hoc justification — potential red flag).  ",
        "Purple = both."
      ),
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x      = element_text(angle = 45, hjust = 1, size = 7),
      axis.text.y      = element_text(size = 9),
      legend.position  = "bottom",
      plot.title       = element_text(size = 13, face = "bold"),
      plot.subtitle    = element_text(size = 9, colour = "grey40"),
      strip.text       = element_text(face = "bold", size = 11),
      strip.background = element_rect(fill = "#F5F5F5", colour = NA),
      panel.grid       = element_blank(),
      panel.spacing    = unit(1, "lines")
    )
}

# ── Period stacked bar (Risk Rankings) ───────────────────────
build_period_bar <- function(data) {
  bar_data <- data %>%
    group_by(agent_label, period, divergence) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(
      period    = factor(period, levels = c("Pre-Embargo", "Post-Embargo", "Crisis Day")),
      divergence = factor(divergence, levels = c("Consistent", "DIVERGED"))
    )
  
  ggplot(bar_data, aes(x = agent_label, y = n, fill = divergence)) +
    geom_bar(stat = "identity", position = "stack", width = 0.7) +
    facet_wrap(~period, nrow = 1, scales = "free_x") +
    scale_fill_manual(
      values = c("Consistent" = "#81C784", "DIVERGED" = "#E53935"),
      name   = "Behaviour"
    ) +
    labs(
      title    = "Consistent vs Diverged rounds per agent by period",
      subtitle = "Compares agent compliance before the embargo, after it, and on Crisis Day",
      x = NULL, y = "Number of rounds"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x      = element_text(angle = 45, hjust = 1, size = 9),
      legend.position  = "bottom",
      plot.title       = element_text(size = 12, face = "bold"),
      plot.subtitle    = element_text(size = 9, colour = "grey40"),
      strip.text       = element_text(face = "bold", size = 11),
      strip.background = element_rect(fill = "#F5F5F5", colour = NA),
      panel.grid.major.x = element_blank(),
      panel.spacing    = unit(1, "lines")
    )
}


# ════════════════════════════════════════════════════════════
# UI HELPERS
# ════════════════════════════════════════════════════════════

# Renders a short description box above a plot or table.
# accent = left-border colour; text = plain string.
plot_desc <- function(text, accent = "#1565C0") {
  tags$div(
    style = paste0(
      "border-left: 4px solid ", accent, "; ",
      "background: #FAFAFA; ",
      "padding: 7px 12px; ",
      "margin-bottom: 10px; ",
      "border-radius: 0 4px 4px 0; ",
      "font-size: 12.5px; color: #444;"
    ),
    text
  )
}

# ════════════════════════════════════════════════════════════
# UI
# ════════════════════════════════════════════════════════════
swimlaneUI <- function(id) {
  ns <- NS(id)
  
  layout_sidebar(
    sidebar = sidebar(
      title = "Swimlane Controls",
      width = 290,
      
      # ── Agent Selection ──
      selectizeInput(
        ns("agents"),
        "Select Agent(s)",
        choices  = c("All Agents", sort(unique(action_final_tbl$agent_label))),
        selected = "All Agents",
        multiple = TRUE,
        options  = list(plugins = list("remove_button"))
      ),
      
      # ── Period ──
      selectInput(
        ns("period"),
        "Period",
        choices  = c("All Periods", "Pre-Embargo", "Post-Embargo", "Crisis Day"),
        selected = "All Periods"
      ),
      
      hr(),
      
      # ── Tab-specific filters ──
      conditionalPanel(
        condition = paste0("input['", ns("active_tab"), "'] === 'Swimlane Timeline'"),
        selectizeInput(
          ns("actions"),
          "Select Action(s)",
          choices  = c("All Actions", "Monitoring", "Side huddle only",
                       "Official post", "Personal post",
                       "Anonymous post", "Compliance warning"),
          selected = "All Actions",
          multiple = TRUE,
          options  = list(plugins = list("remove_button"))
        ),
        radioButtons(
          ns("tab1_view"),
          "Show Plot",
          choices  = c("Action Heatmap", "Divergence Trend", "Divergence Table"),
          selected = "Action Heatmap"
        )
      ),
      
      conditionalPanel(
        condition = paste0("input['", ns("active_tab"), "'] === 'Behaviour Scores'"),
        selectizeInput(
          ns("severity"),
          "Select Violation Severity",
          choices  = c("All Severities", "Consistent (0)", "Low (1)",
                       "Medium (2)", "High (3)"),
          selected = "All Severities",
          multiple = TRUE,
          options  = list(plugins = list("remove_button"))
        ),
        radioButtons(
          ns("tab2_view"),
          "Show Plot",
          choices  = c("Severity Heatmap", "Internal State Heatmap", "Summary Table"),
          selected = "Severity Heatmap"
        )
      ),
      
      conditionalPanel(
        condition = paste0("input['", ns("active_tab"), "'] === 'Risk Rankings'"),
        selectizeInput(
          ns("violation"),
          "Select Compliance Violation",
          choices  = c("All", "None", "Low", "Medium", "High"),
          selected = "All",
          multiple = TRUE,
          options  = list(plugins = list("remove_button"))
        ),
        radioButtons(
          ns("tab3_view"),
          "Show Plot",
          choices  = c("Period Bar Chart", "Risk Table"),
          selected = "Period Bar Chart"
        )
      ),
      
      hr(),
      
      conditionalPanel(
        condition = paste0("input['", ns("active_tab"), "'] === 'Swimlane Timeline'"),
        checkboxInput(ns("highlight_anomalies"), "Highlight Anomalies", value = TRUE),
        checkboxInput(ns("show_prebreach"),      "Show Pre-breach Zoom", value = FALSE)
      ),
      
      hr(),
      
      # ── Download buttons ──
      conditionalPanel(
        condition = paste0("input['", ns("active_tab"), "'] === 'Swimlane Timeline'"),
        downloadButton(ns("dl_swimlane"), "Download Heatmap",  class = "btn-sm btn-outline-secondary w-100 mb-1"),
        downloadButton(ns("dl_trend"),    "Download Trend",    class = "btn-sm btn-outline-secondary w-100 mb-1"),
        downloadButton(ns("dl_diverge"),  "Download Table",    class = "btn-sm btn-outline-secondary w-100")
      ),
      conditionalPanel(
        condition = paste0("input['", ns("active_tab"), "'] === 'Behaviour Scores'"),
        downloadButton(ns("dl_score"),    "Download Score Heatmap",   class = "btn-sm btn-outline-secondary w-100 mb-1"),
        downloadButton(ns("dl_internal"), "Download Internal Heatmap",class = "btn-sm btn-outline-secondary w-100 mb-1"),
        downloadButton(ns("dl_score_tbl"),"Download Summary Table",   class = "btn-sm btn-outline-secondary w-100")
      ),
      conditionalPanel(
        condition = paste0("input['", ns("active_tab"), "'] === 'Risk Rankings'"),
        downloadButton(ns("dl_bar"),      "Download Bar Chart", class = "btn-sm btn-outline-secondary w-100 mb-1"),
        downloadButton(ns("dl_risk_tbl"), "Download Risk Table",class = "btn-sm btn-outline-secondary w-100")
      )
    ),
    
    # ── Main Panel ──
    div(
      class = "p-3",
      navset_card_tab(
        id = ns("active_tab"),
        
        # ── Tab 1: Swimlane Timeline ──────────────────────
        nav_panel(
          "Swimlane Timeline",
          card_body(
            conditionalPanel(
              condition = paste0("input['", ns("tab1_view"), "'] === 'Action Heatmap'"),
              plot_desc(
                "Action Heatmap — Each row is an agent; each column is a communication round.
                 Tile colour shows what the agent did that round (see legend). Background shading
                 marks the three periods: blue = Pre-Embargo, yellow = Post-Embargo, red = Crisis Day.
                 A thick red border means the agent declared MONITORING but actually used a posting
                 channel (divergence). The red vertical line marks when the embargo was declared.
                 Click any tile to read the agent's messages for that round.",
                accent = "#1565C0"
              ),
              plotOutput(ns("swimlane_plot"),
                         height = "500px",
                         click  = ns("tile_click")),
              conditionalPanel(
                condition = paste0("input['", ns("show_prebreach"), "']"),
                hr(),
                plot_desc(
                  "Pre-breach Zoom — Focuses on the five rounds immediately before Crisis Day.
                   Use this to spot early warning signs: agents switching to side-huddle or
                   personal channels before the main breach occurred.",
                  accent = "#B71C1C"
                ),
                plotOutput(ns("prebreach_plot"), height = "320px")
              )
            ),
            conditionalPanel(
              condition = paste0("input['", ns("tab1_view"), "'] === 'Divergence Trend'"),
              plot_desc(
                "Divergence Trend — Counts how many agents diverged (declared MONITORING but posted)
                 in each round, coloured by period. A flat line near zero is normal; a rising trend
                 signals escalating non-compliance. Use this to judge whether the breach was sudden
                 or built up gradually.",
                accent = "#E65100"
              ),
              plotOutput(ns("diverge_trend"), height = "400px")
            ),
            conditionalPanel(
              condition = paste0("input['", ns("tab1_view"), "'] === 'Divergence Table'"),
              plot_desc(
                "Divergence Table — Lists every round where an agent formally declared MONITORING
                 but used a posting channel. Includes the period, actual channels used, and message
                 count. Note: declared actions are only recorded by The Judge on Crisis Day, so
                 earlier rounds will not appear here.",
                accent = "#B71C1C"
              ),
              tags$style(HTML("
                .diverge-table thead tr th {
                  background-color: #B71C1C !important;
                  color: white; font-size: 12px;
                }
                .diverge-table tbody tr:hover { background-color: #fdecea !important; }
              ")),
              DTOutput(ns("diverge_table"))
            )
          )
        ),
        
        # ── Tab 2: Behaviour Scores ───────────────────────
        nav_panel(
          "Behaviour Scores",
          card_body(
            conditionalPanel(
              condition = paste0("input['", ns("tab2_view"), "'] === 'Severity Heatmap'"),
              plot_desc(
                "Violation Severity Heatmap — Same grid layout as the Swimlane Timeline, but tile
                 colour now shows how serious the violation was: green = consistent behaviour,
                 yellow = Low (side huddle, weight 1), orange = Medium (personal post, weight 2),
                 red = High (anonymous post, weight 3). Split into three panels by period so you
                 can directly compare Pre-Embargo, Post-Embargo, and Crisis Day side by side.",
                accent = "#388E3C"
              ),
              plotOutput(ns("score_heatmap"), height = "500px")
            ),
            conditionalPanel(
              condition = paste0("input['", ns("tab2_view"), "'] === 'Internal State Heatmap'"),
              plot_desc(
                "Internal State Heatmap — Shows whether agents were deliberating (blue, forward
                 planning) or rationalising (red, post-hoc justification) each round, also split
                 by period. Rationalising on the same round as a high-severity violation is a
                 strong indicator that the agent knew the action was questionable. Purple means
                 both states were active.",
                accent = "#6A1B9A"
              ),
              plotOutput(ns("internal_heatmap"), height = "500px")
            ),
            conditionalPanel(
              condition = paste0("input['", ns("tab2_view"), "'] === 'Summary Table'"),
              plot_desc(
                "Period Summary Table — Per-agent, per-period counts of each severity level
                 (Consistent / Low / Medium / High). Each agent has up to three rows — one for
                 each period — so you can directly compare whether violations escalated after
                 the embargo. Sort by High (3) descending to rank the most serious offenders.",
                accent = "#388E3C"
              ),
              tags$style(HTML("
                .score-summary-table thead tr th {
                  background-color: #388E3C !important;
                  color: white; font-size: 12px;
                }
                .score-summary-table tbody tr:hover { background-color: #f1f8e9 !important; }
              ")),
              DTOutput(ns("score_summary_table"))
            )
          )
        ),
        
        # ── Tab 3: Risk Rankings ──────────────────────────
        nav_panel(
          "Risk Rankings",
          card_body(
            conditionalPanel(
              condition = paste0("input['", ns("tab3_view"), "'] === 'Period Bar Chart'"),
              plot_desc(
                "Period Bar Chart — Shows Consistent (green) vs Diverged (red) rounds per agent,
                 split into three panels by period. An agent with only green bars Pre-Embargo but
                 red on Crisis Day changed behaviour specifically on breach day. An agent with red
                 bars across all periods had pre-existing non-compliance.",
                accent = "#1565C0"
              ),
              plotOutput(ns("period_bar"), height = "500px")
            ),
            conditionalPanel(
              condition = paste0("input['", ns("tab3_view"), "'] === 'Risk Table'"),
              plot_desc(
                "Risk Rankings Table — One row per agent, ranked by overall Risk Score.
                 Pre-Embargo, Post-Embargo, and Crisis scores are shown separately so you can
                 see when risk was highest. Compliance Violation shows the worst channel type
                 observed. Click any row to open a full agent profile card.",
                accent = "#1565C0"
              ),
              tags$style(HTML("
                .risk-table thead tr th {
                  background-color: #1565C0 !important;
                  color: white; font-size: 12px;
                }
                .risk-table tbody tr:hover { background-color: #e3f2fd !important; }
              ")),
              DTOutput(ns("risk_table"))
            )
          )
        )
      )
    )
  )
}


# ════════════════════════════════════════════════════════════
# SERVER
# ════════════════════════════════════════════════════════════
swimlaneServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # ── Filtered data ─────────────────────────────────────
    filtered_data <- reactive({
      data <- action_final_tbl
      
      if (!is.null(input$agents) && !"All Agents" %in% input$agents)
        data <- data %>% filter(agent_label %in% input$agents)
      
      if (!is.null(input$period) && input$period != "All Periods")
        data <- data %>% filter(period == input$period)
      
      if (!is.null(input$actions) && !"All Actions" %in% input$actions)
        data <- data %>% filter(as.character(display_action) %in% input$actions)
      
      data %>% mutate(round_label = droplevels(round_label))
    })
    
    # ── Swimlane heatmap ──────────────────────────────────
    output$swimlane_plot <- renderPlot({
      req(nrow(filtered_data()) > 0)
      build_heatmap(filtered_data(), highlight_anomalies = input$highlight_anomalies)
    }, height = 500)
    
    # ── Clickable tile → message drawer ──────────────────
    observeEvent(input$tile_click, {
      click <- input$tile_click
      req(click)
      
      data         <- filtered_data()
      agent_levels <- sort(unique(data$agent_label))
      round_levels <- levels(data$round_label)
      
      agent_idx <- round(click$y)
      round_idx <- round(click$x)
      
      req(agent_idx >= 1, agent_idx <= length(agent_levels),
          round_idx >= 1, round_idx <= length(round_levels))
      
      clicked_agent <- agent_levels[agent_idx]
      clicked_round <- round_levels[round_idx]
      
      msgs <- messages_tbl %>%
        filter(
          coalesce(agent_label, agent_id) == clicked_agent,
          format(round_hour, "%b %d %H:%M") == clicked_round
        )
      
      showModal(modalDialog(
        title     = paste0(clicked_agent, "  —  Round: ", clicked_round),
        size      = "l",
        easyClose = TRUE,
        div(
          style = "max-height: 480px; overflow-y: auto;",
          if (nrow(msgs) == 0) {
            tags$p("No messages found for this agent/round.", style = "color: grey;")
          } else {
            tagList(lapply(seq_len(nrow(msgs)), function(i) {
              m <- msgs[i, ]
              div(
                style = paste0("border: 1px solid #e0e0e0; border-radius: 6px;",
                               "padding: 10px; margin-bottom: 10px;"),
                tags$p(
                  tags$strong("Channel: "), m$channel, "  |  ",
                  tags$strong("Type: "), m$message_type
                ),
                if (!is.na(m$content) && m$content != "")
                  tags$p(tags$strong("Content:"), tags$br(), m$content),
                if (!is.na(m$deliberating) && m$deliberating != "")
                  tags$p(tags$strong("Deliberating:"), tags$br(),
                         tags$em(m$deliberating),
                         style = "color: #1565C0; background: #E3F2FD; padding: 6px; border-radius: 4px;"),
                if (!is.na(m$reacting) && m$reacting != "")
                  tags$p(tags$strong("Reacting:"), tags$br(),
                         tags$em(m$reacting),
                         style = "color: #4a4a4a; background: #F5F5F5; padding: 6px; border-radius: 4px;"),
                if (!is.na(m$rationalizing) && m$rationalizing != "")
                  tags$p(tags$strong("Rationalizing:"), tags$br(),
                         tags$em(m$rationalizing),
                         style = "color: #B71C1C; background: #FFEBEE; padding: 6px; border-radius: 4px;")
              )
            }))
          }
        ),
        footer = modalButton("Close")
      ))
    })
    
    # ── Divergence trend line ─────────────────────────────
    output$diverge_trend <- renderPlot({
      req(nrow(filtered_data()) > 0)
      build_divergence_trend(filtered_data())
    }, height = 400)
    
    # ── Pre-breach zoom ───────────────────────────────────
    output$prebreach_plot <- renderPlot({
      build_heatmap(prebreach_tbl, highlight_anomalies = TRUE)
    }, height = 320)
    
    # ── Behaviour score heatmap (faceted) ─────────────────
    output$score_heatmap <- renderPlot({
      req(nrow(filtered_data()) > 0)
      build_score_heatmap(filtered_data(), severity = input$severity)
    }, height = 450)
    
    # ── Internal state heatmap ────────────────────────────
    output$internal_heatmap <- renderPlot({
      req(nrow(filtered_data()) > 0)
      build_internal_heatmap(filtered_data())
    }, height = 420)
    
    # ── Behaviour scores summary table (with period) ──────
    output$score_summary_table <- renderDT({
      score_summary <- score_tbl %>%
        filter(
          agent_label %in% unique(filtered_data()$agent_label),
          round_label %in% levels(filtered_data()$round_label)
        ) %>%
        {
          d <- .
          if (!is.null(input$severity) && !"All Severities" %in% input$severity)
            d <- d %>% filter(as.character(score_label) %in% input$severity)
          d
        } %>%
        mutate(period = factor(period,
                               levels = c("Pre-Embargo", "Post-Embargo", "Crisis Day"))) %>%
        group_by(agent_label, period) %>%
        summarise(
          `Consistent (0)` = sum(violation_score == 0),
          `Low (1)`        = sum(violation_score == 1),
          `Medium (2)`     = sum(violation_score == 2),
          `High (3)`       = sum(violation_score == 3),
          `Total Rounds`   = n(),
          .groups = "drop"
        ) %>%
        arrange(agent_label, period) %>%
        rename(Agent = agent_label, Period = period)
      
      datatable(
        score_summary,
        class    = "score-summary-table",
        rownames = FALSE,
        options  = list(
          pageLength = 20,
          dom        = "tip",
          columnDefs = list(
            list(className = "dt-center", targets = c(2, 3, 4, 5, 6))
          )
        )
      ) %>%
        formatStyle(columns = names(score_summary), fontSize = "12px") %>%
        formatStyle("Period",
                    backgroundColor = styleEqual(
                      c("Pre-Embargo", "Post-Embargo", "Crisis Day"),
                      c("#E3F2FD",     "#FFF8E1",      "#FFEBEE")
                    )
        ) %>%
        formatStyle("High (3)",
                    backgroundColor = styleInterval(c(0, 1), c("#F1F8E9", "#FFEBEE", "#FFCDD2"))) %>%
        formatStyle("Medium (2)",
                    backgroundColor = styleInterval(c(0, 1), c("#F1F8E9", "#FFF8E1", "#FFE0B2"))) %>%
        formatStyle("Low (1)",
                    backgroundColor = styleInterval(c(0, 1), c("#F1F8E9", "#FFFDE7", "#FFF176")))
    })
    
    # ── Period stacked bar ────────────────────────────────
    output$period_bar <- renderPlot({
      req(nrow(filtered_data()) > 0)
      build_period_bar(filtered_data())
    }, height = 500)
    
    # ── Risk Rankings table ───────────────────────────────
    output$risk_table <- renderDT({
      rt <- risk_tbl
      if (!is.null(input$agents) && !"All Agents" %in% input$agents)
        rt <- rt %>% filter(Agent %in% input$agents)
      if (!is.null(input$violation) && !"All" %in% input$violation)
        rt <- rt %>% filter(`Compliance Violation` %in% input$violation)
      
      violation_colours <- c(
        "High" = "#FFCDD2", "Medium" = "#FFE0B2",
        "Low"  = "#FFF9C4", "None"   = "#F1F8E9"
      )
      
      datatable(
        rt, class = "risk-table", rownames = FALSE,
        options = list(
          pageLength = 20, dom = "tip",
          order = list(list(6, "desc")),
          columnDefs = list(
            list(className = "dt-center", targets = c(1, 2, 3, 4, 5, 6, 7))
          )
        )
      ) %>%
        formatStyle(columns = names(rt), fontSize = "12px") %>%
        formatStyle("Compliance Violation",
                    backgroundColor = styleEqual(names(violation_colours),
                                                 unname(violation_colours))) %>%
        formatStyle("Pre-Embargo Score",
                    background = styleColorBar(c(0, max(rt$`Pre-Embargo Score`, 1)), "#90CAF9"),
                    backgroundSize = "100% 80%", backgroundRepeat = "no-repeat",
                    backgroundPosition = "center") %>%
        formatStyle("Post-Embargo Score",
                    background = styleColorBar(c(0, max(rt$`Post-Embargo Score`, 1)), "#FFCC80"),
                    backgroundSize = "100% 80%", backgroundRepeat = "no-repeat",
                    backgroundPosition = "center") %>%
        formatStyle("Crisis Score",
                    background = styleColorBar(c(0, max(rt$`Crisis Score`, 1)), "#EF9A9A"),
                    backgroundSize = "100% 80%", backgroundRepeat = "no-repeat",
                    backgroundPosition = "center") %>%
        formatStyle("Risk Score",
                    background = styleColorBar(rt$`Risk Score`, "#1565C0"),
                    backgroundSize = "100% 80%", backgroundRepeat = "no-repeat",
                    backgroundPosition = "center") %>%
        formatStyle("Consistency Score",
                    background = styleColorBar(c(0, 1), "#43A047"),
                    backgroundSize = "100% 80%", backgroundRepeat = "no-repeat",
                    backgroundPosition = "center")
    })
    
    # ── Agent profile modal (row click in risk table) ─────
    observeEvent(input$risk_table_rows_selected, {
      sel <- input$risk_table_rows_selected
      req(length(sel) > 0)
      
      rt <- risk_tbl
      if (!is.null(input$agents) && !"All Agents" %in% input$agents)
        rt <- rt %>% filter(Agent %in% input$agents)
      
      agent_name <- rt$Agent[sel]
      agent_data <- action_final_tbl %>% filter(agent_label == agent_name)
      agent_msgs <- messages_tbl %>%
        filter(coalesce(agent_label, agent_id) == agent_name)
      
      channel_breakdown <- agent_msgs %>%
        group_by(channel) %>% summarise(n = n(), .groups = "drop") %>%
        arrange(desc(n))
      
      showModal(modalDialog(
        title     = paste0("Agent Profile — ", agent_name),
        size      = "l",
        easyClose = TRUE,
        fluidRow(
          column(6,
                 tags$h6("Overall Stats"),
                 tags$p(tags$strong("Total rounds active: "), nrow(agent_data)),
                 tags$p(tags$strong("Mismatch count: "),
                        rt$`Mismatch Count`[sel]),
                 tags$p(tags$strong("Risk score: "), rt$`Risk Score`[sel]),
                 tags$p(tags$strong("Compliance violation: "),
                        rt$`Compliance Violation`[sel]),
                 tags$p(tags$strong("Consistency score: "),
                        rt$`Consistency Score`[sel])
          ),
          column(6,
                 tags$h6("Period Risk Scores"),
                 tags$p(tags$strong("Pre-Embargo: "),  rt$`Pre-Embargo Score`[sel]),
                 tags$p(tags$strong("Post-Embargo: "), rt$`Post-Embargo Score`[sel]),
                 tags$p(tags$strong("Crisis Day: "),   rt$`Crisis Score`[sel]),
                 tags$h6("Channel Breakdown"),
                 renderTable(channel_breakdown, striped = TRUE, bordered = TRUE)
          )
        ),
        footer = modalButton("Close")
      ))
    })
    
    # ── Divergence table ──────────────────────────────────
    output$diverge_table <- renderDT({
      div_data <- filtered_data() %>%
        filter(divergence == "DIVERGED") %>%
        mutate(
          Date              = format(round_hour, "%d %b %H:%M"),
          Agent             = agent_label,
          Period            = period,
          Declared          = coalesce(declared_type, "—"),
          `Actual channels` = map_chr(channels, ~paste(sort(.x), collapse = ", ")),
          `Messages sent`   = n_messages
        ) %>%
        select(Date, Agent, Period, Declared, `Actual channels`, `Messages sent`)
      
      datatable(
        div_data, class = "diverge-table", rownames = FALSE,
        options = list(
          pageLength = 15, dom = "tip",
          columnDefs = list(
            list(width = "10%", targets = 0),
            list(width = "10%", targets = 1),
            list(width = "12%", targets = 2),
            list(width = "13%", targets = 3),
            list(width = "44%", targets = 4),
            list(width = "8%",  className = "dt-center", targets = 5)
          )
        )
      ) %>%
        formatStyle(columns = names(div_data), fontSize = "11px") %>%
        formatStyle("Period",
                    backgroundColor = styleEqual(
                      c("Pre-Embargo", "Post-Embargo", "Crisis Day"),
                      c("#E3F2FD",     "#FFF8E1",      "#FFEBEE")
                    )
        )
    })
    
    # ── Download handlers ─────────────────────────────────
    output$dl_swimlane <- downloadHandler(
      filename = function() paste0("swimlane_heatmap_", Sys.Date(), ".png"),
      content  = function(file) {
        p <- build_heatmap(filtered_data(),
                           highlight_anomalies = input$highlight_anomalies)
        ggsave(file, plot = p, width = 14, height = 6, dpi = 150)
      }
    )
    output$dl_trend <- downloadHandler(
      filename = function() paste0("divergence_trend_", Sys.Date(), ".png"),
      content  = function(file) {
        p <- build_divergence_trend(filtered_data())
        ggsave(file, plot = p, width = 12, height = 4, dpi = 150)
      }
    )
    output$dl_diverge <- downloadHandler(
      filename = function() paste0("divergence_table_", Sys.Date(), ".csv"),
      content  = function(file) {
        div_data <- filtered_data() %>%
          filter(divergence == "DIVERGED") %>%
          mutate(
            Date   = format(round_hour, "%d %b %H:%M"),
            Agent  = agent_label, Period = period,
            `Actual channels` = map_chr(channels, ~paste(sort(.x), collapse = ", "))
          ) %>%
          select(Date, Agent, Period, `Actual channels`, n_messages)
        write.csv(div_data, file, row.names = FALSE)
      }
    )
    output$dl_score <- downloadHandler(
      filename = function() paste0("score_heatmap_", Sys.Date(), ".png"),
      content  = function(file) {
        p <- build_score_heatmap(filtered_data(), severity = input$severity)
        ggsave(file, plot = p, width = 14, height = 5, dpi = 150)
      }
    )
    output$dl_internal <- downloadHandler(
      filename = function() paste0("internal_state_heatmap_", Sys.Date(), ".png"),
      content  = function(file) {
        p <- build_internal_heatmap(filtered_data())
        ggsave(file, plot = p, width = 14, height = 5, dpi = 150)
      }
    )
    output$dl_score_tbl <- downloadHandler(
      filename = function() paste0("score_summary_", Sys.Date(), ".csv"),
      content  = function(file) {
        score_summary <- score_tbl %>%
          filter(agent_label %in% unique(filtered_data()$agent_label),
                 round_label %in% levels(filtered_data()$round_label)) %>%
          group_by(agent_label, period) %>%
          summarise(
            Consistent = sum(violation_score == 0),
            Low        = sum(violation_score == 1),
            Medium     = sum(violation_score == 2),
            High       = sum(violation_score == 3),
            .groups    = "drop"
          )
        write.csv(score_summary, file, row.names = FALSE)
      }
    )
    output$dl_bar <- downloadHandler(
      filename = function() paste0("period_bar_", Sys.Date(), ".png"),
      content  = function(file) {
        p <- build_period_bar(filtered_data())
        ggsave(file, plot = p, width = 12, height = 5, dpi = 150)
      }
    )
    output$dl_risk_tbl <- downloadHandler(
      filename = function() paste0("risk_rankings_", Sys.Date(), ".csv"),
      content  = function(file) write.csv(risk_tbl, file, row.names = FALSE)
    )
    
  })
}