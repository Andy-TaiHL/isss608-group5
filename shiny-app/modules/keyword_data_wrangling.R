# ============================================================
# keyword_data_wrangling.R
# Shared data wrangling for all keyword visualisation modules.
# Run standalone to verify data loads correctly:
#   source("modules/keyword_data_wrangling.R")
#   glimpse(all_messages); glimpse(risk_tbl)
# ============================================================

library(tidyverse)
library(tidytext)
library(jsonlite)
library(lubridate)

`%||%` <- function(x, y) if (is.null(x)) y else x

# ── KEY DATES ──────────────────────────────────────────────────────────────────
START_DATE    <- ymd_hms("2046-05-17T09:00:00")  # simulation start
EMBARGO_DATE  <- ymd_hms("2046-05-23T09:00:00")  # embargo begins
LEAK_DATE     <- ymd_hms("2046-05-29T09:00:00")  # @Elena faux pas / first leak
END_DATE      <- ymd_hms("2046-06-05T18:00:00")  # crisis day end

# ── PERIOD ASSIGNMENT ──────────────────────────────────────────────────────────
# Pre-Embargo  : [May 17 09:00,  May 23 09:00)
# Embargo→Leak : [May 23 09:00,  May 29 09:00]
# Post-Leak    : (May 29 09:00,  Jun 05 18:00]
assign_period <- function(ts) {
  case_when(
    ts <  EMBARGO_DATE               ~ "Pre-Embargo",
    ts <= LEAK_DATE                  ~ "Embargo to Leak",
    ts <= END_DATE                   ~ "Post-Leak",
    TRUE                             ~ NA_character_
  )
}

PERIOD_LEVELS <- c("Pre-Embargo", "Embargo to Leak", "Post-Leak")

# ── STOP WORDS ─────────────────────────────────────────────────────────────────
EXTRA_STOPS <- c(
  stop_words$word,
  "the","and","for","this","that","with","are","have","will","from","been",
  "they","their","what","need","can","all","not","but","its","our","any",
  "has","was","re","s","t","it","is","in","on","at","to","of","a","i","be",
  "do","so","if","no","up","or","an","as","by","we","he","she","him","her",
  "us","my","me","would","should","could","make","know","going","right",
  "team","want","now","good","take","think","time","use","see","just","also",
  "one","get","let","out","more","about","very","here","there","when","then",
  "them","these","those","into","even","both","back","well","still","said",
  "says","platform_trust","social_manager","legal_agent","pr_agent",
  "pr_intern","judge_agent","intern_agent","agent","agents","message",
  "messages","post","posts","today","tenantthread","flex","posted"
)

DEFAULT_SEEDS <- "risk, embargo, leak, merger, legal, compliance, anonymous, monitor, breach, exposure"

# ── LOAD JSON ──────────────────────────────────────────────────────────────────
# Determine data path whether run standalone or from app.R
DATA_PATH <- if (file.exists("data/MC1_final_00.json")) {
  "data/MC1_final_00.json"
} else if (file.exists("../data/MC1_final_00.json")) {
  "../data/MC1_final_00.json"
} else {
  stop("Cannot find MC1_final_00.json. Place it in the data/ folder.")
}

raw <- fromJSON(DATA_PATH, simplifyVector = FALSE)

# ── ALL MESSAGES ───────────────────────────────────────────────────────────────
# One row per communication message, with internal state fields unpacked
all_messages <- map_dfr(raw$rounds, function(r) {
  rh <- ymd_hms(r$hour)
  map_dfr(r$communications, function(m) {
    tibble(
      round_hour    = rh,
      timestamp     = ymd_hms(m$timestamp %||% r$hour),
      agent_label   = m$agent_label   %||% NA_character_,
      agent_role    = m$agent_role    %||% NA_character_,
      channel       = m$channel       %||% NA_character_,
      content       = m$content       %||% NA_character_,
      deliberating  = m$internal_state$deliberating  %||% NA_character_,
      rationalizing = m$internal_state$rationalizing %||% NA_character_,
      reacting      = m$internal_state$reacting      %||% NA_character_
    )
  })
}) %>%
  mutate(Period = assign_period(timestamp)) %>%
  filter(!is.na(Period))  # drop anything outside simulation window

# ── RISK TABLE ─────────────────────────────────────────────────────────────────
# Participant-level data: declared_action + sentiment_at_turn per agent per round
# NOTE: sentiment_at_turn is only populated from Jun 5 onwards in this dataset.
# For earlier periods, risk is DERIVED (see keyword_risk_timeline.R).
risk_tbl <- map_dfr(raw$rounds, function(r) {
  rh <- ymd_hms(r$hour)
  map_dfr(r$participants, function(p) {
    meta    <- p$agent_round_metadata %||% list()
    da_raw  <- p$declared_action %||% NA_character_
    act_type <- if (!is.na(da_raw))
                  str_extract(da_raw, "^[^:]+") %>% str_trim()
                else NA_character_
    act_text <- if (!is.na(da_raw) && str_detect(da_raw, ":"))
                  str_trim(str_remove(da_raw, "^[^:]+:\\s*"))
                else da_raw
    tibble(
      round_hour   = rh,
      agent_label  = p$agent_label %||% NA_character_,
      agent_role   = p$agent_role  %||% NA_character_,
      risk_level   = meta$sentiment_at_turn %||% NA_character_,
      action_type  = act_type,
      action_text  = act_text
    )
  })
}) %>%
  mutate(
    Period = assign_period(round_hour),
    risk_score = case_when(
      risk_level == "CRITICAL" ~ 4,
      risk_level == "HIGH"     ~ 3,
      risk_level == "MEDIUM"   ~ 2,
      risk_level == "LOW"      ~ 1,
      TRUE                     ~ NA_real_
    )
  ) %>%
  filter(!is.na(Period))

# ── FILTER HELPER ──────────────────────────────────────────────────────────────
# Returns filtered all_messages for given periods / channels / internal states.
# Used by all three chart modules.
filter_msgs <- function(periods,
                        channels   = c("comms_huddle","private","public_post"),
                        int_states = character(0)) {
  ch_map <- list(
    comms_huddle = "comms_huddle",
    private      = c("one_on_one_chat", "side_huddle"),
    public_post  = c("official_post", "anonymous_post", "personal_post")
  )
  keep_ch <- unlist(ch_map[channels])

  base <- all_messages %>%
    filter(Period %in% periods,
           channel %in% keep_ch,
           !is.na(content), content != "")

  int_rows <- map_dfr(int_states, function(s) {
    all_messages %>%
      filter(Period %in% periods) %>%
      mutate(content = .data[[s]]) %>%
      filter(!is.na(content), content != "")
  })

  bind_rows(base, int_rows)
}

# ── STANDALONE TEST ────────────────────────────────────────────────────────────
if (!exists("KEYWORD_WRANGLING_SOURCED")) {
  KEYWORD_WRANGLING_SOURCED <- TRUE
  message("keyword_data_wrangling.R loaded successfully.")
  message(sprintf("  all_messages: %d rows across %d periods",
                  nrow(all_messages),
                  n_distinct(all_messages$Period)))
  message(sprintf("  risk_tbl    : %d rows, %d with formal risk scores",
                  nrow(risk_tbl),
                  sum(!is.na(risk_tbl$risk_score))))
  message("  Periods in all_messages:")
  all_messages %>% count(Period) %>% print()
}
