# ============================================================
# keyword_risk_timeline.R  — v3
# Three-layer convergence chart
# ── Layer 1: PT-Agent X/10 scores (extracted from deliberating)
#             + Jun 5 formal sentiment_at_turn as SEPARATE markers
# ── Layer 2: Warning signals (diamonds, coloured by agent)
# ── Layer 3: Off-script posts (triangles, coloured by agent)
# ============================================================

library(ggrepel)
library(text2vec)

if (!exists("KEYWORD_WRANGLING_SOURCED")) {
  source(file.path(dirname(sys.frame(1)$ofile %||% "."),
                   "keyword_data_wrangling.R"))
}

# ── AGENT COLOUR PALETTE ───────────────────────────────────────────────────────
AGENT_COLS <- c(
  "Legal-Agent"          = "#3266ad",
  "Platform-Trust-Agent" = "#1D9E75",
  "Social-Manager-Agent" = "#D85A30",
  "PR-Agent"             = "#B07EB8",
  "PR-Intern-Agent"      = "#BA7517",
  "Intern-Agent"         = "#888780",
  "Judge-Agent"          = "#c0392b"
)

# ── RISK COLOUR HELPERS ────────────────────────────────────────────────────────
risk_colour <- function(score) {
  case_when(
    score >= 8 ~ "#7f1d1d",
    score >= 6 ~ "#ef4444",
    score >= 3 ~ "#f59e0b",
    TRUE       ~ "#16a34a"
  )
}
risk_fill <- function(score) {
  case_when(
    score >= 8 ~ "#fee2e2",
    score >= 6 ~ "#fecaca",
    score >= 3 ~ "#fef3c7",
    TRUE       ~ "#dcfce7"
  )
}
risk_label <- function(score) {
  case_when(
    score >= 8 ~ "CRITICAL",
    score >= 6 ~ "HIGH",
    score >= 3 ~ "MODERATE",
    TRUE       ~ "LOW"
  )
}

RISK_SCORE_FILLS <- list(
  CRITICAL = "#fee2e2", HIGH = "#fecaca",
  MODERATE = "#fef3c7", LOW  = "#dcfce7", NONE = "#ffffff"
)

# ── EMPTY PLOT ─────────────────────────────────────────────────────────────────
tl_empty_plot <- function(msg) {
  ggplot() +
    annotate("text", x=0.5, y=0.5, label=msg, size=3.8,
             colour="#64748b", hjust=0.5, vjust=0.5, lineheight=1.5) +
    theme_void() +
    theme(plot.background=element_rect(fill="#f8fafc",
                                       colour="#e2e8f0", linewidth=0.5))
}

# ── LAYER 1a: PT-AGENT EXTRACTED X/10 SCORES ──────────────────────────────────
# Source: deliberating text, rounds 7,10,11 only
build_pt_extracted <- function(periods) {
  result <- map_dfr(raw$rounds, function(r) {
    rh <- ymd_hms(r$hour)
    if (!assign_period(rh) %in% periods) return(NULL)
    map_dfr(r$communications, function(m) {
      if ((m$agent_role %||% "") != "platform_trust") return(NULL)
      ist    <- m$internal_state %||% list()
      delib  <- ist$deliberating %||% ""
      scores <- str_extract_all(
        delib, "\\d+\\.?\\d*(?=\\s*/\\s*10)")[[1]]
      if (length(scores) == 0) return(NULL)
      tibble(
        timestamp  = ymd_hms(m$timestamp %||% r$hour),
        round_hour = rh,
        agent      = m$agent_label %||% NA_character_,
        risk_score = as.numeric(scores[1]),
        source     = "extracted",
        # short excerpt: two lines of ~40 chars each
        excerpt_short = {
          txt   <- str_remove(delib, "^Risk score[:\\s]+[\\d\\.]+/10[\\s\\.\\-—]*")
          txt   <- str_squish(txt)
          words <- str_split(txt, " ")[[1]]
          line1 <- ""; line2 <- ""; on2 <- FALSE
          for (w in words) {
            if (!on2) {
              cand <- if (nchar(line1)==0) w else paste(line1, w)
              if (nchar(cand) > 38) { on2 <- TRUE; line2 <- w }
              else line1 <- cand
            } else {
              cand <- if (nchar(line2)==0) w else paste(line2, w)
              if (nchar(cand) > 38) break
              line2 <- cand
            }
          }
          if (nchar(line2) > 0)
            paste0('"', line1, "\n", line2, '..."')
          else
            paste0('"', line1, '..."')
        },
        excerpt_full = str_trunc(delib, 300)
      )
    })
  })
  if (nrow(result) == 0) return(NULL)
  result %>%
    arrange(timestamp) %>%
    mutate(
      risk_band  = risk_label(risk_score),
      point_col  = risk_colour(risk_score),
      point_fill = risk_fill(risk_score),
      Period     = assign_period(timestamp)
    )
}

# ── LAYER 1b: JUN 5 FORMAL SENTIMENT_AT_TURN ──────────────────────────────────
# Shown as SEPARATE tick markers, NOT connected to the extracted line
build_pt_formal <- function(periods) {
  result <- risk_tbl %>%
    filter(Period %in% periods,
           !is.na(risk_level),
           agent_role == "platform_trust") %>%
    mutate(
      risk_score = case_when(
        risk_level == "CRITICAL" ~ 8,
        risk_level == "HIGH"     ~ 6,
        risk_level == "MEDIUM"   ~ 4,
        risk_level == "LOW"      ~ 2
      ),
      timestamp     = round_hour,
      agent         = agent_label,
      source        = "formal",
      excerpt_short = paste0("formal: ", risk_level),
      excerpt_full  = coalesce(action_text, "")
    ) %>%
    select(timestamp, round_hour, agent, risk_score, source,
           excerpt_short, excerpt_full, Period)

  if (nrow(result) == 0) return(NULL)
  result %>%
    mutate(
      risk_band  = risk_label(risk_score),
      point_col  = risk_colour(risk_score),
      point_fill = risk_fill(risk_score)
    )
}

# ── LAYER 2: WARNING SIGNALS ───────────────────────────────────────────────────
build_warnings <- function(periods, seeds, mode, threshold) {
  msgs <- all_messages %>%
    filter(Period %in% periods,
           !is.na(deliberating), deliberating != "")
  if (nrow(msgs) == 0) return(NULL)

  if (mode == "cosine" && length(seeds) > 0) {
    corpus <- msgs %>%
      mutate(doc_id  = as.character(row_number()),
             content = str_to_lower(deliberating) %>%
               str_replace_all("[^a-z\\s]", " ") %>% str_squish()) %>%
      filter(content != "") %>%
      select(doc_id, content) %>% deframe()
    corpus <- as.character(corpus)
    corpus <- corpus[nchar(corpus) > 0]
    names(corpus) <- seq_along(corpus)

    expanded_vocab <- tryCatch({
      it    <- itoken(corpus, tokenizer=word_tokenizer, progressbar=FALSE)
      vocab <- create_vocabulary(it) %>%
        prune_vocabulary(term_count_min=2, doc_count_min=1) %>%
        filter(!term %in% EXTRA_STOPS, nchar(term) > 2)
      if (nrow(vocab) == 0) stop("empty")
      vec       <- vocab_vectorizer(vocab)
      dtm       <- create_dtm(it, vec)
      tfidf     <- TfIdf$new()
      dtm_tfidf <- fit_transform(dtm, tfidf)
      tdm       <- Matrix::t(dtm_tfidf)
      valid     <- seeds[seeds %in% rownames(tdm)]
      if (length(valid) == 0) stop("no seeds")
      map_dfr(valid, function(seed) {
        sv  <- tdm[seed, , drop=FALSE]
        sim <- sim2(tdm, sv, method="cosine", norm="l2")
        tibble(word=rownames(sim), sim=as.numeric(sim)) %>%
          filter(word != seed, !word %in% EXTRA_STOPS, sim > threshold) %>%
          slice_max(sim, n=20)
      }) %>% pull(word) %>% unique() %>% c(seeds)
    }, error = function(e) seeds)
    warn_pattern <- paste0("\\b(",
      paste(expanded_vocab, collapse="|"), ")\\b")
  } else {
    base_kw <- c("concern","worry","risk","warning","compounding",
                 "hard to manage","narrative","one bad","exposure",
                 "liability","breach","leak","embargo")
    warn_pattern <- paste0("\\b(",
      paste(unique(c(base_kw, seeds)), collapse="|"), ")\\b")
  }

  result <- msgs %>%
    filter(str_detect(str_to_lower(deliberating), warn_pattern)) %>%
    mutate(
      matched_word = str_extract(str_to_lower(deliberating), warn_pattern),
      n_hits       = str_count(str_to_lower(deliberating), warn_pattern),
      excerpt_full = str_trunc(deliberating, 300),
      layer        = "Warning Signal",
      Period       = assign_period(timestamp)
    ) %>%
    group_by(round_hour, agent=agent_label) %>%
    slice_max(n_hits, n=1, with_ties=FALSE) %>%
    ungroup() %>%
    select(timestamp, round_hour, agent, matched_word,
           excerpt_full, layer, Period)

  if (nrow(result) == 0) return(NULL)
  result
}

# ── LAYER 3: OFF-SCRIPT POSTS ──────────────────────────────────────────────────
build_offscript <- function(periods) {
  result <- all_messages %>%
    filter(Period %in% periods,
           channel %in% c("personal_post", "anonymous_post"),
           !is.na(content), content != "") %>%
    mutate(layer        = "Off-script Post",
           excerpt_full = str_trunc(content, 300),
           Period       = assign_period(timestamp)) %>%
    group_by(round_hour, agent=agent_label) %>%
    slice(1) %>% ungroup() %>%
    select(timestamp, round_hour, agent, channel,
           excerpt_full, layer, Period)

  if (nrow(result) == 0) return(NULL)
  result
}

# ── TABLE DATA (exported) ──────────────────────────────────────────────────────
build_timeline_table <- function(periods, seeds, mode, threshold) {
  pt_ex  <- build_pt_extracted(periods)
  pt_fm  <- build_pt_formal(periods)
  warn   <- build_warnings(periods, seeds, mode, threshold)
  off    <- build_offscript(periods)

  bind_rows(
    if (!is.null(pt_ex))
      pt_ex %>% mutate(
        Layer     = "PT Risk Score",
        Signal    = paste0(risk_score, "/10 — ", risk_band),
        Source    = "extracted (deliberating)",
        risk_band = risk_band
      ) %>%
      select(Date=timestamp, Period, Layer, Agent=agent,
             Signal, Source, Excerpt=excerpt_full, risk_band),

    if (!is.null(pt_fm))
      pt_fm %>% mutate(
        Layer     = "PT Risk Score",
        Signal    = paste0(risk_score, "/10 — ", risk_band),
        Source    = "formal (sentiment_at_turn)",
        risk_band = risk_band
      ) %>%
      select(Date=timestamp, Period, Layer, Agent=agent,
             Signal, Source, Excerpt=excerpt_full, risk_band),

    if (!is.null(warn))
      warn %>% mutate(
        Layer     = "Warning Signal",
        Signal    = paste0("keyword: ", matched_word),
        Source    = "deliberating",
        risk_band = "NONE"
      ) %>%
      select(Date=timestamp, Period, Layer, Agent=agent,
             Signal, Source, Excerpt=excerpt_full, risk_band),

    if (!is.null(off))
      off %>% mutate(
        Layer     = "Off-script Post",
        Signal    = channel,
        Source    = "channel",
        risk_band = "NONE"
      ) %>%
      select(Date=timestamp, Period, Layer, Agent=agent,
             Signal, Source, Excerpt=excerpt_full, risk_band)
  ) %>%
    arrange(Date) %>%
    mutate(Date = format(Date, "%d %b %Y %H:%M"))
}

# ── MAIN PLOT ──────────────────────────────────────────────────────────────────
plot_risk_timeline <- function(periods,
                               seeds     = character(0),
                               mode      = "cosine",
                               threshold = 0.15) {

  if (length(periods) == 0)
    return(tl_empty_plot("Select at least one period."))

  pt_ex  <- build_pt_extracted(periods)
  pt_fm  <- build_pt_formal(periods)
  warn   <- build_warnings(periods, seeds, mode, threshold)
  off    <- build_offscript(periods)

  has_pt_ex  <- !is.null(pt_ex)  && nrow(pt_ex)  > 0
  has_pt_fm  <- !is.null(pt_fm)  && nrow(pt_fm)  > 0
  has_warn   <- !is.null(warn)   && nrow(warn)   > 0
  has_off    <- !is.null(off)    && nrow(off)    > 0

  if (!has_pt_ex && !has_pt_fm && !has_warn && !has_off)
    return(tl_empty_plot(paste0(
      "No signals found for: ", paste(periods, collapse=", "), "\n\n",
      "Pre-Embargo has no PT risk scores.\n",
      "Try enabling internal states or checking seed words."
    )))

  # x-axis from data
  all_times <- as.POSIXct(c(
    if (has_pt_ex) pt_ex$timestamp,
    if (has_pt_fm) pt_fm$timestamp,
    if (has_warn)  warn$timestamp,
    if (has_off)   off$timestamp
  ))
  x_min <- min(all_times) - days(1)
  x_max <- max(all_times) + days(3)

  # background shading
  shade_df <- tibble(
    period = PERIOD_LEVELS,
    xmin   = as.POSIXct(c(START_DATE, EMBARGO_DATE, LEAK_DATE)),
    xmax   = as.POSIXct(c(EMBARGO_DATE, LEAK_DATE, END_DATE)),
    fill   = c("#E3F2FD", "#FFF3E0", "#FCE4EC")
  ) %>% filter(period %in% periods)

  p <- ggplot()

  for (i in seq_len(nrow(shade_df))) {
    p <- p + annotate("rect",
      xmin  = max(shade_df$xmin[i], x_min),
      xmax  = min(shade_df$xmax[i], x_max),
      ymin  = -Inf, ymax = Inf,
      fill  = shade_df$fill[i], alpha = 0.25)
  }

  # layer band backgrounds
  if (has_warn)
    p <- p + annotate("rect", xmin=x_min, xmax=x_max,
                      ymin=1.5, ymax=3.5, fill="#f8fafc", alpha=0.4)
  if (has_off)
    p <- p + annotate("rect", xmin=x_min, xmax=x_max,
                      ymin=0.3, ymax=1.6, fill="#f0f9ff", alpha=0.4)

  # ── Layer 1a: extracted X/10 line ─────────────────────────────────────────
  if (has_pt_ex) {
    pt_ex <- pt_ex %>%
      mutate(risk_band = factor(risk_band,
               levels=c("LOW","MODERATE","HIGH","CRITICAL")))

    p <- p +
      geom_line(data=pt_ex,
                aes(x=timestamp, y=risk_score),
                colour="#1D9E75", linewidth=1.2, linetype="dashed") +
      geom_point(data=pt_ex,
                 aes(x=timestamp, y=risk_score, fill=risk_band),
                 shape=21, size=5, colour="white", stroke=1.5) +
      # label: score + short excerpt only
      geom_label_repel(
        data               = pt_ex,
        aes(x              = timestamp,
            y              = risk_score,
            label          = paste0(risk_score, "/10\n", excerpt_short),
            colour         = risk_band),
        size               = 2.4,
        fill               = "white",
        label.padding      = unit(0.2, "lines"),
        label.size         = 0.3,
        max.overlaps       = Inf,
        force              = 8,
        force_pull         = 0.1,
        box.padding        = 1.2,
        point.padding      = 0.5,
        min.segment.length = 0.1,
        nudge_y            = 0.8,   # push labels upward from points
        direction          = "x",   # spread horizontally first
        show.legend        = FALSE
      )
  }

  # ── Layer 1b: formal Jun 5 markers (ticks, not connected) ─────────────────
  if (has_pt_fm) {
    pt_fm <- pt_fm %>%
      mutate(risk_band = factor(risk_band,
               levels=c("LOW","MODERATE","HIGH","CRITICAL")))

    p <- p +
      geom_point(data=pt_fm,
                 aes(x=timestamp, y=risk_score, fill=risk_band),
                 shape=23, size=4, colour="white", stroke=1.2,
                 alpha=0.75)
      # no labels — formal scores are secondary; table shows detail
  }

  # ── Layer 2: warning diamonds ─────────────────────────────────────────────
  if (has_warn) {
    agent_y_warn <- c(
      "Legal-Agent"          = 3.2,
      "Platform-Trust-Agent" = 2.9,
      "Social-Manager-Agent" = 2.6,
      "PR-Agent"             = 2.3,
      "PR-Intern-Agent"      = 2.0,
      "Intern-Agent"         = 1.7,
      "Judge-Agent"          = 1.5
    )
    warn <- warn %>%
      mutate(y_pos = coalesce(agent_y_warn[agent], 2.5))

    p <- p +
      geom_point(data=warn,
                 aes(x=timestamp, y=y_pos, colour=agent),
                 shape=18, size=4, alpha=0.85)
  }

  # ── Layer 3: off-script triangles ─────────────────────────────────────────
  if (has_off) {
    agent_y_off <- c(
      "Legal-Agent"          = 1.5,
      "Platform-Trust-Agent" = 1.3,
      "Social-Manager-Agent" = 1.1,
      "PR-Agent"             = 0.9,
      "PR-Intern-Agent"      = 0.7,
      "Intern-Agent"         = 0.5,
      "Judge-Agent"          = 0.4
    )
    off <- off %>%
      mutate(y_pos = coalesce(agent_y_off[agent], 1.0))

    p <- p +
      geom_point(data=off,
                 aes(x=timestamp, y=y_pos, colour=agent),
                 shape=17, size=3.5, alpha=0.85)
  }

  # ── event lines ───────────────────────────────────────────────────────────
  if (as.POSIXct(EMBARGO_DATE) >= x_min &&
      as.POSIXct(EMBARGO_DATE) <= x_max) {
    p <- p +
      geom_vline(xintercept=as.POSIXct(EMBARGO_DATE),
                 colour="#E65100", linetype="dashed", linewidth=0.8) +
      annotate("text", x=as.POSIXct(EMBARGO_DATE), y=10.2,
               label="Embargo (23 May)", hjust=-0.05,
               size=2.6, colour="#E65100", fontface="italic")
  }
  if (as.POSIXct(LEAK_DATE) >= x_min &&
      as.POSIXct(LEAK_DATE) <= x_max) {
    p <- p +
      geom_vline(xintercept=as.POSIXct(LEAK_DATE),
                 colour="#B71C1C", linewidth=1.2) +
      annotate("text", x=as.POSIXct(LEAK_DATE), y=10.2,
               label="@Elena post (29 May)", hjust=-0.05,
               size=2.6, colour="#B71C1C", fontface="bold")
  }

  # ── row annotations ───────────────────────────────────────────────────────
  if (has_pt_ex || has_pt_fm)
    p <- p + annotate("text", x=x_max, y=7.0,
                      label="← PT risk score (/10)\n  (dashed=extracted, ◆=formal)",
                      hjust=1, size=2.4, colour="#1D9E75", fontface="bold")
  if (has_warn)
    p <- p + annotate("text", x=x_max, y=2.5,
                      label="← Warning signals\n  (deliberating)",
                      hjust=1, size=2.4, colour="#64748b")
  if (has_off)
    p <- p + annotate("text", x=x_max, y=1.0,
                      label="← Off-script posts\n  (personal/anon)",
                      hjust=1, size=2.4, colour="#64748b")

  # ── scales ────────────────────────────────────────────────────────────────
  risk_fills <- c(LOW="#dcfce7", MODERATE="#fef3c7",
                  HIGH="#fecaca", CRITICAL="#fee2e2")
  risk_cols  <- c(LOW="#16a34a", MODERATE="#f59e0b",
                  HIGH="#ef4444", CRITICAL="#7f1d1d")

  agents_used <- unique(c(
    if (has_warn) warn$agent,
    if (has_off)  off$agent
  ))
  agent_cols_used <- AGENT_COLS[names(AGENT_COLS) %in% agents_used]
  missing <- setdiff(agents_used, names(AGENT_COLS))
  if (length(missing) > 0) {
    extra <- setNames(scales::hue_pal()(length(missing)), missing)
    agent_cols_used <- c(agent_cols_used, extra)
  }

  p <- p +
    scale_fill_manual(
      values = risk_fills, name = "Risk level",
      guide  = guide_legend(
        nrow=1, title.position="top",
        override.aes=list(shape=21, size=5, stroke=1.5))
    ) +
    scale_colour_manual(
      values = c(risk_cols, agent_cols_used),
      name   = "Agent",
      breaks = names(agent_cols_used),
      guide  = guide_legend(
        nrow=2, title.position="top",
        override.aes=list(shape=18, size=4))
    ) +
    scale_x_datetime(
      date_labels = "%b %d",
      date_breaks = "3 days",
      limits      = c(x_min, x_max)
    ) +
    scale_y_continuous(
      name   = "Risk score (/10)",
      limits = c(0, 10.5),
      breaks = c(0, 2, 4, 6, 8, 10)
    ) +
    labs(
      x        = NULL,
      subtitle = paste0(
        "Layer 2 (warning signals) adapts to analysis mode  |  ",
        "Layers 1 and 3 are independent of cosine/frequency setting"
      )
    ) +
    theme_minimal(base_size=11) +
    theme(
      axis.text.x      = element_text(angle=45, hjust=1),
      legend.position  = "bottom",
      legend.box       = "horizontal",
      legend.text      = element_text(size=8),
      legend.title     = element_text(size=8, face="bold"),
      legend.key.size  = unit(0.8, "lines"),
      legend.spacing.x = unit(0.3, "cm"),
      plot.subtitle    = element_text(size=8.5, colour="grey50"),
      panel.grid.minor = element_blank(),
      plot.background  = element_rect(fill="white", colour=NA),
      plot.margin      = margin(10, 100, 25, 10)
    )

  p
}

message("keyword_risk_timeline.R loaded.")