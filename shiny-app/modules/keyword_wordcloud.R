# ============================================================
# keyword_wordcloud.R
# Word cloud visualisation module.
# Run standalone to test:
#   source("modules/keyword_data_wrangling.R")
#   source("modules/keyword_wordcloud.R")
#   msgs <- filter_msgs("Pre-Embargo")
#   seeds <- c("risk","embargo","leak","merger","legal")
#   print(plot_wordcloud(msgs, seeds, mode="freq", threshold=0.15))
#   print(plot_wordcloud(msgs, seeds, mode="cosine", threshold=0.15))
# ============================================================

library(tidytext)
library(ggwordcloud)
library(text2vec)

# Guard: load wrangling if run standalone
if (!exists("KEYWORD_WRANGLING_SOURCED")) {
  source(file.path(dirname(sys.frame(1)$ofile %||% "."), "keyword_data_wrangling.R"))
}

# ── EMPTY PLOT HELPER ──────────────────────────────────────────────────────────
wc_empty_plot <- function(msg) {
  ggplot() +
    annotate("text", x=0.5, y=0.5, label=msg, size=3.8,
             colour="#64748b", hjust=0.5, vjust=0.5, lineheight=1.5) +
    theme_void() +
    theme(plot.background=element_rect(fill="#f8fafc", colour="#e2e8f0", linewidth=0.5))
}

# ── COSINE FREQUENCY (seed-associated vocab) ────────────────────────────────────
# Builds TF-IDF from msgs corpus, finds words most similar to seeds via cosine,
# then returns frequency of those words in the original msgs.
get_cosine_freq <- function(msgs, seeds, threshold) {
  if (nrow(msgs) == 0 || length(seeds) == 0) return(NULL)

  corpus <- msgs %>%
    filter(!is.na(content), content != "") %>%
    mutate(
      doc_id  = as.character(row_number()),
      content = str_to_lower(content) %>%
        str_replace_all("[^a-z\\s]", " ") %>%
        str_squish()
    ) %>%
    filter(content != "") %>%
    select(doc_id, content) %>%
    deframe()

  corpus <- as.character(corpus)
  corpus <- corpus[nchar(corpus) > 0]
  names(corpus) <- seq_along(corpus)
  if (length(corpus) == 0) return(NULL)

  it    <- itoken(corpus, tokenizer = word_tokenizer, progressbar = FALSE)
  vocab <- create_vocabulary(it) %>%
    prune_vocabulary(term_count_min = 2, doc_count_min = 1) %>%
    filter(!term %in% EXTRA_STOPS, nchar(term) > 2)
  if (nrow(vocab) == 0) return(NULL)

  vec       <- vocab_vectorizer(vocab)
  dtm       <- create_dtm(it, vec)
  tfidf     <- TfIdf$new()
  dtm_tfidf <- fit_transform(dtm, tfidf)
  tdm       <- Matrix::t(dtm_tfidf)

  valid_seeds <- seeds[seeds %in% rownames(tdm)]
  if (length(valid_seeds) == 0) return(NULL)

  seed_vocab <- map_dfr(valid_seeds, function(seed) {
    sv  <- tdm[seed, , drop = FALSE]
    sim <- sim2(tdm, sv, method = "cosine", norm = "l2")
    tibble(word = rownames(sim), sim = as.numeric(sim)) %>%
      filter(word != seed,
             !word %in% seeds,
             !word %in% EXTRA_STOPS,
             sim > threshold) %>%
      slice_max(sim, n = 15)
  }) %>%
    pull(word) %>%
    unique()

  if (length(seed_vocab) == 0) return(NULL)

  msgs %>%
    unnest_tokens(word, content) %>%
    filter(word %in% seed_vocab) %>%
    count(word, sort = TRUE) %>%
    rename(freq = n)
}

# ── MAIN PLOT FUNCTION ─────────────────────────────────────────────────────────
#' @param msgs     Filtered message tibble (from filter_msgs())
#' @param seeds    Character vector of seed words
#' @param mode     "cosine" (TF-IDF similarity) or "freq" (simple frequency)
#' @param threshold Cosine similarity threshold (only used when mode="cosine")
plot_wordcloud <- function(msgs,
                           seeds     = character(0),
                           mode      = "cosine",
                           threshold = 0.15) {

  if (nrow(msgs) == 0)
    return(wc_empty_plot("No messages in this selection."))

  if (mode == "freq") {
    # Simple frequency: all non-stopword tokens, no seed filtering
    freq <- msgs %>%
      unnest_tokens(word, content) %>%
      filter(!word %in% EXTRA_STOPS, nchar(word) > 2) %>%
      count(word, sort = TRUE) %>%
      rename(freq = n) %>%
      slice_max(freq, n = 80)

    if (nrow(freq) == 0)
      return(wc_empty_plot("No words remain after removing stop words."))

  } else {
    # Cosine mode: seed-associated vocabulary
    if (length(seeds) == 0)
      return(wc_empty_plot("Enter seed words to use cosine mode."))

    freq <- get_cosine_freq(msgs, seeds, threshold)

    if (is.null(freq) || nrow(freq) == 0)
      return(wc_empty_plot(
        "No words found above cosine threshold.\nTry lowering the threshold or switching to Simple Frequency."
      ))
  }

  ggplot(freq, aes(label = word, size = freq, colour = freq)) +
    geom_text_wordcloud(
      area_corr    = TRUE,
      seed         = 42,
      family       = "sans",
      fontface     = "bold",
      shape        = "square",
      rm_outside   = TRUE,
      eccentricity = 1        # perfectly square spiral
    ) +
    scale_size_area(max_size = 28) +
    scale_colour_gradient(low = "#1d4ed8", high = "#0f172a") +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", colour = NA),
      plot.margin     = margin(5, 5, 5, 5)
    )
}

message("keyword_wordcloud.R loaded.")
