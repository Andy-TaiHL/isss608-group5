# ============================================================
# keyword_semantic_network.R
# Semantic network (cosine TF-IDF) visualisation module.
# Run standalone to test:
#   source("modules/keyword_data_wrangling.R")
#   source("modules/keyword_semantic_network.R")
#   msgs <- filter_msgs("Embargo to Leak")
#   seeds <- c("risk","embargo","leak","merger","legal")
#   print(plot_network(msgs, seeds, threshold=0.15))
# ============================================================

library(text2vec)
library(ggraph)
library(tidygraph)

if (!exists("KEYWORD_WRANGLING_SOURCED")) {
  source(file.path(dirname(sys.frame(1)$ofile %||% "."), "keyword_data_wrangling.R"))
}

# ── EMPTY PLOT HELPER ──────────────────────────────────────────────────────────
net_empty_plot <- function(msg) {
  ggplot() +
    annotate("text", x=0.5, y=0.5, label=msg, size=3.8,
             colour="#64748b", hjust=0.5, vjust=0.5, lineheight=1.5) +
    theme_void() +
    theme(plot.background=element_rect(fill="#f8fafc", colour="#e2e8f0", linewidth=0.5))
}

# ── MAIN PLOT FUNCTION ─────────────────────────────────────────────────────────
#' @param msgs      Filtered message tibble (from filter_msgs())
#' @param seeds     Character vector of seed words
#' @param threshold Cosine similarity threshold
#' @param top_n     Max associated words per seed node
plot_network <- function(msgs,
                         seeds     = character(0),
                         threshold = 0.15,
                         top_n     = NULL,
                         mode      = "cosine") {

  if (mode == "freq")
    return(net_empty_plot(
      "Semantic Network requires Cosine (TF-IDF) mode.\nSwitch Analysis Mode to Cosine to view."
    ))

  if (length(seeds) == 0)
    return(net_empty_plot("Enter seed words to build the network."))
  if (nrow(msgs) == 0)
    return(net_empty_plot("No messages in this selection."))

  # Auto top_n: fewer connections for larger corpora to avoid noise
  if (is.null(top_n)) {
    top_n <- case_when(
      nrow(msgs) > 400 ~ 4,
      nrow(msgs) > 200 ~ 6,
      TRUE             ~ 10
    )
  }

  # Log-scaled threshold: grows continuously with corpus size
  # formula: threshold + log1p(n/100) * 0.05
  # e.g. 145 msgs -> +0.049, 595 msgs -> +0.092, 117 msgs -> +0.038
  effective_threshold <- threshold + log1p(nrow(msgs) / 100) * 0.05

  # Build corpus
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
  if (length(corpus) == 0) return(net_empty_plot("Corpus empty after cleaning."))

  it    <- itoken(corpus, tokenizer = word_tokenizer, progressbar = FALSE)
  # stricter pruning for large corpora to reduce noise
  min_docs <- if (length(corpus) > 400) 3 else 2
  vocab <- create_vocabulary(it) %>%
    prune_vocabulary(term_count_min = 2, doc_count_min = min_docs) %>%
    filter(!term %in% EXTRA_STOPS, nchar(term) > 2)
  if (nrow(vocab) == 0) return(net_empty_plot("No vocabulary found."))

  vec       <- vocab_vectorizer(vocab)
  dtm       <- create_dtm(it, vec)
  tfidf     <- TfIdf$new()
  dtm_tfidf <- fit_transform(dtm, tfidf)
  tdm       <- Matrix::t(dtm_tfidf)

  valid_seeds <- seeds[seeds %in% rownames(tdm)]
  if (length(valid_seeds) == 0)
    return(net_empty_plot(
      "None of the seed words appear in this corpus.\nTry different seeds or a different period."
    ))

  # Build edges: seed → associated word, weight = cosine similarity
  edges <- map_dfr(valid_seeds, function(seed) {
    sv  <- tdm[seed, , drop = FALSE]
    sim <- sim2(tdm, sv, method = "cosine", norm = "l2")
    tibble(
      from   = seed,
      to     = rownames(sim),
      weight = as.numeric(sim)
    ) %>%
      filter(
        to != seed,
        !to %in% seeds,
        !to %in% EXTRA_STOPS,
        weight > effective_threshold
      ) %>%
      slice_max(weight, n = top_n)
  })

  if (nrow(edges) == 0)
    return(net_empty_plot(
      "No connections above threshold.\nTry lowering the cosine threshold."
    ))

  g <- edges %>%
    as_tbl_graph() %>%
    activate(nodes) %>%
    mutate(is_seed = name %in% valid_seeds)

  set.seed(42)
  ggraph(g, layout = "fr") +
    geom_edge_link(
      aes(width = weight, alpha = weight),
      colour = "#94a3b8", show.legend = FALSE
    ) +
    geom_node_point(aes(colour = is_seed, size = is_seed)) +
    geom_node_text(
      aes(label = name, fontface = if_else(is_seed, "bold", "plain")),
      repel = TRUE, size = 3, colour = "#1e293b", max.overlaps = 25
    ) +
    scale_colour_manual(
      values = c("TRUE" = "#1d4ed8", "FALSE" = "#64748b"),
      labels = c("TRUE" = "Seed word", "FALSE" = "Associated word"),
      name   = NULL
    ) +
    scale_size_manual(
      values = c("TRUE" = 5.5, "FALSE" = 3),
      guide  = "none"
    ) +
    scale_edge_width(range = c(0.4, 2.2)) +
    scale_edge_alpha(range = c(0.3, 0.9)) +
    labs(caption = paste0(
      "Cosine threshold: ", round(effective_threshold, 3),
      " (base: ", threshold, " + log-scale corpus adjustment: +",
      round(effective_threshold - threshold, 3), ")"
    )) +
    theme_graph(base_family = "sans") +
    theme(
      legend.position = "bottom",
      plot.background = element_rect(fill = "white", colour = NA)
    )
}

message("keyword_semantic_network.R loaded.")
