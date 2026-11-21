#' Knowledge-Building Score
#'
#' Balance between factual statements and interpretation (1 = perfectly balanced).
#'
#' @param s Sentence data.frame from [split_sentences()].
#' @param lex Word lists, see [default_lexicons()].
#' @return List with `score`, `fact`, `interpretation`.
#' @export
knowledge_building <- function(s, lex = default_lexicons()) {
  f <- sum(count_hits(s$sentence, lex$fact))
  i <- sum(count_hits(s$sentence, lex$interpretation))
  list(score = if (f + i == 0) NA_real_ else 1 - abs(f - i) / (f + i),
       fact = f, interpretation = i)
}

#' Conclusion Density
#'
#' Proportion of sentences containing a conclusion word.
#'
#' @inheritParams knowledge_building
#' @return List with `score`, `n`, `by_slide`.
#' @export
conclusion_density <- function(s, lex = default_lexicons()) {
  hit <- count_hits(s$sentence, lex$conclusion) > 0
  list(score = mean(hit), n = sum(hit), by_slide = tapply(hit, s$slide, sum))
}

#' Novel Information Load
#'
#' Mean sentence novelty (see [flag_novelty()]) plus count of newly introduced abbreviations
#' (gene/protein/term names written in capitals, e.g. MDA5, ISG15).
#'
#' @inheritParams knowledge_building
#' @param threshold Novelty cutoff passed to [flag_novelty()].
#' @return List with `score`, `new_info_sentences`, `new_abbreviations`, `abbreviations`.
#' @export
novel_info <- function(s, threshold = 0.5) {
  s <- flag_novelty(s, threshold)
  abbr <- regmatches(s$sentence, gregexpr("\\b[A-Z][A-Za-z]*[A-Z0-9]+\\b", s$sentence))
  seen <- character(0)
  new_abbr <- integer(nrow(s))
  for (i in seq_len(nrow(s))) {
    a <- unique(abbr[[i]])
    new_abbr[i] <- sum(!a %in% seen)
    seen <- union(seen, a)
  }
  list(score = mean(s$novelty), new_info_sentences = sum(s$new_info),
       new_abbreviations = sum(new_abbr), abbreviations = seen)
}

#' Storytelling Score
#'
#' Labels each sentence Problem (P), Evidence (E) or Resolution (R) by its dominant
#' word list, then counts complete P -> E -> R sequences in order.
#'
#' @inheritParams knowledge_building
#' @return List with `score` (share of labelled sentences inside complete cycles, 0-1),
#'   `cycles`, `sequence` (the P/E/R string).
#' @export
storytelling <- function(s, lex = default_lexicons()) {
  h <- cbind(P = count_hits(s$sentence, lex$problem),
             E = count_hits(s$sentence, lex$evidence),
             R = count_hits(s$sentence, lex$resolution))
  stage <- apply(h, 1, function(r) {
    if (max(r) == 0 || sum(r == max(r)) > 1) NA_character_ else colnames(h)[which.max(r)]
  })
  stage <- stage[!is.na(stage)]
  state <- 0
  cycles <- 0
  for (st in stage) {
    if (st == "P") {
      state <- 1
    } else if (st == "E" && state >= 1) {
      state <- 2
    } else if (st == "R" && state == 2) {
      cycles <- cycles + 1
      state <- 0
    }
  }
  list(score = if (length(stage)) min(1, 3 * cycles / length(stage)) else NA_real_,
       cycles = cycles, sequence = paste(stage, collapse = ""))
}

#' Clinical Relevance Score
#'
#' Share of mechanistic sentences that are the same as, or directly adjacent to,
#' a clinical sentence.
#'
#' @inheritParams knowledge_building
#' @return List with `score`, `clinical`, `mechanistic` (sentence counts).
#' @export
clinical_relevance <- function(s, lex = default_lexicons()) {
  cl <- count_hits(s$sentence, lex$clinical) > 0
  me <- count_hits(s$sentence, lex$mechanistic) > 0
  near <- cl | c(FALSE, head(cl, -1)) | c(tail(cl, -1), FALSE)
  list(score = if (sum(me)) sum(me & near) / sum(me) else NA_real_,
       clinical = sum(cl), mechanistic = sum(me))
}

#' Reinforcement Score
#'
#' How often and how widely key concepts are revisited across slides.
#'
#' @inheritParams knowledge_building
#' @param concepts Regex patterns of key concepts.
#' @return List with `score` (mean share of slides mentioning each concept),
#'   `spread`, `frequency`, `by_slide` (slide x concept count matrix).
#' @export
reinforcement <- function(s, concepts = default_lexicons()$key_concepts) {
  slides <- unique(s$slide)
  m <- sapply(concepts, function(cn)
    sapply(slides, function(sl) sum(count_hits(s$sentence[s$slide == sl], cn))))
  m <- matrix(m, nrow = length(slides), dimnames = list(slide = slides, concept = concepts))
  list(score = mean(colMeans(m > 0)), spread = colMeans(m > 0),
       frequency = colSums(m), by_slide = m)
}

#' Scientific Uncertainty Score
#'
#' Share of hedging terms among certainty plus uncertainty terms (0 = all certain, 1 = all hedged).
#'
#' @inheritParams knowledge_building
#' @return List with `score`, `certainty`, `uncertainty`.
#' @export
uncertainty <- function(s, lex = default_lexicons()) {
  ce <- sum(count_hits(s$sentence, lex$certainty))
  un <- sum(count_hits(s$sentence, lex$uncertainty))
  list(score = if (ce + un == 0) NA_real_ else un / (ce + un),
       certainty = ce, uncertainty = un)
}
