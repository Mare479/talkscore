#' Knowledge-building Score
#'
#' Balance between facts and interpretation (1 = perfect balance).
#'
#' @param s Sentence data.frame from [split_sentences()].
#' @param lex Word lists, see [default_lexicons()].
#' @return List with `score`, `fact`, `interpretation`.
#' @export
knowledge_building <- function(s, lex = default_lexicons()) {  # Define function
  #and load "dictionary"
  f <- sum(count_hits(s$sentence, lex$fact)) # Count all facts
  i <- sum(count_hits(s$sentence, lex$interpretation)) # Count all interpretation words
  list(score = if (f + i == 0) NA_real_ else 1 - abs(f - i) / (f + i), # if neither fact or interpretation return NA, calculate balance between facts and interpretation
       fact = f, interpretation = i) # Return total fact count and interpretation count
}

#' Conclusion Density
#'
#' Proportion of sentences containing a conclusion word.
#'
#' @inheritParams knowledge_building
#' @return List with `score`, `n`, `by_slide`.
#' @export
conclusion_density <- function(s, lex = default_lexicons()) {
  hit <- count_hits(s$sentence, lex$conclusion) > 0 # Mark each sentence TRUE if it contains at least one conclusion word
  list(score = mean(hit), # Proportion of sentences having conclusion words
       n = sum(hit), # Total n of sentences with conclusion word
       by_slide = tapply(hit, s$slide, sum)) # Number of conclusion sentences per slide
}

#' Novel Information
#'
#' Mean sentence novelty (see [flag_novelty()]) plus count of new introduced abbreviations
#' (terms written in capitals like MDA5, ISG15).
#'
#' @inheritParams knowledge_building
#' @param threshold Novelty cutoff passed to [flag_novelty()].
#' @return List with `score`, `new_info_sentences`, `new_abbreviations`, `abbreviations`.
#' @export
novel_info <- function(s, threshold = 0.5) {
  s <- flag_novelty(s, threshold) # Calculate novelty score per sentence using flag_novelty output
  abbr <- regmatches(s$sentence, gregexpr("\\b[A-Z][A-Za-z]*[A-Z0-9]+\\b", s$sentence)) # Capitalized terms that could be a new abbreviation
  seen <- character(0) # Make memory of all seen abbreviations
  new_abbr <- integer(nrow(s)) # Store nr of new abbreviations per sentence
  for (i in seq_len(nrow(s))) {
    a <- unique(abbr[[i]]) # Unique abbreviations in a sentence
    new_abbr[i] <- sum(!a %in% seen) #Count the ones not seen so far
    seen <- union(seen, a) #Update memory
  }
  list(score = mean(s$novelty), # Average novelty score in all sentences
       new_info_sentences = sum(s$new_info), # Nr of sentences flagged as new info
       new_abbreviations = sum(new_abbr), # Nr of abbreviations shown for the first time
       abbreviations = seen) # List all abbreaviations
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
  # Count problem, evidence and resolution words per sentence
  stage <- apply(h, 1, function(r) {
    if (max(r) == 0 || sum(r == max(r)) > 1) NA_character_
    # Ignore sentences without clear category

    else colnames(h)[which.max(r)] # Choose dominant category in sentence
  })
  stage <- stage[!is.na(stage)] # Remove sentences without category
  state <- 0
  cycles <- 0 # Storytelling tracing from problem to resolution (P-> E -> R)
  #in cycles where cycle is sentences in PER order
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
       # Proportion of sentences taking place in full cycles
       cycles = cycles, sequence = paste(stage, collapse = "")) # Number of complete P→E→R cycles
}

#' Clinical Relevance Score
#'
#' Proportion of mechanistic sentences that are directly adjacent to
#' a sentence with clinical terms.
#'
#' @inheritParams knowledge_building
#' @return List with `score`, `clinical`, `mechanistic` (sentence counts).
#' @export
clinical_relevance <- function(s, lex = default_lexicons()) {
  cl <- count_hits(s$sentence, lex$clinical) > 0   # Identify clinical sentences
  me <- count_hits(s$sentence, lex$mechanistic) > 0 # Identify mechanistic sentences
  near <- cl | c(FALSE, head(cl, -1)) | c(tail(cl, -1), FALSE) # Mark clinical sentences and their immediate neighbors
  list(score = if (sum(me)) sum(me & near) / sum(me) else NA_real_,   # Proportion of mechanistic sentences located near clinical sentence
       clinical = sum(cl), mechanistic = sum(me)) # Number of clinical and mechanistic sentences
}

#' Reinforcement Score
#'
#' How often key concepts are mentioned across slides
#'
#' @inheritParams knowledge_building
#' @param concepts Regex patterns of key concepts.
#' @return List with `score` (mean of slides mentioning each key concept),
#'   `spread`, `frequency`, `by_slide` (slide x concept count matrix).
#' @export
reinforcement <- function(s, concepts = default_lexicons()$key_concepts) {
  slides <- unique(s$slide) # Get all slides
  m <- sapply(concepts, function(cn) # Count how many time concept occurs per slide
    sapply(slides, function(sl) sum(count_hits(s$sentence[s$slide == sl], cn))))
  m <- matrix(m, nrow = length(slides), dimnames = list(slide = slides, concept = concepts)) # Average proportion of slides mentioning each key concept
  list(score = mean(colMeans(m > 0)), # Average of slides mentioning each key concept
       spread = colMeans(m > 0),
       frequency = colSums(m), # Total nr of mentions per concept
       by_slide = m) #By slide key concepts
}

#' Scientific Uncertainty Score
#'
#' Proportion of "uncertain terms" among certainty plus uncertainty terms (0 = all certain, 1 = all uncertain ).
#'
#' @inheritParams knowledge_building
#' @return List with `score`, `certainty`, `uncertainty`.
#' @export
uncertainty <- function(s, lex = default_lexicons()) {
  ce <- sum(count_hits(s$sentence, lex$certainty)) # Count certainty words
  un <- sum(count_hits(s$sentence, lex$uncertainty)) # Count uncertainty words
  list(score = if (ce + un == 0) NA_real_ else un / (ce + un),    # Proportion of uncertainty language
       certainty = ce, # Nr of certainty terms
       uncertainty = un) # Nr of uncertainty terms
}
