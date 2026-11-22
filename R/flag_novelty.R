#' Flag sentences that introduce new information
#'
#' Novelty defined as a proportion of a sentence's content words not used in any earlier occasion.
#'
#' @param s Output of [split_sentences()].
#' @param threshold Novelty cutoff (0-1) above which a sentence counts as new information.
#' @return `s` with added columns `novelty` and `new_info` (logical).
#' @export
flag_novelty <- function(s, threshold = 0.5) {  # Define function and novelty cutoff
  seen <- character(0)  # Start with no words we saw yet
  s$novelty <- vapply(s$sentence, function(x) { # Loop through every sentence
    w <- content_words(x)  # Extract meaningful words by removing stop words (see content_words)
    nv <- if (length(w)) mean(!w %in% seen) else 0  # Calculate proportion of words not seen before
    seen <<- union(seen, w) # Add current words to memory
    nv # Return novelty score for this sentence
  }, numeric(1), USE.NAMES = FALSE)    # Store one numeric value per sentence
  s$new_info <- s$novelty >= threshold # Mark highly novel sentences as new information
  s # Return updated data frame
}
