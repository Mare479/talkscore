#' Flag sentences that introduce new information
#'
#' Novelty defined as a proportion of a sentence's content words not used in any earlier occasion.
#'
#' @param s Output of [split_sentences()].
#' @param threshold Novelty cutoff (0-1) above which a sentence counts as new information.
#' @return `s` with added columns `novelty` and `new_info` (logical).
#' @export
flag_novelty <- function(s, threshold = 0.5) {
  seen <- character(0)
  s$novelty <- vapply(s$sentence, function(x) {
    w <- content_words(x)
    nv <- if (length(w)) mean(!w %in% seen) else 0
    seen <<- union(seen, w)
    nv
  }, numeric(1), USE.NAMES = FALSE)
  s$new_info <- s$novelty >= threshold
  s
}
