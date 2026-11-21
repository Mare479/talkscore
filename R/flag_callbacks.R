#' Flag sentences that circle back to earlier points
#'
#' A sentence is a callback if it contains a callback phrase, or its content words
#' overlap (Jaccard) with a sentence at least `gap` sentences earlier.
#'
#' @param s Sentence data.frame.
#' @param sim Jaccard similarity cutoff.
#' @param gap Minimum distance (in sentences) to an earlier sentence.
#' @param phrases Callback cue phrases.
#' @return `s` with columns `max_sim` and `callback` (logical).
#' @export
flag_callbacks <- function(s, sim = 0.3, gap = 3,
                           phrases = c("as i said","as mentioned","going back","again","earlier",
                                       "remember","once more","as we saw","coming back","return to")) {
  W <- lapply(s$sentence, content_words)
  s$max_sim <- vapply(seq_along(W), function(i) {
    if (i <= gap) return(0)
    max(vapply(seq_len(i - gap), function(j) {
      u <- length(union(W[[i]], W[[j]]))
      if (u == 0) 0 else length(intersect(W[[i]], W[[j]])) / u
    }, numeric(1)))
  }, numeric(1))
  cue <- vapply(tolower(s$sentence), function(x)
    any(vapply(phrases, grepl, logical(1), x = x, fixed = TRUE)), logical(1), USE.NAMES = FALSE)
  s$callback <- s$max_sim >= sim | cue
  s
}
