#' Analyze a talk script end to end
#'
#' Runs [read_talk()], [split_sentences()], [flag_novelty()], [flag_cues()],
#' [flag_callbacks()] and [summarise_talk()].
#'
#' @param path Path to a .txt script.
#' @param ... Passed to [flag_novelty()] (e.g. `threshold`).
#' @return A list: `sentences` (sentence-level flags) and `summary` (per-slide counts).
#' @export
#' @examples
#' f <- system.file("extdata", "talk.txt", package = "talkscore")
#' analyze_talk(f)$summary
analyze_talk <- function(path, ...) {
  s <- split_sentences(read_talk(path))
  s <- flag_callbacks(flag_cues(flag_novelty(s, ...)))
  list(sentences = s, summary = summarise_talk(s))
}