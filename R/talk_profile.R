#' Overall Presentation Profile
#'
#' Runs all seven metrics on a script and returns them as a `talk_profile` object.
#'
#' @param path Path to a .txt script (slides labelled `S0:`, `S1:`...).
#' @param lex Word lists, see [default_lexicons()].
#' @return Object of class `talk_profile`: `scores` (named numeric vector),
#'   `details` (full output of each metric), `sentences`.
#' @export
#' @examples
#' f <- system.file("extdata", "talk.txt", package = "talkscore")
#' p <- talk_profile(f)
#' p
#' p$details$reinforcement$by_slide
talk_profile <- function(path, lex = default_lexicons()) {
  s <- split_sentences(read_talk(path))
  d <- list(
    knowledge_building = knowledge_building(s, lex),
    conclusion_density = conclusion_density(s, lex),
    novel_info         = novel_info(s),
    storytelling       = storytelling(s, lex),
    clinical_relevance = clinical_relevance(s, lex),
    reinforcement      = reinforcement(s, lex$key_concepts),
    uncertainty        = uncertainty(s, lex)
  )
  structure(list(scores = vapply(d, function(x) x$score, numeric(1)),
                 details = d, sentences = s),
            class = "talk_profile")
}

#' @rdname talk_profile
#' @param x A `talk_profile` object.
#' @param ... Unused.
#' @export
print.talk_profile <- function(x, ...) {
  cat("Presentation profile (", nrow(x$sentences), " sentences)\n", sep = "")
  print(round(x$scores, 2))
  invisible(x)
}

