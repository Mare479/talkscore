#' Summarise talk metrics per slide
#'
#' @param s Output of the flag_* functions.
#' @return A data.frame with one row per slide, with counts of sentences,
#'   new-information sentences, conclusions, callbacks and positive/negative words.
#' @export
summarise_talk <- function(s) {
  agg <- aggregate(cbind(sentences = 1, new_info, conclusion, callback, n_pos, n_neg) ~ slide,
                   data = transform(s, new_info = as.integer(new_info),
                                    conclusion = as.integer(conclusion),
                                    callback = as.integer(callback)),
                   FUN = sum)
  agg[order(as.integer(sub("S", "", agg$slide))), ]
}