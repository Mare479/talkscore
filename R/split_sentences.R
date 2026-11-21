#' Split a talk into sentences
#'
#' @param df Output of [read_talk()].
#' @return A data.frame with `id`, `slide`, `sentence`.
#' @export
split_sentences <- function(df) {
  res <- lapply(seq_len(nrow(df)), function(i) {
    s <- unlist(strsplit(df$text[i], "(?<=[.!?])\\s+", perl = TRUE))
    data.frame(slide = df$slide[i], sentence = s, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, res)
  out <- out[nzchar(trimws(out$sentence)), ]
  out$id <- seq_len(nrow(out))
  out[, c("id", "slide", "sentence")]
}