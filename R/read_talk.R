#' Read talk notes saved as txt file
#'
#' Reads a txt file and tags each text line with its slide label. A label is a line
#' consisting of `S` followed by a number (`S3`), optionally with a colon and the
#' text on the same line (`S3: text`). Blank lines are ignored.
#'
#' @param path Path to a .txt file containing presentation notes.
#' @return A data.frame with columns `slide` and `text`.
#' @export
#' @examples
#' f <- system.file("extdata", "talk.txt", package = "talkscore")
#' read_talk(f)
read_talk <- function(path) {
  lines <- suppressWarnings(
    read.delim(path, header = FALSE, sep = "\n", quote = "",
               stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")[[1]]
  )
  lines <- trimws(lines)
  slide <- NA_character_
  out <- list()
  for (l in lines[lines != ""]) {
    m <- regmatches(l, regexec("^(S[0-9]+)(:\\s*|\\s*$)(.*)$", l))[[1]]
    if (length(m)) { slide <- m[2]; l <- m[4] }
    if (nzchar(l)) {
      out[[length(out) + 1]] <- data.frame(slide = slide, text = l,
                                           stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, out)
}