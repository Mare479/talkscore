#' Extract content words
#'
#' Extracts informative words from a text string and removes common stop words.
#'
#' @param x Character string.
#' @return Character vector.
#' @noRd
content_words <- function(x) {

  words <- unlist(
    strsplit(tolower(x), "[^[:alpha:]]+")
  )

  words <- words[nchar(words) > 2]

  stop_words <- c(
    "the", "and", "for", "with", "that", "this",
    "from", "into", "have", "been", "were", "will",
    "their", "they", "them", "about", "also",
    "what", "when", "where", "which"
  )

  words[!words %in% stop_words]
}
