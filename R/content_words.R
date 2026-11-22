#' Extract content words
#'
#' Extracts informative words from text and removes common words that do not
#' contribute with meaning.
#'
#' @param x Character string.
#' @return Character vector.
#' @noRd
content_words <- function(x) { # Define function

  words <- unlist( # Convert list output into a simple vector
    strsplit(tolower(x), "[^[:alpha:]]+") # Make lowercase letters and split into words
  )

  words <- words[nchar(words) > 2]  # Remove very short words with 1-2 letters

  stop_words <- c(  # Common words to ignore
    "the", "and", "for", "with", "that", "this",
    "from", "into", "have", "been", "were", "will",
    "their", "they", "them", "about", "also",
    "what", "when", "where", "which"
  )

  words[!words %in% stop_words] # Return only words not in stop_words
}
