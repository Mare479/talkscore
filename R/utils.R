count_hits <- function(x, patterns) {
  rx <- paste0("\\b(", paste(patterns, collapse = "|"), ")\\b")
  lengths(regmatches(x, gregexpr(rx, x, ignore.case = TRUE, perl = TRUE)))
}
