#' Flag conclusions and positive/negative wording
#'
#' Matches sentences against cue-phrase and word lists.
#'
#' @param s Sentence data.frame.
#' @param conclusion Character vector of conclusion cue phrases.
#' @param positive,negative Character vectors of sentiment words.
#' @return `s` with columns `conclusion`, `n_pos`, `n_neg`.
#' @export
flag_cues <- function(s,
                      conclusion = c("ultimately","in conclusion","this supports","therefore","thus",
                                     "which means","this suggests","taken together","our main finding",
                                     "in summary","overall"),
                      positive = c("remarkably","valuable","unique","efficient","remission","motivation",
                                   "important","significant","strong","perfect"),
                      negative = c("unknown","unclear","unresolved","relapse","damage","injury","death",
                                   "lymphopenia","pathogenic","deadliest","poorly")) {
  low <- tolower(s$sentence)
  s$conclusion <- vapply(low, function(x) any(vapply(conclusion, grepl, logical(1), x = x, fixed = TRUE)), logical(1), USE.NAMES = FALSE)
  count <- function(lex) vapply(low, function(x) {
    w <- unlist(strsplit(gsub("[^a-z ]", " ", x), "\\s+"))
    sum(w %in% lex)
  }, numeric(1), USE.NAMES = FALSE)
  s$n_pos <- count(positive)
  s$n_neg <- count(negative)
  s
}