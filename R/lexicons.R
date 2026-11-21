#' Default word lists
#'
#' Regular-expression word lists used by all metric functions. Edit or replace
#' entries and pass the result as `lex`.
#'
#' @return A named list of character vectors (regex patterns).
#' @export
default_lexicons <- function() list(
  fact           = c("show\\w*","observ\\w*","measur\\w*","detect\\w*","identif\\w*","isolat\\w*","profil\\w*","found","find"),
  interpretation = c("suggest\\w*","indicat\\w*","impl(y|ies|ied)","support\\w*","therefore","hypothes\\w*"),
  conclusion     = c("therefore","thus","collectively","overall","together","demonstrat\\w*","ultimately"),
  problem        = c("unknown","unclear","question\\w*","unresolved","not understood","do not know","still do not"),
  evidence       = c("show\\w*","observ\\w*","found","finding\\w*","profil\\w*"),
  resolution     = c("therefore","thus","suggest\\w*","indicat\\w*","support\\w*"),
  clinical       = c("disease","patient\\w*","fibrosis","treatment","therapy","prognosis","pathogenesis","remission","relapse\\w*","clinical\\w*"),
  mechanistic    = c("protein\\w*","receptor\\w*","signal\\w*","pathway\\w*","RNA","transcript\\w*","domain\\w*","epitope\\w*"),
  certainty      = c("demonstrat\\w*","confirm\\w*","establish\\w*","prove\\w*"),
  uncertainty    = c("may","might","could","possibl\\w*","potential\\w*"),
  key_concepts   = c("MDA5","interferon","B cell\\w*","ISG15","autoantibod\\w*")
)
