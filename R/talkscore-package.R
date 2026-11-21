#' talkscore: Semantic Analysis and Scoring of Scientific Presentations
#'
#' Scores a presentation script on seven metrics: knowledge building, conclusion
#' density, novel information load, storytelling, clinical relevance,
#' reinforcement and scientific uncertainty.
#'
#' Workflow: [read_talk()] -> [split_sentences()] -> metric functions
#' ([knowledge_building()], [conclusion_density()], [novel_info()],
#' [storytelling()], [clinical_relevance()], [reinforcement()], [uncertainty()]),
#' all driven by [default_lexicons()]. [talk_profile()] runs everything and
#' returns a `talk_profile` object.
#'
#' @keywords internal
"_PACKAGE"
