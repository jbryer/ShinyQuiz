#' Returns TRUE or FALSE if the user has answered all the quiz questions.
#'
#' @param mod the ShinyQuiz module
#' @return TRUE if the user has viewed all the quiz questions.
#' @export
quiz_complete <- function(mod) {
	all(!is.na(mod()$answer))
}

#' Returns TRUE if the user has viewed all the quiz questions.
#'
#' @param mod the ShinyQuiz module
#' @return TRUE if the user has viewed all the quiz questions.
#' @export
quiz_viewed <- function(mod) {
	all(mod()$viewed)
}
