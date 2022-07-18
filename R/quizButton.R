#' Button to begin a Shiny Quiz.
#'
#' @param id The id string to be namespaced .
#' @param label Label for the button.
#' @importFrom shiny actionButton NS
#' @export
quizButton <- function(id, label = 'Start Quiz') {
	ns <- shiny::NS(id)
	shiny::actionButton(ns("startQuiz"), label)
}

