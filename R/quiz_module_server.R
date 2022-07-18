#' Shiny Quiz Server Module
#'
#' ```
#' quiz <- list(
#'     directions = '',
#'     questions = list(
#'         list(
#'             stem = 'Question 1',
#'             type = 'multiple_choice',
#'             choices = c(''),
#'             required = FALSE,
#'             multiple = FALSE
#'         ),
#'         list(
#'             stem = 'Question 2',
#'             type = 'text',
#'             required = FALSE
#'         )
#'     )
#' )
#' ```
#'
#' Types include:
#' * `multiple_choice` - Multiple choice questions using  `selectInput`.
#' * `text` - Text input using `textAreaInput`.
#' * `numeric` - Numeric input using `numericInput`.
#' * `slider` - Numeric input using `sliderInput`.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param quiz a list with the quiz details.
#' @param title the title of the modal dialog.
#' @param show_on_startup if TRUE the quiz will be displayed when the Shiny app starts.
#' @param size One of "s" for small, "m" (the default) for medium, or "l" for large.
#' @param fade If FALSE, the modal dialog will have no fade-in animation
#'        (it will simply appear rather than fade in to view).
#' @param required_message the message displayed to the user when they don't anser
#'        a required message.
#' @param progress_bar whether to display a progress bar.
#' @import shiny
#' @export
quizServer <- function(id,
					   quiz,
					   title = 'Shiny Quiz',
					   show_on_startup = TRUE,
					   size = 'xl',
					   fade = TRUE,
					   next_button = "Next",
					   previous_button = "Previous",
					   finish_button = "Finish",
					   cancel_button = NULL,
					   progress_bar = TRUE,
					   required_message = 'You need to answer this question before moving on.') {
	shiny::moduleServer(
		id,
		function(input, output, session) {
			modal_page <- shiny::reactiveVal(0)
			message <- shiny::reactiveVal('')

			answers <- data.frame(
				stem = sapply(quiz$questions, FUN = function(x) { x$stem }),
				answer = rep(NA, length(quiz$questions)),
				viewed = rep(FALSE, length(quiz$questions)),
				stringsAsFactors = FALSE
			)

			shiny::makeReactiveBinding("answers")
			getAnswers <- shiny::reactive({
				return(answers)
			})

			modalUI <- function() {
				ns <- session$ns
				ui <- list()

				title <- title
				ui[[length(ui) + 1]] <- strong(message())

				if(modal_page() == 0) {
					ui[[length(ui) + 1]] <- quiz$directions
				} else if(modal_page() > length(quiz$questions) ) {
					# Do nothing for now. May want to have a quiz complete screen
				} else  {
					answers[modal_page(),]$viewed <<- TRUE

					qtype <- 'multiple_choice'
					if(!is.null(quiz$questions[[modal_page()]]$type)) {
						qtype <- quiz$questions[[modal_page()]]$type
					}
					multiple <- FALSE
					if(!is.null(quiz$questions[[modal_page()]]$multiple)) {
						multiple <- quiz$questions[[modal_page()]]$multiple
					}

					title <- quiz$questions[[modal_page()]]$stem

					if(qtype == 'multiple_choice') {
						choices <- quiz$questions[[modal_page()]]$choices
						if(multiple) {
							selected <- character()
							if(!is.na(answers[modal_page(),]$answer)) {
								selected <- unlist(
									strsplit(answers[modal_page(),]$answer, ';'))
							}
							ui[[length(ui) + 1]] <- shiny::checkboxGroupInput(
								inputId = ns(paste0('question_', modal_page())),
								# label = quiz$questions[[modal_page()]]$stem,
								label = '',
								choices = choices,
								selected = selected
							)
						} else {
							selected <- character()
							if(!is.na(answers[modal_page(),]$answer)) {
								selected <- answers[modal_page(),]$answer
							}
							ui[[length(ui) + 1]] <- shiny::radioButtons(
								inputId = ns(paste0('question_', modal_page())),
								# label = quiz$questions[[modal_page()]]$stem,
								label = '',
								choices = choices,
								selected = selected
							)
						}
					} else if(qtype == 'text') {
						value <- ''
						if(!is.na(answers[modal_page(),]$answer)) {
							value <- answers[modal_page(),]$answer
						}
						ui[[length(ui) + 1]] <- shiny::textAreaInput(
							inputId = ns(paste0('question_', modal_page())),
							# label = quiz$questions[[modal_page()]]$stem,
							label = '',
							width = '100%',
							height = '100%'
						)
					} else if(qtype == 'numeric') {
						value <- 0
						if(!is.na(answers[modal_page(),]$answer)) {
							value <- answers[modal_page(),]$answer
						}
						ui[[length(ui) + 1]] <- shiny::numericInput(
							inputId = ns(paste0('question_', modal_page())),
							label = '',
							width = '100%',
							value = value,
							min = ifelse(!is.null(quiz$questions[[modal_page()]]$min),
										 quiz$questions[[modal_page()]]$min,
										 NA),
							max = ifelse(!is.null(quiz$questions[[modal_page()]]$max),
										 quiz$questions[[modal_page()]]$max,
										 NA)
						)
					} else if(qtype == 'slider') {
						value <- 0
						if(!is.na(answers[modal_page(),]$answer)) {
							value <- answers[modal_page(),]$answer
						} else if(!is.null(quiz$questions[[modal_page()]]$value)) {
							value <- quiz$questions[[modal_page()]]$value
						}
						ui[[length(ui) + 1]] <- shiny::sliderInput(
							inputId = ns(paste0('question_', modal_page())),
							label = '',
							width = '100%',
							min = quiz$questions[[modal_page()]]$min,
							max = quiz$questions[[modal_page()]]$max,
							value = value
						)
					} else {
						ui[[length(ui) + 1]] <- p('Unknown question type.')
					}

				}

				ui$title <- title

				if(progress_bar) {
					ui[[length(ui) + 1]] <- hr()
					ui[[length(ui) + 1]] <- shinyWidgets::progressBar(
						id = ns('progress_bar'),
						value = modal_page(),
						total = length(quiz$questions),
						display_pct = TRUE)
				}

				footer <- list()
				if(!is.null(cancel_button)) {
					footer[[length(footer) + 1]] <- shiny::modalButton(cancel_button)
				}
				if(modal_page() > 0 & !is.null(previous_button)) {
					footer[[length(footer) + 1]] <- shiny::actionButton(ns('previous_page'), previous_button)
				}
				if(modal_page() < length(quiz$questions) & !is.null(next_button)) {
					footer[[length(footer) + 1]] <- shiny::actionButton(ns('next_page'), next_button)
				}
				if(modal_page() == length(quiz$questions)) {
					footer[[length(footer) + 1]] <- shiny::actionButton(ns('next_page'), finish_button)
				}

				footer$width <- 12
				ui$footer <- do.call(shiny::mainPanel, footer)
				ui$size <- size
				ui$fade <- TRUE

				do.call(shiny::modalDialog, ui)
			}

			# Next page button
			observeEvent(input$next_page, {
				if(modal_page() > 0) {
					required <- FALSE
					if(!is.null(quiz$questions[[modal_page()]]$required)) {
						required <- quiz$questions[[modal_page()]]$required
					}
					ans <- paste0(
						input[[paste0('question_', modal_page())]],
						collapse = ';'
					)
					if(required & (is.null(ans) | ans == '')) {
						message(required_message)
						shiny::showModal(modalUI())
						return()
					} else {
						message('')
					}
					answers[modal_page(),]$answer <<- ans
				}
				modal_page(modal_page() + 1)
				if(modal_page() > length(quiz$questions)) {
					modal_page(0)
					removeModal()
				} else {
					showModal(modalUI())
				}
			})

			# Previous page button
			observeEvent(input$previous_page, {
				if(modal_page() > 0) {
					answers[modal_page(),]$answer <<- paste0(
						input[[paste0('question_', modal_page())]],
						collapse = '; '
					)
				}
				if(modal_page() > 0) {
					modal_page(modal_page() - 1)
				}
				shiny::showModal(modalUI())
			})

			# open modal on button click
			shiny::observeEvent(input$startQuiz,
								ignoreNULL = !show_on_startup, # Show modal on start up
								shiny::showModal(modalUI())
			)

			return(getAnswers)
		}
	)
}
