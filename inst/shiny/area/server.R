shinyServer(function(input, output) {
	user_quiz <- quiz
	question_order <- sample(length(user_quiz$questions))
	user_quiz$questions <- user_quiz$questions[question_order]

	quiz_answers <- ShinyQuiz::quizServer('simple_quiz',
										  user_quiz,
										  title = 'Area of Shapes',
										  cancel_button = 'Cancel',
										  show_on_startup = TRUE)

	output$results_summary <- renderText({
		if(ShinyQuiz::quiz_complete(quiz_answers)) {
			answers <- get_table()
			msg <- paste0(
				'You were off by approximately ',
				mean(answers$difference),
				' percent. Click the explore tab to see how area changes for the various shapes.'
			)
		} else {
			msg <- 'Please complete the quiz.'
		}
		return(msg)
	})

	get_table <- reactive({
		tab <- quiz_answers()
		questions$answer <- NA
		questions$viewed <- NA
		questions[question_order,]$viewed <- tab$viewed
		questions[question_order,]$answer <- tab$answer
		if(!ShinyQuiz::quiz_complete(quiz_answers)) {
			questions$percent <- NA
		} else {
			questions$difference <- abs(questions$percent - as.numeric(questions$answer) / 100)
		}
		return(questions)
	})

    output$answers <- renderTable({
    	get_table() |>
    		dplyr::rename(your_answer = answer,
    					  correct_answer = percent,
    					  question_viewed = viewed)
    })

    output$shape_plot <- renderPlot({
    	req(input$area)
    	cowplot::plot_grid(
    		plot_inner_circle(input$area, y_inner = 'middle'),
    		plot_inner_circle(input$area, y_inner = 'bottom'),
    		plot_inner_square(input$area, inner = 'middle'),
    		plot_inner_square(input$area, inner = 'bottom'),
    		plot_bar(input$area),
    		nrow = 1
    	)
    }, width = "auto", height = 250)
})
