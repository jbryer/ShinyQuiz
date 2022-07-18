shinyUI(navbarPage(
    title = "Area Perception",

    tabPanel(
        'Quiz',
        quizButton('simple_quiz', 'Show Quiz'),
        textOutput('results_summary'),
        tableOutput("answers")
    ),
    tabPanel(
        'Explore',
        sidebarLayout(
            sidebarPanel(
                sliderInput("area",
                            "Select area of inner shape:",
                            min = 0,
                            max = 1,
                            value = 0.5,
                            step = 0.05)
            ),

            # Show a plot of the generated distribution
            mainPanel(
                plotOutput('shape_plot')
            )
        )
    )
))
