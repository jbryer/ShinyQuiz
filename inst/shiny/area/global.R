# Adapted from: https://observablehq.com/@mizinov/area-comparison
library(ShinyQuiz)
library(shiny)
library(ggplot2)
library(ggforce)
library(cowplot)

source('plot_bar.R')
source('plot_inner_circle.R')
source('plot_inner_square.R')

# library(base64enc)


percents <- c(0.25, 0.5, 0.75)
# functions <- c(plot_inner_circle, plot_inner_circle,
# 			   plot_inner_square, plot_inner_square,
# 			   plot_bar)
shape <- c('Circle', 'Circle', 'Square', 'Square', 'Bar')
position <- c('center', 'bottom', 'center', 'bottom', NA)
questions <- data.frame(
	shape = rep(shape, length(percents)),
	position = rep(position, length(percents)),
	percent = rep(percents, each = length(position))
)

quiz <- list(
	directions = paste0(
		'You will be presented with a series of shapes nested in one another. ',
		'For each shape, what proportion of the outer shape is covered by the inner shape?'
	),
	questions = list()
)

for(i in 1:nrow(questions)) {
	p <- NULL
	if(questions[i,]$shape == 'Circle') {
		p <- plot_inner_circle(questions[i,]$percent,
							   y_inner = questions[i,]$position)
	} else if(questions[i,]$shape == 'Square') {
		p <- plot_inner_square(questions[i,]$percent,
							   inner = questions[i,]$position)
	} else if(questions[i,]$shape == 'Bar') {
		p <- plot_bar(questions[i,]$percent)
	}
	fn <- tempfile(fileext='.png')
	png(fn)
	print(p)
	dev.off()
	img <- paste0('data:image/png;base64,', base64enc::base64encode(fn))
	quiz$questions[[i]] <- list(
		stem = HTML(paste0("<center><img src='", img, "' width='400' /></center><br/>",
						   "What proportion of the outer ",
						   questions[i,]$shape,
						   " is covered by the inner ",
						   questions[i,]$shape, "?")),
		type = 'slider',
		min = 0, max = 100, step = 1
	)

}

