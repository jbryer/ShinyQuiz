library(devtools)

document()
install()
build()
check()

# Add packages
usethis::use_package('cowplot', type = 'Imports')



################################################################################
# Hex logo
library(hexSticker)
library(tidyverse)

p <- 'man/figures/ShinyQuiz_source.png'

hexSticker::sticker(p,
					filename = 'man/figures/ShinyQuiz.png',
					p_size = 18,
					package = 'ShinyQuiz',
					url = "jbryer.github.io/ShinyQuiz/",
					u_size = 5,
					s_width = .5, s_height = .5,
					s_x = 1, s_y = 0.85,
					p_x = 1, p_y = 1.49,
					p_color = "#000000",
					h_fill = '#9BC9FF',
					h_color = '#1E81CE',
					white_around_sticker = FALSE)
