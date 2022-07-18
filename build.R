library(devtools)

document()
install()
build()
check()

# Add packages
usethis::use_package('cowplot', type = 'Imports')
