library(shiny)
library(rprojroot)

root <- rprojroot::is_rstudio_project

runApp(appDir = root$find_file("code"))
