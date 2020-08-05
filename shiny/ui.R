library(shiny)


shinyUI(fluidPage(
  titlePanel("Obnovljivi viri po svetu"), 
  DT::dataTableOutput("TD_world_obnovljivi"))
)