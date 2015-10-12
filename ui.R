
#
# UI to download sample datasets
# 


library(shiny)

shinyUI(fluidPage(
  titlePanel('Downloading Data'),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose a dataset:", 
                  choices = c("2020-Jan", "2021-Jan")),
      downloadButton('downloadData', 'Download')
    ),
    mainPanel(
      tableOutput('table')
    )
  )
))