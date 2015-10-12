
#
# Server side script for downloading sample datasets
#

library(shiny)
library(xlsx)

shinyServer(function(input, output) {
    datasetInput <- reactive({
        switch(input$dataset,
               "2020-Jan" = rock,
               "2021-Jan" = pressure)
    })
    
    output$table <- renderTable({
        datasetInput()
    })
    
    output$downloadData <- downloadHandler(
        filename = function() { 
            paste(input$dataset, '.csv', sep='') 
        },
        content = function(file) {
            write.csv(datasetInput(), file)
        }
    )
})

