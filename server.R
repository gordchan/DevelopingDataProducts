
#
# Server side script for downloading sample datasets
#

library(shiny)
library(xlsx)

#test pre-computation

a = c(101:110)
b = sample(11:20, 10, replace = TRUE)
c = sample(101:200, 10, replace = TRUE)

df1 <- data.frame("Serial" = a, "Banana" = b)
df2 <- data.frame("Serial" = a, "Cats" = c)

shinyServer(function(input, output) {
    datasetInput <- reactive({
        switch(input$dataset,
               "2020-Jan" = df1,
               "2021-Jan" = df2)
    })
    
    output$table <- renderTable({
        datasetInput()
    })
    

})

