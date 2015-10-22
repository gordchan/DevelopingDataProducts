
#
# Server side script for downloading sample datasets
#

# Load libraries ----

library(shiny)
library(xlsx)
library(dplyr)
library(reshape2)
library(ggplot2)

# Initialise ----

# Period value

Y = 2021
M = 01

# Filepath

kpi.template <- file.path("template", "A Clinical KPI.xls")

kpi.report <- file.path("report", "A Clinical KPI", Y, "-", M , ".xls")

# Empty dataframe
empty.frame <- data.frame(A1 = numeric(0),
                           A2 = numeric(0),
                           A3 = numeric(0),
                           A4 = numeric(0),
                           A5 = numeric(0),
                           A6 = numeric(0),
                           A7 = numeric(0),
                           A8 = numeric(0),
                           A = numeric(0),
                           HQ = numeric(0))

# Copy template for use as blank report

file.copy(from = kpi.template,
          to = kpi.report,
          overwrite = TRUE,
          copy.mode = TRUE, copy.date = FALSE)

# Function to read xlsx file
read_range <- function (input, ri, ci, si = 1){
    
    require("xlsx")
    
    all_range <- read.xlsx(file=input, sheetIndex=si, rowIndex=ri, colIndex=ci,
                           as.data.frame=TRUE, header=FALSE, colClasses=NA,
                           keepFormulas=FALSE, stringsAsFactors = FALSE)
    
    for (i in 1:length(all_range)){
        if (!is.na(all_range[2,i])){
            
            all_range[1,i] <- all_range[2,i]
            
        }
    }
    if (sum(grepl("Overall", all_range[1,]))==2){
        all_range[1,] <- gsub("(^Overall \\(|\\))", "", all_range[1,])
        all_range[1,] <- gsub("Overall$", "HQ", all_range[1,]) ## Will be triggered if KWC is repeated in same axis
    } else {
        all_range[1,] <- gsub("Overall$", "A", all_range[1,])
    }
    
    
    var_col_i <- which(is.na(all_range[1,]))
    
    for (i in 1:length(var_col_i)){
        all_range[1,i] <- paste("var",i, sep = "")
    }
    
    names(all_range) <- all_range[1,]
    all_range <- all_range[-c(1,2),]
    
    all_range$var1 <- gsub("(^ *)", "", all_range$var1)
    # rownames(all_range) <- all_range$var1 ## Will casue error if row names are redundant
    
    all_range
    
}

# Function to process kpi
kpi.1 <- function(Mmm){
    
    path <- grep("(.*kpi.1 .*)", source.kpi$filepath, value = TRUE)
    
    AE_WT <- read_range(path, 5:12, 1:14)
    
    AE_WT.frame <- empty.frame
    
    row.index <- c(2:5)
    
    for (i in 1:length(row.index)){
        AE_WT.frame[i,] <- rep(NA, length(AE_WT.frame))
    }
    
    # Fit processed data to dataframe and show NA data
    
    AE_WT <- AE_WT[row.index,]
    
    
    for (i in 1:length(AE_WT.frame)){
        if (names(AE_WT.frame)[i] %in% names(AE_WT)){
            AE_WT.frame[i] <- AE_WT[names(AE_WT.frame)[i]]
        }
    }
    
    
    # Replace NA with N.A. for production use in Excel
    
    AE_WT.prod <- as.matrix(AE_WT.frame, rownames.force = TRUE)
    AE_WT.prod <- apply(AE_WT.prod, 2, FUN = function(x){ifelse(is.na(x), "N.A.", x)})
    
    AE_WT.prod <- data.frame(
        lapply(split(AE_WT.prod, col(AE_WT.prod)), type.convert, as.is = TRUE),
        stringsAsFactors = FALSE
    )
    row.names(AE_WT.prod) <- row.names(AE_WT.frame)
    names(AE_WT.prod) <- names(AE_WT.frame)
    
    for (i in 1:length(AE_WT.prod)){
        AE_WT.prod[,i] <- sapply(AE_WT.prod[,i], FUN = function(x) ifelse(is.numeric(x), x/100, x))
    }
    
    # Return production ready dataframe
    
    AE_WT.prod
}

# Sample data

Y = 2021
M = 01

a = c(101:110)
b = sample(11:20, 10, replace = TRUE)
c = sample(101:200, 10, replace = TRUE)

df1 <- data.frame("Serial" = a, "Cats" = b)
df2 <- data.frame("Serial" = a, "Cats" = c)

sampleFile20 <- file.path("kpi.1_AE_WT_2020-01.xlsx")
sampleFile21 <- file.path("kpi.1_AE_WT_2021-01.xlsx")

df20 <- read_range(sampleFile20, 5:12, 1:14)
    df20 <- df20 %>% filter(var1 != "A&E")

df21 <- read_range(sampleFile21, 5:12, 1:14)
    df21 <- df21 %>% filter(var1 != "A&E")

# Shiny server side code ----

shinyServer(function(input, output) {
    
    datasetInput <- reactive({
        switch(input$dataset,
               "2020-Jan" = df20,
               "2021-Jan" = df21)
    })
    
    # Read & process uploaded xlsx files
    output$tyTable <- renderTable({
        
        kpi.1.tyFile <- input$kpi.1_ty
        
        if (is.null(kpi.1.tyFile))
            return(NULL)
        
        read_range(kpi.1.tyFile$datapath, 5:12, 1:14)
    })
    output$lyTable <- renderTable({
        
        kpi.1.lyFile <- input$kpi.1_ly
        
        if (is.null(kpi.1.lyFile))
            return(NULL)
        
        read_range(kpi.1.lyFile$datapath, 5:12, 1:14)
    }) 
    
    
    output$table <- renderTable({
        datasetInput()
    })
    
    output$dsname <- renderText({
        input$dataset
    })
    
    selectedData <- reactive({
        datasetInput() %>% filter(var1 == input$triage) %>% melt(id=("var1")) %>% select(institution = variable, percentage = value)
    })
    
    output$selectedData <- renderTable({
        datasetInput() %>% filter(var1 == input$triage) %>% melt(id=("var1")) %>% select(institution = variable, percentage = value)
    })
    
    output$samplePlot <- renderPlot({
        
        df <- selectedData()
        
        if(!is.null(df)){
        ggplot(data = df, aes(x = institution, y = percentage, fill = institution))+
                geom_bar(stat="identity")+
                labs(title="Percentage of A&E cases seen within target time")
        }

    })
    
    output$kpi.1Plot <- renderPlot({
        ggplot()+
            geom_line(data = df1, aes(Serial, Cats), colour = "red")+
            geom_line(data = df2, aes(Serial, Cats), colour = "blue")+
            theme(legend.position="right")+
            scale_fill_discrete(name = "Period", labels = c("This Year", "Last Year"))
    })

    output$downloadData <- downloadHandler(
        filename = function() { 
            paste("KPI Report", Y, "-", M, ".xlsx", sep="") 
        },
        content = function(file) {
            write.xlsx(datasetInput(), file, sheetName = "KPI")
        }
    )
    
})

