
#
# UI to download sample datasets
# 


library(shiny)

shinyUI(fluidPage(
    titlePanel('Compilation of KPI report'),
    sidebarLayout(
        # 1st Sidebar
        sidebarPanel(
            # adding the new div tag to the sidebar            
            tags$div(class="header", checked=NA,
                tags$strong("1. Download the sample data of A&E waiting time"), br(),br(),
                tags$a(href="./source/Jan20/kpi.1_AE_WT_2020-01.xlsx", "January 2020"), br(),
                tags$a(href="./source/Jan21/kpi.1_AE_WT_2021-01.xlsx", "January 2021"), br(), br(),
                # view sample data
                selectInput("dataset", "View the sample dataset:", 
                choices = c("2020-Jan", "2021-Jan")),
                
                # Upload data
                tags$strong("2. Upload the system generated data required for each KPI item"), br(),br(),
                tags$strong("kpi.1 A&E Waiting Time"), br(),
                
                
                tags$strong("kpi.2 SOP waiting time"), br()
                
            )
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Readme!", h4("How to use")),
                tabPanel("Sample Dataset", h4("Summary"), tableOutput('table'))
            )
        )
        
    )
))