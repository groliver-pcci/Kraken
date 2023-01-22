library(shiny)
library(DT)
library(tidyverse)
library(scales)
library(formattable)


dtSettings = list(scrollX = TRUE, scrollY = TRUE)

ui <- navbarPage(
  
  "Kraken",
  
  tabPanel("Input Files",
           fluidPage(
             column(fluidRow(
             sidebarPanel(
                fileInput(
                  "file",
                  "Upload CSV",
                   multiple = FALSE
                ),
                width = 12
             )),
             fluidRow(
               sidebarPanel(
                 titlePanel("Apply Data?"),
                 actionButton("yesData", "Yes", width = '60px'),
                 actionButton("noData", "No", width = '60px'),
                 width = 12
               )
             ),
             width = 3
             ),
             column(
             
               DTOutput("preview"),
           
             width = 9
             )
           )
  ),
  
  tabPanel("Teams",
           fluidPage(
             column(fluidRow(
               sidebarPanel(
                 textInput("search",
                           "Search:",
                           placeholder = "enter team number"),
                 
                 
                 width = 12
               )
             ),
             fluidRow(sidebarPanel(
               titlePanel("hello"),
               width = 12
             )),
             width = 3),
             column(mainPanel(DTOutput("searchDT")),
                    width = 9),
           )), 
  
  tabPanel("Matches"),
  
  tabPanel("Graph"),
  
  tabPanel("Match Planner"),
  
  tabPanel("Import Data"),
  
  tabPanel("Export Data"),
  
  selected = "Teams"
)

server <- function(input, output) {
    
    f <- NA
    
    observeEvent(input$file, {
      d <- input$file
      
      if(is.null(d)) {
        return(NULL)
      }
      
      f <- read.csv(d$datapath, header = TRUE, sep = ",")
      
      vals$mainframe <- rbind(vals$mainframe, f)
      
      output$preview <- renderDT(datatable(f, options = dtSettings))
      
      })
    
    observeEvent(input$search, {
      s <- input$search
      
      if(is.null(s) || is.null(vals$mainframe)) {
        return(NULL)
      }
      
      for(newrow in vals$mainframe$teamNum) {
        if(vals$mainframe$teamNum[row] == s) {
          rbind(vals$searchframe, split(vals$mainframe, vals$mainframe[newrow, ]))
        }
      }
      
      output$searchDT <- renderDT(datatable(vals$searchframe, options = dtSettings))
      
    }
      
    )
    
    vals <- reactiveValues(
      mainframe = data.frame(),
      searchframe = data.frame()
    )
    
    
}

shinyApp(ui, server)