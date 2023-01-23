library(shiny)
library(DT)
library(tidyverse)
library(scales)
library(formattable)


numCols <- 28

dtSettings <- list(scrollX = TRUE, scrollY = TRUE)

ui <- navbarPage(
  
  "Kraken",
  
  tags$head(tags$style(
    HTML('')
  )),
  
  tabPanel("Data",
           fluidPage(
             column(fluidRow(
             sidebarPanel(
                fileInput(
                  "file",
                  "Upload CSV",
                   multiple = FALSE
                ),
                fluidRow(
                h4("Apply Data?"),
                actionButton("yesData", "Yes", width = '60px'),
                actionButton("noData", "No", width = '60px')),
                width = 12
             )),
             
             fluidRow(
               sidebarPanel(
                 fileInput(
                   "dataImport",
                   "Import Data",
                   multiple = FALSE
                 ),
                 downloadButton(
                   "dataExport",
                   "Export Current Data"
                 ),
                 width = 12
               )
             ),
             fluidRow(
               h3("Clear Data"),
               sidebarPanel(
                 h5("WARNING: this button will delete all current data. Consider exporting the data first."),
                 actionButton("deleteFiles", "Delete Files"),
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
               width = 12
             )),
             width = 3),
             column(DTOutput("searchDT"),
                    width = 9),
           )), 
  
  tabPanel("Matches"),
  
  tabPanel("Qualitative"),
  
  tabPanel("Graph"),
  
  tabPanel("Match Planner"),
  
  tabPanel("TBA"),
  
  selected = "Data"
)

server <- function(input, output) {
    
    f <- NA
    
    output$dataExport <- downloadHandler("scoutingdata.csv", 
                                         content = function(file) {
                                           write.csv(vals$mainframe, file)
                                         })
    
    observeEvent(input$file, {
      d <- input$file
      
      if(is.null(d)) {
        return(NULL)
      }
      
      f <- read.csv(d$datapath, header = TRUE, sep = ",")
      
      vals$mainframe <- rbind(vals$mainframe, f)
      
      colnames(vals$searchframe) = colnames(vals$mainframe)
      
      output$preview <- renderDT(datatable(f, options = dtSettings))
      
      })
    
    observeEvent(input$dataImport, {
      importedFile <- input$dataImport
      
      if(is.null(importedData)) {
        return(NULL)
      }
      
      importedData <- read.csv(importedFile$datapath, header = TRUE, sep = ",")
      
      vals$mainframe <- importedData
      
    }
                 
    )
    
    observeEvent(input$deleteFiles, {
      
      showModal(modalDialog(
        tagList(actionButton("confirmDelete", "Yes")),
        title = "Are you sure you want to delete all data?"
        
      )
        
      )
      
    })
    
    observeEvent(input$search, {
      s <- input$search
      
      if(is.null(s) || is.null(vals$mainframe)) {
        return(NULL)
      }
      
      for(newrow in length(vals$mainframe$teamNum)) {
        if(toString(vals$mainframe$teamNum[newrow]) == s) {
          vals$searchframe <- rbind(vals$searchframe, vals$mainframe[newrow, ])
        } else {
          return(NULL)
        }
      }
      
      output$searchDT <- renderDT(datatable(vals$searchframe, options = dtSettings))
      
    }
      
    )
    
    vals <- reactiveValues(
      mainframe = data.frame(),
      searchframe = data.frame(matrix(nrow = 0, ncol = numCols))
    )
    
    
}

shinyApp(ui, server)