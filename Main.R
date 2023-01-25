library(shiny)
library(DT)
library(tidyverse)
library(scales)
library(formattable)


numCols <- 28

dtSettings <- list(scrollX = TRUE, scrollY = TRUE, fixedColumns = list(leftColumns = c(2, 3)))

ui <- navbarPage(
  
  title = div(icon("gitkraken", lib = "font-awesome"), "  Kraken"),
  
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
  
  tabPanel("Competition",
           DTOutput("mainframeOutput")),
  
  tabPanel("Qualitative"),
  
  tabPanel("Graph"),
  
  tabPanel("Match Planner"),
  
  tabPanel("TBA"),
  
  selected = "Data",
  
  tags$script(src="https://kit.fontawesome.com/7f698a1940.js"),
  
  tags$head(tags$style(
    HTML('
         
         #confirmDelete, #deleteFiles {
            background-color: #d41704;
            color: white;
         }
         
         ')
  ))
)

server <- function(input, output) {
    
    f <- NA
    
    output$dataExport <- downloadHandler("scoutingdata.csv", 
                                         content = function(file) {
                                           write.csv(vals$mainframe, file)
                                         })
    
    deleteModal <- function() {
      modalDialog(
        tagList(actionButton("confirmDelete", "Yes")),
        title = "Are you sure you want to delete all data?"
        
      )
    }
    
    repeatModal <- function() {
      modalDialog(
        tagList(
                h4("This looks like repeat data. Are you sure you want to add another entry to the system?"),
                actionButton("confirmApplyRepeat", "Yes")
                ),
        title = "Repeat Data?"
      )
    }
    
    output$mainframeOutput <- renderDT(datatable(vals$mainframe, extensions = "FixedColumns", options = dtSettings))
    
    observeEvent(input$file, {
      d <- input$file
      
      if(is.null(d)) {
        return(NULL)
      }
      
      f <- read.csv(d$datapath, header = TRUE, sep = ",")
      
      vals$previewframe <- f
      
      colnames(vals$searchframe) = colnames(vals$previewframe)
      
      output$preview <- renderDT(datatable(f, extensions = "FixedColumns", options = dtSettings))
      
      
      
      })
    
    observeEvent(input$yesData, {
      if(nrow(vals$previewframe) == 1) {
        
        if(nrow(vals$mainframe) == 0) {
          vals$mainframe <- rbind(vals$mainframe, vals$previewframe[1, ])
          vals$previewframe <- data.frame()
          return(NULL)
        } else {
        
        repeatFound <- FALSE
          
        for(row in 1:length(vals$mainframe$teamNum)) {
          if(vals$mainframe[row, 1] == vals$previewframe[1, 1] & vals$mainframe[row, 2] == vals$previewframe[1, 2]) {
            repeatFound <- TRUE
            showModal(repeatModal())
          }
        }
        
        print(repeatFound)
        
        if(repeatFound == FALSE) {
          vals$mainframe <- rbind(vals$mainframe, vals$previewframe[1, ])
          vals$previewframe <- data.frame()
        }
          
        }
        
      } else if(nrow(vals$previewframe) > 1) {
        print("error")
        return(NULL)
      } else {
        return(NULL)
      }
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
      
      showModal(deleteModal())
      })
    
    observeEvent(input$confirmDelete, {
      vals$mainframe <- data.frame()
      removeModal()
    })
    
    observeEvent(input$search, {
      s <- input$search
      
      if(is.null(s) || is.null(vals$mainframe)) {
        return(NULL)
      }
      
      for(newrow in 1:length(vals$mainframe$teamNum)) {
        if(toString(vals$mainframe$teamNum[newrow]) == s) {
          vals$searchframe <- rbind(vals$searchframe, vals$mainframe[newrow, ])
        } else {
          return(NULL)
        }
      }
      
      output$searchDT <- renderDT(datatable(vals$searchframe, extensions = "FixedColumns", options = dtSettings))
      
    }
      
    )
    
    vals <- reactiveValues(
      mainframe = data.frame(),
      searchframe = data.frame(matrix(nrow = 0, ncol = numCols)),
      previewframe = data.frame()
    )
    
    
}

shinyApp(ui, server)