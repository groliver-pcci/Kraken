library(shiny)
library(DT)
library(tidyverse)
library(scales)
library(formattable)
library(devtools)
load_all("C:\\Users\\wcbri\\Documents\\tbaR_1.0.1\\tbaR\\tbaR.Rproj")


statbotics <- "https://api.statbotics.io/v2/"

tbaKey <- "2022txirv"

week <- 1

year <- "2022"

numCols <- 28

dtSettings <- list(scrollX = TRUE, scrollY = TRUE, fixedColumns = list(leftColumns = c(2, 3)))

updateTeamValues <- function() {
  for(row in 1:length(vals$mainframe$teamNum)) {
    teamNumber <- vals$mainframe[row, 1]
    
    
  }
}

findTeamIndex <- function(teamNum) {
  for(row in 1:length(vals$infoframe$teamNum)) {
    if(toString(teamNum) == vals$infoframe$teamNum[row]) {
      return(row)
    }
  }
}


getStatboticsTeam <- function(teamNum) {
  link <- paste(statbotics, "team_year/", teamNum, "/", year, sep = "")
  
  team <- content(GET(link))
}

vals <- reactiveValues(
  mainframe = data.frame(),
  searchframe = data.frame(matrix(nrow = 0, ncol = numCols)),
  previewframe = data.frame(),
  
  calcframe = data.frame(epa = c()
  ),
  
  scheduleframe = data.frame(round = c(),
                             match_number = c(),  
                             red1 = c(),
                             red2 = c(),
                             red3 = c(),
                             blue1 = c(),
                             blue2 = c(),
                             blue3 = c()
  ),
  
  infoframe = data.frame(teamNum = c(),
                         epa = c(),
                         matchesPlayed = c()
  ),
  
  constantframe = data.frame(teamNum = c(),
                             k = c(),
                             m = c()
  )
)


ui <- navbarPage(
  
  title = div(icon("gitkraken", lib = "font-awesome", id="krakenicon"), "  Kraken"),
  
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
               h3("Clear Data", id="clearDataText"),
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
  
  tabPanel("Graph",
           DTOutput("tbaDT")),
  
  tabPanel("Match Planner"),
  
  tabPanel("TBA",
           actionButton("getSchedule", "Get Match Schedule")),
  
  tabPanel("Statbotics",
           actionButton("getStatbotics", "Get Statbotics Info"),
           DTOutput("statboticsData")),
  
  tabPanel("Schedule",
           DTOutput("matchScheduleDT")),
  
  selected = "Data",
  
  tags$script(src="https://kit.fontawesome.com/7f698a1940.js"),
  
  tags$head(tags$style(
    HTML('
         
         #confirmDelete, #deleteFiles {
            background-color: #d41704;
         }
         
         #krakenicon {
            color: #bb520a;
         }
         
         ')
  ))
)

server <- function(input, output) {
    
    f <- NA
    
    output$tbaDT <- renderDT(event_matches("2022txirv"))
    
    output$statboticsData <- renderDT(vals$infoframe)
    
    output$dataExport <- downloadHandler("scoutingdata.csv", 
                                         content = function(file) {
                                           write.csv(vals$mainframe, file)
                                         })
    
    output$matchScheduleDT <- renderDT(datatable(vals$scheduleframe, extensions = "FixedColumns", options = dtSettings))
    
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
    
    
    observeEvent(input$getStatbotics, {
      
      teamsInfo <- event_teams(tbaKey)
      
      teamNums <- c()
      
      for(team in 1:length(teamsInfo$key)) {
        teamNums <- append(teamNums, substr(teamsInfo$key[team], 4, 7))
      }
      
      for(team in  1:length(teamsInfo$key)) {
        t <- list(teamNum = NA,
                  epa = NA,
                  matchesPlayed = NA)
        
        teamNumber <- teamNums[team]
        
        teamInfo <- getStatboticsTeam(teamNumber)
        
        t$teamNum <- teamNumber
        
        if(!(is.null(teamInfo$epa_end))) {
          t$epa <- teamInfo$epa_end
        } else {
          t$epa <- "NA"
        }
        
        if(!(is.null(teamInfo$count))) {
          t$matchesPlayed <- teamInfo$count
        } else {
          if(week == 1) {
            t$matchesPlayed <- 0
          } else {
            t$matchesPlayed <- 5
          }
        }
        
        vals$infoframe <- rbind(vals$infoframe, t)
      }
      
      
      
    })
    
    
    observeEvent(input$getSchedule, {
      
      vals$scheduleframe = data.frame(round = c(),
                                      match_number = c(),  
                                      red1 = c(),
                                      red2 = c(),
                                      red3 = c(),
                                      blue1 = c(),
                                      blue2 = c(),
                                      blue3 = c()
      )
      
      tbaMatchListTemp <- event_matches("2022txirv")
    
      tbaMatchListTemp <- tbaMatchListTemp[order(tbaMatchListTemp$match_number), ]
      
      for(match in 1:nrow(tbaMatchListTemp)) {
        if(tbaMatchListTemp$comp_level[match] == "qm") {
          currentMatchTeams <- data.frame(round = tbaMatchListTemp$comp_level[match],
                                          match_number = tbaMatchListTemp$match_number[match],
                                          red1 = substr(tbaMatchListTemp$red1[match], 4, 7),
                                          red2 = substr(tbaMatchListTemp$red2[match], 4, 7),
                                          red3 = substr(tbaMatchListTemp$red3[match], 4, 7),
                                          blue1 = substr(tbaMatchListTemp$blue1[match], 4, 7),
                                          blue2 = substr(tbaMatchListTemp$blue2[match], 4, 7),
                                          blue3 = substr(tbaMatchListTemp$blue3[match], 4, 7)
                                          )
          
          vals$scheduleframe <- rbind(vals$scheduleframe, currentMatchTeams)
        }
      }
      
    })
    
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
          
          teamIndex <- findTeamIndex(vals$previewframe$teamNum[1])
          
          vals$infoframe$matchesPlayed[teamIndex] <- vals$infoframe$matchesPlayed[teamIndex] + 1
          
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
        
        if(repeatFound == FALSE) {
          vals$mainframe <- rbind(vals$mainframe, vals$previewframe[1, ])
          
          teamIndex <- findTeamIndex(vals$previewframe$teamNum[1])
          
          vals$infoframe$matchesPlayed[teamIndex] <- vals$infoframe$matchesPlayed[teamIndex] + 1
          
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
    
}

shinyApp(ui, server)