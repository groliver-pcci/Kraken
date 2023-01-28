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

vals <- reactiveValues(
  mainframe = data.frame(),
  searchframe = data.frame(matrix(nrow = 0, ncol = numCols)),
  previewframe = data.frame(),
  sspreviewframe = data.frame(),
  
  calcframe = data.frame(epa = c()
  ),
  
  scheduleframe = data.frame(round = c(),
                             match_number = c(),  
                             red1 = c(),
                             red2 = c(),
                             red3 = c(),
                             blue1 = c(),
                             blue2 = c(),
                             blue3 = c(),
                             winChances = c(),
                             predictedWinners = c()
  ),
  
  infoframe = data.frame(teamNum = c(),
                         epa = c(),
                         matchesPlayed = c()
  ),
  
  constantframe = data.frame(teamNum = c(),
                             k = c(),
                             m = c()
  ),
  
  matches6672 = c(),
  
  winnersCalculated = FALSE
  
  
)

wintable = reactive({
  datatable(vals$scheduleframe)
})

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

calculateWinChance <- function(matchNum, fromAlliance = "default") {
  
  red1 <- vals$scheduleframe$red1[matchNum]
  red2 <- vals$scheduleframe$red2[matchNum]
  red3 <- vals$scheduleframe$red3[matchNum]
  
  blue1 <- vals$scheduleframe$blue1[matchNum]
  blue2 <- vals$scheduleframe$blue2[matchNum]
  blue3 <- vals$scheduleframe$blue3[matchNum]
  
  red1EPA <- vals$infoframe$epa[findTeamIndex(red1)]
  red2EPA <- vals$infoframe$epa[findTeamIndex(red2)]
  red3EPA <- vals$infoframe$epa[findTeamIndex(red3)]
  
  blue1EPA <- vals$infoframe$epa[findTeamIndex(blue1)]
  blue2EPA <- vals$infoframe$epa[findTeamIndex(blue2)]
  blue3EPA <- vals$infoframe$epa[findTeamIndex(blue3)]
  
  epaDiff <- (red1EPA + red2EPA + red3EPA) - (blue1EPA + blue2EPA + blue3EPA)
  
  winChance <- 1/(1 + 10 ^ (epaDiff/400))
  
  formattedWinChance <- format(winChance * 100, digits = 3)
  
  return(formattedWinChance)
}



updateOurMatches <- function() {
  for(match in 1:nrow(vals$scheduleframe)) {
    if(vals$scheduleframe$red1[match] == "6672" ||
       vals$scheduleframe$red2[match] == "6672" ||
       vals$scheduleframe$red3[match] == "6672" ||
       vals$scheduleframe$blue1[match] == "6672" ||
       vals$scheduleframe$blue2[match] == "6672" ||
       vals$scheduleframe$blue3[match] == "6672"
       ) {
      vals$matches6672 <- append(vals$matches6672, vals$scheduleframe$match_number[match])
    }
  }
}


getStatboticsTeam <- function(teamNum) {
  link <- paste(statbotics, "team_year/", teamNum, "/", year, sep = "")
  
  team <- content(GET(link))
}

ui <- navbarPage(
  
  title = div(icon("gitkraken", lib = "font-awesome", id="krakenicon"), "  Kraken"),
  
  tabPanel("Data",
           fluidPage(
             column(fluidRow(
             sidebarPanel(
                radioButtons(
                  "inputType", "What type of data?",
                  c("Regular Scout" = "regScout",
                              "Superscout" = "supScout")
                ),
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
               sidebarPanel(
                 h5("WARNING: this button will delete all current data. Consider exporting the data first."),
                 actionButton("deleteFiles", "Delete Files"),
                 width = 12
               )
             ),
             
             width = 3
             ),
             column(
               conditionalPanel(
                 condition = "input.inputType == 'regScout'",
                 DTOutput("preview")
               ),
               conditionalPanel(
                 condition = "input.inputType == 'supScout'",
                 DTOutput("ssPreview")
               ),
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
  
  tabPanel("Match Planner",
           fluidPage(
             column(
               fluidRow(
                 sidebarPanel(
                   selectInput(
                     "selectedMatch",
                     "Select a Match",
                     choices = ""
                   ),
                   width = 12
                 )
               ),
               fluidRow(
                 sidebarPanel(
                  textOutput("winChance"),
                  width = 12 
                 )
               ),
               width = 3
             )
           )),
  
  tabPanel("TBA",
           actionButton("getSchedule", "Get Match Schedule"),
           actionButton("getWinChances", "Get Win Percents (load statbotics first)")),
  
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

server <- function(input, output, session) {
    
    f <- NA
    
    output$tbaDT <- renderDT(event_matches(tbaKey))
    
    output$statboticsData <- renderDT(vals$infoframe)
    
    output$preview <- renderDT(datatable(vals$previewframe, extensions = "FixedColumns", options = dtSettings))
    
    output$ssPreview <- renderDT(datatable(vals$sspreviewframe, extensions = "FixedColumns", options = dtSettings))
    
    output$dataExport <- downloadHandler("scoutingdata.csv", 
                                         content = function(file) {
                                           write.csv(vals$mainframe, file)
                                         })
    
    
    observe({
    
    if(vals$winnersCalculated == FALSE) {
      output$matchScheduleDT <- renderDT(vals$scheduleframe)
    } else if (vals$winnersCalculated == TRUE) {
      output$matchScheduleDT <- renderDT(wintable)
    }
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
    
    observe({
      updateSelectInput(session, "selectedMatch",
                        choices = vals$matches6672)
    })
    
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
      
      updateOurMatches()
      
    })
    
    observeEvent(input$getWinChances, {
      
      wintable <- datatable(vals$scheduleframe)
      
      winChances <- c()
      predictedWinners <- c()
      
      for(matchNum in 1:nrow(vals$scheduleframe)) {
        
        winChance <- as.numeric(calculateWinChance(matchNum))
        
        predictedWinner <- NA
        
        if(winChance > 50) {
          print("greater")
          predictedWinner <- "r"
        } else if(winChance < 50) {
          print("lesser")
          predictedWinner <- "b"
          winChance <- 100 - winChance
        } else {
          predictedWinner <- "even"
        }
        
        winChances <- append(winChances, winChance)
        predictedWinners <- append(predictedWinners, predictedWinner)
      }
      
      vals$scheduleframe["winChances"] <- winChances
      vals$scheduleframe["predictedWinners"] <- predictedWinners
      
      wintable %>% formatStyle(
        9, 10,
        background_color = styleEqual(c("r", "b", "even"), c("red", "blue", "gray"))
      )
      
      
      
    })
    
    observeEvent(input$file, {
      
      d <- input$file
      
      if(is.null(d)) {
        return(NULL)
      }
      
      f <- read.csv(d$datapath, header = TRUE, sep = ",")
      
      if(input$inputType == "regScout") {
    
        vals$previewframe <- f
        
        colnames(vals$searchframe) = colnames(vals$previewframe)
        
      } else if (input$inputType == "supScout") {
        
        vals$sspreviewframe <- f
        
      }
      
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