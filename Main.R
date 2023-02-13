library(shiny)
library(DT)
library(tidyverse)
library(scales)
library(formattable)
library(utils)
library(devtools)
library(shinyFiles)
load_all("C:\\Users\\wcbri\\Documents\\tbaR_1.0.1\\tbaR\\tbaR.Rproj")

#link to pull from statbotics
statbotics <- "https://api.statbotics.io/v2/"

tbaKey <- "2022txirv"

week <- 1

path <- "C:\\Users\\wcbri\\Documents\\krakendata\\"

year <- "2022"

numCols <- 28



setwd(path)

# The object that stores all of the values for the app
vals <- reactiveValues(
  mainframe = data.frame(),
  searchframe = data.frame(teamNum = c(),
                           matchNum = c(),
                           alliance = c(),
                           startLocation = c(),
                           preload = c(),
                           mobility = c(),
                           autoPickups = c(),
                           autoCones = c(),
                           autoCubes = c(),
                           autoBalance = c(),
                           communityPickups = c(),
                           neutralPickups = c(), 
                           singlePickups = c(),
                           doublePickups = c(),
                           teleopCones = c(),
                           teleopCubes = c(),
                           shuttle = c(),
                           teleopBalance = c(),
                           buddyClimb = c(),
                           balanceTime = c(),
                           everybot = c(),
                           drivetrainType = c(),
                           drivetrain = c(),
                           intake = c(),
                           speed = c(),
                           driver = c(),
                           scoutName = c(),
                           comments = c()
                           ),
  
  matchsearchframe = data.frame(teamNum = c(),
                           matchNum = c(),
                           alliance = c(),
                           startLocation = c(),
                           preload = c(),
                           mobility = c(),
                           autoPickups = c(),
                           autoCones = c(),
                           autoCubes = c(),
                           autoBalance = c(),
                           communityPickups = c(),
                           neutralPickups = c(), 
                           singlePickups = c(),
                           doublePickups = c(),
                           teleopCones = c(),
                           teleopCubes = c(),
                           shuttle = c(),
                           teleopBalance = c(),
                           buddyClimb = c(),
                           balanceTime = c(),
                           everybot = c(),
                           drivetrainType = c(),
                           drivetrain = c(),
                           intake = c(),
                           speed = c(),
                           driver = c(),
                           scoutName = c(),
                           comments = c()
  ),
  
  
  previewframe = data.frame(),
  sspreviewframe = data.frame(),
  
  teamframe = data.frame(teamNum = c(),
                         matchesPlayed = c(),
                         EPA = c(),
                         ECT = c(),
                         aPPG = c(),
                         SEf = c(),
                         sFlex = c(),
                         aSt = c(),
                         aSa = c(),
                         aS = c(),
                         ABT = c(),
                         BC = c()
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
  
  plannerframe = data.frame(),
  
  constantframe = data.frame(teamNum = c(),
                             k = c(),
                             m = c()
  ),
  
  matches6672 = data.frame(matches = c(),
                           allliance = c(),
                           station = c()
                           ),
  
  autonScoring = data.frame(r1 = c("O", "O", "X"),
                            r2 = c("X", "X", "X"),
                            r3 = c("O", "O", "X"),
                            r4 = c("O", "O", "X"),
                            r5 = c("X", "X", "X"),
                            r6 = c("O", "O", "X"),
                            r7 = c("O", "O", "X"),
                            r8 = c("X", "X", "X"),
                            r9 = c("O", "O", "X")
  ),
  
  teleopScoring = data.frame(r1 = c("O", "O", "X"),
                             r2 = c("X", "X", "X"),
                             r3 = c("O", "O", "X"),
                             r4 = c("O", "O", "X"),
                             r5 = c("X", "X", "X"),
                             r6 = c("O", "O", "X"),
                             r7 = c("O", "O", "X"),
                             r8 = c("X", "X", "X"),
                             r9 = c("O", "O", "X")
  ),
  
  matchautonScoring = data.frame(r1 = c("O", "O", "X"),
                            r2 = c("X", "X", "X"),
                            r3 = c("O", "O", "X"),
                            r4 = c("O", "O", "X"),
                            r5 = c("X", "X", "X"),
                            r6 = c("O", "O", "X"),
                            r7 = c("O", "O", "X"),
                            r8 = c("X", "X", "X"),
                            r9 = c("O", "O", "X")
  ),
  
  matchteleopScoring = data.frame(r1 = c("O", "O", "X"),
                             r2 = c("X", "X", "X"),
                             r3 = c("O", "O", "X"),
                             r4 = c("O", "O", "X"),
                             r5 = c("X", "X", "X"),
                             r6 = c("O", "O", "X"),
                             r7 = c("O", "O", "X"),
                             r8 = c("X", "X", "X"),
                             r9 = c("O", "O", "X")
  ),
  
  winnersCalculated = FALSE,
  
  startupDone = FALSE
)



# Functions to update values

updateCalcValues <- function() {
  teamNum <- vals$previewframe$teamNum[1]
  teamIdx <- which(vals$teamframe$teamNum == teamNum)
  
  
  info <- data.frame(
    teamNum = c(),
    matchNum = c(),
    alliance = c(),
    startLocation = c(),
    preload = c(),
    mobility = c(),
    autoPickups = c(),
    autoCones = c(),
    autoCubes = c(),
    autoBalance = c(),
    communityPickups = c(),
    neutralPickups = c(), 
    singlePickups = c(),
    doublePickups = c(),
    teleopCones = c(),
    teleopCubes = c(),
    shuttle = c(),
    teleopBalance = c(),
    buddyClimb = c(),
    balanceTime = c(),
    everybot = c(),
    drivetrainType = c(),
    drivetrain = c(),
    intake = c(),
    speed = c(),
    driver = c(),
    scoutName = c(),
    comments = c(),
    
    scoredT = c(),
    scoredA = c(),
    scoredCones = c(),
    scoredCubes = c(),
    scoredTCones = c(),
    scoredTCubes =  c(),
    scoredACones = c(),
    scoredACubes = c(),
    totalPickups = c(),
    pointsT = c(),
    pointsA = c(),
    pointsTotal = c(),
    
    scoredLowT = c(),
    scoredMidT = c(),
    scoredHighT = c(),
    scoredLowA = c(),
    scoredMidA = c(),
    scoredHighA = c(),
    
    ct = c()
  )
  
  indexes <- which(vals$mainframe$teamNum == teamNum)
  
  for(idx in 1:length(indexes)) {
    info <- rbind(info, vals$mainframe[indexes[idx], ])
  }
  
  nrows <- nrow(info)
  
  
  parsevals <- function(string) {
    values <- as.integer(unlist(strsplit(string, ",")))
    return(values)
  }
  
  findLength <- function(string) {
    return(length(parsevals(string)))
  }
  
  findPointVal <- function(string, time) {
    pScores <- as.integer(unlist(strsplit(string, ",")))
    points <- c(0, 0, 0)
    
    if(time == "t") {
      points[1] <- length(which(pScores <= 9))
      points[2] <- length(which(pScores > 9 & pScores <= 18))
      points[3] <- length(which(pScores > 18))
    } else if(time == "a") {
      points[1] <- length(which(pScores <= 9))
      points[2] <- length(which(pScores > 9 & pScores <= 18))
      points[3] <- length(which(pScores > 18))
    }
    
    return(points)
  }
  
  info$scoredT <- numeric(nrows)
  info$scoredA <- numeric(nrows)
  info$scoredCones <- numeric(nrows)
  info$scoredCubes <- numeric(nrows)
  info$totalPickups <-  numeric(nrows)
  
  info$scoredLowT <- numeric(nrows)
  info$scoredMidT <- numeric(nrows)
  info$scoredHightT <- numeric(nrows)
  info$scoredLowA <- numeric(nrows)
  info$scoredMidA <- numeric(nrows)
  info$scoredHighA <- numeric(nrows)
  
  info$scoredTCones <- numeric(nrows)
  info$scoredTCubes <- numeric(nrows)
  info$scoredACones <- numeric(nrows)
  info$scoredACubes <- numeric(nrows)
  
  # ERROR HERE
  
  # Interior vals
  
  info$scoredTCones <- lapply(info$teleopCones, findLength)
  info$scoredTCubes <- lapply(info$teleopCubes, findLength)
  info$scoredACones <- lapply(info$autoCones, findLength)
  info$scoredACubes <- lapply(info$autoCubes, findLength)
  
  info$scoredT <- unlist(info$scoredTCones) + unlist(info$scoredTCubes)
  info$scoredA <- unlist(info$scoredACones) + unlist(info$scoredACubes)
  
  info$scoredCones <- unlist(info$scoredTCones) + unlist(info$scoredACones)
  info$scoredCubes <- unlist(info$scoredTCubes) + unlist(info$scoredACubes)
  
  info$totalPickups <- unlist(info$scoredCones) + unlist(info$scoredCubes)
  
  tConePoints <- findPointVal(info$teleopCones, "t")
  tCubePoints <- findPointVal(info$teleopCubes, "t")
  aConePoints <- findPointVal(info$autoCones, "a")
  aCubePoints <- findPointVal(info$autoCubes, "a")
  
  info$scoredLowT <- tConePoints[1] + tCubePoints[1]
  info$scoredMidT <- tConePoints[2] + tCubePoints[2]
  info$scoredHighT <- tConePoints[3] + tCubePoints[3]
  
  info$scoredLowA <- aConePoints[1] + aCubePoints[1]
  info$scoredMidA <- aConePoints[2] + aCubePoints[2]
  info$scoredHighA <- aConePoints[3] + aCubePoints[3]
  
  info$pointsT <- info$scoredLowT * 2 + info$scoredMidT * 3 + info$scoredHighT * 5
  info$pointsA <- info$scoredLowA * 3 + info$scoredMidA * 4 + info$scoredHighT * 6
  info$pointsTotal <- info$pointsT + info$pointsA
  
  info$ct <- (135 - as.double(info$balanceTime)) / unlist(info$scoredT)
  
  # aSt
  vals$teamframe$aSt[teamIdx] <- mean(info$scoredT)
  
  
  
  # aSa
  vals$teamframe$aSa[teamIdx] <- mean(info$scoredA)
  
  # aS
  vals$teamframe$aS[teamIdx] <- vals$teamframe$aSa[teamIdx] + vals$teamframe$aSt[teamIdx]
  
  # ECT
  vals$teamframe$ECT[teamIdx] <- round(mean(as.double(info$ct)), digits = 1)
  
  # ABT
  vals$teamframe$ABT[teamIdx] <- mean(as.double(info$balanceTime))
  
  # BC
  # Can't be done until SS data is integrated
  
  # aPPG
  vals$teamframe$aPPG[teamIdx] <- mean(info$pointsTotal)
  
  
  
  
  
}

findTeamIndex <- function(teamNum) {
  return(which(vals$teamframe$teamNum == teamNum))
}

updatePlannerTable <- function(idx) {
  vals$plannerframe <- data.frame()
  
  matchrow <- vals$scheduleframe[idx, ]
  
  red1 <- matchrow$red1[1]
  red2 <- matchrow$red2[1]
  red3 <- matchrow$red3[1]
  
  blue1 <- matchrow$blue1[1]
  blue2 <- matchrow$blue2[1]
  blue3 <- matchrow$blue3[1]
  
  vals$plannerframe <- rbind(vals$plannerframe, vals$teamframe[findTeamIndex(red1), ])
  vals$plannerframe <- rbind(vals$plannerframe, vals$teamframe[findTeamIndex(red2), ])
  vals$plannerframe <- rbind(vals$plannerframe, vals$teamframe[findTeamIndex(red3), ])
  
  vals$plannerframe <- rbind(vals$plannerframe, vals$teamframe[findTeamIndex(blue1), ])
  vals$plannerframe <- rbind(vals$plannerframe, vals$teamframe[findTeamIndex(blue2), ])
  vals$plannerframe <- rbind(vals$plannerframe, vals$teamframe[findTeamIndex(blue3), ])
}

saveMainframe <- function() {
  write.csv(vals$mainframe, paste0(path, "mainframe.csv"), row.names = FALSE)
}

saveTeamframe <- function() {
  write.csv(vals$teamframe, paste0(path, "teamframe.csv"), row.names = FALSE)
}

findOurMatchIndex <- function(match) {
  for(row in 1:nrow(vals$matches6672)) {
    if(vals$matches6672$matches[row] == as.integer(match)) {
      return(row)
    }
  }
}

canPingSite <- function(test.site) {
  !as.logical(system(paste("ping", test.site)))
}

parseData <- function(string) {
  info <- unlist(strsplit(unlist(strsplit(string, ";")), "="))
  
  names <- info[seq(1, length(info), 2)]
  values <- info[seq(2, length(info), 2)]
  
  names(values) <- names
  
  parsedData <- data.frame(as.list(values))
  
  parsedData$teamNum[1] <- as.integer(parsedData$teamNum[1])
  parsedData$matchNum[1] <- as.integer(parsedData$matchNum[1])
  parsedData$startLocation[1] <- as.integer(parsedData$startLocation[1])
  parsedData$communityPickups[1] <- as.integer(parsedData$communityPickups[1])
  parsedData$neutralPickups[1] <- as.integer(parsedData$neutralPickups[1])
  parsedData$singlePickups[1] <- as.integer(parsedData$singlePickups[1])
  parsedData$doublePickups[1] <- as.integer(parsedData$doublePickups[1])
  parsedData$balanceTime[1] <- as.double(parsedData$balanceTime[1])
  parsedData$drivetrain[1] <- as.integer(parsedData$drivetrain[1])
  parsedData$intake[1] <- as.integer(parsedData$intake[1])
  parsedData$speed[1] <- as.integer(parsedData$speed[1])
  parsedData$driver[1] <- as.integer(parsedData$driver[1])
  
  
  parsedData$alliance[1] <- tolower(parsedData$alliance[1])
  parsedData$autoBalance[1] <- tolower(parsedData$autoBalance[1])
  parsedData$teleopBalance[1] <- tolower(parsedData$teleopBalance[1])
  parsedData$drivetrainType[1] <- tolower(parsedData$drivetrainType[1])
  
  
  parsedData$mobility[1] <- as.logical(parsedData$mobility[1])
  parsedData$shuttle[1] <- as.logical(parsedData$shuttle[1])
  parsedData$buddyClimb[1] <- as.logical(parsedData$buddyClimb[1])
  parsedData$everybot[1] <- as.logical(parsedData$everybot[1])
  
  
  if(parsedData$autoPickups[1] == "[]") {
    parsedData$autoPickups[1] <- "NA"
  } else {
    parsedData$autoPickups[1] <- str_replace_all(parsedData$autoPickups[1], "\\[|\\]", "")
    parsedData$autoPickups[1] <- str_replace_all(parsedData$autoPickups[1], " ", "")
  }
  
  if(parsedData$autoCones[1] == "[]") {
    parsedData$autoCones[1] <- "NA"
  } else {
    parsedData$autoCones[1] <- str_replace_all(parsedData$autoCones[1], "\\[|\\]", "")
    parsedData$autoCones[1] <- str_replace_all(parsedData$autoCones[1], " ", "")
  }
  
  if(parsedData$autoCubes[1] == "[]") {
    parsedData$autoCubes[1] <- "NA"
  } else {
    parsedData$autoCubes[1] <- str_replace_all(parsedData$autoCubes[1], "\\[|\\]", "")
    parsedData$autoCubes[1] <- str_replace_all(parsedData$autoCubes[1], " ", "")
  }
  
  if(parsedData$teleopCones[1] == "[]") {
    parsedData$teleopCones[1] <- "NA"
  } else {
    parsedData$teleopCones[1] <- str_replace_all(parsedData$teleopCones[1], "\\[|\\]", "")
    parsedData$teleopCones[1] <- str_replace_all(parsedData$teleopCones[1], " ", "")
  }
  
  if(parsedData$teleopCubes[1] == "[]") {
    parsedData$teleopCubes[1] <- "NA"
  } else {
    parsedData$teleopCubes[1] <- str_replace_all(parsedData$teleopCubes[1], "\\[|\\]", "")
    parsedData$teleopCubes[1] <- str_replace_all(parsedData$teleopCubes[1], " ", "")
  }
  
  return(parsedData)
}

clearTFrame <- function(type) {
  if(type == "team") {
    vals$teleopScoring <- data.frame(r1 = c("O", "O", "X"),
                                     r2 = c("X", "X", "X"),
                                     r3 = c("O", "O", "X"),
                                     r4 = c("O", "O", "X"),
                                     r5 = c("X", "X", "X"),
                                     r6 = c("O", "O", "X"),
                                     r7 = c("O", "O", "X"),
                                     r8 = c("X", "X", "X"),
                                     r9 = c("O", "O", "X"))
  } else if(type == "match") {
    vals$matchteleopScoring <- data.frame(r1 = c("O", "O", "X"),
                                     r2 = c("X", "X", "X"),
                                     r3 = c("O", "O", "X"),
                                     r4 = c("O", "O", "X"),
                                     r5 = c("X", "X", "X"),
                                     r6 = c("O", "O", "X"),
                                     r7 = c("O", "O", "X"),
                                     r8 = c("X", "X", "X"),
                                     r9 = c("O", "O", "X"))
  }
}

clearAFrame <- function(type) {
  if(type == "team") {
    vals$autonScoring <- data.frame(r1 = c("O", "O", "X"),
                                    r2 = c("X", "X", "X"),
                                    r3 = c("O", "O", "X"),
                                    r4 = c("O", "O", "X"),
                                    r5 = c("X", "X", "X"),
                                    r6 = c("O", "O", "X"),
                                    r7 = c("O", "O", "X"),
                                    r8 = c("X", "X", "X"),
                                    r9 = c("O", "O", "X"))
  } else if(type == "match") {
    vals$matchautonScoring <- data.frame(r1 = c("O", "O", "X"),
                                    r2 = c("X", "X", "X"),
                                    r3 = c("O", "O", "X"),
                                    r4 = c("O", "O", "X"),
                                    r5 = c("X", "X", "X"),
                                    r6 = c("O", "O", "X"),
                                    r7 = c("O", "O", "X"),
                                    r8 = c("X", "X", "X"),
                                    r9 = c("O", "O", "X"))
  }
}

getCoords <- function(index) {
  coords <- c()
  
  if(index <= 9) {
    coords <- append(coords, 1)
    coords <- append(coords, index)
  } else if(9 < index & index <= 18) {
    coords <- append(coords, 2)
    coords <- append(coords, (index - 9))
  } else if(18 < index & index <= 27) {
    coords <- append(coords, 3)
    coords <- append(coords, (index - 18))
  }
  
  return(coords)
}

calculatePredScore <- function(match) {
  r1EPA <- vals$teamframe$EPA[findTeamIndex(vals$scheduleframe$red1[as.integer(match)])]
  r2EPA <- vals$teamframe$EPA[findTeamIndex(vals$scheduleframe$red2[as.integer(match)])]
  r3EPA <- vals$teamframe$EPA[findTeamIndex(vals$scheduleframe$red3[as.integer(match)])]
  
  b1EPA <- vals$teamframe$EPA[findTeamIndex(vals$scheduleframe$blue1[as.integer(match)])]
  b2EPA <- vals$teamframe$EPA[findTeamIndex(vals$scheduleframe$blue2[as.integer(match)])]
  b3EPA <- vals$teamframe$EPA[findTeamIndex(vals$scheduleframe$blue3[as.integer(match)])]
  
  redScore <- r1EPA + r2EPA + r3EPA
  blueScore <- b1EPA + b2EPA + b3EPA
  
  return(c(redScore, blueScore))
}

addScores <- function(data, time, type, datatype) {
  
  for(num in 1:length(data)) {
    loc <- as.integer(data[num])
    row <- getCoords(loc)[1]
    col <- getCoords(loc)[2]
    
    if(datatype == "team") {
      if(time == "a") {
        if(type == "cone") {
          vals$autonScoring[row, col] <- "🔺"
        } else if(type == "cube") {
          vals$autonScoring[row, col] <- "🟦"
        }
      } else if(time == "t") {
        if(type == "cone") {
          vals$teleopScoring[row, col] <- "🔺"
        } else if(type == "cube") {
          vals$teleopScoring[row, col] <- "🟦"
        }
      }
    } else if(datatype == "match") {
      if(time == "a") {
        if(type == "cone") {
          vals$matchautonScoring[row, col] <- "🔺"
        } else if(type == "cube") {
          vals$matchautonScoring[row, col] <- "🟦"
        }
      } else if(time == "t") {
        if(type == "cone") {
          vals$matchteleopScoring[row, col] <- "🔺"
        } else if(type == "cube") {
          vals$matchteleopScoring[row, col] <- "🟦"
        }
      }
    }
    
  }
}

pullTBAData <- function() {
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
}

pullStatboticsData <- function() {
  teamsInfo <- event_teams(tbaKey)
  
  teamNums <- c()
  
  for(team in 1:length(teamsInfo$key)) {
    teamNums <- append(teamNums, substr(teamsInfo$key[team], 4, 7))
  }
  
  for(team in  1:length(teamsInfo$key)) {
    t <- list(teamNum = NA,
              matchesPlayed = NA,
              EPA = NA)
    
    teamNumber <- teamNums[team]
    
    teamInfo <- getStatboticsTeam(teamNumber)
    
    t$teamNum <- teamNumber
    
    if(!(is.null(teamInfo$epa_end))) {
      t$EPA <- teamInfo$epa_end
    } else {
      t$EPA <- "NA"
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
    
    vals$teamframe <- rbind(vals$teamframe, t)
  }
  nrows <- nrow(vals$teamframe)
  
  vals$teamframe$ECT <- numeric(nrows)
  vals$teamframe$aPPG <- numeric(nrows)
  vals$teamframe$SEf <- numeric(nrows)
  vals$teamframe$sFlex <- numeric(nrows)
  vals$teamframe$aSt <- numeric(nrows)
  vals$teamframe$aSa <- numeric(nrows)
  vals$teamframe$aS <- numeric(nrows)
  vals$teamframe$ABT <- numeric(nrows)
  vals$teamframe$BC <- numeric(nrows)
}

calculateWinChance <- function(matchNum, fromAlliance = "default") {
  
  red1 <- vals$scheduleframe$red1[matchNum]
  red2 <- vals$scheduleframe$red2[matchNum]
  red3 <- vals$scheduleframe$red3[matchNum]
  
  blue1 <- vals$scheduleframe$blue1[matchNum]
  blue2 <- vals$scheduleframe$blue2[matchNum]
  blue3 <- vals$scheduleframe$blue3[matchNum]
  
  red1EPA <- vals$teamframe$EPA[findTeamIndex(red1)]
  red2EPA <- vals$teamframe$EPA[findTeamIndex(red2)]
  red3EPA <- vals$teamframe$EPA[findTeamIndex(red3)]
  
  blue1EPA <- vals$teamframe$EPA[findTeamIndex(blue1)]
  blue2EPA <- vals$teamframe$EPA[findTeamIndex(blue2)]
  blue3EPA <- vals$teamframe$EPA[findTeamIndex(blue3)]
  
  epaDiff <- (red1EPA + red2EPA + red3EPA) - (blue1EPA + blue2EPA + blue3EPA)
  
  winChance <- 1/(1 + 10 ^ (epaDiff/400))
  
  formattedWinChance <- format(winChance * 100, digits = 3)
  
  return(formattedWinChance)
}



updateOurMatches <- function() {
  for(match in 1:nrow(vals$scheduleframe)) {
    
    d <- NA
    if(vals$scheduleframe$red1[match] == 6672) {
      d <- list(match, "r", 1)
    } else if(vals$scheduleframe$red2[match] == 6672) {
      d <- list(match, "r", 2)
    } else if(vals$scheduleframe$red3[match] == 6672) {
      d <- list(match, "r", 3)
    } else if(vals$scheduleframe$blue1[match] == 6672) {
      d <- list(match, "b", 1)
    } else if(vals$scheduleframe$blue2[match] == 6672) {
      d <- list(match, "b", 2)
    } else if(vals$scheduleframe$blue3[match] == 6672) {
      d <- list(match, "b", 3)
    }
    
    if(!(is.na(d[1]))) {
      names(d) <- list("matches", "alliance", "station")
      vals$matches6672 <- rbind(vals$matches6672, d)
    }
  }
}


getStatboticsTeam <- function(teamNum) {
  link <- paste(statbotics, "team_year/", teamNum, "/", year, sep = "")
  
  team <- content(GET(link))
}

# Dictates the layout of the UI



ui <- navbarPage(
  
  title = div(icon("gitkraken", lib = "font-awesome", style = "color: #bb520a;"), "  Kraken"),
  
  tabPanel("Data",
           fluidPage(
               column(fluidRow(
                 sidebarPanel(
                   radioButtons(
                     "inputType", "What type of data?",
                     c("Regular Scout" = "regScout",
                       "Superscout" = "supScout")
                   ),
                   textAreaInput("dataInput", "Input Scout Data", width = "300px", height = "100px", resize = "none"),
                   actionButton("enterData", "Enter"),
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
                     downloadButton("dataExport"),
                     h5("WARNING: this button will delete all current data. Consider exporting the data first."),
                     actionButton("deleteFiles", "Delete Files", style = "background-color: #d41704;"),
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
                   actionButton("enterSearch", "Enter"),
                   
                   width = 12
                 )
               ),
               fluidRow(sidebarPanel(
                 textOutput("EPA"),
                 textOutput("ECT"),
                 textOutput("aS"),
                 textOutput("aPPG"),
                 textOutput("sFlex"),
                 textOutput("ABT"),
                 textOutput("SEf"),
                 width = 12
               )),
               width = 3),
               column(fluidRow(DTOutput("searchDT")),
                      fluidRow(
                        actionButton("clearscoringDTs", "Clear Score Viewers")
                      ),
                      fluidRow(
                        column(
                          sidebarPanel(
                            h5("Auton:"),
                            tableOutput("autonScoring"),
                            width = 12
                          ),
                          width = 6
                        ),
                        column(
                          sidebarPanel(
                            h5("Teleop:"),
                            tableOutput("teleopScoring"),
                            width = 12
                          ),
                          width = 6
                        )
                      ),
                      width = 9),
             
           )), 
  
  tabPanel("Matches",
           fluidPage(
             column(
               fluidRow(
                 sidebarPanel(
                   textInput("matchsearch",
                             "Search:",
                             placeholder = "enter match number"),
                   actionButton("entermatchSearch", "Enter"),
                   width = 12
                 )
               ),
               fluidRow(
                 sidebarPanel(
                   width = 12
                 )
               ),
               width = 3
             ),
             column(
               fluidRow(
                 DTOutput("matchsearchDT")
               ),
               fluidRow(
                 actionButton("clearmatchscoringDTs", "Clear Score Viewers")
               ),
               fluidRow(
                 column(
                   sidebarPanel(
                     h5("Auton:"),
                     tableOutput("matchautonScoring"),
                     width = 12
                   ),
                   width = 6
                 ),
                 column(
                   sidebarPanel(
                     h5("Teleop:"),
                     tableOutput("matchteleopScoring"),
                     width = 12
                   ),
                   width = 6
                 )
               ),
               width = 9
             )
           )),
  
  tabPanel("Competition",
           DTOutput("mainframeOutput")),
  
  tabPanel("Graph"),
  
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
                  textOutput("winChance6672"),
                  textOutput("driverStation"),
                  textOutput("predictedScore"),
                  width = 12 
                 )
               ),
               width = 3
             ),
             column(
               DTOutput("plannertable"),
               width = 9
             )
           )),
  
  tabPanel("Stats",
           DTOutput("statsData")),
  
  tabPanel("Schedule",
           DTOutput("matchScheduleDT")),
  
  tabPanel("Functions",
           actionButton("getWinChances", "Get Win Percents")),
  
  selected = "Data"
)


tags$script(src="https://kit.fontawesome.com/7f698a1940.js")


# Outlines the server function which dictates the logic of the app

server <- function(input, output, session) {
  
  observe({  
    if(!vals$startupDone) {
      if(!(file.exists(paste0(path, "schedule.csv")))) {
        pullTBAData()
        
        write.csv(vals$scheduleframe, paste0(path, "schedule.csv"), row.names = FALSE)
      } else {
        vals$scheduleframe <- read.csv(paste0(path, "schedule.csv"))
      }
      
      if(file.exists(paste0(path, "mainframe.csv"))) {
        vals$mainframe <- read.csv(paste0(path, "mainframe.csv"))
      }
      if(file.exists(paste0(path, "teamframe.csv"))) {
        vals$teamframe <- read.csv(paste0(path, "teamframe.csv"))
      } else {
        pullStatboticsData()
        
        write.csv(vals$teamframe, paste0(path, "teamframe.csv"), row.names = FALSE)
      }
      
      
      vals$startupDone <- TRUE
    }
  })
  
  
  # Data Page
  
  observeEvent(input$file, {
    f <- NA
    
    d <- input$file
    
    if(is.null(d)) {
      return(NULL)
    }
    
    f <- read.csv(d$datapath, header = TRUE, sep = ",")
    
    if(input$inputType == "regScout") {
      
      vals$previewframe <- f
      
    } else if (input$inputType == "supScout") {
      
      vals$sspreviewframe <- f
      
    }
    
  })
  
  observeEvent(input$enterData, {
    f <- input$dataInput
    
    parsed <- parseData(f)
    
    vals$previewframe <- parsed
  })
  
  observeEvent(input$yesData, {
    
    
    
    if(nrow(vals$previewframe) == 1) {
      if(nrow(vals$mainframe) == 0) {
        vals$mainframe <- rbind(vals$mainframe, vals$previewframe[1, ])
        
        teamIndex <- findTeamIndex(vals$previewframe$teamNum[1])
        
        vals$teamframe$matchesPlayed[teamIndex] <- vals$teamframe$matchesPlayed[teamIndex] + 1
        
        saveMainframe()
        updateCalcValues()
        saveTeamframe()
        vals$previewframe <- data.frame()
        updateTextAreaInput(session, "dataInput", value = "")
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
          
          vals$teamframe$matchesPlayed[teamIndex] <- vals$teamframe$matchesPlayed[teamIndex] + 1
          
          saveMainframe()
          updateCalcValues()
          saveTeamframe()
          vals$previewframe <- data.frame()
          updateTextAreaInput(session, "dataInput", value = "")
        }
        
      }
    } else if(nrow(vals$previewframe) > 1) {
      print("error")
      return(NULL)
    } else {
      return(NULL)
    }
  })
  
  observeEvent(input$noData, {
    vals$previewframe <- data.frame()
    updateTextAreaInput(session, "dataInput", value = "")
  })
  
  observeEvent(input$dataImport, {
    importedFile <- input$dataImport
    
    if(is.null(importedData)) {
      return(NULL)
    }
    
    importedData <- read.csv(importedFile$datapath, header = TRUE, sep = ",")
    
    vals$mainframe <- importedData
    
  })
  
  output$dataExport <- downloadHandler(filename = "scoutingdata.zip", 
                                       content = function(file) {
                                         dFiles <- c(paste0(path, "schedule.csv"), paste0(path, "teamframe.csv"))
                                         zip(file, files = dFiles)
                                       },
                                       contentType = "application.zip"
                                       )
  
  observeEvent(input$deleteFiles, {
    
    showModal(deleteModal())
  })
  
  observeEvent(input$confirmDelete, {
    vals$mainframe <- data.frame()
    if(file.exists(paste0(path, "mainframe.csv"))) {
      file.remove(paste0(path, "mainframe.csv"))
    }
    removeModal()
  })
  
  deleteModal <- function() {
    modalDialog(
      tagList(actionButton("confirmDelete", "Yes", style = "background-color: #d41704;")),
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
  
  output$preview <- renderDT(datatable(vals$previewframe, extensions = "FixedColumns", 
                                       options = list(scrollX = TRUE, paging = FALSE),
                                       selection = "single"))
  output$ssPreview <- renderDT(datatable(vals$sspreviewframe, extensions = "FixedColumns", 
                                         options = list(scrollX = TRUE, paging = FALSE),
                                         selection = "single"))
  
  
  
  
  
  
  
  
  
  # Teams Page
  
  observeEvent(input$enterSearch, {
    vals$searchframe <- data.frame(teamNum = c(),
                                   matchNum = c(),
                                   alliance = c(),
                                   startLocation = c(),
                                   preload = c(),
                                   mobility = c(),
                                   autoPickups = c(),
                                   autoCones = c(),
                                   autoCubes = c(),
                                   autoBalance = c(),
                                   communityPickups = c(),
                                   neutralPickups = c(),
                                   singlePickups = c(),
                                   doublePickups = c(),
                                   teleopCones = c(),
                                   teleopCubes = c(),
                                   shuttle = c(),
                                   teleopBalance = c(),
                                   buddyClimb = c(),
                                   balanceTime = c(),
                                   everybot = c(),
                                   drivetrainType = c(),
                                   drivetrain = c(),
                                   intake = c(),
                                   speed = c(),
                                   driver = c(),
                                   scoutName = c(),
                                   comments = c()
                                   )
    
    s <- input$search
    
    if(is.null(s) || is.null(vals$mainframe)) {
      return(NULL)
    }
    
    for(newrow in 1:nrow(vals$mainframe)) {
      if(toString(vals$mainframe$teamNum[newrow]) == s) {
        vals$searchframe <- rbind(vals$searchframe, vals$mainframe[newrow, ])
      } else {
        return(NULL)
      }
    }
    
    searchVal <- as.integer(input$search)
    
    tIndex <- which(vals$teamframe$teamNum == searchVal)
    
    teamEPA <- vals$teamframe$EPA[tIndex]
    teamECT <- vals$teamframe$ECT[tIndex]
    teamaS <- vals$teamframe$aS[tIndex]
    teamaPPG <- vals$teamframe$aPPG[tIndex]
    teamABT <- vals$teamframe$ABT[tIndex]
      
    output$EPA <- renderText(paste0("EPA: ", teamEPA))
    output$ECT <- renderText(paste0("ECT: ", teamECT))
    output$aS <- renderText(paste0("aS: ", teamaS))
    output$aPPG <- renderText(paste0("aPPG: ", teamaPPG))
    output$ABT <- renderText(paste0("ABT: ", teamABT))
    
  })
  
  observe({
    searchDT <- datatable(vals$searchframe, options = list(scrollX = TRUE, scrollY = "260px", paging = FALSE))
    
    output$searchDT <- renderDT(searchDT)
    
    observeEvent(input$clearscoringDTs, {
      clearAFrame("team")
      clearTFrame("team")
    })
    
    observeEvent(input$searchDT_rows_selected, {
      
      row <- vals$searchframe[input$searchDT_rows_selected, ]
      
      aCones <- unlist(strsplit(toString(row$autoCones), ","))
      aCubes <- unlist(strsplit(toString(row$autoCubes), ","))
      
      tCones <- unlist(strsplit(toString(row$teleopCones), ","))
      tCubes <- unlist(strsplit(toString(row$teleopCubes), ","))
      
      
      if(aCones[1] != "NA") {
        addScores(aCones, "a", "cone", "team")
      }
      
      if(aCubes[1] != "NA") {
        addScores(aCubes, "a", "cube", "team")
      }
      
      if(tCones[1] != "NA") {
        addScores(tCones, "t", "cone", "team")
      }
      
      if(tCubes[1] != "NA") {
        addScores(tCubes, "t", "cube", "team")
      }
    })
  })
  
  
  output$autonScoring <- renderTable(vals$autonScoring)
  output$teleopScoring <- renderTable(vals$teleopScoring)
  
  
  
  
  
  
  
  # Matches Page
  
  observeEvent(input$entermatchSearch, {
    vals$matchsearchframe <- data.frame(teamNum = c(),
                                   matchNum = c(),
                                   alliance = c(),
                                   startLocation = c(),
                                   preload = c(),
                                   mobility = c(),
                                   autoPickups = c(),
                                   autoCones = c(),
                                   autoCubes = c(),
                                   autoBalance = c(),
                                   communityPickups = c(),
                                   neutralPickups = c(),
                                   singlePickups = c(),
                                   doublePickups = c(),
                                   teleopCones = c(),
                                   teleopCubes = c(),
                                   shuttle = c(),
                                   teleopBalance = c(),
                                   buddyClimb = c(),
                                   balanceTime = c(),
                                   everybot = c(),
                                   drivetrainType = c(),
                                   drivetrain = c(),
                                   intake = c(),
                                   speed = c(),
                                   driver = c(),
                                   scoutName = c(),
                                   comments = c()
    )
    
    s <- input$matchsearch
    
    if(is.null(s) || is.null(vals$mainframe)) {
      return(NULL)
    }
    
    for(newrow in 1:nrow(vals$mainframe)) {
      if(toString(vals$mainframe$matchNum[newrow]) == s) {
        vals$matchsearchframe <- rbind(vals$matchsearchframe, vals$mainframe[newrow, ])
      } else {
        return(NULL)
      }
    }
    
    searchVal <- as.integer(input$search)
    
    mIndex <- which(vals$teamframe$matchNum == searchVal)
    
    
  })
  
  observe({
    matchsearchDT <- datatable(vals$matchsearchframe, options = list(scrollX = TRUE, scrollY = "260px", paging = FALSE))
    
    output$matchsearchDT <- renderDT(matchsearchDT)
    
    observeEvent(input$clearmatchscoringDTs, {
      clearAFrame("match")
      clearTFrame("match")
    })
    
    observeEvent(input$matchsearchDT_rows_selected, {
      
      row <- vals$matchsearchframe[input$matchsearchDT_rows_selected, ]
      
      aCones <- unlist(strsplit(toString(row$autoCones), ","))
      aCubes <- unlist(strsplit(toString(row$autoCubes), ","))
      
      tCones <- unlist(strsplit(toString(row$teleopCones), ","))
      tCubes <- unlist(strsplit(toString(row$teleopCubes), ","))
      
      
      if(aCones[1] != "NA") {
        addScores(aCones, "a", "cone", "match")
      }
      
      if(aCubes[1] != "NA") {
        addScores(aCubes, "a", "cube", "match")
      }
      
      if(tCones[1] != "NA") {
        addScores(tCones, "t", "cone", "match")
      }
      
      if(tCubes[1] != "NA") {
        addScores(tCubes, "t", "cube", "match")
      }
    })
  })
  
  
  output$matchautonScoring <- renderTable(vals$matchautonScoring)
  output$matchteleopScoring <- renderTable(vals$matchteleopScoring)
  
  
  
  
  
  
  
  # Competition Page
  
  output$mainframeOutput <- renderDT(datatable(vals$mainframe, options = list(scrollX = TRUE, scrollY = "540px",
                                                                              paging = FALSE)))
  
  
  
  # Qualitative Page
  
  
  
  
  # Graphs Page
  
  
  
  
  
  # Match Planner Page
  
  observe({
    updateSelectInput(session, "selectedMatch",
                      choices = vals$matches6672$matches)
  })
  
  observeEvent(input$selectedMatch, {
    if(vals$winnersCalculated) {
      updatePlannerTable(input$selectedMatch)
      
      winPC <- vals$scheduleframe$winChances[as.integer(input$selectedMatch)]
      winTeam <- vals$scheduleframe$predictedWinner[as.integer(input$selectedMatch)]
      
      index <- findOurMatchIndex(input$selectedMatch)
      alliance <- vals$matches6672$alliance[index]
      station <- vals$matches6672$station[index]
      
      scores <- calculatePredScore(input$selectedMatch)
      
      redScore <- as.integer(scores[1])
      blueScore <- as.integer(scores[2])
      
      
      if(alliance != winTeam) {
        winPC <- 100 - winPC
      }
      
      output$winChance6672 <- renderText(paste("Win Chance: ", winPC, "%", sep = ""))
      output$driverStation <- renderText(paste("Driver Station: ", station))
      output$predictedScore <- renderText(paste("Predicted Score: ", redScore, " - ", blueScore, sep = ""))
      output$plannertable <- renderDT(datatable(vals$plannerframe))
    }
    
  })
  
  
  
  # TBA Page
  
  observeEvent(input$getWinChances, {
    updateOurMatches()
    
    vals$winnersCalculated <- TRUE
    
    winChances <- c()
    predictedWinners <- c()
    
    for(matchNum in 1:nrow(vals$scheduleframe)) {
      winChance <- as.numeric(calculateWinChance(matchNum))
      predictedWinner <- NA
      
      # ERROR HERE
      if(winChance > 50) {
        predictedWinner <- "r"
      } else if(winChance < 50) {
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
    
  })
  
  
  
  
  
  # Stats Page

  
  output$statsData <- renderDT(datatable(vals$teamframe, options = list(scrollX = TRUE, scrollY = "540px",
                                                                             paging = FALSE)))
  
  
  
  # Schedule Page
  observe({
    output$matchScheduleDT <- renderDT({
      datatable(
        vals$scheduleframe,
        extensions = "FixedColumns",
        options = list(scrollX = TRUE, scrollY = "540px", paging = FALSE),
        selection = "single"
      ) %>% formatStyle(
        9, 10,
        color = styleEqual(c("r", "b", "even"), c("red", "blue", "gray"))
      )
    })
  })
  
  

    
}

shinyApp(ui, server)