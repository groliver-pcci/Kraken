##Kraken
This is a back-end scouting data compiler for the 2023 FRC Charged Up season written in R

##environmental setup
for local use modify 
'''

load_all(' ...
path <-'...
'''
to local computer data folder for the tbaR package local folder( from 
github.com/GKrotkov/tbaR
and the local data folder in the kraken repot (with trailing / in path 
string)

to allow for operation when 
schedule data is not available online

## requirements
library(shiny)
##sessioninfo()

 [1] ggrepel_0.9.2     officer_0.5.1     plotly_4.10.1    
 [4] bslib_0.4.2       zip_2.2.2         shinyFiles_0.9.3 
 [7] devtools_2.4.5    usethis_2.1.6     formattable_0.2.1
[10] scales_1.2.1      forcats_0.5.2     stringr_1.5.0    
[13] dplyr_1.0.10      purrr_1.0.1       readr_2.1.3      
[16] tidyr_1.2.1       tibble_3.1.8      ggplot2_3.4.0    
[19] tidyverse_1.3.2   DT_0.27           shiny_1.7.4  

#Data Sources
## tba API via tbaR https://www.thebluealliance.com/apidocs/v3
## statbotics API :https://www.statbotics.io/api/rest
## manually collected data using 6674 scouting app

#Data files
## teamframe
stores calculated average or total values per team
-matchesPlayedi internalMatchesplayed and scouted
-EPAi  Expected Points Added internal data 
-EPA statbotics calculated (https://www.statbotics.io/blog/epa)
matchesPlayed
ECT evaluated count of good matches?? ask will
aPPG  average points per game
SEf
aSt average score teleop
aSa  average score auton
aS  average score total auto+teleop
BC  depends on superscout data, not implemented yet

##scheduleframe
schedule data with predictions from statbotics
note: can crash if not populated, and manual backup file or dummy file 
maybe needed until schedule released
winChances - prediction from statbotics event api "epa_win_prob"
predictedWinners- "epa_winner"

##mainframe variable names and descriptors  

teamNum team number as int
matchNum match number
alliance  coor
driveStation 
startLocation 
preload string cone,cube,na
mobility - during auton
autoPickups int
autoCones placed
autoCubes placed
autoBalance balance status
communityPickups 
neutralPickups  neutral zone
singlePickups 
doublePickups 
teleopCones 
teleopCubes 
shuttle moved cargo
teleopBalance 
buddyClimb bool
driver qualitative score 1-5
scoutName string
comments string
scoredT 
scoredA 
scoredCones 
scoredCubes 
scoredTCones 
scoredTCubes =  c(),
scoredACones 
scoredACubes 
totalPickups 
pointsT 
pointsA 
pointsTotal 
scoredLowT 
scoredMidT 
scoredHighT 
scoredLowA 
scoredMidA 
scoredHighA                        
ct

