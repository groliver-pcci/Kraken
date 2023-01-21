library(shiny)

ui <- navbarPage(
  
  "Hydra",
  
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
             mainPanel(
               
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
                 verbatimTextOutput("text"),
                 
                 
                 width = 12
               )
             ),
             fluidRow(sidebarPanel(
               titlePanel("hello"),
               width = 12
             )),
             width = 3),
             column(mainPanel(plotOutput("graph")),
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
    
    output$text <- renderText({input$search})
    
    output$graph <- renderPlot({
      hist(c(3, 4, 5, 7))
    })
}

shinyApp(ui, server)