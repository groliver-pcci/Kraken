library(shiny)

ui <- navbarPage(
  "Hydra",
  
  tabPanel("Input Files",
           fluidPage(
             fileInput(
               "file",
               "Upload CSV",
               multiple = FALSE
             )
           )
  ),
  
  tabPanel("Teams",
           fluidPage(
             sidebarPanel(
             textInput(
               "search",
               "Search:",
               placeholder = "enter team or match number"
             ),
             verbatimTextOutput("text")),
             
           )
           ),
  
  tabPanel("Matches")
  
  tabPanel("Graph"),
  
  selected = "Search"
)

server <- function(input, output) {
    
    output$text <- renderText({input$search})
  
}

shinyApp(ui, server)