library(shiny)
library(shinydashboard)
library(rsconnect)
library(tidyverse)

header <- dashboardHeader()
sidebar <- dashboardSidebar(
      radioButtons("Serving_Team", "Serving Team", choices = c("Team_A", "Team_B"), selected = "Team_A"),
      numericInput("Starting_Score", "Starting Score", value = 0, min = 0),
      textInput("Team_A", "Enter Team A Starting Rotation (commas separated):", value = "1st, 2nd, 3rd, 4th, 5th, 6th"),
      textInput("Team_B", "Enter Team B Starting Rotation (commas separated):", value = "1st, 2nd, 3rd, 4th, 5th, 6th")
)
body <- dashboardBody(
  mainPanel(      
    actionButton("Point_TeamA", "Team A Score"),
    actionButton("Point_TeamB", "Team B Score"),

    textOutput("Score_Out"),
    br(),
    textOutput("TAServer"),
    br(),
    textOutput("TBServer"),
    br(),
    textOutput("Win")
  )
)
ui <- dashboardPage(skin = "red",
                    header = header,
                    sidebar = sidebar,
                    body = body
)

server <- function(input, output) {
  Team_A_Rotation <- reactive({
    strsplit(input$Team_A, ",") %>% unlist()
  })
  Team_B_Rotation <- reactive({
    strsplit(input$Team_B, ",") %>% unlist()
  })
  
  d <- reactive({
    dist <- switch(input$Serving_Team,
                   Team_A = 1,
                   Team_B = 0)
    
    dist(input$n)
  })
  
  Score <- reactiveValues(Team_A_Score = 0,
                          Team_B_Score = 0,
                          Serving_Team = 1,
                          TA_Server = 0,
                          TB_Server = 0) 
  
  reactive({
    if(input$Starting_Score > 0 ) {Score$Team_A_Score = input$Starting_Score}
  })
  
  observeEvent(input$Point_TeamA, {
    Score$Team_A_Score <- Score$Team_A_Score + 1
    if(Score$Serving_Team == 2) {Score$TA_Server = Score$TA_Server + 1}
    Score$Serving_Team = 1
  })
  
  output$TAServer = renderText({
    paste("Team A's next server is: ", Team_A_Rotation()[1 + Score$TA_Server %% 6])
    })
  
  observeEvent(input$Point_TeamB, {
    Score$Team_B_Score <- Score$Team_B_Score + 1
    if(Score$Serving_Team == 1) {
      Score$TB_Server = Score$TB_Server + 1
    }
    Score$Serving_Team = 2
  })
  
  output$TBServer = renderText({
    paste("Team B's next server is: ", Team_B_Rotation()[1 + Score$TB_Server %% 6])
    })
 
  output$Score_Out <- renderText({
    paste("Team A Score: ", Score$Team_A_Score, "|  Team B Score: ", Score$Team_B_Score)   
  })
  
  output$Serving_Team <- renderText({paste("Serving Team: ", Score$Serving_Team)})
    
  output$Win <- renderText({
    if (Score$Team_A_Score >= 25 & Score$Team_A_Score >= Score$Team_B_Score + 2) {"Team A Wins"} 
    else if (Score$Team_B_Score >= 25 & Score$Team_B_Score >= Score$Team_A_Score + 2) {"Team B Wins"}
    else {""}
  })
}

shinyApp(ui, server)