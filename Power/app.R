library(shiny)
library(BSDA)
library(tidyverse)

ui <- fluidPage(
   
   # Application title
   titlePanel("1 Sample Power Analysis"),
   
   sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "Population_Distribution",
                    label = "Population Distribution",
                    choices = c("Normal", "Uniform")),
        sliderInput("Std",
                    "Population Standard Deviation",
                    min = 0.1,
                    max = 10,
                    value = 2,
                    step = 0.1), 
        sliderInput("TRUE_DIFF",
                     "True Difference",
                     min = 0,
                     max = 9,
                     value = 1,
                     step = 0.5),
         sliderInput("Sample_Size",
                     "Sample Size",
                     min = 3,
                     max = 50,
                     value = 5,
                     step = 1),
         sliderInput("Type_1_Error",
                     "Type I Error (Alpha)",
                     min = 0.01,
                     max = 0.10,
                     value = 0.05),
        checkboxInput("Type_I", "Type I Error", value = FALSE),
        checkboxInput("Type_II", "Type II Error", value = FALSE),
        checkboxInput("Power", "Power", value = FALSE)
      ),
      mainPanel(
         textOutput("TrueDiff"),
         htmlOutput("T_Power"),
         plotOutput("Population", height = 200),
         plotOutput("t_plot", height = 200)
      )
   )
)

# Define server 
server <- function(input, output) {
  output$TrueDiff <- renderText({
    paste("True Difference: ", input$TRUE_DIFF)
  })
 
    output$T_Power <- renderUI({
      
    TTest <- function(){
      
      Sample <- if (input$Population_Distribution == "Normal" ){
        rnorm(input$Sample_Size, mean = 0 + input$TRUE_DIFF, sd = input$Std)
      }
      else if (input$Population_Distribution == "Uniform" ){
        runif(input$Sample_Size, min = input$TRUE_DIFF - input$Std, max = input$TRUE_DIFF + input$Std)
      }
      t_test <- t.test(Sample, alternative = "greater", mu = 0, conf.level = 1-input$Type_1_Error)
      Reject <- t_test$p.value < input$Type_1_Error
      return(Reject)
    }
    
    Sign_test <- function(){
      Sample <- if (input$Population_Distribution == "Normal" ){
        rnorm(input$Sample_Size, mean = 0 + input$TRUE_DIFF, sd = input$Std)
      }
      else if (input$Population_Distribution == "Uniform" ){
        runif(input$Sample_Size, min = input$TRUE_DIFF - input$Std, max = input$TRUE_DIFF + input$Std)
      }
      Sign_T <- SIGN.test(Sample, md = 0, alternative = "greater", conf.level = 1-input$Type_1_Error)
      Reject_Sign <- Sign_T$p.value < input$Type_1_Error
      return(Reject_Sign)
    }
    
    set.seed(1)    
    Power_t <- sum(replicate(TTest(), n = 1000))/1000
    set.seed(1)
    Power_sign <- sum(replicate(Sign_test(), n = 1000))/1000
    HTML(paste("The simulated Power from a t-test is: ", Power_t,
               "The simulated Power from a sign test is: ", Power_sign, sep = '<br/>'))
  })
  
  output$Sign_Power <- renderText({
    TTest <- function(){
      Sample <- if (input$Population_Distribution == "Normal" ){
        rnorm(input$Sample_Size, mean = 0 + input$TRUE_DIFF, sd = input$Std)
      }
      else if (input$Population_Distribution == "Uniform" ){
        runif(input$Sample_Size, min = input$TRUE_DIFF - input$Std, max = input$TRUE_DIFF + input$Std)
      }
      t_test <- t.test(Sample, alternative = "greater", mu = 0, conf.level = 1-input$Type_1_Error)
      Reject <- t_test$p.value < input$Type_1_Error
      return(Reject)
    }
    Power <- sum(replicate(TTest(), n = 1000))/1000
    paste("The simulated Power from a t-test is: ", Power)
    
  })
  
  output$Population <- renderPlot({
    #Pop <- function(x) {dnorm(x, mean = 0 + input$TRUE_DIFF, sd = input$Std)}
    if (input$Population_Distribution == "Normal" ){
      Pop <- function(x) {dnorm(x, mean = 0 + input$TRUE_DIFF, sd = input$Std)}
      #Pop <- function(x) {dnorm(input$Sample_Size, mean = 0 + input$TRUE_DIFF, sd = input$Std)}
    }
    else if (input$Population_Distribution == "Uniform" ){
      Pop <- function(x) {dunif(x, min = -3.99, max = 11.99)}
    }
    ggplot(data.frame(x = c(-4, 12)), aes(x = x)) +
      #ylim(c(0,0.4)) +
      stat_function(fun = Pop)
    }
  )
  
  output$t_plot <- renderPlot({
    standard <- function(x) {dnorm(x, mean = 0, sd = input$Std / sqrt(input$Sample_Size))}
    shifted <- function(x) {dnorm(x, mean = input$TRUE_DIFF, sd = input$Std / sqrt(input$Sample_Size))}
    p <- ggplot(data.frame(x = c(-4, 12)), aes(x = x)) +
      #ylim(c(0,0.6)) +
      stat_function(fun = shifted, col = "red") + 
      stat_function(fun = standard)
      if(input$Type_I == T) {
        p <- p + stat_function(fun = dnorm, args = list(mean = 0, sd = input$Std / sqrt(input$Sample_Size)), 
                    xlim = c(qnorm(1 - input$Type_1_Error, mean = 0, sd = input$Std / sqrt(input$Sample_Size)), 12), 
                    geom = "area",
                    alpha = .5)
      }
      if(input$Power == T) {
          p <- p + stat_function(fun = shifted, 
                                 xlim = c(qnorm(1 - input$Type_1_Error, mean = 0, sd = input$Std / sqrt(input$Sample_Size)), 12), 
                                 geom = "area",
                                 col = "red",
                                 fill = "red",
                                 alpha = .5)
      }
    if(input$Type_II == T) {
      p <- p + stat_function(fun = shifted, 
                             xlim = c(-4, qnorm(1 - input$Type_1_Error, mean = 0, sd = input$Std / sqrt(input$Sample_Size))), 
                             geom = "area",
                             col = "green",
                             fill = "green",
                             alpha = .5)
    }
    p
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

