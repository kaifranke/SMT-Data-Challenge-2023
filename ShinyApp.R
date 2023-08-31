library(shinydashboard)
library(rsconnect)
library(shiny)
library(shinythemes)
library(shinyjs)
library(tidyverse)
library(remotes)
library(sportyR)
library(shinyWidgets)
library(gganimate)
library(png)
library(gifski)



# Import data
model1 <- read.csv("model1_coefficients.csv")
model2 <- read.csv("model2_coefficients.csv")
model3 <- read.csv("model3_coefficients.csv")
model4 <- read.csv("model4_coefficients.csv")

# Timestamps and Locations for All Fielders
locations <- read_csv("SBmove.csv") %>%
  na.omit()

# Timestamps for Key Events
key_moments <- read_csv("SBsimple.csv") %>%
  merge(locations, by = c('game_str', 'play_id', 'timestamp'))


ui <- fluidPage(
  
  
  # Title and theme
  titlePanel(
    h1("Evaluating Player Relationships in Stolen Base Defense", align = "center", style = "font-family: 'Times', serif; font-weight: 1000px; font-size: 50px; text-shadow: 2px 2px 2px #aaa; line-height: 1; color:black"),
  ),
  titlePanel(
    h6("SMT 2023 Data Challenge", align = "center", style = "font-family: 'Times', serif; font-weight: 1000px; font-size: 20px; line-height: 1; color:black"),
  ),
  theme = shinythemes::shinytheme("journal"),
  
  
  # Setting the background color to cream here
  setBackgroundColor(
    color = c("bisque"),
    shinydashboard = FALSE
  ),
  
  
  # First tab is where we allow the user to play with the parameters to determine SB %
  # Left out exchange/timeToThrow here because it's the same as popTime
  # Also left out y-coordinate in data (depth), but should probably add that in (but is hard to visualize)
  # Finally, also left out rDist2B
  tabsetPanel(
    tabPanel("Testing Scenarios", 
             fluidRow(
               column(2,
                      
                      selectInput('moment_in_time',
                                  'Select Moment in Time:',
                                  choices = c("Pitch Release", "Catcher Retrieval", "Catcher Release", "Middle Infielder Retrieval"))
                      ,
                      
                      # Inputs for Time of Pitch
                      conditionalPanel(
                        "input.moment_in_time == 'Pitch Release'",
                        sliderInput("maxSpeed1",
                                    "Max Spd (ft/sec):",
                                    min = 20,
                                    max = 30,
                                    value = 28.5,
                                    step = 0.05)
                      ),
                      
                      # Inputs for Time of Catch
                      conditionalPanel(
                        "input.moment_in_time == 'Catcher Retrieval'",
                        sliderInput("leadoff2",
                                    "Lead off of First (ft):",
                                    min = 0,
                                    max = 90,
                                    value = 17.8,
                                    step = 0.1)
                        ,
                        sliderInput("x_coordinate2",
                                    "Horizontal Pitch Location (ft):",
                                    min = -2,
                                    max = 2,
                                    value = -1.32,
                                    step = 0.01)
                        ,
                        sliderInput("y_coordinate2",
                                    "Vertical Pitch Location (ft):",
                                    min = 0,
                                    max = 4.5,
                                    value = 1.02,
                                    step = 0.01)
                        
                      ),
                      
                      # Inputs for Time of Throw
                      conditionalPanel(
                        "input.moment_in_time == 'Catcher Release'",
                        sliderInput("rDist3",
                                    "Distance to 2nd (ft):",
                                    min = 0,
                                    max = 90,
                                    value = 44.2,
                                    step = 0.1)
                        ,
                        sliderInput("x_coordinate3",
                                    "Horizontal Throw Location (ft):",
                                    min = -3,
                                    max = 3,
                                    value = -0.02,
                                    step = 0.01)
                        ,
                        sliderInput("y_coordinate3",
                                    "Vertical Throw Location (ft):",
                                    min = 4,
                                    max = 7,
                                    value = 5.97,
                                    step = 0.01)
                        ,
                        sliderInput("timeOfThrow3",
                                    "Time of Throw (sec):",
                                    min = 0.8,
                                    max = 2,
                                    value = 1.2,
                                    step = 0.01)
                        ,
                        sliderInput("exchange3",
                                    "Exchange (sec):",
                                    min = 0.5,
                                    max = 1.5,
                                    value = 0.8,
                                    step = 0.01)
                      ),
                      
                      # Inputs for Time of Fielder Catch
                      conditionalPanel(
                        "input.moment_in_time == 'Middle Infielder Retrieval'",
                        sliderInput("rDist4",
                                    "Distance to 2nd (ft):",
                                    min = 0,
                                    max = 90,
                                    value = 9.8,
                                    step = 0.1)
                        ,
                        sliderInput("x_coordinate4",
                                    "Horizontal Catch Location (ft):",
                                    min = -5,
                                    max = 5,
                                    value = 4.43,
                                    step = 0.01)
                        ,
                        sliderInput("y_coordinate4",
                                    "Vertical Catch Location (ft):",
                                    min = -0.25,
                                    max = 7,
                                    value = -0.19,
                                    step = 0.01)
                        ,
                        sliderInput("timeToPlate4",
                                    "Time to Plate (sec):",
                                    min = 0.4,
                                    max = 0.6,
                                    value = 0.5,
                                    step = 0.01)
                        ,
                        sliderInput("timeOfThrow4",
                                    "Time of Throw (sec):",
                                    min = 0.8,
                                    max = 2,
                                    value = 1.2,
                                    step = 0.01)
                        ,
                        sliderInput("exchange4",
                                    "Exchange (sec):",
                                    min = 0.5,
                                    max = 1.5,
                                    value = 0.8,
                                    step = 0.01)
                        ,
                        sliderInput("depth4",
                                    "Infielder Catch Depth (0 ft is middle of 2nd base):",
                                    min = -5,
                                    max = 5,
                                    value = 1,
                                    step = 0.1)
                        
                      ),
                      
               ),
               
               mainPanel(
                 
                 conditionalPanel(
                   "input.moment_in_time == 'Pitch Release'",
                   fluidRow(
                     # Max Speed
                     column(3, align = "center",
                            br(),
                            br(),
                            h4(strong("Max Speed"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 30px; line-height: 1; color:dimgrey;"),
                            br(),
                            h5(textOutput("maxSpeed1"), align = "center", style = "font-family: 'Times', serif; font-weight: 250px; font-size: 40px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:red;")
                     ),
                     # Filler Here
                     column(7
                            
                     ),
                     # The final SB probablity
                     column(2, align = "center",
                            br(),
                            h4(strong("SB Prob"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 50px; line-height: 1; color:black;"),
                            br(),
                            h5(textOutput("modelOutput1"), align = "center", style = "font-family: 'Times', serif; font-weight: 250px; font-size: 40px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;")
                     )
                   )
                 ),
                 
                 
                 
                 conditionalPanel(
                   "input.moment_in_time == 'Catcher Retrieval'",
                   fluidRow(
                     # The field with a movable player's lead
                     column(5, align = "center",
                            h4(strong("Distance to 2B"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 30px; line-height: 1; color:dimgrey;"),
                            plotOutput('EditLead2', height = 300, width = 300)
                     ),
                     # Pitch Location
                     column(5, align = "center",
                            h4(strong("Pitch Location"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 30px; line-height: 1; color:dimgrey;"),
                            plotOutput('EditBallLocation2', height = 300, width = 250)
                     ),
                     # The final SB probablity
                     column(2, align = "center",
                            br(),
                            h4(strong("SB Prob"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 50px; line-height: 1; color:black;"),
                            br(),
                            h5(textOutput("modelOutput2"), align = "center", style = "font-family: 'Times', serif; font-weight: 250px; font-size: 40px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;")
                     )
                   )
                 ),
                 
                 
                 
                 conditionalPanel(
                   "input.moment_in_time == 'Catcher Release'",
                   fluidRow(
                     # The field with a movable player's lead
                     column(5, align = "center",
                            h4(strong("Distance to 2B"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 30px; line-height: 1; color:dimgrey;"),
                            plotOutput('EditLead3', height = 300, width = 300)
                     ),
                     # Throw Location
                     column(5, align = "center",
                            h4(strong("Catcher Throw Location"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 30px; line-height: 1; color:dimgrey;"),
                            plotOutput('EditBallLocation3', height = 300, width = 200)
                     ),
                     # The final SB probablity
                     column(2, align = "center",
                            br(),
                            h4(strong("SB Prob"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 50px; line-height: 1; color:black;"),
                            br(),
                            h5(textOutput("modelOutput3"), align = "center", style = "font-family: 'Times', serif; font-weight: 250px; font-size: 40px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;")
                     )
                   ),
                   fluidRow(
                     # Exchange
                     column(5, align = "center",
                            br(),
                            br(),
                            h4(strong("Exchange Time"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 30px; line-height: 1; color:dimgrey;"),
                            br(),
                            h5(textOutput("exchange3"), align = "center", style = "font-family: 'Times', serif; font-weight: 250px; font-size: 40px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:red;")
                     ),
                     # Time of Throw
                     column(5, align = "center",
                            br(),
                            br(),
                            h4(strong("Time of Throw"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 30px; line-height: 1; color:dimgrey;"),
                            br(),
                            h5(textOutput("timeOfThrow3"), align = "center", style = "font-family: 'Times', serif; font-weight: 250px; font-size: 40px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:red;")
                     )
                     
                   )
                 ),
                 
                 
                 conditionalPanel(
                   "input.moment_in_time == 'Middle Infielder Retrieval'",
                   fluidRow(
                     # The field with a movable player's lead
                     column(5, align = "center",
                            h4(strong("Distance to 2B"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 30px; line-height: 1; color:dimgrey;"),
                            plotOutput('EditLead4', height = 300, width = 300)
                     ),
                     # Catch Location
                     column(5, align = "center",
                            h4(strong("Fielder Catch Location (Home Plate View)"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 30px; line-height: 1; color:dimgrey;"),
                            plotOutput('EditBallLocation4', height = 300, width = 400)
                     ),
                     # The final SB probablity
                     column(2, align = "center",
                            br(),
                            h4(strong("SB Prob"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 50px; line-height: 1; color:black;"),
                            br(),
                            h5(textOutput("modelOutput4"), align = "center", style = "font-family: 'Times', serif; font-weight: 250px; font-size: 40px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;")
                     )
                   ),
                   fluidRow(
                     # Exchange
                     column(3, align = "center",
                            br(),
                            br(),
                            h4(strong("Exchange Time"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 30px; line-height: 1; color:dimgrey;"),
                            br(),
                            h5(textOutput("exchange4"), align = "center", style = "font-family: 'Times', serif; font-weight: 250px; font-size: 40px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:red;")
                     ),
                     # Time of Throw
                     column(4, align = "center",
                            br(),
                            br(),
                            h4(strong("Time of Throw"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 30px; line-height: 1; color:dimgrey;"),
                            br(),
                            h5(textOutput("timeOfThrow4"), align = "center", style = "font-family: 'Times', serif; font-weight: 250px; font-size: 40px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:red;")
                     ),
                     # Time to Plate
                     column(3, align = "center",
                            br(),
                            br(),
                            h4(strong("Time to Plate"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 30px; line-height: 1; color:dimgrey;"),
                            br(),
                            h5(textOutput("timeToPlate4"), align = "center", style = "font-family: 'Times', serif; font-weight: 250px; font-size: 40px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:red;")
                     )
                   ),
                   fluidRow(
                     # Catch Depth
                     column(10, align = "center",
                            br(),
                            br(),
                            h4(strong("Fielder Catch Depth (View from First Base Side)"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 30px; line-height: 1; color:dimgrey;"),
                            plotOutput('CatchDepth4', height = 140, width = 200)
                     )
                     
                   )
                 )
                 
               )
             )
    ),
    
    # Second tab is where there will be various plots describing how the individual features affect SB % both in real life and in the model    
    tabPanel("Animated Plays",
             fluidRow(
               # Inputs for Animation
               column(2, 
                      checkboxInput('SB', 'SB Occurred', TRUE)
                      ,
                      selectInput('game',
                                  'Pick the Game:',
                                  choices = unique(locations$game_str))
                      ,
                      selectInput('play',
                                  'Pick the Play From that Game:',
                                  choices = unique(locations$play_id))
                      ,
                      br(),
                      h5("**Disclaimer: Some plays may take consider time to load", align = "left", style = "font-family: 'Times', serif; font-weight: 250px; font-size: 15px; line-height: 1; color:black;"),
                      br(),
                      h5("**Disclaimer: Some plays have missing data points and will jump around or end early", align = "left", style = "font-family: 'Times', serif; font-weight: 250px; font-size: 15px; line-height: 1; color:black;")
                      
               ),
               # Play Animation
               column(6, align = "center",
                      h4(strong("Play Animation"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 40px; line-height: 1; color:black;"),
                      br(),
                      h4(textOutput('stolenBaseText'), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 20px; line-height: 1; color:dimgrey;"),
                      br(),
                      imageOutput('animate', height = 500, width = 500)
               ),
               # Displayed Percentages for that play
               column(2, 
                      br(),
                      br(),
                      br(),
                      br(),
                      h4(strong("Pitch Release SB Prob"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 25px; line-height: 1; color:dimgrey;"),
                      h5(textOutput("pitch_release_model1"), align = "center", style = "font-family: 'Times', serif; font-weight: 250px; font-size: 35px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;"),
                      br(),
                      h4(strong("Catcher Retrieval SB Prob"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 25px; line-height: 1; color:dimgrey;"),
                      h5(textOutput("catcher_retrieval_model2"), align = "center", style = "font-family: 'Times', serif; font-weight: 250px; font-size: 35px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;"),
                      br(),
                      h4(strong("Catcher Throw SB Prob"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 25px; line-height: 1; color:dimgrey;"),
                      h5(textOutput("catcher_throw_model3"), align = "center", style = "font-family: 'Times', serif; font-weight: 250px; font-size: 35px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;"),
                      br(),
                      h4(strong("Middle Infielder Retrieval SB Prob"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 25px; line-height: 1; color:dimgrey;"),
                      h5(textOutput("middle_infielder_retrieval_model4"), align = "center", style = "font-family: 'Times', serif; font-weight: 250px; font-size: 35px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;"),
                      br(),
                      h4(strong("End Result SB Prob (0 or 100)"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 25px; line-height: 1; color:dimgrey;"),
                      h5(textOutput("end_result"), align = "center", style = "font-family: 'Times', serif; font-weight: 250px; font-size: 35px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;")
               )
               
             )
    )
    
    
  )
  
  
  
)     




server <- function(input, output, session) {
  
  # Update Choices for the Animated Play based on SB
  
  observe({
    x <- as.numeric(input$SB)
    new_options <- filter(locations, SB == x)
    
    updateSelectInput(session, "game",
                      choices = new_options$game_str
    )
    
  })
  
  
  
  # Update Choices for the Animated Play based on SB and Game
  
  observe({
    x <- input$game
    y <- as.numeric(input$SB)
    new_options <- filter(locations, game_str == x & SB == y)
    
    updateSelectInput(session, "play",
                      choices = new_options$play_id
    )
    
  })
  
  
  # Changing output of Stolen Base Success
  stolenBase = reactive({
    
    stolenBase = "Yes"
    
    if(input$SB == TRUE){
      stolenBase = "Yes"
    }
    else {
      stolenBase = "No"
    }
    
    stolenBase
    
  })
  
  
  # Creating text for Stolen Base result
  output$stolenBaseText = renderText(paste0("Stolen Base: ", stolenBase()))
  
  
  # Various maxSpeed inputs
  output$maxSpeed1 = renderText(paste0(input$maxSpeed1, " ft/sec"))
  
  
  # Various exchange inputs
  output$exchange3 = renderText(paste0(input$exchange3, " sec"))
  
  output$exchange4 = renderText(paste0(input$exchange4, " sec"))
  
  
  # Various timeOfThrow inputs
  output$timeOfThrow3 = renderText(paste0(input$timeOfThrow3, " sec"))
  
  output$timeOfThrow4 = renderText(paste0(input$timeOfThrow4, " sec"))
  
  
  # Various timetoPlate inputs
  output$timeToPlate4 = renderText(paste0(input$timeToPlate4, " sec"))


  
  
  output$modelOutput1 = reactive({
    
    # 1 for intercept 
    
    model1["inputs"] = c(1, input$maxSpeed1)
    
    new_table <- model1 %>%
      mutate(product = model1[,2] * inputs)
    
    sum_products <- sum(new_table$product)
    
    # Log Odds
    prob <- round(100 * 1 / (1 + exp(-(sum_products))), digits = 2)
    
    if(prob > 100){
      prob = 100
    }
    else if (prob < 0){
      prob = 0
    }
    
    paste0(prob, "%")
  })
  
  
  output$modelOutput2 = reactive({
    
    # 1 for intercept and multiply by 1000 here for times because the sliders are scaled down to seconds
    
    # 0s are included where the coefficient estimate is 0
    
    # Order:  Intercept, timetoPlate, leadoff, rDist2B, maxSpeed, ball_x, ball_y, ball_z
    
    model2["inputs"] = c(1, 0, input$leadoff2, 0, 0, input$x_coordinate2, 0, input$y_coordinate2)
    
    new_table <- model2 %>%
      mutate(product = estimate * inputs)
    
    sum_products <- sum(new_table$product)
    
    # Log Odds
    prob <- round(100 * 1 / (1 + exp(-(sum_products))), digits = 2)
    
    if(prob > 100){
      prob = 100
    }
    else if (prob < 0){
      prob = 0
    }
    
    paste0(prob, "%")
  })
  
  
  output$modelOutput3 = reactive({
    
    # 1 for intercept and multiply by 1000 here for times because the sliders are scaled down to seconds
    
    # 0s are included where the coefficient estimate is 0
    
    # Order:  Intercept, exchange, timeOfThrow, timetoPlate, rDist2B, maxSpeed, ball_x, ball_y, ball_z
    
    model3["inputs"] = c(1, 1000 * input$exchange3, 1000 * input$timeOfThrow3, 0, input$rDist3, 0, input$x_coordinate3, 0, 0)
    
    new_table <- model3 %>%
      mutate(product = estimate * inputs)
    
    sum_products <- sum(new_table$product)
    
    # Log Odds
    prob <- round(100 * 1 / (1 + exp(-(sum_products))), digits = 2)
    
    if(prob > 100){
      prob = 100
    }
    else if (prob < 0){
      prob = 0
    }
    
    paste0(prob, "%")
  })
  
  
  output$modelOutput4 = reactive({
    
    # 1 for intercept and multiply by 1000 here for times because the sliders are scaled down to seconds
    
    # 0s are included where the coefficient estimate is 0
    
    # Order:  Intercept, exchange, timeOfThrow, timetoPlate, rDist2B, maxSpeed, ball_x, ball_y, ball_z
    
    # + 127.28125 on catch depth for second base
    
    model4["inputs"] = c(1, 1000 * input$exchange4, 1000 * input$timeOfThrow4, 1000 * input$timeToPlate4, input$rDist4, input$x_coordinate4, input$depth4 + 127.28125, input$y_coordinate4)
    
    new_table <- model4 %>%
      mutate(product = estimate * inputs)
    
    sum_products <- sum(new_table$product)
    
    # Log Odds
    prob <- round(100 * 1 / (1 + exp(-(sum_products))), digits = 2)
    
    if(prob > 100){
      prob = 100
    }
    else if (prob < 0){
      prob = 0
    }
    
    paste0(prob, "%")
  })
  
  
  
  
  
  # For the changing strike zone in 2nd model
  ball_location2 = reactive({
    
    ball_location2 <- data.frame(input$x_coordinate2, input$y_coordinate2)
    colnames(ball_location2) <- c("x", "y")
    ball_location2
    
  })
  
  
  # Plot for the changing strike zone in 2nd model
  output$EditBallLocation2 = renderPlot({
    
    # dimensions for home plate
    df <- data.frame(x = c(-0.85, -1, 0, 1, 0.85), y = c(0, -.2, -.45, -.2, 0))
    
    
    ggplot(ball_location2(), aes(x = x, y = y)) +
      geom_polygon(df, mapping=aes(x = x, y = y), fill="white") +
      geom_point(size = 5, color = "red") +
      # creating strike zone here
      geom_segment(aes(x = -0.85, y = 1.6, xend = -0.85, yend = 3.5), linewidth = 1) + 
      geom_segment(aes(x = -0.85, y = 1.6, xend = 0.85, yend = 1.6), linewidth = 1) + 
      geom_segment(aes(x = 0.85, y = 3.5, xend = -0.85, yend = 3.5), linewidth = 1) + 
      geom_segment(aes(x = 0.85, y = 3.5, xend = 0.85, yend = 1.6), linewidth = 1) +
      xlim(-3.5, 3.5) +
      ylim(-1,5) +
      theme_void()
    
  }, bg = "bisque")
  
  
  
  # For the changing throw location in 3rd model
  ball_location3 = reactive({
    
    ball_location3 <- data.frame(input$x_coordinate3, input$y_coordinate3)
    colnames(ball_location3) <- c("x", "y")
    ball_location3
    
  })
  
  
  # Plot for the changing throw location in 3rd model
  output$EditBallLocation3 = renderPlot({
    
    # dimensions for home plate
    df <- data.frame(x = c(-0.85, -1, 0, 1, 0.85), y = c(0, -.2, -.45, -.2, 0))
    
    
    ggplot(ball_location3(), aes(x = x, y = y)) +
      geom_polygon(df, mapping=aes(x = x, y = y), fill="white") +
      geom_point(size = 5, color = "red") +
      # creating strike zone here
      geom_segment(aes(x = -0.85, y = 1.6, xend = -0.85, yend = 3.5), linewidth = 1) + 
      geom_segment(aes(x = -0.85, y = 1.6, xend = 0.85, yend = 1.6), linewidth = 1) + 
      geom_segment(aes(x = 0.85, y = 3.5, xend = -0.85, yend = 3.5), linewidth = 1) + 
      geom_segment(aes(x = 0.85, y = 3.5, xend = 0.85, yend = 1.6), linewidth = 1) +
      xlim(-3.5, 3.5) +
      ylim(-1,7) +
      theme_void()
    
  }, bg = "bisque")
  
  
  
  # For the changing catch location in 4th model
  ball_location4 = reactive({
    
    ball_location4 <- data.frame(input$x_coordinate4, input$y_coordinate4)
    colnames(ball_location4) <- c("x", "y")
    ball_location4
    
  })
  
  
  # Plot for the changing catch location in 4th model
  output$EditBallLocation4 = renderPlot({
    
    # dimensions for second plate
    df <- data.frame(x = c(0, 0.884, 0, -0.884), y = c(0, 0.171, 0.342, 0.171))
    
    dftop <- data.frame(x = c(0, 0.884, 0, -0.884), y = c(0.1, 0.271, 0.442, 0.271))
    
    dfconnect <- data.frame(x = c(0.884, 0.884, -0.884, -0.884), y = c(0.171, 0.321, 0.321, 0.171))
    
    
    ggplot(ball_location4(), aes(x = x, y = y)) +
      geom_polygon(df, mapping=aes(x = x, y = y), fill="white") +
      geom_polygon(dftop, mapping=aes(x = x, y = y), fill="white") +
      geom_polygon(dfconnect, mapping=aes(x = x, y = y), fill="white") +
      geom_point(size = 5, color = "red") +
      xlim(-5, 5) +
      ylim(-0.25,7) +
      theme_void()
    
  }, bg = "bisque")
  
  
  
  
  # For the changing catch depth in 4th model
  ball_location_depth4 = reactive({
    
    ball_location_depth4 <- data.frame(input$depth4, input$y_coordinate4)
    colnames(ball_location_depth4) <- c("depth", "y")
    ball_location_depth4
    
  })
  
  
  # Plot for the catch depth in 4th model
  output$CatchDepth4 = renderPlot({
    
    
    # dimensions for second plate
    df1 <- data.frame(x = c(0, 0.884, 0, -0.884), y = c(0, 0.221, 0.442, 0.221))
    
    dftop1 <- data.frame(x = c(0, 0.884, 0, -0.884), y = c(0.3, 0.521, 0.742, 0.521))
    
    dfconnect1 <- data.frame(x = c(0.884, 0.884, -0.884, -0.884), y = c(0.221, 0.521, 0.521, 0.221))
    
    
    ggplot(ball_location_depth4(), aes(x = depth, y = y)) +
      geom_polygon(df1, mapping=aes(x = x, y = y), fill="white") +
      geom_polygon(dftop1, mapping=aes(x = x, y = y), fill="white") +
      geom_polygon(dfconnect1, mapping=aes(x = x, y = y), fill="white") +
      geom_point(size = 3, color = "red") +
      xlim(-5, 5) +
      ylim(-0.25, 7.25) +
      theme_void()
    
  }, bg = "bisque")
  
  
  
  
  # For the changing runner 2B distance in 2nd model
  runner_location2 = reactive({
    
    x = 63.64
    y = 63.64
    runner = input$leadoff2
    
    diff <- sqrt(runner^2 / 2)
    
    runner_location2 <- data.frame(x - diff, y + diff)
    colnames(runner_location2) <- c("x", "y")
    runner_location2
    
  })
  
  
  # For the changing runner 2B distance in 3rd model
  runner_location3 = reactive({
    
    x = 0
    y = 127.3
    runner = input$rDist3
    
    diff <- sqrt(runner^2 / 2)
    
    runner_location3 <- data.frame(x + diff, y - diff)
    colnames(runner_location3) <- c("x", "y")
    runner_location3
    
  })
  
  
  # For the changing runner 2B distance in 4th model
  runner_location4 = reactive({
    
    x = 0
    y = 127.3
    runner = input$rDist4
    
    diff <- sqrt(runner^2 / 2)
    
    runner_location4 <- data.frame(x + diff, y - diff)
    colnames(runner_location4) <- c("x", "y")
    runner_location4
    
  })
  
  
  # Baseball field with runner in 2nd model
  output$EditLead2 = renderPlot({
    
    geom_baseball(league = "MLB", rotation = 0, display_range = "infield") +
      geom_point(runner_location2(), mapping = aes(x = x, y = y), size = 5, color = "red") + 
      labs(caption = "Field generated by sportyR")
    
  }, bg = "bisque")
  
  
  # Baseball field with runner in 3rd model
  output$EditLead3 = renderPlot({
    
    geom_baseball(league = "MLB", rotation = 0, display_range = "infield") +
      geom_point(runner_location3(), mapping = aes(x = x, y = y), size = 5, color = "red") + 
      labs(caption = "Field generated by sportyR")
    
  }, bg = "bisque")
  
  
  # Baseball field with runner in 4th model
  output$EditLead4 = renderPlot({
    
    geom_baseball(league = "MLB", rotation = 0, display_range = "infield") +
      geom_point(runner_location4(), mapping = aes(x = x, y = y), size = 5, color = "red") + 
      labs(caption = "Field generated by sportyR")
    
  }, bg = "bisque")
  
  
  
  # The chosen play to animate
  play = reactive({
    
    locations %>%
      filter(game_str == input$game & play_id == input$play)
    
  })
  
  
  
  output$animate = renderImage({
    
    # Will prevent an error from showing until animation has loaded
    validate(
      need(nrow(play()) != 0, message = 'Loading Animation')
    )
    
    outfile <- tempfile(fileext='.gif')
    
    p <- geom_baseball(league = "MLB", rotation = 0, display_range = "infield") +
      geom_point(play(), mapping = aes(x = ball_position_x, y = ball_position_y), size = 2, color = "white") + 
      geom_point(play(), mapping = aes(x = fP_x, y = fP_y), size = 4, color = 'red') + 
      geom_point(play(), mapping = aes(x = fC_x, y = fC_y), size = 4, color = "red") + 
      geom_point(play(), mapping = aes(x = f2B_x, y = f2B_y), size = 4, color = "red") + 
      geom_point(play(), mapping = aes(x = fSS_x, y = fSS_y), size = 4, color = "red") +
      geom_point(play(), mapping = aes(x = f1BsRn_x, y = f1BsRn_y), size = 4, color = "blue") + 
      labs(caption = "Field generated by sportyR") +
      transition_time(timestamp) +
      labs(title = "Frame: {frame_time}") +
      theme(plot.background = element_rect(fill = "bisque"))
    
    
    anim_save("outfile.gif", animate(p)) # New
    
    
    list(src = "outfile.gif",
         contentType = 'image/gif'
         # width = 400,
         # height = 300,
         # alt = "This is alternate text"
    )}, deleteFile = TRUE)
  
  
  
  
  # The pitch release for the chosen play to animate
  output$pitch_release_model1 = reactive({
    
    # Will prevent an error from showing until input has loaded
    validate(
      need(nrow(play()) != 0, message = 'Loading %')
    )
    
    # Obtaining specific moment
    time <- key_moments %>%
      filter(game_str == play()$game_str & play_id == play()$play_id) %>%
      filter(playerAct == 'P' & event == 'pitch') %>%
      select(maxSpeed) %>%
      na.omit() %>%
      head(1)
    
    # If data is missing for that point
    if(nrow(time) == 0){
      return ("Missing Data")
    }
    
    # Now applying model1 to the information
    
    # 1 for intercept 
    
    model1["inputs"] = c(1, time$maxSpeed)
    
    new_table <- model1 %>%
      mutate(product = model1[,2] * inputs)
    
    sum_products <- sum(new_table$product)
    
    # Log Odds
    prob <- round(100 * 1 / (1 + exp(-(sum_products))), digits = 2)
    
    if(prob > 100){
      prob = 100
    }
    else if (prob < 0){
      prob = 0
    }
    
    paste0(prob, "%")
    
  })
  
  
  
  
  # The catcher retrieval for the chosen play to animate
  output$catcher_retrieval_model2 = reactive({
    
    # Will prevent an error from showing until input has loaded
    validate(
      need(nrow(play()) != 0, message = 'Loading %')
    )
    
    # Obtaining specific moment (and the first occurrence)
    time <- key_moments %>%
      filter(game_str == play()$game_str & play_id == play()$play_id) %>%
      filter(playerAct == 'C' & event == 'ball acquired') %>%
      select(leadoff, ball_x, ball_z) %>%
      na.omit() %>%
      head(1)
    
    # If data is missing for that point
    if(nrow(time) == 0){
      return ("Missing Data")
    }
    
    # Now applying model2 to the information
    
    
    # 1 for intercept 
    
    # 0s are included where the coefficient estimate is 0
    
    # Order:  Intercept, timetoPlate, leadoff, rDist2B, maxSpeed, ball_x, ball_y, ball_z 
    
    model2["inputs"] = c(1, 0, time$leadoff, 0, 0, time$ball_x, 0, time$ball_z)
    
    new_table <- model2 %>%
      mutate(product = estimate * inputs)
    
    sum_products <- sum(new_table$product)
    
    # Log Odds
    prob <- round(100 * 1 / (1 + exp(-(sum_products))), digits = 2)
    
    if(prob > 100){
      prob = 100
    }
    else if (prob < 0){
      prob = 0
    }
    
    paste0(prob, "%")
    
  })
  
  
  
  
  # The catcher throw for the chosen play to animate
  output$catcher_throw_model3 = reactive({
    
    # Will prevent an error from showing until input has loaded
    validate(
      need(nrow(play()) != 0, message = 'Loading %')
    )
    
    # Obtaining specific moment (and the first occurrence)
    time <- key_moments %>%
      filter(game_str == play()$game_str & play_id == play()$play_id) %>%
      filter(playerAct == 'C' & event == 'throw (ball-in-play)') %>%
      select(exchange, timeOfThrow, rDist2B, ball_x) %>%
      na.omit() %>%
      head(1)
    
    # If data is missing for that point
    if(nrow(time) == 0){
      return ("Missing Data")
    }
    
    # Now applying model2 to the information
    
    
    # 1 for intercept 
    
    # 0s are included where the coefficient estimate is 0
    
    # Order:  Intercept, exchange, timeOfThrow, timetoPlate, rDist2B, maxSpeed, ball_x, ball_y, ball_z
    
    model3["inputs"] = c(1, time$exchange, time$timeOfThrow, 0, time$rDist2B, 0, time$ball_x, 0, 0)
    
    new_table <- model3 %>%
      mutate(product = estimate * inputs)
    
    sum_products <- sum(new_table$product)
    
    # Log Odds
    prob <- round(100 * 1 / (1 + exp(-(sum_products))), digits = 2)
    
    if(prob > 100){
      prob = 100
    }
    else if (prob < 0){
      prob = 0
    }
    
    paste0(prob, "%")
    
  })
  
  
  
  
  # The middle infielder retrieval for the chosen play to animate
  output$middle_infielder_retrieval_model4 = reactive({
    
    # Will prevent an error from showing until input has loaded
    validate(
      need(nrow(play()) != 0, message = 'Loading %')
    )
    
    # Obtaining specific moment (and the first occurrence)
    time <- key_moments %>%
      filter(game_str == play()$game_str & play_id == play()$play_id) %>%
      filter((playerAct == 'SS' | playerAct == '2B') & event == 'ball acquired') %>%
      select(exchange, timeOfThrow, timeToPlate, rDist2B, ball_x, ball_y, ball_z) %>%
      na.omit() %>%
      head(1)
    
    # If data is missing for that point
    if(nrow(time) == 0){
      return ("Missing Data")
    }
    
    # Now applying model2 to the information
    
    
    # 1 for intercept 
    
    # 0s are included where the coefficient estimate is 0
    
    # Order:  Intercept, exchange, timeOfThrow, timetoPlate, rDist2B, maxSpeed, ball_x, ball_y, ball_z
    
    model4["inputs"] = c(1, time$exchange, time$timeOfThrow, time$timeToPlate, time$rDist2B, time$ball_x, time$ball_y, time$ball_z)
    
    new_table <- model4 %>%
      mutate(product = estimate * inputs)
    
    sum_products <- sum(new_table$product)
    
    # Log Odds
    prob <- round(100 * 1 / (1 + exp(-(sum_products))), digits = 2)
    
    if(prob > 100){
      prob = 100
    }
    else if (prob < 0){
      prob = 0
    }
    
    paste0(prob, "%")
    
  })
  
  
  
  
  output$end_result = reactive({
    
    # Will prevent an error from showing until input has loaded
    validate(
      need(nrow(play()) != 0, message = 'Loading %')
    )
    
    if(stolenBase() == 'Yes'){
      return ("100%")
    }
    else {
      return ("0%")
    }
    
  })

  
  
}


shinyApp(ui = ui, server = server)



