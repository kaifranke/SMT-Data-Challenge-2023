library(shinydashboard)
library(rsconnect)
library(shiny)
library(shinythemes)
library(shinyjs)
library(tidyverse)
library(remotes)
library(sportyR)
library(shinyWidgets)



# Import data

# The predictions joined with the original SBsimple.csv
model_predictions <- read_csv("all_data_v1.csv")
# The coefficients of the final mode (Haven't decided on the model)
model_coefficients <- read_csv("final_model_v1.csv")


ui <- fluidPage(
  
 
  # Title and theme
  titlePanel(
    h1("Stolen Base Predictor", align = "center", style = "font-family: 'Times', serif; font-weight: 1000px; font-size: 50px; line-height: 1; color:black;")
  ),
  theme = shinythemes::shinytheme("journal"),
  
  
  # Setting the background color to cream here
  setBackgroundColor(
    color = c("antiquewhite"),
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
                      
                      sliderInput("x_coordinate",
                                  "Horizontal Pitch Location (ft):",
                                  min = -2,
                                  max = 2,
                                  value = 0,
                                  step = 0.01)
                      ,
                      sliderInput("y_coordinate",
                                  "Vertical Pitch Location (ft):",
                                  min = 0,
                                  max = 4.5,
                                  value = 2.5,
                                  step = 0.01)
                      ,
                      sliderInput("leadoff",
                                  "Runner Lead (ft):",
                                  min = 0,
                                  max = 90,
                                  value = 20,
                                  step = 0.1)
                      ,
                      sliderInput("popTime",
                                  "Pop Time (sec):",
                                  min = 1.5,
                                  max = 3.5,
                                  value = 2.0,
                                  step = 0.01)
                      ,
                      sliderInput("timeToPlate",
                                  "Time to Plate (sec):",
                                  min = 4.5,
                                  max = 5.5,
                                  value = 5,
                                  step = 0.01)
                      ,
                      sliderInput("maxSpeed",
                                  "Max Spd (mph):",
                                  min = 20,
                                  max = 30,
                                  value = 25,
                                  step = 0.05)
               ),
               
               mainPanel(
                 
                 fluidRow(
                   # The strike zone with a movable pitch
                   column(5, align = "center",
                          h4(strong("Pitch Location"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 30px; line-height: 1; color:dimgrey;"),
                          plotOutput("EditBallLocation", height = 300, width = 250)
                   ),
                   # The field with a movable player's lead
                   column(5, align = "center",
                          h4(strong("Lead"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 30px; line-height: 1; color:dimgrey;"),
                          plotOutput("EditLead", height = 300, width = 300)
                   ),
                   # The final SB probablity
                   column(2, align = "center",
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          h4(strong("SB Prob"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 50px; line-height: 1; color:black;"),
                          br(),
                          h5(textOutput("modelOutput"), align = "center", style = "font-family: 'Times', serif; font-weight: 250px; font-size: 40px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;")
                   )
                 ),
                 fluidRow(
                   # Pop Time
                   column(3, align = "center",
                          br(),
                          br(),
                          h4(strong("Pop Time"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 30px; line-height: 1; color:dimgrey;"),
                          br(),
                          h5(textOutput("popTime"), align = "center", style = "font-family: 'Times', serif; font-weight: 250px; font-size: 40px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:red;")
                   ),
                   # Time to Plate
                   column(4, align = "center",
                          br(),
                          br(),
                          h4(strong("Time to Plate"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 30px; line-height: 1; color:dimgrey;"),
                          br(),
                          h5(textOutput("timeToPlate"), align = "center", style = "font-family: 'Times', serif; font-weight: 250px; font-size: 40px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:red;")
                   ),
                   # Max Speed
                   column(3, align = "center",
                          br(),
                          br(),
                          h4(strong("Max Speed"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 30px; line-height: 1; color:dimgrey;"),
                          br(),
                          h5(textOutput("maxSpeed"), align = "center", style = "font-family: 'Times', serif; font-weight: 250px; font-size: 40px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:red;")
                   )
                 )
               )
             )
    ),

    # Second tab is where there will be various plots describing how the individual features affect SB % both in real life and in the model    
    tabPanel("The Model",
             fluidRow(
               column(4, align = "center",
                      plotOutput("cather_ball_acquistion", height = 300, width = 250)
               ),
               column(4, align = "center",
                      plotOutput("cather_ball_acquistion_prediction", height = 300, width = 250)
               ),
               column(4, align = "center",
                      plotOutput("allTimeEvents", height = 275, width = 300)
               )
             )
    )
    
    
  )
  
  
  
)     




server <- function(input, output, session) {
  
  output$popTime = renderText(paste0(input$popTime, " sec"))
  
  output$timeToPlate = renderText(paste0(input$timeToPlate, " sec"))
  
  output$maxSpeed = renderText(paste0(input$maxSpeed, " mph"))
  
  output$modelOutput = reactive({
    
    # Not sure if this logic is right - Was having issues with final probablity #
    # 1 for intercept, 0s for exchange, timeToThrow, and y-coordinate in data (depth)
      # Will obviously have to change b/c messes up model (either add them to sliders or eliminate from model)
    # Also multiply by 1000 here for times because the sliders are scaled down to seconds
    
    model_coefficients["inputs"] = c(1, 1000 * input$popTime, 0, 0, 1000 * input$timeToPlate, input$leadoff, 90 - input$leadoff, input$maxSpeed, input$x_coordinate, 0, input$y_coordinate)
    
    new_table <- model_coefficients %>%
      mutate(product = x * inputs)
    
    prob <- round(100 * sum(new_table$product), digits = 2)
    
    paste0(prob, " %")
  })
  
  
  # Plot for generating catcher ball acquistion in 'Model" tab
  output$cather_ball_acquistion = renderPlot({
    
    # dimensions for home plate
    df <- data.frame(x = c(-0.85, -1, 0, 1, 0.85), y = c(0, -.2, -.45, -.2, 0))
    
    
    ggplot((filter(model_predictions, event == 'ball acquired' & playerAct == 'C')), aes(ball_x, ball_z)) +
      geom_polygon(df, mapping=aes(x = x, y = y), fill="white") +
      # color by SB (0 or 1)
      geom_point(mapping = aes(color = SB), size = 5) +
      scale_color_gradient(low = "red", high = "blue") +
      # creating strike zone here
      geom_segment(aes(x = -0.85, y = 1.6, xend = -0.85, yend = 3.5), linewidth = 1) + 
      geom_segment(aes(x = -0.85, y = 1.6, xend = 0.85, yend = 1.6), linewidth = 1) + 
      geom_segment(aes(x = 0.85, y = 3.5, xend = -0.85, yend = 3.5), linewidth = 1) + 
      geom_segment(aes(x = 0.85, y = 3.5, xend = 0.85, yend = 1.6), linewidth = 1) +
      xlim(-3.5, 3.5) +
      ylim(-1,5) +
      labs(title = "Catcher Ball Acquistion (Actual)") +
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.title = element_text(face="bold")) +
      theme(plot.title = element_text(color="black"))
 
  }, bg = "antiquewhite")
  
  
  
  # Plot for generating catcher ball acquistion prediction in 'Model" tab
  output$cather_ball_acquistion_prediction = renderPlot({
    
    # dimensions for home plate
    df <- data.frame(x = c(-0.85, -1, 0, 1, 0.85), y = c(0, -.2, -.45, -.2, 0))
    
    
    ggplot((filter(model_predictions, event == 'ball acquired' & playerAct == 'C')), aes(ball_x, ball_z)) +
      geom_polygon(df, mapping=aes(x = x, y = y), fill="white") +
      # color by predicted SB (0 to 1)
      geom_point(mapping = aes(color=ACV), size = 5) +    # randomly choosing ACV here
      scale_color_gradient(low = "red", high = "blue") +
      # creating strike zone here
      geom_segment(aes(x = -0.85, y = 1.6, xend = -0.85, yend = 3.5), linewidth = 1) + 
      geom_segment(aes(x = -0.85, y = 1.6, xend = 0.85, yend = 1.6), linewidth = 1) + 
      geom_segment(aes(x = 0.85, y = 3.5, xend = -0.85, yend = 3.5), linewidth = 1) + 
      geom_segment(aes(x = 0.85, y = 3.5, xend = 0.85, yend = 1.6), linewidth = 1) +
      xlim(-3.5, 3.5) +
      ylim(-1,5) +
      labs(title = "Catcher Ball Acquistion (Model)") +
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.title = element_text(face="bold")) +
      theme(plot.title = element_text(color="black")) +
      labs(color='SB%')
    
  }, bg = "antiquewhite")
  
  
  # Plot for a simple representation of the timing events v. SB in 'Model' tab
  output$allTimeEvents = renderPlot({
    
    temp <- model_predictions %>% 
      filter(event == 'ball acquired' & playerAct == 'C') %>%
      dplyr::select(popTime, timeToPlate, SB) %>%
      # Scaling down to seconds
      mutate(popTime = popTime/1000,
             timeToPlate = timeToPlate/1000) %>%
      pivot_longer(cols = c("popTime", "timeToPlate"),
                            names_to = "feature",
                            values_to ='time')
      
    
    ggplot(temp, aes(time, SB)) +
      geom_point(mapping = aes(color = feature), size = 4) +
      labs(title = "Times v. SB", subtitle = "(2 outliers eliminated)",
           x = "Time (Seconds)",
           y = "SB") +
      xlim(0.35, 4) + 
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.subtitle = element_text(hjust = 0.5)) +
      theme(plot.title = element_text(face="bold")) +
      theme(plot.title = element_text(color="black")) +
      scale_y_continuous(breaks = seq(0, 1, 1))
    
  }, bg = "antiquewhite")
  
  
  # For the changing strike zone in 'Testing' tab
  ball_location = reactive({

    ball_location <- data.frame(input$x_coordinate, input$y_coordinate)
    colnames(ball_location) <- c("x", "y")
    ball_location

  })

  
  # Plot for the changing strike zone in 'Testing' tab
  output$EditBallLocation = renderPlot({
    
    # dimensions for home plate
    df <- data.frame(x = c(-0.85, -1, 0, 1, 0.85), y = c(0, -.2, -.45, -.2, 0))
    
    
    ggplot(ball_location(), aes(x = x, y = y)) +
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
    
  }, bg = "antiquewhite")
  
  
  # For the changing runner lead in 'Testing' tab
  runner_location = reactive({
    
    x = 63
    y = 63
    runner = input$leadoff
    
    diff <- sqrt(runner^2 / 2)
    
    runner_location <- data.frame(x - diff, y + diff)
    colnames(runner_location) <- c("x", "y")
    runner_location
    
  })
  
  
  # Baseball field with runner in 'Testing' tab
  output$EditLead = renderPlot({
    
    geom_baseball(league = "MLB", rotation = 0, display_range = "infield") +
      geom_point(runner_location(), mapping = aes(x = x, y = y), size = 5, color = "red") + 
      labs(caption = "Field generated by sportyR")
    
  }, bg = "antiquewhite")

  
}


shinyApp(ui = ui, server = server)


#runApp("Baseball Stuff/Arsenal App")


