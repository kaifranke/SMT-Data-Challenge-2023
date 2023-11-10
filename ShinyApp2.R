###################### No comments yet #######################
######################  Will add later  ######################


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

# Function for Social Media tags
# Reference: mine-cetinkaya-rundel 
# Reference: on Github: https://github.com/rstudio/shiny-gallery/blob/master/nz-trade-dash/helper_funs.R 2172-2212
dropdownMenuCustom <- function (..., type = c("messages", "notifications", "tasks"), 
                                badgeStatus = "primary", icon = NULL, .list = NULL) 
{
  type <- match.arg(type)
  if (!is.null(badgeStatus)) shinydashboard:::validateStatus(badgeStatus)
  items <- c(list(...), .list)
  lapply(items, shinydashboard:::tagAssert, type = "li")
  dropdownClass <- paste0("dropdown ", type, "-menu")
  if (is.null(icon)) {
    icon <- switch(type, messages = shiny::icon("envelope"), 
                   notifications = shiny::icon("warning"), tasks = shiny::icon("tasks"))
  }
  numItems <- length(items)
  if (is.null(badgeStatus)) {
    badge <- NULL
  }
  else {
    badge <- tags$span(class = paste0("label label-", badgeStatus), 
                       numItems)
  }
  tags$li(
    class = dropdownClass, 
    a(
      href = "#", 
      class = "dropdown-toggle", 
      `data-toggle` = "dropdown", 
      icon, 
      badge
    ), 
    tags$ul(
      class = "dropdown-menu", 
      tags$li(
        class = "header"
      ), 
      tags$li(
        tags$ul(class = "menu", items)
      )
    )
  )
}

header <- dashboardHeader(
  title = h4("Evaluating Player Relationships in Stolen Base Defense   ", align = "right", style = "font-family: 'Times', serif; font-weight: 1000px; font-size: 28px; text-shadow: 1px 1px 1px #aaa; color:white; white-space: pre;"), 
  disable = FALSE,
  titleWidth = 910,
  
  # Social Media Tags
  dropdownMenuCustom( type = 'message',
                      icon = icon("share-alt"),
                      messageItem(
                        from = 'Twitter',
                        message = "UMN SAC",
                        icon = icon("twitter"),
                        href = "https://twitter.com/umn_sac?lang=en"
                      ),
                      messageItem(
                        from = 'LinkedIn',
                        message = "Isaac Blumhoefer",
                        icon = icon("linkedin"),
                        href = "https://www.linkedin.com/in/isaac-blumhoefer/"
                      ),
                      messageItem(
                        from = 'LinkedIn',
                        message = "Jack Rogers",
                        icon = icon("linkedin"),
                        href = "https://www.linkedin.com/in/jack-rogers-data/"
                      ),
                      messageItem(
                        from = 'LinkedIn',
                        message = "Kai Franke",
                        icon = icon("linkedin"),
                        href = "https://www.linkedin.com/in/kai-franke-70076017a/"
                      ),
                      messageItem(
                        from = 'LinkedIn',
                        message = "Jackson Balch",
                        icon = icon("linkedin"),
                        href = "https://www.linkedin.com/in/jackson-balch-66a910231/"
                      )
                      
  ))
  
  
  # Title and theme
  # div(style = "border-style: solid; border-color: black; background-color: #adceff;",
  # titlePanel(
  #   h1("Evaluating Player Relationships in Stolen Base Defense", align = "center", style = "font-family: 'Times', serif; font-weight: 1000px; font-size: 50px; text-shadow: 3px 3px 3px #aaa; line-height: 1; color:black"),
  # ),
  # titlePanel(
  #   h6("SMT 2023 Data Challenge", align = "center", style = "font-family: 'Times', serif; font-weight: 1000px; font-size: 20px; line-height: 1; color:black"),
  # )),
  # theme = shinythemes::shinytheme("journal"),
  
  
  # Setting the background color to cream here
  # setBackgroundColor(
  #   color = c("navajowhite"),
  #   shinydashboard = FALSE
  # ),
  
  # First tab is where we allow the user to play with the parameters to determine SB %
  # Left out exchange/timeToThrow here because it's the same as popTime
  # Also left out y-coordinate in data (depth), but should probably add that in (but is hard to visualize)
  # Finally, also left out rDist2B

header$children[[2]]$children[[2]] <- header$children[[2]]$children[[1]]
header$children[[2]]$children[[1]] <- tags$a(href = 'https://www.stat.cmu.edu/cmsac/conference/2023/#:~:text=Undergraduate%20Division%20Finalists',
                                             imageOutput('image', height = "2px", width = "10%"),
                                             target = '_blank')
  
  sidebar <- dashboardSidebar( 
    width = 200,
    sidebarMenu(
      id = 'sidebar',
      style = "position: relative; overflow: visible;",
      #style = "position: relative; overflow: visible; overflow-y:scroll",
      #style = 'height: 90vh; overflow-y: auto;',
      ## 1st tab show the Main dashboard -----------
      menuItem( "Testing Scenarios", tabName = 'testing_scenarios', icon = icon('dashboard'), startExpanded = T,
                menuSubItem('Pitch Release', tabName = "pitch_release"),
                menuSubItem('Catcher Retrieval', tabName = "catcher_retrieval"),
                menuSubItem('Catcher Release', tabName = "catcher_release"),
                menuSubItem('Middle Infielder Retrieval', tabName = "middle_infielder_retrieval")),
      
      #useShinyjs(),
      
      ## give sidebar inputs a id so that it can be manipulated by css
      div( id = 'sidebar_pitch_release',
           conditionalPanel("input.sidebar === 'pitch_release'",
                                                        
                            ## radio buttons to ask user to choose prebuilt commodity groups or build their owns
                            sliderInput("maxSpeed1",
                                        "Max Spd (ft/sec):",
                                        min = 20,
                                        max = 30,
                                        value = 28.5,
                                        step = 0.05
                            )
                                        
      )),
      
      div( id = 'sidebar_catcher_retrieval',
           conditionalPanel("input.sidebar === 'catcher_retrieval'",
                            
                            ## radio buttons to ask user to choose prebuilt commodity groups or build their owns
                            sliderInput("timeToPlate2",
                                        "Time to Plate (sec):",
                                        min = 0.4,
                                        max = 0.6,
                                        value = 0.5,
                                        step = 0.01)
                            ,
                            sliderInput("leadoff2",
                                        "Lead off of First (ft):",
                                        min = 10,
                                        max = 30,
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
                            
                            
                            
           )),
      
      div( id = 'sidebar_catcher_release',
           conditionalPanel("input.sidebar === 'catcher_release'",
                            
                            ## radio buttons to ask user to choose prebuilt commodity groups or build their owns
                            sliderInput("exchange3",
                                        "Exchange (sec):",
                                        min = 0.5,
                                        max = 1.5,
                                        value = 0.8,
                                        step = 0.01)
                            ,
                            sliderInput("rDist3",
                                        "Distance to 2nd (ft):",
                                        min = 30,
                                        max = 60,
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
                            
           )),
      
      div( id = 'sidebar_middle_infielder_retrieval',
           conditionalPanel("input.sidebar === 'middle_infielder_retrieval'",
                            
                            ## radio buttons to ask user to choose prebuilt commodity groups or build their owns
                            checkboxInput('SB4', 'SB Occurred', FALSE), 
                            sliderInput("timeOfThrow4",
                                        "Time of Throw (sec):",
                                        min = 0.8,
                                        max = 2,
                                        value = 1.2,
                                        step = 0.01)
                            ,
                            sliderInput("rDist4",
                                        "Distance to 2nd (ft):",
                                        min = 0,
                                        max = 16,
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
                            sliderInput("depth4",
                                        HTML("Infielder Catch Depth <br/> (0 = 2nd Base middle):"),
                                        min = -5,
                                        max = 5,
                                        value = 1,
                                        step = 0.1)
                            
                                   
           )),
      
      
      ## add conditional panel to show more
      # conditionalPanel( "input.sidebar === 'dashboard'",
      #                   actionButton("btn_show_more",
      #                                paste0(' Show more details'),
      #                                icon = icon('chevron-circle-down'),
      #                                style='padding-top:0px; padding-bottom:0px;padding-left:3px;padding-right:3px; '
      #                                ) 
      #                   ),
      
      
      ## 4th tab HS finder -------------------------
      #menuItem("HS code finder", tabName = 'hs_finder', icon = icon('search') ),
      
      ## 5th tab Data source, definition , i.e., help ---------------
      menuItem( "Animated Plays", tabName = 'animation', icon = icon('video')),
      
      
      # Animation Inputs
      div( id = 'sidebar_animation',
           conditionalPanel("input.sidebar === 'animation'",
                            
                            checkboxInput('SB', 'SB Occurred', FALSE)
                            ,
                            selectInput('game',
                                        'Pick the Game:',
                                        choices = unique(locations$game_str))
                            ,
                            selectInput('play',
                                        'Pick the Play From that Game:',
                                        choices = unique(locations$play_id))
           ))
      
 
    )
  )
  
  
  body <- dashboardBody(
    tabItems(
      tabItem(tabName = "pitch_release",
          fluidRow(
            # Filler so Black box does not extend entire length
            column(1),
            # The final SB probablity
            column(4, align = "center",
                   br(),
                   div(style = "border-style: solid; border-color: black; border-width: thick; background: Gainsboro",
                       h4(strong("SB Probability"), align = "center", style = "font-family: 'Times', serif; font-weight: 900; font-size: 50px; line-height: 1; color:black;"),
                       br(),
                       h5(textOutput("modelOutput1"), align = "center", style = "font-family: 'Times', serif; font-weight: 700; font-size: 50px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;")
                   )
            ),
            # Filler so Black box does not extend entire length
            column(1),
            # Pitcher Outs Gained
            column(2, align = "center",
                   div(style = "height:68px"),
                   div(style = "border-style: solid; border-color: black; border-width: thin",
                       h4("Pitcher", align = "center", style = "font-family: 'Times', serif; font-weight: 10; font-size: 30px; line-height: 1; color:gray5;"),
                       br(),
                       h5("--", align = "center", style = "font-family: 'Times', serif; font-weight: 700; font-size: 30px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;")
                   )
            ),
            # Catcher Outs Gained
            column(2, align = "center",
                   h4(strong("Outs This Play"), align = "center", style = "font-family: 'Ariel Narrow', sans-serif; font-weight: 10; font-size: 35px; line-height: 1; color:DimGray;"),
                   div(style = "height:13px"),
                   div(style = "border-style: solid; border-color: black; border-width: thin",
                       h4("Catcher", align = "center", style = "font-family: 'Times', serif; font-weight: 10; font-size: 30px; line-height: 1; color:gray5;"),
                       br(),
                       h5("--", align = "center", style = "font-family: 'Times', serif; font-weight: 700; font-size: 30px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;")
                   )
            ),
            # Infielder Outs Gained
            column(2, align = "center",
                   div(style = "height:68px"),
                   div(style = "border-style: solid; border-color: black; border-width: thin",
                       h4("Infielder", align = "center", style = "font-family: 'Times', serif; font-weight: 10; font-size: 30px; line-height: 1; color:gray5;"),
                       br(),
                       h5("--", align = "center", style = "font-family: 'Times', serif; font-weight: 700; font-size: 30px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;")
                   ),
            ),
            # Max Speed
            column(6, align = "center",
                   div(style = "height:68px"),
                   h4(strong("Max Speed"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 30px; line-height: 1; color:black;"),
                   br(),
                   h5(textOutput("maxSpeed1"), align = "center", style = "font-family: 'Times', serif; font-weight: 250px; font-size: 40px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:red;")
            ),
            # Total Pitcher Outs Gained
            column(2, align = "center",
                   br(),
                   div(style = "height:68px"),
                   div(style = "border-style: solid; border-color: black; border-width: thin",
                       h4("Pitcher", align = "center", style = "font-family: 'Times', serif; font-weight: 10; font-size: 30px; line-height: 1; color:gray5;"),
                       br(),
                       h5(textOutput("modelOutput2Pitcher1"), align = "center", style = "font-family: 'Times', serif; font-weight: 700; font-size: 30px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;")
                   )
            ),
            # Total Catcher Outs Gained
            column(2, align = "center",
                   br(),
                   h4(strong("Total Outs"), align = "center", style = "font-family: 'Ariel Narrow', sans-serif; font-weight: 10; font-size: 35px; line-height: 1; color:dimgrey;"),
                   div(style = "height:13px"),
                   div(style = "border-style: solid; border-color: black; border-width: thin",
                       h4("Catcher", align = "center", style = "font-family: 'Times', serif; font-weight: 10; font-size: 30px; line-height: 1; color:gray5;"),
                       br(),
                       h5(textOutput("OutputTotalCatcher1"), align = "center", style = "font-family: 'Times', serif; font-weight: 700; font-size: 30px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;")
                   )
            ),
            # Total Infielder Outs Gained
            column(2, align = "center",
                   br(),
                   div(style = "height:68px"),
                   div(style = "border-style: solid; border-color: black; border-width: thin",
                       h4("Infielder", align = "center", style = "font-family: 'Times', serif; font-weight: 10; font-size: 30px; line-height: 1; color:gray5;"),
                       br(),
                       h5(textOutput("OutputTotalFielder1"), align = "center", style = "font-family: 'Times', serif; font-weight: 700; font-size: 30px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;")
                   )
            )
            
          )  
              
      ),
      
      tabItem(tabName = "catcher_retrieval",
              fluidRow(
                # Filler so Black box does not extend entire length
                column(1),
                # The final SB probablity
                column(4, align = "center",
                       br(),
                       div(style = "border-style: solid; border-color: black; border-width: thick; background: Gainsboro",
                           h4(strong("SB Probability"), align = "center", style = "font-family: 'Times', serif; font-weight: 900; font-size: 50px; line-height: 1; color:black;"),
                           br(),
                           h5(textOutput("modelOutput2"), align = "center", style = "font-family: 'Times', serif; font-weight: 700; font-size: 50px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;")
                       )
                ),
                # Filler so Black box does not extend entire length
                column(1),
                # Pitcher Outs Gained
                column(2, align = "center",
                       div(style = "height:68px"),
                       div(style = "border-style: solid; border-color: black; border-width: thin",
                           h4("Pitcher", align = "center", style = "font-family: 'Times', serif; font-weight: 10; font-size: 30px; line-height: 1; color:gray5;"),
                           br(),
                           h5(textOutput("modelOutput2Pitcher"), align = "center", style = "font-family: 'Times', serif; font-weight: 700; font-size: 30px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;")
                       )
                ),
                # Catcher Outs Gained
                column(2, align = "center",
                       h4(strong("Outs This Play"), align = "center", style = "font-family: 'Ariel Narrow', sans-serif; font-weight: 10; font-size: 35px; line-height: 1; color:DimGray;"),
                       div(style = "height:13px"),
                       div(style = "border-style: solid; border-color: black; border-width: thin",
                           h4("Catcher", align = "center", style = "font-family: 'Times', serif; font-weight: 10; font-size: 30px; line-height: 1; color:gray5;"),
                           br(),
                           h5("--", align = "center", style = "font-family: 'Times', serif; font-weight: 700; font-size: 30px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;")
                       )
                ),
                # Infielder Outs Gained
                column(2, align = "center",
                       div(style = "height:68px"),
                       div(style = "border-style: solid; border-color: black; border-width: thin",
                           h4("Infielder", align = "center", style = "font-family: 'Times', serif; font-weight: 10; font-size: 30px; line-height: 1; color:gray5;"),
                           br(),
                           h5("--", align = "center", style = "font-family: 'Times', serif; font-weight: 700; font-size: 30px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;")
                       ),
                ),
                # Time to Plate
                column(6, align = "center",
                       div(style = "height:68px"),
                       h4(strong("Time to Plate"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 30px; line-height: 1; color:black;"),
                       br(),
                       h5(textOutput("timeToPlate2"), align = "center", style = "font-family: 'Times', serif; font-weight: 250px; font-size: 40px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:red;")
                ),
                # Total Pitcher Outs Gained
                column(2, align = "center",
                       br(),
                       div(style = "height:68px"),
                       div(style = "border-style: solid; border-color: black; border-width: thin",
                           h4("Pitcher", align = "center", style = "font-family: 'Times', serif; font-weight: 10; font-size: 30px; line-height: 1; color:gray5;"),
                           br(),
                           h5(textOutput("modelOutput2Pitcher2"), align = "center", style = "font-family: 'Times', serif; font-weight: 700; font-size: 30px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;")
                       )
                ),
                # Total Catcher Outs Gained
                column(2, align = "center",
                       br(),
                       h4(strong("Total Outs"), align = "center", style = "font-family: 'Ariel Narrow', sans-serif; font-weight: 10; font-size: 35px; line-height: 1; color:dimgrey;"),
                       div(style = "height:13px"),
                       div(style = "border-style: solid; border-color: black; border-width: thin",
                           h4("Catcher", align = "center", style = "font-family: 'Times', serif; font-weight: 10; font-size: 30px; line-height: 1; color:gray5;"),
                           br(),
                           h5(textOutput("OutputTotalCatcher2"), align = "center", style = "font-family: 'Times', serif; font-weight: 700; font-size: 30px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;")
                       )
                ),
                # Total Infielder Outs Gained
                column(2, align = "center",
                       br(),
                       div(style = "height:68px"),
                       div(style = "border-style: solid; border-color: black; border-width: thin",
                           h4("Infielder", align = "center", style = "font-family: 'Times', serif; font-weight: 10; font-size: 30px; line-height: 1; color:gray5;"),
                           br(),
                           h5(textOutput("OutputTotalFielder2"), align = "center", style = "font-family: 'Times', serif; font-weight: 700; font-size: 30px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;")
                       )
                ),
                # The field with a movable player's lead
                column(3, align = "center",
                       br(),
                       br(),
                       h4(strong("Distance to 2B"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 30px; line-height: 1; color:black;"),
                       plotOutput('EditLead2', height = 300, width = 300)
                ),
                # Pitch Location
                column(3, align = "center",
                       br(),
                       br(),
                       h4(strong("Pitch Location"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 30px; line-height: 1; color:black;"),
                       plotOutput('EditBallLocation2', height = 300, width = 250)
                ),
              )  
              
      ),
      
      tabItem(tabName = "catcher_release",
              fluidRow(
                # Filler so Black box does not extend entire length
                column(1),
                # The final SB probablity
                column(4, align = "center",
                       br(),
                       div(style = "border-style: solid; border-color: black; border-width: thick; background: Gainsboro",
                           h4(strong("SB Probability"), align = "center", style = "font-family: 'Times', serif; font-weight: 900; font-size: 50px; line-height: 1; color:black;"),
                           br(),
                           h5(textOutput("modelOutput3"), align = "center", style = "font-family: 'Times', serif; font-weight: 700; font-size: 50px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;")
                       )
                ),
                # Filler so Black box does not extend entire length
                column(1),
                # Pitcher Outs Gained
                column(2, align = "center",
                       div(style = "height:68px"),
                       div(style = "border-style: solid; border-color: black; border-width: thin",
                           h4("Pitcher", align = "center", style = "font-family: 'Times', serif; font-weight: 10; font-size: 30px; line-height: 1; color:gray5;"),
                           br(),
                           h5("--", align = "center", style = "font-family: 'Times', serif; font-weight: 700; font-size: 30px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;")
                       )
                ),
                # Catcher Outs Gained
                column(2, align = "center",
                       h4(strong("Outs This Play"), align = "center", style = "font-family: 'Ariel Narrow', sans-serif; font-weight: 10; font-size: 35px; line-height: 1; color:DimGray;"),
                       div(style = "height:13px"),
                       div(style = "border-style: solid; border-color: black; border-width: thin",
                           h4("Catcher", align = "center", style = "font-family: 'Times', serif; font-weight: 10; font-size: 30px; line-height: 1; color:gray5;"),
                           br(),
                           h5(textOutput("modelOutput3Catcher"), align = "center", style = "font-family: 'Times', serif; font-weight: 700; font-size: 30px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;")
                       )
                ),
                # Infielder Outs Gained
                column(2, align = "center",
                       div(style = "height:68px"),
                       div(style = "border-style: solid; border-color: black; border-width: thin",
                           h4("Infielder", align = "center", style = "font-family: 'Times', serif; font-weight: 10; font-size: 30px; line-height: 1; color:gray5;"),
                           br(),
                           h5("--", align = "center", style = "font-family: 'Times', serif; font-weight: 700; font-size: 30px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;")
                       ),
                ),
              ),
              fluidRow(
                # Exchange
                column(6, align = "center",
                       div(style = "height:68px"),
                       h4(strong("Exchange Time"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 30px; line-height: 1; color:black;"),
                       br(),
                       h5(textOutput("exchange3"), align = "center", style = "font-family: 'Times', serif; font-weight: 250px; font-size: 40px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:red;")
                ),
                # Total Pitcher Outs Gained
                column(2, align = "center",
                       br(),
                       div(style = "height:68px"),
                       div(style = "border-style: solid; border-color: black; border-width: thin",
                           h4("Pitcher", align = "center", style = "font-family: 'Times', serif; font-weight: 10; font-size: 30px; line-height: 1; color:gray5;"),
                           br(),
                           h5(textOutput("modelOutput2Pitcher3"), align = "center", style = "font-family: 'Times', serif; font-weight: 700; font-size: 30px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;")
                       )
                ),
                # Total Catcher Outs Gained
                column(2, align = "center",
                       br(),
                       h4(strong("Total Outs"), align = "center", style = "font-family: 'Ariel Narrow', sans-serif; font-weight: 10; font-size: 35px; line-height: 1; color:dimgrey;"),
                       div(style = "height:13px"),
                       div(style = "border-style: solid; border-color: black; border-width: thin",
                           h4("Catcher", align = "center", style = "font-family: 'Times', serif; font-weight: 10; font-size: 30px; line-height: 1; color:gray5;"),
                           br(),
                           h5(textOutput("OutputTotalCatcher3"), align = "center", style = "font-family: 'Times', serif; font-weight: 700; font-size: 30px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;")
                       )
                ),
                # Total Infielder Outs Gained
                column(2, align = "center",
                       br(),
                       div(style = "height:68px"),
                       div(style = "border-style: solid; border-color: black; border-width: thin",
                           h4("Infielder", align = "center", style = "font-family: 'Times', serif; font-weight: 10; font-size: 30px; line-height: 1; color:gray5;"),
                           br(),
                           h5(textOutput("OutputTotalFielder3"), align = "center", style = "font-family: 'Times', serif; font-weight: 700; font-size: 30px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;")
                       )
                ),
                # Distance to 2nd
                column(3, align = "center",
                       br(),
                       br(),
                       h4(strong("Distance to 2B"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 30px; line-height: 1; color:black;"),
                       plotOutput('EditLead3', height = 300, width = 300)
                ),
                # Throw Location
                column(3, align = "center",
                       br(),
                       br(),
                       h4(strong("Catcher Throw Location"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 30px; line-height: 1; color:black;"),
                       plotOutput('EditBallLocation3', height = 300, width = 200)
                ),
                
              )
              
      ),
      
      tabItem(tabName = "middle_infielder_retrieval",
              fluidRow(
                # Filler so Black box does not extend entire length
                column(1),
                # The final SB probablity
                column(4, align = "center",
                       br(),
                       div(style = "border-style: solid; border-color: black; border-width: thick; background: Gainsboro",
                           h4(strong("SB Probability"), align = "center", style = "font-family: 'Times', serif; font-weight: 900; font-size: 50px; line-height: 1; color:black;"),
                           br(),
                           h5(textOutput("modelOutput4"), align = "center", style = "font-family: 'Times', serif; font-weight: 700; font-size: 50px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;")
                       )
                ),
                # Filler so Black box does not extend entire length
                column(1),
                # Pitcher Outs Gained
                column(2, align = "center",
                       div(style = "height:68px"),
                       div(style = "border-style: solid; border-color: black; border-width: thin",
                           h4("Pitcher", align = "center", style = "font-family: 'Times', serif; font-weight: 10; font-size: 30px; line-height: 1; color:gray5;"),
                           br(),
                           h5("--", align = "center", style = "font-family: 'Times', serif; font-weight: 700; font-size: 30px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;")
                       )
                ),
                # Catcher Outs Gained
                column(2, align = "center",
                       h4(strong("Outs This Play"), align = "center", style = "font-family: 'Ariel Narrow', sans-serif; font-weight: 10; font-size: 35px; line-height: 1; color:DimGray;"),
                       div(style = "height:13px"),
                       div(style = "border-style: solid; border-color: black; border-width: thin",
                           h4("Catcher", align = "center", style = "font-family: 'Times', serif; font-weight: 10; font-size: 30px; line-height: 1; color:gray5;"),
                           br(),
                           h5(textOutput("modelOutput4Catcher"), align = "center", style = "font-family: 'Times', serif; font-weight: 700; font-size: 30px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;")
                       )
                ),
                # Infielder Outs Gained
                column(2, align = "center",
                       div(style = "height:68px"),
                       div(style = "border-style: solid; border-color: black; border-width: thin",
                           h4("Infielder", align = "center", style = "font-family: 'Times', serif; font-weight: 10; font-size: 30px; line-height: 1; color:gray5;"),
                           br(),
                           h5(textOutput("modelOutput4Infielder"), align = "center", style = "font-family: 'Times', serif; font-weight: 700; font-size: 30px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;")
                       ),
                )
              ),
              fluidRow(
                # Time of Throw
                column(6, align = "center",
                       div(style = "height:68px"),
                       h4(strong("Time of Throw"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 30px; line-height: 1; color:black;"),
                       br(),
                       h5(textOutput("timeOfThrow4"), align = "center", style = "font-family: 'Times', serif; font-weight: 250px; font-size: 40px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:red;")
                ),
                # Total Pitcher Outs Gained
                column(2, align = "center",
                       br(),
                       div(style = "height:68px"),
                       div(style = "border-style: solid; border-color: black; border-width: thin",
                           h4("Pitcher", align = "center", style = "font-family: 'Times', serif; font-weight: 10; font-size: 30px; line-height: 1; color:gray5;"),
                           br(),
                           h5(textOutput("modelOutput2Pitcher4"), align = "center", style = "font-family: 'Times', serif; font-weight: 700; font-size: 30px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;")
                       )
                ),
                # Total Catcher Outs Gained
                column(2, align = "center",
                       br(),
                       h4(strong("Total Outs"), align = "center", style = "font-family: 'Ariel Narrow', sans-serif; font-weight: 10; font-size: 35px; line-height: 1; color:dimgrey;"),
                       div(style = "height:13px"),
                       div(style = "border-style: solid; border-color: black; border-width: thin",
                           h4("Catcher", align = "center", style = "font-family: 'Times', serif; font-weight: 10; font-size: 30px; line-height: 1; color:gray5;"),
                           br(),
                           h5(textOutput("OutputTotalCatcher4"), align = "center", style = "font-family: 'Times', serif; font-weight: 700; font-size: 30px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;")
                       )
                ),
                # Total Infielder Outs Gained
                column(2, align = "center",
                       br(),
                       div(style = "height:68px"),
                       div(style = "border-style: solid; border-color: black; border-width: thin",
                           h4("Infielder", align = "center", style = "font-family: 'Times', serif; font-weight: 10; font-size: 30px; line-height: 1; color:gray5;"),
                           br(),
                           h5(textOutput("OutputTotalFielder4"), align = "center", style = "font-family: 'Times', serif; font-weight: 700; font-size: 30px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;")
                       )
                )
              ),
              fluidRow(
                # The field with a movable player's lead
                column(4, align = "center",
                       br(),
                       br(),
                       h4(strong("Distance to 2B"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 30px; line-height: 1; color:black;"),
                       plotOutput('EditLead4', height = 300, width = 300)
                ),
                # Catch Location
                column(4, align = "center",
                       br(),
                       br(),
                       h4(strong("Fielder Catch Location"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 30px; line-height: 1; color:black;"),
                       h4("(Home Plate View)", align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 18px; line-height: 1; color:black;"),
                       plotOutput('EditBallLocation4', height = 300, width = 400)
                ),
                # Catch Depth
                column(4, align = "center",
                       br(),
                       br(),
                       h4(strong("Fielder Catch Depth"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 30px; line-height: 1; color:black;"),
                       h4("(Right Side View)", align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 18px; line-height: 1; color:black;"),
                       plotOutput('CatchDepth4', height = 300, width = 400)
                )
              )
      ),
      
      tabItem(tabName = "animation",
              fluidRow(
                # Filler
                column(1),
                # Play Animation
                column(4, align = "center",
                       h4(strong("Play Animation"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 40px; line-height: 1; color:black;"),
                       br(),
                       h4(textOutput('stolenBaseText'), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 20px; line-height: 1; color:dimgrey;"),
                       br(),
                       imageOutput('animate', height = 500, width = 500)
                ),
                # Filler
                column(1),
                # Displayed Percentages for that play
                column(3, 
                       h4(strong("SB Probabilities"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 40px; line-height: 1; color:black;"),
                       br(),
                       br(),
                       div(style = "border-style: solid; border-color: black; border-width: thick",
                           h4(strong("Pitch Release"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 25px; line-height: 1; color:dimgrey;"),
                           h5(textOutput("pitch_release_model1"), align = "center", style = "font-family: 'Times', serif; font-weight: 250px; font-size: 35px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;"),
                           br(),
                           h4(strong("Catcher Retrieval"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 25px; line-height: 1; color:dimgrey;"),
                           h5(textOutput("catcher_retrieval_model2"), align = "center", style = "font-family: 'Times', serif; font-weight: 250px; font-size: 35px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;"),
                           br(),
                           h4(strong("Catcher Throw"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 25px; line-height: 1; color:dimgrey;"),
                           h5(textOutput("catcher_throw_model3"), align = "center", style = "font-family: 'Times', serif; font-weight: 250px; font-size: 35px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;"),
                           br(),
                           h4(strong("Middle Infielder Retrieval"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 25px; line-height: 1; color:dimgrey;"),
                           h5(textOutput("middle_infielder_retrieval_model4"), align = "center", style = "font-family: 'Times', serif; font-weight: 250px; font-size: 35px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;"),
                           br(),
                           h4(strong("End Result (0 or 100)"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 25px; line-height: 1; color:dimgrey;"),
                           h5(textOutput("end_result"), align = "center", style = "font-family: 'Times', serif; font-weight: 250px; font-size: 35px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;")
                       )
                ),
                column(3,
                       h4(strong("Total Outs Earned"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 40px; line-height: 1; color:black;"),
                       div(style = "height:151px"),
                       div(style = "border-style: solid; border-color: black; border-width: thick",
                           h4(strong("Pitcher Outs"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 25px; line-height: 1; color:dimgrey;"),
                           h5(textOutput("pitcher_outs2"), align = "center", style = "font-family: 'Times', serif; font-weight: 250px; font-size: 35px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;"),
                           br(),
                           h4(strong("Catcher Outs"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 25px; line-height: 1; color:dimgrey;"),
                           h5(textOutput("catcher_outs34"), align = "center", style = "font-family: 'Times', serif; font-weight: 250px; font-size: 35px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;"),
                           br(),
                           h4(strong("Infielder Outs"), align = "center", style = "font-family: 'Times', serif; font-weight: 500px; font-size: 25px; line-height: 1; color:dimgrey;"),
                           h5(textOutput("infielder_outs4"), align = "center", style = "font-family: 'Times', serif; font-weight: 250px; font-size: 35px; text-shadow: 1px 1px 1px #aaa; line-height: 1; color:blue;"),
                       )
                ),
                # Disclaimers
                column(12,
                       br(),
                       h5("**Disclaimer: Some plays may take consider time to load", align = "left", style = "font-family: 'Times', serif; font-weight: 250px; font-size: 15px; line-height: 1; color:black;"),
                       br(),
                       h5("**Disclaimer: Some plays have missing data points and will jump around or end early", align = "left", style = "font-family: 'Times', serif; font-weight: 250px; font-size: 15px; line-height: 1; color:black;")
                )
                
              )  
              
      )
      
      
      
    )
  )

  
ui <- dashboardPage(header, sidebar, body)  
  
  
server <- function(input, output, session) {
  
  # Credit to Posit Community
  # Visit https://community.rstudio.com/t/update-scatter-in-plotly-by-dragging-shapes-shiny/44809
  # for more information

  output$image <- renderImage({
    list(src = "smt_banner_20_1_25.png",
         alt = "This is alternate text"
    )
  }, deleteFile = FALSE)
  
  # Update Choices for the Animated Play based on SB
  
  observe({
    x <- as.numeric(input$SB)
    new_options <- filter(locations, SB == x)
    
    updateSelectizeInput(session, "game",
                      choices = new_options$game_str,
                      server = TRUE
    )
    
  })
  
  
  
  # Update Choices for the Animated Play based on SB and Game
  
  observe({
    x <- input$game
    y <- as.numeric(input$SB)
    new_options <- filter(locations, game_str == x & SB == y)
    
    updateSelectizeInput(session, "play",
                      choices = new_options$play_id,
                      server = TRUE
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
  output$exchange4 = renderText(paste0(input$exchange3, " sec"))
  
  
  # Various timeOfThrow inputs
  output$timeOfThrow4 = renderText(paste0(input$timeOfThrow4, " sec"))
  
  
  # Various timetoPlate inputs
  output$timeToPlate2 = renderText(paste0(input$timeToPlate2, " sec"))
  
  
  # 1st model calculation
  model1calc = reactive({
    
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
    
    prob
  })
  
  # Creating text for 1st model
  output$modelOutput1 = renderText(paste0(model1calc(), "%"))
  
  
  
  # 2nd model calculation
  model2calc = reactive({
    
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
    
    prob
  })
  
  # Creating text for 2nd model
  output$modelOutput2 = renderText(paste0(model2calc(), "%"))
  
  
  
  model2Pitcher = reactive({
    prob <- round((model1calc()/100 - model2calc()/100), digits = 4)
  })
  
  # Creating text for 2nd model - pitcher
  output$modelOutput2Pitcher = renderText(paste0(model2Pitcher()))
  
  output$modelOutput2Pitcher1 = renderText(paste0(model2Pitcher()))
  output$modelOutput2Pitcher2 = renderText(paste0(model2Pitcher()))
  output$modelOutput2Pitcher3 = renderText(paste0(model2Pitcher()))
  output$modelOutput2Pitcher4 = renderText(paste0(model2Pitcher()))
  
  
  
  # 3rd model calculation
  model3calc = reactive({
    
    # 1 for intercept and multiply by 1000 here for times because the sliders are scaled down to seconds
    
    # 0s are included where the coefficient estimate is 0
    
    # Order:  Intercept, exchange, timeOfThrow, timetoPlate, rDist2B, maxSpeed, ball_x, ball_y, ball_z
    
    model3["inputs"] = c(1, 1000 * input$exchange3, 1000 * input$timeOfThrow4, 0, input$rDist3, 0, input$x_coordinate3, 0, 0)
    
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
    
    prob
  })
  
  # Creating text for 3rd model
  output$modelOutput3 = renderText(paste0(model3calc(), "%"))
  
  
  
  model3Catcher = reactive({
    prob <- round((model2calc()/100 - model3calc()/100), digits = 4)
  })
  
  # Creating text for 2nd model - pitcher
  output$modelOutput3Catcher = renderText(paste0(model3Catcher()))
  
  
  
  # 4th model calculation
  model4calc = reactive({
    
    # 1 for intercept and multiply by 1000 here for times because the sliders are scaled down to seconds
    
    # 0s are included where the coefficient estimate is 0
    
    # Order:  Intercept, exchange, timeOfThrow, timetoPlate, rDist2B, maxSpeed, ball_x, ball_y, ball_z
    
    # + 127.28125 on catch depth for second base
    
    model4["inputs"] = c(1, 1000 * input$exchange3, 1000 * input$timeOfThrow4, 1000 * input$timeToPlate2, input$rDist4, input$x_coordinate4, input$depth4 + 127.28125, input$y_coordinate4)
    
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
    
    prob
  })
  
  # Creating text for 4th model
  output$modelOutput4 = renderText(paste0(model4calc(), "%"))
  
  
  
  model4Catcher = reactive({
    prob <- round((model3calc()/100 - model4calc()/100), digits = 4)
  })
  
  # Creating text for 2nd model - pitcher
  output$modelOutput4Catcher = renderText(paste0(model4Catcher()))
  
  
  
  
  TotalCatcher = reactive({
    prob <- round(model4Catcher() + model3Catcher(), digits = 4)
  })
  
  # Creating text for 2nd model - pitcher
  output$OutputTotalCatcher1 = renderText(paste0(TotalCatcher()))
  output$OutputTotalCatcher2 = renderText(paste0(TotalCatcher()))
  output$OutputTotalCatcher3 = renderText(paste0(TotalCatcher()))
  output$OutputTotalCatcher4 = renderText(paste0(TotalCatcher()))
  
  
  # Middle Infielder Outs based on SB4
  TotalFielder = reactive({
    
    if (input$SB4 == TRUE){
      prob <- round(model4calc()/100 - 1, digits = 4)
    }
    else {
      prob <- round(model4calc()/100, digits = 4)
    }
    
    prob
  })
  
  # Creating text for 2nd model - pitcher
  output$modelOutput4Infielder = renderText(paste0(TotalFielder()))
  
  # Creating text for 4th model - infielder
  output$OutputTotalFielder1 = renderText(paste0(TotalFielder()))
  output$OutputTotalFielder2 = renderText(paste0(TotalFielder()))
  output$OutputTotalFielder3 = renderText(paste0(TotalFielder()))
  output$OutputTotalFielder4 = renderText(paste0(TotalFielder()))
  
  
  
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
      geom_polygon(df, mapping=aes(x = x, y = y), fill="gray97", color = "gray75") +
      # creating strike zone here
      geom_segment(aes(x = -0.85, y = 1.6, xend = -0.85, yend = 3.5), linewidth = 1) + 
      geom_segment(aes(x = -0.85, y = 1.6, xend = 0.85, yend = 1.6), linewidth = 1) + 
      geom_segment(aes(x = 0.85, y = 3.5, xend = -0.85, yend = 3.5), linewidth = 1) + 
      geom_segment(aes(x = 0.85, y = 3.5, xend = 0.85, yend = 1.6), linewidth = 1) +
      # Baseball
      geom_point(size = 5, fill = "white", color = "gray25", pch = 21) +
      # Adding the red stripes on baseball here
      geom_curve(aes(x = ball_location2()$x - 0.1, y = ball_location2()$y - 0.1, 
                     xend = ball_location2()$x - 0.1, yend = ball_location2()$y + 0.1), 
                 linewidth = 0.5, color = "red", curvature = 0.25) + 
      geom_curve(aes(x = ball_location2()$x + 0.1, y = ball_location2()$y - 0.1, 
                     xend = ball_location2()$x + 0.1, yend = ball_location2()$y + 0.1), 
                 linewidth = 0.5, color = "red", curvature = -0.25) + 
      xlim(-3.5, 3.5) +
      ylim(-1,5) +
      theme_void()
      #theme(panel.border = element_rect(colour = "black", fill=NA, size=3))
    
  }, bg = "#ECF0F5")
  
  
  
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
      geom_polygon(df, mapping=aes(x = x, y = y), fill="gray97", color = "gray75") +
      # creating strike zone here
      geom_segment(aes(x = -0.85, y = 1.6, xend = -0.85, yend = 3.5), linewidth = 1) + 
      geom_segment(aes(x = -0.85, y = 1.6, xend = 0.85, yend = 1.6), linewidth = 1) + 
      geom_segment(aes(x = 0.85, y = 3.5, xend = -0.85, yend = 3.5), linewidth = 1) + 
      geom_segment(aes(x = 0.85, y = 3.5, xend = 0.85, yend = 1.6), linewidth = 1) +
      # Baseball
      geom_point(size = 5, fill = "white", color = "gray25", pch = 21) +
      # Adding the red stripes on baseball here
      geom_curve(aes(x = ball_location3()$x - 0.12, y = ball_location3()$y - 0.12, 
                     xend = ball_location3()$x - 0.12, yend = ball_location3()$y + 0.12), 
                 linewidth = 0.5, color = "red", curvature = 0.25) + 
      geom_curve(aes(x = ball_location3()$x + 0.12, y = ball_location3()$y - 0.12, 
                     xend = ball_location3()$x + 0.12, yend = ball_location3()$y + 0.12), 
                 linewidth = 0.5, color = "red", curvature = -0.25) + 
      xlim(-3.5, 3.5) +
      ylim(-1,7) +
      theme_void()
    
  }, bg = "#ECF0F5")
  
  
  
  # For the changing catch location in 4th model
  ball_location4 = reactive({
    
    # Dividing by certain scalars to make graph seem more proportional
    ball_location4 <- data.frame(input$x_coordinate4/1.25, input$y_coordinate4/1.5)
    colnames(ball_location4) <- c("x", "y")
    ball_location4
    
  })
  
  
  # Plot for the changing catch location in 4th model
  output$EditBallLocation4 = renderPlot({
    
    # dimensions for second plate
    df <- data.frame(x = c(0, 0.884, 0, -0.884), y = c(0, 0.171, 0.342, 0.171))
    dftop <- data.frame(x = c(0, 0.884, 0, -0.884), y = c(0.15, 0.3, 0.39, 0.3))
    dfconnect <- data.frame(x = c(0.884, 0.884, -0.884, -0.884), y = c(0.171, 0.3, 0.3, 0.171))
    
    # dimension parabola for outfield
    f <- function (x) -1/120*(x^2) + 1.75
    numbers <- seq(-5.5,5.5,0.01)
    dfoutfield <- data.frame(x = c(numbers, 5.5, -5.5), y = c(f(numbers), 5, 5))
    
    # dimension for the infield 
    dfinfield <- data.frame(x = c(-5.5,5.5,5.5,-5.5), y = c(-1,-1,5,5))
    
    # dimension parabole for infield grass
    f1 <- function (x) 1/150*(x^2) - 0.75
    numbers <- seq(-2.5,2.5,0.01)
    dfinfieldgrass <- data.frame(x = c(numbers, 5.5, -5.5), y = c(f1(numbers), -1, -1))
    
    
    ggplot(ball_location4(), aes(x = x, y = y)) +
      geom_polygon(dfinfield, mapping=aes(x=x,y=y), fill = "#9B7653") +
      geom_polygon(dfoutfield, mapping=aes(x=x,y=y), fill = "#395D33") +
      geom_polygon(dfinfieldgrass, mapping=aes(x=x,y=y), fill = "#395D33") +
      #stat_function(fun = f, geom = "polygon", colour="green", fill = "green") +
      geom_polygon(df, mapping=aes(x = x, y = y), fill="gray97") +
      geom_polygon(dfconnect, mapping=aes(x = x, y = y), fill="gray97") +
      geom_polygon(dftop, mapping=aes(x = x, y = y), fill="gray97", color = "gray90") +
      # Baseball
      geom_point(size = 5, fill = "white", color = "gray25", pch = 21) +
      # Adding the red stripes on baseball here
      geom_curve(aes(x = ball_location4()$x - 0.1, y = ball_location4()$y - 0.1, 
                     xend = ball_location4()$x - 0.1, yend = ball_location4()$y + 0.1), 
                 linewidth = 0.5, color = "red", curvature = 0.25) + 
      geom_curve(aes(x = ball_location4()$x + 0.1, y = ball_location4()$y - 0.1, 
                     xend = ball_location4()$x + 0.1, yend = ball_location4()$y + 0.1), 
                 linewidth = 0.5, color = "red", curvature = -0.25) + 
      xlim(-5.5, 5.5) +
      ylim(-2,5) +
      theme_void()
    
  }, bg = "#ECF0F5")
  
  
  
  
  # For the changing catch depth in 4th model
  ball_location_depth4 = reactive({
    
    ball_location_depth4 <- data.frame(input$depth4/1.25, input$y_coordinate4/1.5)
    colnames(ball_location_depth4) <- c("depth", "y")
    ball_location_depth4
    
  })
  
  
  # Plot for the catch depth in 4th model
  output$CatchDepth4 = renderPlot({
    
    
    # dimensions for second base
    df1 <- data.frame(x = c(0, 0.884, 0, -0.884), y = c(0, 0.171, 0.342, 0.171))
    dftop1 <- data.frame(x = c(0, 0.884, 0, -0.884), y = c(0.15, 0.3, 0.39, 0.3))
    dfconnect1 <- data.frame(x = c(0.884, 0.884, -0.884, -0.884), y = c(0.171, 0.3, 0.3, 0.171))
    
    # dimension parabola for infield grass
    f1 <- function (y) 1.5*(y-0.3)^2 - 4.15
    numbers <- seq(0.1,0.5,0.01)
    dfinfieldgrass <- data.frame(x = c(f1(numbers), -5.5, -5.5), y = c(numbers, 0.7, -0.1))
    
    # dimension for the infield 
    dfinfield <- data.frame(x = c(-5.5,5.5,5.5,-5.5), y = c(-1,-1,5,5))
    
    # dimension parabola for outfield
    f <- function (x) 0.75*sqrt(-x+10)
    numbers <- seq(-5.5,5.5,0.01)
    dfoutfield <- data.frame(x = c(numbers, 5.5, -5.5), y = c(f(numbers), 5, 5))
    
    
    ggplot(ball_location_depth4(), aes(x = depth, y = y)) +
      geom_polygon(dfinfield, mapping=aes(x = x, y = y), fill="#9B7653") +
      geom_polygon(dfoutfield, mapping=aes(x=x,y=y), fill = "#395D33") +
      geom_polygon(df1, mapping=aes(x = x, y = y), fill="gray97") +
      geom_polygon(dfconnect1, mapping=aes(x = x, y = y), fill="gray97") +
      geom_polygon(dftop1, mapping=aes(x = x, y = y), fill="gray97", color = "gray90") +
      geom_polygon(dfinfieldgrass, mapping=aes(x = x, y = y), fill="#395D33") +
      # Baseball
      geom_point(size = 5, fill = "white", color = "gray25", pch = 21) +
      # Adding the red stripes on baseball here
      geom_curve(aes(x = ball_location_depth4()$depth - 0.1, y = ball_location_depth4()$y - 0.1, 
                     xend = ball_location_depth4()$depth - 0.1, yend = ball_location_depth4()$y + 0.1), 
                 linewidth = 0.5, color = "red", curvature = 0.25) + 
      geom_curve(aes(x = ball_location_depth4()$depth + 0.1, y = ball_location_depth4()$y - 0.1, 
                     xend = ball_location_depth4()$depth + 0.1, yend = ball_location_depth4()$y + 0.1), 
                 linewidth = 0.5, color = "red", curvature = -0.25) + 
      xlim(-5.5, 5.5) +
      ylim(-2, 5) +
      theme_void()
    
  }, bg = "#ECF0F5")
  
  
  
  
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
    
  }, bg = "#ECF0F5")
  
  
  # Baseball field with runner in 3rd model
  output$EditLead3 = renderPlot({
    
    geom_baseball(league = "MLB", rotation = 0, display_range = "infield") +
      geom_point(runner_location3(), mapping = aes(x = x, y = y), size = 5, color = "red") + 
      labs(caption = "Field generated by sportyR")
    
  }, bg = "#ECF0F5")
  
  
  # Baseball field with runner in 4th model
  output$EditLead4 = renderPlot({
    
    geom_baseball(league = "MLB", rotation = 0, display_range = "infield") +
      geom_point(runner_location4(), mapping = aes(x = x, y = y), size = 5, color = "red") + 
      labs(caption = "Field generated by sportyR")
    
  }, bg = "#ECF0F5")
  
  
  
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
      #labs(title = "Frame: {frame_time}") +
      theme(plot.background = element_rect(fill = "#ECF0F5"))
    
    
    anim_save("outfile.gif", animate(p)) # New
    
    
    list(src = "outfile.gif",
         contentType = 'image/gif'
         # width = 400,
         # height = 300,
         # alt = "This is alternate text"
    )}, deleteFile = TRUE)
  
  
  
  
  # The pitch release for the chosen play to animate
 pitch_release_model1calc = reactive({
    
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
    
    prob
    
  })
  
 
  # Text for Above
  output$pitch_release_model1 <- reactive({
    
    # Will prevent an error from showing until input has loaded
    validate(
      need(nrow(play()) != 0, message = '--')
    )
    
    if (pitch_release_model1calc() == "Missing Data"){
      return ("Missing Data")
    }
    paste0(pitch_release_model1calc(), "%")
  })
  
  
  
  # The catcher retrieval for the chosen play to animate
  catcher_retrieval_model2calc = reactive({
    
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
    
    prob
    
  })
  
  
  # Text for Above
  output$catcher_retrieval_model2 <- reactive({
    
    # Will prevent an error from showing until input has loaded
    validate(
      need(nrow(play()) != 0, message = '--')
    )
    
    if (catcher_retrieval_model2calc() == "Missing Data"){
      return ("Missing Data")
    }
    paste0(catcher_retrieval_model2calc(), "%")
  })
  
  # Pitcher Outs Calculation
  output$pitcher_outs2 <- reactive({
    
    # Will prevent an error from showing until input has loaded
    validate(
      need(nrow(play()) != 0, message = '--')
    )
    
    if (catcher_retrieval_model2calc() == "Missing Data"){
      return ("--")
    }
    round(pitch_release_model1calc()/100 - catcher_retrieval_model2calc()/100, digits = 2)
  })
  
  
  
  # The catcher throw for the chosen play to animate
  catcher_throw_model3calc = reactive({
    
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
    
    prob
    
  })
  
  
  # Text for Above
  output$catcher_throw_model3 <- reactive({
    
    # Will prevent an error from showing until input has loaded
    validate(
      need(nrow(play()) != 0, message = '--')
    )
    
    if (catcher_throw_model3calc() == "Missing Data"){
      return ("Missing Data")
    }
    paste0(catcher_throw_model3calc(), "%")
  })
  
  
  
  
  
  # The middle infielder retrieval for the chosen play to animate
  middle_infielder_retrieval_model4calc = reactive({
    
    # Obtaining specific moment (and the first occurrence)
    time <- key_moments %>%
      filter(game_str == play()$game_str & play_id == play()$play_id) %>%
      filter((playerAct == 'SS' | playerAct == '2B') & (event == 'ball acquired' | event == 'ball deflected')) %>%
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
    
    prob
    
  })
  
  
  # Text for Above
  output$middle_infielder_retrieval_model4 <- reactive({
    
    # Will prevent an error from showing until input has loaded
    validate(
      need(nrow(play()) != 0, message = '--')
    )
    
    if (middle_infielder_retrieval_model4calc() == "Missing Data"){
      return ("Missing Data")
    }
    paste0(middle_infielder_retrieval_model4calc(), "%")
  })
  
  # Catcher Outs Calculation
  output$catcher_outs34 <- reactive({
    
    # Will prevent an error from showing until input has loaded
    validate(
      need(nrow(play()) != 0, message = '--')
    )
    
    if ((middle_infielder_retrieval_model4calc() == "Missing Data") | catcher_retrieval_model2calc() == "Missing Data"){
      return ("--")
    }
    round(catcher_retrieval_model2calc()/100 - middle_infielder_retrieval_model4calc()/100, digits = 2)
  })
  
  
  # Infielder Outs Calculation
  output$infielder_outs4 <- reactive({
    
    # Will prevent an error from showing until input has loaded
    validate(
      need(nrow(play()) != 0, message = '--')
    )
    
    if (middle_infielder_retrieval_model4calc() == "Missing Data"){
      return ("--")
    }
    
    if(stolenBase() == 'Yes'){
      return (round(middle_infielder_retrieval_model4calc()/100 - 1, digits = 2))
    }
    else {
      return (round(middle_infielder_retrieval_model4calc()/100, digits = 2))
    }
    
  })
  
  
  
  output$end_result = reactive({
    
    # Will prevent an error from showing until input has loaded
    validate(
      need(nrow(play()) != 0, message = '--')
    )
    
    if(stolenBase() == 'Yes'){
      return ("100%")
    }
    else {
      return ("0%")
    }
    
  })
  
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  
  
}


shinyApp(ui = ui, server = server)
