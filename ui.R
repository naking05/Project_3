#load libraries
library(shiny)
library(tidyverse)
library(plotly)
library(caret)

#data management
load("nba_shots.RData")
players = nba_shots %>% distinct(player_name) %>% pull()
made = nba_shots %>% distinct(shot_made_flag ) %>% pull()
distance = nba_shots %>% distinct(shot_distance) %>% pull()
shotZone = nba_shots %>% distinct(shot_zone_area) %>% pull()
period = nba_shots %>% distinct(period) %>% pull()

#define UI for application
shinyUI(fluidPage(
    
    tagList(
        navbarPage(
            "NBA Player Shooting App",
            
            #describe app
            tabPanel("About The Application", 
                     helpText(h1(tags$b("What does this app do?")),
                        h4("This application is designed to help visualize the shooting statistics of some of the NBA's current top athletes.
                        Users will find various tools for analysis of every shot the athletes have taken over the span of their careers.
                        This application aims to give users control over this analysis by allowing for user input on which players, seasons, 
                        and various other shooting variables to include. For further analysis of NBA data please visit the NBA advanced 
                        statistics", a("website.", href="https://stats.nba.com"))
                     ),
                     tags$hr(),
                     helpText(h1(tags$b("How to use the app?")),
                        h4("The tabs across the top allow users to navigate through the different visualizations and analysis tools within
                           the application. The tabs are Shot Data, Shot Summaries, Shot Clustering, Shot Period Modeling, and Shot Zone Modeling"),
                        h3(tags$u("Shot Data")),
                        h4("The Shot Data tab contains the raw data used for designing this application. On this tab users can search, sort, 
                           and download the data for their own use outside of the application."),
                        br(),
                        h3(tags$u("Shot Summaries")),
                        h4("The Shot Summaries tab provides users with the ability to control different plots to better visualize the data. 
                           Within this tab are four sub tabs. These sub tabs all share the same side panel which allows users to filter by
                           player, season(s) played, and the outcome of the shot. The first sub tab (Spatial Display) allows users to view, on a 
                           basketball court plot, the location of each shot taken by each respective player and the outcome of that shot. The next sub tab 
                           (Basket Distance) allows users to see players shots broken down in a box and whiskers plot. Continuing to the next sub tab 
                           (Court Position), users will find a line chart depicting the percentage of shots taken from each region of the court for each respective
                           player. And lastly, on the final sub tab (Time Remaining) is a feature that gives users the option to select a portion of 
                           shot data they would like to plot and output that data to a .png file"),
                        br(),
                        h3(tags$u("Shot Clustering")),
                        h4("The Shot Clustering page presents a dendrogram to users and gives them the option to choose which methods to use for clustering
                           player shot data"),
                        br(),
                        h3(tags$u("Shot Period Modeling")),
                        h4("The Shot Period Modeling tab provides a predictive model that breaks down shot outcomes based on distance across different quarters of play.
                          This tab gives users the ability to select different options they wish to include in their modeling within the variables: player, season, and 
                          quarter of play."),
                        br(),
                        h3(tags$u("Shot Zone Modeling")),
                        h4("The Shot Zone Modeling tab is similar to the Shot Period Modeling tab. In this tab users can filter between player, season, and different 
                           regions of the court to perform modeling on the outcome of shots at different distances.")
                     )
                     ),
            
            #show raw data
            tabPanel("Shot Data", 
                     sidebarLayout(
                         sidebarPanel(
                             downloadButton("nbadata", "Download"), width = 1.5),
                         
                         mainPanel(
                             h2("NBA Player Shots Information"),
                             dataTableOutput("rawdata")
                         ))
            ),
            
            #user data exploration
            tabPanel("Shot Summaries",
                     #sidebar with a slider input for number of bins 
                     sidebarLayout(
                         sidebarPanel(
                             #drop down menu for player
                             selectInput("player_choice", label = h3("Select Player"),
                                         choices = players, selected = "LeBron James"),
                             
                             #drop down menu for season based on a certain player
                             uiOutput("season_choice"),
                             
                             radioButtons("shots_made", label = h3("Shot Status"), choices = list("all", "made", "missed"))
                             
                             
                         ),
                         
                         #show output based on user selections
                         mainPanel(
                             
                             tabsetPanel(type = "tabs",
                                         tabPanel(h4("Spatial Display"), plotOutput("court_shots")),
                                         tabPanel(h4("Basket Distance"),  plotlyOutput("shot_distances")),
                                         tabPanel(h4("Court Position"), plotOutput("court_position"), br(),
                                                  withMathJax(helpText("*Percent of shot attempts calculated using the following equation: $$\\frac{\\text{n() shots from zone}}{\\text{total season shots}}$$"))),
                                         
                                         
                                         tabPanel(h4("Time Remaining"), 
                                                  plotlyOutput("coupled1"),
                                                  plotOutput("coupled2")
                                         ))
                             
                        )
                    )
                ),
            
              tabPanel("Shot Clustering",
                #sidebar with a slider input for number of bins 
                sidebarLayout(
                  sidebarPanel(
                         #drop down menu for player
                         selectInput("player_choice5", label = h3("Select Player"),
                               choices = players, selected = "LeBron James"),
                    
                         #drop down menu for season based on a certain player
                         uiOutput("season_choice2"),
                    
                         selectInput("hclustMethod", label="Method", choices=list(
                           "Single"="single","Complete"="complete","Average"="average",
                           "Centroid"="centroid"
                         ),selected="complete")
                  ),
                  
                  mainPanel(
                  
                  #show a plot of the generated distribution
                         plotOutput("treePlot"))
                  )
                ),
            
            tabPanel("Shot Period Modeling",
                     #sidebar with a slider input for number of bins 
                     sidebarLayout(
                       sidebarPanel(
                         #drop down menu for player
                         selectInput("player_choice2", label = h3("Select Player"),
                                     choices = players, selected = "LeBron James"),
                         
                         #drop down menu for season based on a certain player
                         uiOutput("season_choice3"),
                         
                         selectInput("variable_choice", label = h3("Quarter"),
                                     choices = period, selected = "1")
                       ),
                     
                       mainPanel(
                         plotOutput("period_model"))
                     )
                ),
            
            tabPanel("Shot Zone Modeling",
                     #sidebar with a slider input for number of bins 
                     sidebarLayout(
                       sidebarPanel(
                         #drop down menu for player
                         selectInput("player_choice3", label = h3("Select Player"),
                                     choices = players, selected = "LeBron James"),
                         
                         #drop down menu for season based on a certain player
                         uiOutput("season_choice4"),
                         
                         selectInput("variable_choice2", label = h3("Court Zone"),
                                     choices = shotZone, selected = "Center(C)")
                       ),
                       
                       mainPanel(
                         plotOutput("zone_model"))
                    )
                )
            ) #close navbarPage
        ) #close taglist
    ) #close fluidPage
) #close shinyUI