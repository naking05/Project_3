#load libraries and source data
library(shiny)
library(tidyverse)
library(plotly)
library(caret)    
library(dplyr)
library(ggiraphExtra)

#data management
source("helpers.R")
load("nba_shots.RData")
players = nba_shots %>% distinct(player_name) %>% pull()
made = nba_shots %>% distinct(shot_made_flag ) %>% pull()
distance = nba_shots %>% distinct(shot_distance) %>% pull()
shotZone = nba_shots %>% distinct(shot_zone_area) %>% pull()
period = nba_shots %>% distinct(period) %>% pull()
actions = nba_shots %>% distinct(action_type)


#define plot of court
gg_court = make_court()

#define server logic 
shinyServer(function(input, output) {
    
    #output raw data in table
    output$rawdata = renderDataTable({
        nba_shots
    })
    
    #user option to download data
    output$nbadata <- downloadHandler(
        filename = function() {
            paste("nbashotinfo", ".csv", sep = "")
        },
        content = function(file) {
            write.csv(nba_shots, file, row.names = FALSE)
        })
    
    #set range of seasons based on player choice
    output$season_choice <- renderUI({
        seasons = nba_shots %>% filter(player_name == input$player_choice) %>% 
            distinct(season) %>% pull()
        
        selectizeInput("season_choice", label = "Select Season", choices = seasons,
                       selected = seasons[1], multiple = TRUE)
    })
    
    #subset data by selected player using reactive expression
    player_data = reactive({
        filter(nba_shots, player_name == input$player_choice)
    })
    
    #shot display plot
    output$court_shots <- renderPlot({
        gg_court + geom_point(data = filter(player_data(), season %in% input$season_choice),
                              alpha = 0.75, size = 2.5,
                              aes(loc_x, loc_y, color = shot_made_flag, shape = season)) +
            scale_color_manual("", values = c(made = "blue", missed = "orange"))
    })
    
    #basket distance box and whiskers
    output$shot_distances <- renderPlotly({
        nba_shots %>%
            filter(if(input$shots_made != 'all')  (shot_made_flag == input$shots_made) else TRUE) %>%
            plot_ly(y = ~shot_distance, color = ~player_name, type = "box") %>%
            layout(showlegend = FALSE)
    })
    
    
    #court position line chart
    output$court_position <- renderPlot({
        # subset data by selected player and season(s)
        nba_subset = player_data() %>%
            select(player_name, season, shot_zone_area, shot_made_flag, shot_attempted_flag, shot_type, action_type) %>%
            filter(shot_zone_area != "Back Court(BC)") %>%
            group_by(player_name, season) %>%
            mutate(zone = ifelse(shot_zone_area %in% c("Right Side(R)", "Right Side Center(RC)"), "Right Side",
                                 ifelse(shot_zone_area %in% c("Center(C)") , "Center", "Left Side")),
                   total_season_shots = n()) %>%
            ungroup() %>%
            group_by(player_name, season, zone) %>%
            summarize(total_season_shots = first(total_season_shots), attempts_percent = n()/total_season_shots) %>%
            ungroup() 
        
        ggplot(nba_subset , aes(season, attempts_percent, group = zone, color = zone)) + 
            theme_bw() + geom_line(lwd = 2) + ylab("percent of shot attempts") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                  legend.position = "bottom")
    })
    
    #time remaining interactive user plot
    output$coupled1 <- renderPlotly({
        player_data() %>%
            filter(period < 5) %>% # exclude overtime play
            filter(if(input$shots_made != 'all')  (shot_made_flag == input$shots_made) else TRUE) %>%
            # add plot
            plot_ly(mode = "markers", type = "scatter", showlegend = FALSE,
                    source = "time_plot", key = ~shot_id) %>% 
            layout(
                xaxis = list(title = "distance from basket (feet)"),
                yaxis = list(title = "time remaining (minutes)"),
                dragmode = "select"
            ) %>%
            add_trace(x = ~shot_distance, y = ~ time_remaining,  
                      marker = list(
                          color = 'rgba(245, 137, 15, 0.1)',
                          size = 10,
                          line = list(
                              color = 'rgb(4, 2, 0)',
                              width = 0.5
                          ))) %>%
            add_segments(x = 0, xend = 100, y = 12, yend = 12) %>%
            add_segments(x = 0, xend = 100, y = 24, yend = 24) %>%
            add_segments(x = 0, xend = 100, y = 36, yend = 36) %>%
            add_segments(x = 0, xend = 100, y = 48, yend = 48) %>%
            add_annotations(x = c(80, 80, 80, 80),
                            y = c(12, 24, 36, 48),
                            text = c("quarter 4 start", "quarter 3 start", "quarter 2 start", "game start"))
        
    })
    
    
    output$coupled2 <- renderPlot({
        selected <- event_data("plotly_selected", source = "time_plot")
        
        player_data_subset = filter(player_data(), shot_id %in% selected$key)
        
        gg_court + geom_point(data = player_data_subset,
                              alpha = 0.75, size = 2.5,
                              aes(loc_x, loc_y, color = shot_made_flag)) +
            scale_color_manual("", values = c(made = "blue", missed = "orange"))
        
        
    })
    
     #set range of seasons based on player choice
     output$season_choice2 <- renderUI({
      seasons = nba_shots %>% filter(player_name == input$player_choice) %>% 
        distinct(season) %>% pull()
      
       selectizeInput("season_choice2", label = "Select Season", choices = seasons,
                     selected = seasons[1], multiple = TRUE)
    })
     
     #set range of seasons based on player choice
     output$season_choice3 <- renderUI({
       seasons = nba_shots %>% filter(player_name == input$player_choice) %>% 
         distinct(season) %>% pull()
       
       selectizeInput("season_choice3", label = "Select Season", choices = seasons,
                      selected = seasons[1], multiple = TRUE)
     })
    
    #set range of seasons based on player choice
     output$season_choice4 <- renderUI({
       seasons = nba_shots %>% filter(player_name == input$player_choice) %>% 
         distinct(season) %>% pull()
       
       selectizeInput("season_choice4", label = "Select Season", choices = seasons,
                      selected = seasons[1], multiple = TRUE)
     })
     
     #hierarchical clustering of shot data
     output$treePlot <- renderPlot({
       nbaplayerselect <- nba_shots %>% filter(player_name == input$player_choice5, season == input$season_choice2)
       df <- data.frame(nbaplayerselect$shot_distance, nbaplayerselect$shot_made_numeric)
       hierClust <- hclust(dist(df), method = input$hclustMethod)
       plot(hierClust)
     })
     

     #model shot outcome by distance based on quarter of play
     output$period_model <- renderPlot({
       selected_player <- nba_shots %>% filter(player_name == input$player_choice2, period == input$variable_choice, season == input$season_choice3)
       glmFit <- glm(shot_made_numeric ~ shot_distance, data = selected_player, family = "binomial")
       
       ggiraphExtra::ggPredict(glmFit)
     })
     
     #model shot outcome by distance based on zone of court
     output$zone_model <- renderPlot({
       selected_player2 <- nba_shots %>% filter(player_name == input$player_choice3, shot_zone_area == input$variable_choice2, season == input$season_choice4)
       glmFit <- glm(shot_made_numeric ~ shot_distance, data = selected_player2, family = "binomial")
       
       ggiraphExtra::ggPredict(glmFit)
     })
})