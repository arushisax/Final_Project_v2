# Arushi is analyzing Twitter sentiment about social distancing and looking into social distancing data as well
# We load the necessary packages that will be used in the application.

library(shiny)
library(tidyverse)
library(shinydashboard)
library(DT)
library(leaflet)
library(ggthemes)
library(shinythemes)
library(leaflet.extras)

#Import sentiment data for US map: https://medium.com/@joyplumeri/how-to-make-interactive-maps-in-r-shiny-brief-tutorial-c2e1ef0447da
us_senti <- us_senti_locations

glimpse(us_senti)

#categorize sentiment values
us_senti$sent_type <- ifelse(us_senti$sent.value < 0, "Negative", 
                                    ifelse(us_senti$sent.value == 0, "Neutral", 
                                           ifelse(us_senti$sent.value > 0, "Positive", "other")))

# Define UI for application that draws a histogram
ui <-   shinyUI(
    navbarPage("COVID-19: Early Public Sentiment about #SocialDistancing",
        theme = shinytheme("united"),
        
        ##########
        ##ABOUT##
        #########
        
        tabPanel("About",
                
                 
                 #title and subtitle
                 
                 h2("How do citizens feel about COVID-19 social distancing and how well are US states adhering to the measure? ", align = "center"),
                 h4(em("An analysis of Twitter activity to understand public reaction in April 2020"), align = "center"),
                 br(),
                 div(),
                 
                 
                 
                 br(),
                 
                 fluidRow(column(2), column(8,
                                            
                                            h4(strong("About this Project")),          
                                            
                                            #text to introduce project
                                            
                                            p("The purpose of the project is to...."),
                                            
                                            br(),
                                            
                                            #text to explain how I selected the accounts to analyze and how I coded for gender
                                            
                                            h4(strong("How this project works")),
                                            
                                            
                                            
                                            p("I chose to analyze ...."),
                                            
                                            span(),
                                            
                                            p("I coded for...")
                 ))),
        tabPanel("Tweet Analysis",
                 tabsetPanel(
                     
                     # this page includes the world cloud and sentiment analysis of specific words found throughout the tweets 
                     
                     tabPanel("Word Cloud",
                              
                              h3("Selected English-language Tweets about Social Distancing (April 2020)"),
                              
                              br(),
                              
                              h4("Prominent Expressions about Social Distancing")),
                     
                     tabPanel("Sentiment Analysis",
                              
                              h3("Positive vs. Negative Sentiments for Selected Themes"),
                              
                              br(),
                              
                              h4("[ ] "))
                     
                     
                     )),
        
        #this tab shows the interactive maps 
        
        tabPanel("Geographic Analysis",
                 
                 h3("How positive, negative, or neutral is sentiment across the United States?"),
                 
                 br(),
                 
                 h4("[Insert summary sentence here]"),
        
             mainPanel( 
                #this will create a space for us to display our map
                leafletOutput(outputId = "mymap"), 
                #this allows me to put the checkmarks ontop of the map to allow people to view earthquake depth or overlay a heatmap
                absolutePanel(top = 60, left = 20, 
                          checkboxInput("markers", "Depth", FALSE),
                          checkboxInput("heat", "Heatmap", FALSE)
            )))
        
        )
    )
        
        
                             
# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #create the map
    output$mymap <- renderLeaflet({
        leaflet(us_senti) %>% 
            setView(lng = -99, lat = 45, zoom = 2)  %>% #setting the view over ~ center of North America
            addTiles() %>% 
            addCircles(data = us_senti, lat = ~ lat, lng = ~ lng, weight = 1,popup = ~as.character(sent_type), label = ~as.character(paste0("Sentiment: ", sep = " ", sent_type)), fillOpacity = 0.5)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
