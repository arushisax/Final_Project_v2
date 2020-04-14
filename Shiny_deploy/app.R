# Arushi is analyzing Twitter sentiment about social distancing and looking into social distancing data as well
# We load the necessary packages that will be used in the application.

library(shiny)
library(tidyverse)
library(shinydashboard)
library(DT)
library(leaflet)
library(ggthemes)
library(shinythemes)

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
                 
                 h4("[Insert summary sentence here]"))  
        
        
        )
    )
        
        
                             
# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
