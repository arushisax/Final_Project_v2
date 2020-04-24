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
library(tm)
library(wordcloud)
require(devtools)
library(wordcloud2)
library(ggpubr)

# Import sentiment data for US map: https://medium.com/@joyplumeri/how-to-make-interactive-maps-in-r-shiny-brief-tutorial-c2e1ef0447da
us_senti <- us_senti_locations

# Categorize sentiment values
us_senti$sent_type <- ifelse(us_senti$sent.value < 0, "Negative", 
                                    ifelse(us_senti$sent.value == 0, "Neutral", 
                                           ifelse(us_senti$sent.value > 0, "Positive", "other")))
# Set Colors for each sentiment value
pal <- colorFactor(
    palette = c('red', 'blue', 'green'),
    domain = us_senti$sent_type
)

####### For Google Search analysis tab
# Load google search trends vs. social distancing table
scatterdata <- joined_dataF #Save this as a RDS object in the shinyapp folder structure 

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
                              
                              h4("Prominent Expressions about Social Distancing"),
                              
                              sidebarLayout(
                                  sidebarPanel(sliderInput("freq",
                                                         "Minimum Frequency:",
                                                         min = 1,  max = 50, value = 15),
                                                        sliderInput("max",
                                                         "Maximum Number of Words:",
                                                         min = 1,  max = 200,  value = 100)),
                              # Show Word Cloud
                              mainPanel(
                                  plotOutput("plot")))),
                              
                            
                     
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
            ))),
        
        tabPanel("Google Search Trends",
                 
                 h3("Does adherence to social distancing measures correlate with Google search activity across the United States?"),
                 
                 br(),
                 
                 h4("[Insert summary sentence here]"),
                 
                 sidebarPanel(
                     helpText("Choose a Google search term to understand its search popularity vs. actual social distancing within each US state"),
                     tags$i("Note: The more negative the Social Distancing score, the greater a state's adherence to social distancing (ex: -10 means more distancing than -5)."),
                     br(),
                     
                     selectInput("term", "Google Search Term:",
                                 choices = list("COVID-19" = "COVID-19",
                                                "coronavirus" = "coronavirus",
                                                "social distancing" = "social distancing",
                                                "All of the above" = "all_terms"
                                               ),
                                 selected = "COVID-19")),
                 
                 mainPanel(plotOutput("scatterplot"))
                 
              
                 )
        
        
        )
    )
        
        
                             
# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Make the Word Cloud
    output$plot <- renderPlot({
        wordcloud(names(data_cleaned), data_cleaned,scale=c(8,0.25),
                     min.freq = input$freq, max.words=input$max,
                     colors=brewer.pal(8, "Dark2"))
    })
    
    #create the map
    output$mymap <- renderLeaflet({
        leaflet(us_senti) %>% 
            setView(lng = -99, lat = 45, zoom = 2)  %>% #setting the view over ~ center of North America
            addTiles() %>% 
            addCircles(data = us_senti, lat = ~ lat, lng = ~ lng, weight = 5, 
                       popup = paste("Sentiment:",sep = " ",us_senti$sent_type, "<br>",
                                     "Sentiment Score:", us_senti$sent.value, "<br>",
                                     "Text:", us_senti$text, "<br>"),
                       label = ~as.character(paste0(sent_type)), 
                       fillOpacity = 0.5, color = ~pal(sent_type), radius = 5)
    })
    
    #Scatterplot with reactive data

    output$scatterplot <- renderPlot({
        
        if(input$term == "coronavirus"){            
            scatterdata %>% 
                ggplot(aes(x=average, y=`coronavirus`, label=Abb)) + geom_point() +theme_classic() + theme(axis.text.x = element_text(angle = 45)) + labs(x="Social Distancing Score", y="Search Term Popularity: 'Coronavirus'") + scale_x_reverse() + geom_smooth(method = "glm", se = FALSE, color = "black") + stat_cor(method="pearson", label.x = 5, label.y = 110) + geom_text(check_overlap = FALSE, nudge_x = 0.05, nudge_y = 1, size = 3)
        }
        else if(input$term == "COVID-19"){
            scatterdata %>% 
                ggplot(aes(x=average, y=`COVID-19`, label=Abb)) + geom_point() +theme_classic() + theme(axis.text.x = element_text(angle = 45)) + labs(x="Social Distancing Score", y="Search Term Popularity: 'COVID-19'") + scale_x_reverse() + geom_smooth(method = "glm", se = FALSE, color = "black") + stat_cor(method="pearson", label.x = 5, label.y = 110) + geom_text(check_overlap = FALSE, nudge_x = 0.05, nudge_y = 2, size = 3)
        }
        else if(input$term == "social distancing"){
            scatterdata %>% 
            ggplot(aes(x=average, y=`social distancing`, label=Abb)) + geom_point() +theme_classic() + theme(axis.text.x = element_text(angle = 45)) + labs(x="Social Distancing Score", y="Search Term Popularity: 'Social Distancing'") + scale_x_reverse() + geom_smooth(method = "glm", se = FALSE, color = "black") + stat_cor(method="pearson", label.x = 5, label.y = 110) + geom_text(check_overlap = FALSE, nudge_x = 0.05, nudge_y = 2, size = 3)
         }
        else if(input$term == "all_terms"){
            scatterdata %>% 
                ggplot(aes(x=average, y=all_terms, label=Abb)) + geom_point() +theme_classic() + theme(axis.text.x = element_text(angle = 45)) + labs(x="Social Distancing Score", y="Avg. Search Popularity of COVID-19-related terms") + scale_x_reverse() + geom_smooth(method = "glm", se = FALSE, color = "black") + stat_cor(method="pearson", label.x = 5, label.y = 110) + geom_text(check_overlap = FALSE, nudge_x = 0.05, nudge_y = 2, size = 3)
        }
    
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
