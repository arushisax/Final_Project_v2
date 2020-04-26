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
library(geojsonio)
library(rgdal)

# Import sentiment data for US map: https://medium.com/@joyplumeri/how-to-make-interactive-maps-in-r-shiny-brief-tutorial-c2e1ef0447da

us_senti <- read_rds("us_senti_locations.rds")

# Categorize sentiment values
us_senti$sent_type <- ifelse(us_senti$sent.value < 0, "Negative", 
                                    ifelse(us_senti$sent.value == 0, "Neutral", 
                                           ifelse(us_senti$sent.value > 0, "Positive", "other")))
# Set Colors for each sentiment value
pal <- colorFactor(
    palette = c('red', 'blue', 'green'),
    domain = us_senti$sent_type
)

# Load data for word cloud
data_cleaned <- read_rds("wordcloud_data.rds")

####### For Google Search analysis tab
# Load google search trends vs. social distancing table

scatterdata <- read_rds("joined_dataF.rds")


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
                                            
                                            p("The purpose of the project is to understand public sentiment about social distancing by looking at Twitter data and comparing it to other data sets such as Google Search Trends and Google Mobility data.
                                              I used three different data sets. The first data set was a random sample of 18,000 Tweets from the week of April 12,2019 related to 'social distancing'. The second data set was Google Mobility data by state which demonstrates the average deviance in population movement vs. baseline. The last dataset was the average popularity of certain COVID-19 related search terms by US state."),
                                            
                                            br(),
                                            
                                            #text to explain how I selected the accounts to analyze and how I coded for gender
                                            
                                            h4(strong("How this project works")),
                                            
                                            
                                            
                                            p("My analysis combines sentiment analysis with correlation analysis. First, I seek to understand the general themes and sentiments demonstrated by the Twitter activity. This is reflective of the social pulse and sentiment about social distancing throughout the United States. Next, I seek to understand how does public sentiment by state compare against each state's actual adherence to Social Distancing measures. Adherence is measured by the Google mobility data. The more negative a state's average score, the more their average movement has decreased versus baseline activity and therefore the more they are social distancing. Lastly, I look to understand the average social distancing score of each and see if correlates with their search activity"),
                                            
                                            span(),
                                            
                                            p("I found that there is strong correlation between high social distancing adherence and search activity about those concepts. This leads me to believe that as individuals are forced or inclined to social distance, they want to increase their awareness of the topic. One can also make the reverse claim that as people search more about the topic, they are more likely to social distance. However, I chose social distancing adherence as my indepedent variable, as opposed to the outcome variable, because social distancing is more likely to be an exogenous directive mandated by local and /or state governments. Note that certain states could have differing social distancing scores for a variety of reasons. Firstly, some states have stricter lockdown directives than others (i.e. California). Secondly, other states are less dense and less urban to begin with therefore it is difficult to deviate from baseline activity if baseline activity and movement was already low (i.e. Wyoming). However, it is reasonable to state that as social distancing increases within a US state, the appetite to learn about it also increases.")
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
                              
                              sidebarPanel(
                                    helpText("The first chart demonstrates the sentiment stregnth (polarity) of common words that were found across Tweets related to social distancing. Words with strong positive polarity reflect higher positive number
                                             and words with strong negative polarity are reflected by high negative numbers.'Virus', 'hard', and 'death' had the strongest negative polarity meanwhile 
                                             'happy', 'safe', and 'Trump' have the strongest positive connotations in this Twitter data set."
                                      
                                  )),
                              mainPanel(tabsetPanel(
                                  tabPanel(
                                        imageOutput("polarity_chart2"),
                                        )
                            )
                            ),
                                br(),
                                br(),
                                br(),
                               
                             sidebarPanel(
                                helpText("This chart shows the frequency of most common positive and negative words in the Social Distancing tweets. 'Virus', 'Hard', 'Death' and 'Risk' were most common negative themes whereas as 'happy', 'safe', 'trump', and 'love' where most commonly used in a positive context. These findings are consistent with the Word Cloud in earlier tab.")),
                            
                            mainPanel(tabsetPanel(
                                    tabPanel(
                                        imageOutput("freq_chart"),
                                    ))
                                    )
                            
                     
                     
                     ))),
        
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
                          checkboxInput("markers", "Sentiment", FALSE),
                          checkboxInput("heat", "Social Distancing", FALSE)
            ))),
        
        tabPanel("Geographic Analysis v2",
                 
                 h3("How positive, negative, or neutral is sentiment across the United States?"),
                 
                 br(),
                 
                 h4("[Insert summary sentence here]"),
                 
                 ),
        
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
    
    # First Sentiment Analysis Chart (Polarity) 
    output$polarity_chart2 <- renderImage({
        list(
            src = "polarity_chart2.png",
            contentType = 'image/png',
            width = 600,
            height = 400
        )
    }, deleteFile = FALSE)
    
    # Second Sentiment Analysis Chart (Frequency) 
    output$freq_chart <- renderImage({
        list(
            src = "freq_chart.png",
            contentType = 'image/png',
            width = 600,
            height = 400
        )
    }, deleteFile = FALSE)
    
    
    # create the map
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
