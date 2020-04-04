#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a plot
ui <- fluidPage(
    
    mainPanel (
        plotOutput("word_cloud")
    )
)

# Define server logic required to draw a plot
server <- function(input, output) {
    
    output$word_cloud <- renderImage({
        list(src = "word_cloud.png",
             contentType = 'png',
             width = 300,
             height = 300,
             alt = " ")
    }, deleteFile = FALSE)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
