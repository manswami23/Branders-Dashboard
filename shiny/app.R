library(shiny)
library(tidyverse)
library(readxl)
library(lubridate)
library(nlme)
datasets <- read_excel("data/sentiments.xlsx")
dateTibble <- datasets %>%
  summarize(mindate = min(days), maxdate = max(days)) %>%
  head()

minPossibleDate <- dateTibble$mindate
maxPossibleDate <- dateTibble$maxdate


ui <- pageWithSidebar(
  
  # App title ----
  headerPanel("Sentiment By Day"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    
    # Input: Selector for variable to plot against mpg ----
    selectInput("sentiment", "Sentiment to display:", 
                c("Positive" = "pos",
                  "Negative" = "neg",
                  "Neutral" = "neut"), selected="pos"),
    
    dateRangeInput('dateRange',
                   label = 'Date range input: yyyy-mm-dd',
                   start = minPossibleDate, end = maxPossibleDate,
                   min = minPossibleDate, max = maxPossibleDate
    ),
  
    # Input: Checkbox for whether trend line should be included ----
    checkboxInput("trend", "Show trend line", FALSE)
    
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    # Output: Formatted text for caption ----
    h3(textOutput("caption")),
    
    # Output: Plot of the requested variable against mpg ----
    plotOutput("mpgPlot")
  )
)

# Define server logic to plot
server <- function(input, output) {
  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  # output$caption and output$mpgPlot functions
  formulaText <- reactive({
    paste("Sentiment ~ Day")
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  
  output$mpgPlot <- renderPlot({
    tempData <- datasets %>%
      filter(days >= ymd(input$dateRange[1]), days <= ymd(input$dateRange[2]))
    
    if (!is.null(input$sentiment)) {
      plot = ggplot(data=tempData)
      if ("pos" %in% input$sentiment) {
          plot = plot + 
            geom_point(mapping=aes(x=days, y=positive, colour="Positive"), alpha=.5)
          
          if (input$trend) {
            plot = plot +
              geom_smooth(mapping=aes(x=days, y=positive), colour="black")
          }
      }
      else if ("neg" %in% input$sentiment) {
        plot = plot + 
          geom_point(mapping=aes(x=days, y=negative, colour="Negative"), alpha=.5)
        if (input$trend) {
          plot = plot +
            geom_smooth(mapping=aes(x=days, y=negative), colour="black")
        }        
      }
      else if ("neut" %in% input$sentiment) {
        plot = plot + 
          geom_point(mapping=aes(x=days, y=neutral, colour="Neutral"), alpha=.5)
        
        if (input$trend) {
          plot = plot +
            geom_smooth(mapping=aes(x=days, y=neutral), colour="black")
        }  
      }
      plot = 
        plot +
        labs(x = "Date", y="Number of Mentions by Sentiment") +
        scale_colour_manual("", 
                            breaks = c("Positive", "Negative", "Neutral"),
                            values = c("orange", "blue", "green"))
        
      plot
    }
  })
}
shinyApp(ui, server)