library(shiny)
library(tidyverse)
library(readxl)
library(lubridate)
library(nlme)
datasets <- read_excel("data/sentiments.xlsx")
summaryTibble <- datasets %>%
  summarize(mindate = min(days), maxdate = max(days), 
            maxPos = max(positive), maxNeg = max(negative), 
            maxNeut = max(neutral)) %>%
  head()


minPossibleDate <- summaryTibble$mindate
maxPossibleDate <- summaryTibble$maxdate
maxPos <- summaryTibble$maxPos
maxNeg <- summaryTibble$maxNeg
maxNeut <- summaryTibble$maxNeut

maxY <- maxPos
if (maxNeg > maxY) {
  maxY <- maxNeg
}
if (maxNeut > maxY) {
  maxY <- maxNeut
}



ui <- pageWithSidebar(
  
  # App title ----
  headerPanel("Sentiment By Day"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    
    # Input: Selector for variable to plot against mpg ----
    selectInput("sentiment", "Sentiment to display:", 
                c("Positive" = "Positive",
                  "Negative" = "Negative",
                  "Neutral" = "Neutral"), selected="pos"),
    
    dateRangeInput('dateRange',
                   label = 'Date range input: yyyy-mm-dd',
                   start = minPossibleDate, end = maxPossibleDate,
                   min = minPossibleDate, max = maxPossibleDate
    ),
    sliderInput("maxY", "Maximum Y-axis:",
                min = 0, max = maxY, value = 25000
    ),
  
    # Input: Checkbox for whether pattern line should be included ----
    checkboxInput("pattern", "Show pattern line", FALSE)
    
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    # Output: Formatted text for caption ----
    h3(textOutput("caption")),
    
    # Output: Plot of the requested variable against mpg ----
    plotOutput("sentimentPlot")
  )
)

# Define server logic to plot
server <- function(input, output, session) {
  
  observeEvent(input$sentiment, {
    updateSliderInput(session, "maxY",value=maxY)
  })
  
  # Compute the formula text ----
  formulaText <- reactive({
    paste(input$sentiment, " Sentiment by Day")
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  
  output$sentimentPlot <- renderPlot({
    tempData <- datasets %>%
      filter(days >= ymd(input$dateRange[1]), days <= ymd(input$dateRange[2]))
    
    if (!is.null(input$sentiment))
      plot = ggplot(data=tempData)
      if ("Positive" %in% input$sentiment) {
          plot = plot + 
            geom_point(mapping=aes(x=days, y=positive, colour="Positive"), alpha=.5)
          
          if (input$pattern) {
            plot = plot +
              geom_smooth(method=loess, mapping=aes(x=days, y=positive), colour="black")
          }
      }
      else if ("Negative" %in% input$sentiment) {
        plot = plot + 
          geom_point(mapping=aes(x=days, y=negative, colour="Negative"), alpha=.5)
        if (input$pattern) {
          plot = plot +
            geom_smooth(method=loess, mapping=aes(x=days, y=negative), colour="black")
        }        
      }
      else if ("Neutral" %in% input$sentiment) {
        plot = plot + 
          geom_point(mapping=aes(x=days, y=neutral, colour="Neutral"), alpha=.5)
        
        if (input$pattern) {
          plot = plot +
            geom_smooth(method=loess, mapping=aes(x=days, y=neutral), colour="black")
        }  
      }
      plot = 
        plot +
        labs(x = "Date", y="Number of Mentions by Sentiment") +
        scale_colour_manual("", 
                            breaks = c("Positive", "Negative", "Neutral"),
                            values = c("orange", "blue", "green")) +
        coord_cartesian(ylim=c(0, input$maxY))
      
      plot
    })
}
shinyApp(ui, server)