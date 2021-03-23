library(shiny)
library(tidyverse)
library(readxl)
library(lubridate)
library(nlme)
library(dplyr)
library(plotly)
library(hrbrthemes)
library(shinydashboard)

datasets <- read_excel("data/sentiments.xlsx")
summaryTibble <- datasets %>%
  summarize(mindate = min(days), maxdate = max(days)) %>%
  head()


minPossibleDate <- summaryTibble$mindate
maxPossibleDate <- summaryTibble$maxdate

ui <- dashboardPage(
  dashboardHeader(disable=TRUE),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
        box(background="orange",width=4,
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
          
          # Input: Checkbox for whether pattern line should be included ----
          checkboxInput("pattern", "Show pattern line", FALSE)
          
        ),
        box(background="orange", width= 8,
          mainPanel(
            # Output: Formatted text for caption ----
            h3(textOutput("caption")),
            
            # Output: Plot of the requested variable against mpg ----
            plotlyOutput("sentimentPlot",  width = "500px")
          )
        )
    )
  )
)


# Define server logic to plot
server <- function(input, output, session) {
  
  # Compute the formula text ----
  formulaText <- reactive({
    paste(input$sentiment, " Sentiment by Day")
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  
  output$sentimentPlot <- renderPlotly({
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
          geom_point(mapping=aes(x=days, y=neutral, colour="Neutral"),alpha=.5)
        
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
                            values = c("orange", "blue", "green"))
      
      
      plot <- ggplotly(plot, dynamicTicks=TRUE) %>%
        style(hoverinfo = "none", traces = 3)
      
      text_smooth <- paste0("Date: ",as.Date(as.POSIXct(plot$x$data[[2]]$x, origin='1970-01-01')), 
                       "<br>Count: ",plot$x$data[[2]]$y)
      text_point <-  paste0("Date: ",as.Date(as.POSIXct(plot$x$data[[1]]$x, origin='1970-01-01')), 
                          "<br>Count: ",plot$x$data[[1]]$y)

      plot <- plot %>%
        style(text=text_smooth, traces=2) %>%
        style(text=text_point, traces=1)
      
      plot
    })
}
shinyApp(ui, server)