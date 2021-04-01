library(shiny)
library(tidyverse)
library(readxl)
library(lubridate)
library(nlme)
library(dplyr)
library(plotly)
library(hrbrthemes)
library(shinydashboard)
require(maps)
library(shinyjs)

#Create data for the scatter plot
datasets <- read_excel("data/sentiments.xlsx")
summaryTibble <- datasets %>%
  summarize(mindate = min(days), maxdate = max(days)) %>%
  head()


minPossibleDate <- summaryTibble$mindate
maxPossibleDate <- summaryTibble$maxdate

#Create data for the geography plot
world_map <- map_data("world")
#ggplot(world_map, aes(x=long, y=lat, group=group)) +
#  geom_polygon(fill="lightgray", colour="white")
geography_data <- read_csv("data/geography.csv")

world_map_named <- world_map %>%
  mutate(region = case_when(
    region == "China" & subregion == "Hong Kong" ~ "Hong Kong S.A.R.",
    region == "Trinidad" ~ "Trinidad and Tobago",
    region == "Tobago" ~ "Trinidad and Tobago",
    region == "Virgin Islands" & subregion == " US" ~ "United States Virgin Islands",
    region == "Cyprus" & subregion == "Northern Cyprus" ~ "Northern Cyprus",
    region == "Antigua" ~ "Antigua and Barbuda",
    region == "Barbuda" ~ "Antigua and Barbuda",
    region == "China" & subregion == "Macao" ~ "Macao S.A.R",
    region == "Saint Vincent" ~ "Saint Vincent and the Grenadines",
    region == "Grenadines" ~ "Saint Vincent and the Grenadines",
    region == "Macedonia" ~ "North Macedonia",
    region == "Saint Kitts" ~ "Saint Kitts and Nevis",
    region == "Nevis" ~ "Saint Kitts and Nevis",
    region == "Somalia" & subregion == "Somaliland" ~ "Somaliland",
    region == "Virgin Islands" & subregion == " British" ~ "British Virgin Islands",
    region == "Finland" & subregion == "Aland Islands" ~ "Aland",
    TRUE ~ region
  ))

geography_named <- geography_data %>%
  mutate(countries = case_when(
    countries == "United States of America" ~ "USA",
    countries == "United Kingdom" ~ "UK",
    countries == "Republic of Ireland" ~ "Ireland",
    countries == "The Bahamas" ~ "Bahamas",
    countries == "Republic of Serbia" ~ "Serbia",
    countries == "United Republic of Tanzania" ~ "Tanzania",
    countries == "Cura\xE7ao" ~ "Curacao",
    countries == "Eswatini" ~ "Swaziland",
    countries == "British Indian Ocean Territory" ~ "Chagos Archipelago",
    countries == "The Gambia" ~ "Gambia",
    countries == "Federated States of Micronesia" ~ "Micronesia",
    countries == "East Timor" ~ "Timor-Leste",
    TRUE ~ countries
    
  ))

name_data_change <- geography_named %>%
  left_join(world_map_named, by=c("countries" = "region")) %>%
  filter(is.na(group))

geography_data_with_map <- world_map_named %>%
  left_join(geography_named, by=c("region" = "countries"))

#Create data for the emotions plot
emotions <- read_excel("data/emotions.xlsx")

emotion_pivot <- emotions %>%
  pivot_longer(!days, names_to = "Emotion", values_to = "Count") %>%
  mutate(Day=days)


summaryTibbleEmotions <- datasets %>%
  summarize(mindate = min(days), maxdate = max(days)) %>%
  head()


minPossibleDateEmotion <- summaryTibbleEmotions$mindate
maxPossibleDateEmotion <- summaryTibbleEmotions$maxdate

ui <- dashboardPage(
  dashboardHeader(disable=TRUE),
  dashboardSidebar(),
  dashboardBody(
    fluidPage(
      tabsetPanel(
        tabPanel("Sentiment Over Time", 
                 fluidRow(
                   # Boxes need to be put in a row (or column)
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
                       checkboxInput("pattern", "Show pattern line", FALSE),
                       
                       
                   ),
                   mainPanel(
                     # Output: Formatted text for caption ----
                     h3(textOutput("caption")),
                     
                     # Output: Plot of the requested variable against mpg ----
                     plotlyOutput("sentimentPlot",  width = "500px")
                   )
                 )
        ),
        tabPanel("Emotions over Time", 
                 fluidRow(useShinyjs(),
                   # Boxes need to be put in a row (or column)
                   box(background="orange",width=4,
                       selectInput("type", "Graph to Display:", 
                                   c("Bar" = "Bar",
                                     "Line" = "Line"), selected="Line"),
                       selectInput(
                         inputId = "emotionTypes", label = "Emotions to include:",
                         choices = c("anger", "sadness", "disgust",
                                     "fear", "joy", "surprise"),
                         multiple=TRUE,
                         selected = "joy", width = "350px"
                       ),
                       
                       dateRangeInput('dateRangeEmotion',
                                      label = 'Date range input: yyyy-mm-dd',
                                      start = minPossibleDateEmotion, end = maxPossibleDateEmotion,
                                      min = minPossibleDateEmotion, max = maxPossibleDateEmotion
                       ),
                   ),
                   mainPanel(
                     # Output: Formatted text for caption ----
                     h3("Emotions over Time"),
                     
                     # Output: Plot of the requested variable against mpg ----
                     plotlyOutput("emotionPlot")
                   )
                 )
        ),
        tabPanel("World Tweet Volume",
                 mainPanel(
                   # Output: Formatted text for caption ----
                   h3("World Tweet Volume"),
                   
                   # Output: Plot of the requested variable against mpg ----
                   plotlyOutput("geographyPlot", width="800px")
                 )
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
  
  observeEvent(input$type, {
    
    if("Line" %in% input$type) {
      shinyjs::show(id = "emotionTypes")
    }
    else {
      shinyjs::hide(id = "emotionTypes")
    }
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
  
  output$geographyPlot <- renderPlotly({
    plot <- ggplot(geography_data_with_map, aes(long, lat, group = group,  text=paste("Region: ", region, "\nNumber of Tweets: ", geo_vol)))+
      geom_polygon(aes(fill = log(geo_vol, base=10)), color = "white")+
      scale_fill_viridis_c(option = "C")+
      theme_void() +
      labs(fill="Tweet Volume (Power of 10)")
    axisData <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE
    )
    plot <- ggplotly(plot, tooltip = c("text")) %>%
      layout(xaxis = axisData, yaxis = axisData)

    plot
  })
  
  output$emotionPlot <- renderPlotly({
    
    tempData <- emotion_pivot %>%
      filter(Day >= ymd(input$dateRangeEmotion[1]), Day <= ymd(input$dateRangeEmotion[2]))
    if ("Line" %in% input$type) {
      tempData <- tempData %>%
        filter(str_detect(Emotion, paste(input$emotionTypes, collapse="|")))
      plot = ggplot(data=tempData)
      
      plot <- plot + 
        geom_line(mapping=aes(x=Day, y=Count)) +
        facet_wrap(~ Emotion, nrow=3) +
        labs(x="Date", y = "Frequency (Raw count)")
    }
    else {
      plot = ggplot(data=tempData)
      plot <- plot + 
        geom_col(position="fill", mapping=aes(x=Day, y=Count, fill=Emotion)) + 
        labs(x="Date", y = "Ratio of emotions")
    }
    plot <- ggplotly(plot, dynamicTicks=TRUE)
    plot
  })
}
shinyApp(ui, server)