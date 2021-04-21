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
library(shinycssloaders)
library(viridis)
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
  mutate(Joy = joy,
         Surprise = surprise,
         Anger = anger,
         Fear = fear,
         Sadness = sadness,
         Disgust = disgust) %>%
  pivot_longer(Joy:Disgust, names_to = "Emotion", values_to = "Count") %>%
  mutate(Day=days)


summaryTibbleEmotions <- emotions %>%
  summarize(mindate = min(days), maxdate = max(days)) %>%
  head()


minPossibleDateEmotion <- summaryTibbleEmotions$mindate
maxPossibleDateEmotion <- summaryTibbleEmotions$maxdate

#Create data for the news plot


dataNews2 <- read_csv("data/News_Data.csv")


summaryTibbleNews <- dataNews2 %>%
  summarize(mindate = min(publication_date), maxdate = max(publication_date)) %>%
  head()

minPossibleDateNews <- summaryTibbleNews$mindate
maxPossibleDateNews <- summaryTibbleNews$maxdate


# Options for Spinner
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

ui <- dashboardPage(
  dashboardHeader(disable=TRUE),
  dashboardSidebar(disable=TRUE),
  dashboardBody(
    fluidPage(
      tabsetPanel(
        tabPanel("Social Media Sentiment Over Time", 
                 fluidRow(
                   # Boxes need to be put in a row (or column)
                   box(background="light-blue",width=4,
                       # Input: Selector for variable to plot against mpg ----
                       selectInput("sentiment", "Sentiment to display:", 
                                   c("Positive" = "Positive",
                                     "Negative" = "Negative",
                                     "Neutral" = "Neutral"), 
                                   multiple=TRUE,
                                   selected="Positive"),
                       
                       dateRangeInput('dateRange',
                                      label = 'Date range input: yyyy-mm-dd',
                                      start = minPossibleDate, end = maxPossibleDate,
                                      min = minPossibleDate, max = maxPossibleDate
                       ),
                       
                       # Input: Checkbox for whether confidence interval should be included ----
                       checkboxInput("conf", "Show confidence interval", FALSE),
                   ),
                   mainPanel(
                     # Output: Formatted text for caption ----
                     h3(textOutput("caption")),
                     h3(textOutput("error1"), style = "color: red;"),
                     withSpinner(plotlyOutput("sentimentPlot",  width = "500px"), type = 2)
                   )
                 )
        ),
        tabPanel("News Media Sentiment Over Time", 
                 fluidRow(
                   # Boxes need to be put in a row (or column)
                   box(background="light-blue",width=4,
                       selectInput("newsSentiment", "Sentiment to display:", 
                                   c("Positive" = "Positive",
                                     "Negative" = "Negative"),
                                   multiple=TRUE,
                                   selected="Positive"),
                       
                       
                       dateRangeInput('dateRangeNews',
                                      label = 'Date range input: yyyy-mm-dd',
                                      start = minPossibleDateNews, end = maxPossibleDateNews,
                                      min = minPossibleDateNews, max = maxPossibleDateNews
                       ),
                       # Input: Checkbox for whether confidence interval should be included ----
                       checkboxInput("conf2", "Show confidence interval", FALSE),
                       
                   ),
                   mainPanel(
                     # Output: Formatted text for caption ----
                     h3(textOutput("captionNews")),
                     h3(textOutput("error2"), style = "color: red;"),
                     withSpinner(plotlyOutput("sentimentNewsPlot",  width = "500px"), type = 2)
                   )
                 )
        ),
        tabPanel("Emotions over Time", 
                 fluidRow(useShinyjs(),
                          # Boxes need to be put in a row (or column)
                          box(background="light-blue",width=4,
                              selectInput("type", "Graph to Display:", 
                                          c("Bar (useful for short date ranges)" = "Bar",
                                            "Line" = "Line"), selected="Line"),
                              selectInput(
                                inputId = "emotionTypes", label = "Emotions to include:",
                                choices = c("Anger", "Sadness", "Disgust",
                                            "Fear", "Joy", "Surprise"),
                                multiple=TRUE,
                                selected = "Joy", width = "350px"
                              ),
                              
                              dateRangeInput('dateRangeEmotion',
                                             label = 'Date range input: yyyy-mm-dd',
                                             start = minPossibleDateEmotion, end = maxPossibleDateEmotion,
                                             min = minPossibleDateEmotion, max = maxPossibleDateEmotion
                              ),
                          ),
                          mainPanel(
                            # Output: Formatted text for caption ----
                            h3(textOutput("captionEmotion")),
                            h3(textOutput("error3"), style = "color: red;"),
                            withSpinner(plotlyOutput("emotionPlot"), type = 2)
                          )
                 )
        ),
        tabPanel("World Tweet Volume for Starbucks",
                 mainPanel(
                   # Output: Formatted text for caption ----
                   h3("World Tweet Volume for Starbucks"),
                   withSpinner(plotlyOutput("geographyPlot", width="800px"), type = 2)
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
    paste("Sentiment Pattern by Day - Social Media")
  })
  
  formulaTextNews <- reactive({
    paste("Sentiment Pattern by Day - News")
  })
  
  formulaTextEmotion <- reactive({
    if ("Line" %in% input$type) {
      paste("Emotions over Time")
    }
    else {
      paste("Proportion of Emotions over Time")
    }
  })
  
  formulaTextError1 <- reactive({
    tempData <- datasets %>%
      filter(days >= ymd(input$dateRange[1]), days <= ymd(input$dateRange[2]))
    if(dim(tempData)[1] > 0) {
      paste("")
    }
    else {
      paste("No data available in filtered set")
    }
  })
  
  formulaTextError2 <- reactive({
    tempData <- dataNews2 %>%
      filter(publication_date >= ymd(input$dateRangeNews[1]), publication_date <= ymd(input$dateRangeNews[2]))
    if(dim(tempData)[1] > 0) {
      paste("")
    }
    else {
      paste("No data available in filtered set")
    }
  })
  
  formulaTextError3 <- reactive({
    tempData <- emotion_pivot %>%
      filter(Day >= ymd(input$dateRangeEmotion[1]), Day <= ymd(input$dateRangeEmotion[2]))
    if(dim(tempData)[1] <= 0) {
      paste("No data available in filtered set")
    }
    else {
      if ("Line" %in% input$type) {
        tempData <- tempData %>%
          filter(str_detect(Emotion, paste(input$emotionTypes, collapse="|")))
        if(dim(tempData)[1] <= 0) {
          paste("No data available in filtered set")
        }
        else {
          paste("")
        }
      }
      else {
        paste("")
      }
    }
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  
  output$captionNews <- renderText({
    formulaTextNews()
  })
  
  output$captionEmotion <- renderText({
    formulaTextEmotion()
  })
  
  output$error1 <- renderText({
    formulaTextError1()
  })
  
  output$error2 <- renderText({
    formulaTextError2()
  })
  
  output$error3 <- renderText({
    formulaTextError3()
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
    plot = ggplot(data=tempData)
    if (!is.null(input$sentiment) & dim(tempData)[1] > 0) {
      colors <- c("Positive" = "Orange", "Negative" = "Blue", "Neutral" = "Green")
      if ("Positive" %in% input$sentiment) {
          plot = plot +
            geom_smooth(method=loess, mapping=aes(x=days, y=positive, colour="Positive"), se=input$conf)
      }
      if ("Negative" %in% input$sentiment) {
          plot = plot +
            geom_smooth(method=loess, mapping=aes(x=days, y=negative, colour="Negative"), se=input$conf)
      }
      if ("Neutral" %in% input$sentiment) {
          plot = plot +
            geom_smooth(method=loess, mapping=aes(x=days, y=neutral,  colour="Neutral"), se=input$conf)
      }
      plot = 
        plot +
        labs(x = "Date", y="Number of Mentions by Sentiment", colour = "") +
        scale_color_manual(values = colors)
      
      
      plot <- ggplotly(plot, dynamicTicks=TRUE)
      
      len = length(plot$x$data)
      

      for (i in 1:len) {
        if(plot$x$data[[i]]$showlegend){
          text_smooth <- paste0("Date: ",as.Date(as.POSIXct(plot$x$data[[i]]$x, origin='1970-01-01')), 
                                "<br>Count: ",plot$x$data[[i]]$y)
          plot <- plot %>%
            style(text=text_smooth, traces=i)
        }
        else {
          plot <- plot %>%
            style(hoverinfo="none", traces=i)
        }
      }
      
      
    }
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
    if(dim(tempData)[1] <= 0) {
      return(ggplot(data=tempData))
    }
    if ("Line" %in% input$type) {
      tempData <- tempData %>%
        filter(str_detect(Emotion, paste(input$emotionTypes, collapse="|")))
      if(dim(tempData)[1] <= 0) {
        return(ggplot(data=tempData))
      }
      plot = ggplot(data=tempData)
      
      plot <- plot + 
        geom_line(mapping=aes(x=Day, y=Count)) +
        facet_wrap(~ Emotion, nrow=3) +
        labs(x="Date", y = "Frequency (Raw Count)")
      plot <- ggplotly(plot, dynamicTicks=TRUE)
    }
    else {
      colors3 <- c("Joy" = "#DDCC77", "Surprise" = "#CC6677", "Sadness" = "#88CCEE",
                  "Anger"="#332288", "Disgust" = "#117733", "Fear" = "#882255")
      plot = ggplot(data=tempData)
      plot <- plot + 
        geom_col(position="fill", mapping=aes(x=Day, y=Count, fill=Emotion, text=paste("Day: ", Day, "<br>", "Emotion: ", Emotion, "<br>", "Count: ", Count))) + 
        labs(x="Date", y = "Proportion of Emotions", fill="") +
        scale_fill_manual(values=colors3)
      plot <- ggplotly(plot, dynamicTicks=TRUE, tooltip = c("text"))
    }
    plot
  })
  
  output$sentimentNewsPlot <- renderPlotly({
    tempData <- dataNews2 %>%
      filter(publication_date >= ymd(input$dateRangeNews[1]), publication_date <= ymd(input$dateRangeNews[2]))
    if (!is.null(input$newsSentiment)  & dim(tempData)[1] > 0) {
      plot = ggplot(data=tempData)
    }
    else {
      return(ggplot(data=tempData))
    }
    colors2 <- c("Positive" = "Orange", "Negative" = "Blue")
    if ("Positive" %in% input$newsSentiment) {
      plot = plot +
        geom_smooth(method=loess, mapping=aes(x=publication_date, y=pos, colour="Positive"), se=input$conf2)
    }
    if ("Negative" %in% input$newsSentiment) {
      plot = plot +
        geom_smooth(method=loess, mapping=aes(x=publication_date, y=neg, colour="Negative"), se=input$conf2)
    }
    
    plot = 
      plot +
      labs(x = "Date", y="Sentiment Frequency", colour="") +
      scale_color_manual(values = colors2)
    
    
    plot <- ggplotly(plot, dynamicTicks=TRUE)
    
    len = length(plot$x$data)
    
    for (i in 1:len) {
      if(plot$x$data[[i]]$showlegend){
        text_smooth <- paste0("Date: ",as.Date(as.POSIXct(plot$x$data[[i]]$x, origin='1970-01-01')), 
                              "<br>Score: ",plot$x$data[[i]]$y)
        plot <- plot %>%
          style(text=text_smooth, traces=i)
      }
      else {
        plot <- plot %>%
          style(hoverinfo="none", traces=i)
      }
    }
    
    plot
  })
}

shinyApp(ui, server)
