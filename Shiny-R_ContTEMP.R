#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



#To open excel in R
library(openxlsx)
# To plot using ggplot2
library(ggplot2)
#To plot side by side or on top of each other
library(gridExtra)
#To use date_break functinoallity
library(scales)
library(lubridate)
library(rkt)
library(EnvStats)
library(zoo)
library(dplyr)
library(Kendall)
library(boot)
library(magrittr)
library(ggpubr)
library(ggthemes)
library(plotly)
library(psych)
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(rsconnect)

#path of spreadsheet
dbPath1 <- "All_Loggers.xlsx"

#load the worksheets
NC <-  read.xlsx(dbPath1, "NC", detectDates = T)
HC <-  read.xlsx(dbPath1, "HC", detectDates = T)
PA <-  read.xlsx(dbPath1, "PA", detectDates = T)
WC <-  read.xlsx(dbPath1, "WC", detectDates = T)


#combine worksheets

AllCounts<- rbind(NC, HC, PA, WC)

#add logical for above 16

AllCounts$Temperature.Above.16 <- AllCounts$Seven_DADMax>16
AllCounts$Temp.Type <- ifelse(AllCounts$Seven_DADMax < 16, "below", "above")

nms1 <- names(AllCounts)

f <- list(
  family = "helvetica",
  size = 18,
  color = "Black"
)
y <- list(
  title = "Summer Dissolved Oxygen (mg/L)",
  titlefont = f
)
x <- list(
  title = "Temperature (Degrees Celcius)",
  titlefont = f
)


ui <- fluidPage(

  headerPanel("Bothell Continuous Temperature Loggers"),
  sidebarPanel(
    #sliderInput('sampleSize', 'Sample Size', min = 1, max = nrow(AllCounts),
                #value = 10000, step = 50, round = 0),
    selectInput('x', 'X', choices = "Date", selected = "Date"),
    selectInput('y', 'Y', choices = "Seven_DADMax", selected = "Seven_DadMax"),
    selectInput('color', 'Color', choices = "Temp.Type", selected = "Temp.Type"),
    
    #selectInput("Site", "Choose Site:",
                       #choices = c("HC", "NC", "PA", "WC"),
                       #textOutput("txt")),
    dateRangeInput("Date", "Date Range:",
                   start = "2011-06-06",
                   end = max(AllCounts$Date)),
    
    # selectInput('facet_row', 'Facet Row', c(None = '.', nms2), selected = "Site"), to choose all categories as facet type
    selectInput('facet_row', 'Facet Row', choices = "Stream",, selected = "Stream"),
    selectInput('facet_col', 'Facet Column', c(None = '.', "Stream")),
    sliderInput('plotHeight', 'Height of plot (in pixels)', 
                min = 100, max = 1000, value = 700)
  ),
  
  
  mainPanel(
    plotlyOutput('trendPlot', height = "900px")
  )
)

server <- function(input, output) {
  
  #add reactive data information. Dataset = built in diamonds data
  dataset <- reactive({
    filter(AllCounts, between(Date, input$Date[1], input$Date[2]))
  })
  
  
  output$trendPlot <- renderPlotly({
    #subset using data selected by user

    # build graph with ggplot syntax (dataset())
    p <- ggplot(dataset(), aes_string(x = input$x, y = input$y, color = input$color)) + labs(y = "7 Day Avg Max Daily Temp", x = "Date") +
      geom_point() + geom_hline(yintercept = 16, color = "red") + theme_bw() + theme(axis.title.y = element_text(size=18), title = element_text(size = 18), axis.text.x = element_text(size = 12, angle = 65, vjust = 0.6),
                                         axis.text.y = element_text(size = 15), legend.title = element_text(size=15), legend.text = element_text(size=15), legend.position = "none")
    p
    # if at least one facet column/row is specified, add it
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .') p <- p + facet_grid(facets)
    
    ggplotly(p) %>% 
      layout(height = input$plotHeight, autosize=TRUE)
    
  })
  
  
  
}

shinyApp(ui, server)

