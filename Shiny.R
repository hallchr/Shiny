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

dbPath1 <- "TMDL_Stream_2010-2019_V3.xlsx"

# load the workbook

wb <- loadWorkbook(dbPath1)

#load the worksheets
SARU_Excel <-  read.xlsx(dbPath1, "SARU", detectDates = T)
LS_1_Excel <-  read.xlsx(dbPath1, "LS_1", detectDates = T)
JO_1_Excel <-  read.xlsx(dbPath1, "JO_1", detectDates = T)
PM_1_Excel <-  read.xlsx(dbPath1, "PM_1", detectDates = T)
NCLD_Excel <-  read.xlsx(dbPath1, "NCLD", detectDates = T)
HC_1_Excel <-  read.xlsx(dbPath1, "HC_1", detectDates = T)
HC_2_Excel <-  read.xlsx(dbPath1, "HC_2", detectDates = T)
NC_1_Excel <-  read.xlsx(dbPath1, "NC_1", detectDates = T)
WC_1_Excel <-  read.xlsx(dbPath1, "WC_1", detectDates = T)
QC_1_Excel <- read.xlsx(dbPath1, "QC_1", detectDates = T)
NC_2_Excel <- read.xlsx(dbPath1, "NC_2", detectDates = T)
PR_2_Excel <- read.xlsx(dbPath1, "PR_2", detectDates = T)
MD_1_Excel <- read.xlsx(dbPath1, "MD_1", detectDates = T)
WD_1_Excel <- read.xlsx(dbPath1, "WD_1", detectDates = T)
PA_1_Excel <- read.xlsx(dbPath1, "PA_1", detectDates = T)
PA_2_Excel <- read.xlsx(dbPath1, "PA_2", detectDates = T)
BY_1_Excel <- read.xlsx(dbPath1, "BY_1", detectDates = T)
MH_1_Excel <- read.xlsx(dbPath1, "MH_1", detectDates = T)
# Combine Worksheets
AllCountsAmbient <- rbind(SARU_Excel, LS_1_Excel, JO_1_Excel, PM_1_Excel,
                          NCLD_Excel, HC_1_Excel,HC_2_Excel, NC_1_Excel, WC_1_Excel, QC_1_Excel,
                          NC_2_Excel, PR_2_Excel, MD_1_Excel, WD_1_Excel, PA_1_Excel, PA_2_Excel, BY_1_Excel, MH_1_Excel)
#Creat Year and Month Vectors

AllCountsAmbient$Month <- month(AllCountsAmbient$Date, label = TRUE, abbr = TRUE)

#get objects in correct form
AllCountsAmbient$pH <- as.numeric(AllCountsAmbient$pH)
AllCountsAmbient$Turbidity <- as.numeric(AllCountsAmbient$Turbidity)

AllCountsAmbient$Temp.Type <- ifelse(AllCountsAmbient$TEMP < 16, "below", "above")
AllCountsAmbient$DO.Type <- ifelse(AllCountsAmbient$DO > 9.5, "below", "above")
AllCountsAmbient$Turbidity.Type <- ifelse(AllCountsAmbient$Turbidity < 4, "below", "above")

AllCountsAmbient2019 <- subset(AllCountsAmbient, Year %in% "2019")
AllCountsAmbient2018 <- subset(AllCountsAmbient, Year %in% "2018")
AllCountsAmbient2018_2019 <- subset(AllCountsAmbient, Year %in% c("2018", "2019"))


AllCountsAmbient$CurrentYear <- '2010-2018'
AllCountsAmbient$CurrentYear[AllCountsAmbient$Year == '2019'] <- '2019'

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

AllCountsAmbient3 <- AllCountsAmbient[, c("TEMP", "DO", "Site", "Specific.Cond", "Turbidity", "Fecal.Coliform", "Year", "Precipitation", "BIBI")]
nms3 <- names(AllCountsAmbient3)

ui <- fluidPage(
  
  headerPanel("Bothell Water Quality"),
  sidebarPanel(
    sliderInput('sampleSize', 'Sample Size', min = 1, max = nrow(AllCountsAmbient3),
                value = 1000, step = 50, round = 0),
    selectInput('x', 'X', choices = nms3, selected = "Site"),
    selectInput('y', 'Y', choices = nms3, selected = "TEMP"),
    selectInput('color', 'Color', choices = c("Site", "Year", "Precipitation"), selected = "Site"),
    
    # selectInput('facet_row', 'Facet Row', c(None = '.', nms2), selected = "Site"), to choose all categories as facet type
    selectInput('facet_row', 'Facet Row', c(None = '.', "Site")),
    selectInput('facet_col', 'Facet Column', c(None = '.', "Year", "Precipitation")),
    sliderInput('plotHeight', 'Height of plot (in pixels)', 
                min = 100, max = 2000, value = 1000)
  ),
  
  
  mainPanel(
    plotlyOutput('trendPlot', height = "900px")
  )
)

server <- function(input, output) {
  
  #add reactive data information. Dataset = built in diamonds data
  dataset <- reactive({
    AllCountsAmbient3[sample(nrow(AllCountsAmbient3), input$sampleSize),]
  })
  
  output$trendPlot <- renderPlotly({
    
    # build graph with ggplot syntax
    p <- ggplot(dataset(), aes_string(x = input$x, y = input$y, color = input$color)) + 
      geom_boxplot()
    
    # if at least one facet column/row is specified, add it
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .') p <- p + facet_grid(facets)
    
    ggplotly(p) %>% 
      layout(height = input$plotHeight, autosize=TRUE)
    
  })
  
}

shinyApp(ui, server)

