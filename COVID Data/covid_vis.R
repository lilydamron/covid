#####################################################################################

#	Engagement: COVID-19 Analysis       	                          							    #

#	Program: 	001_mk_data.r                                						          	    #		

#	Last Update Date: 4/1/2020                                          							#

#																	                                        			    #

#	Purpose: Load and prepare data                                                    #

#																                                          					#

#	Notes: 																	                                          #

#####################################################################################





#I. Setup ----------------------------------------------------------------------------

# A. Clear environment

rm(list = ls())



# B. Set working directory

setwd("T:/Damron/COVID Data/")



# C. Import custom functions 



# D. Import packages

require(tidyverse)

require(data.table)

require(dtplyr)

require(stringr)

require(lubridate)

require(openxlsx)

require(stringdist)

require(sqldf)

require(magrittr)
require(maps)
require(geojsonio)
require(ggplot2)
require(leaflet)
require(shiny)
require(data.table)
require(dplyr)
require(tidycensus)
require(choroplethr)
data(df_pop_state)





#II. Data Loading -------------------------------------------------------------------

covid <- readRDS("covid_dat.rds")
usa_conf <- readRDS(file = "covid_conf.rds")

#census API Key
census_api_key(key = "72b26b2258e68a8ed031249509a5023f4d93450d")

## III. Data Analysis --------------------------------------------------------------
usa <- covid %>% filter(country_region == "us")

usa_conf <- as.data.table(usa_conf)
 
#Gather data by date
usa_conf_g <- usa_conf %>% gather(key = "date", value = "confirmed_cases", `1/22/20`:`4/9/20`)
usa_conf_g$date <- as.Date(usa_conf_g$date, format = "%m/%d/%y")
usa_conf_g$confirmed_cases <- as.numeric(usa_conf_g$confirmed_cases)
usa_conf_g <- as.data.table(usa_conf_g)
usa_conf_state <- usa_conf_g %>% group_by(Province_State, date) %>% summarise(confirmed_cases = sum(confirmed_cases))
usa_conf_state <- as.data.table(usa_conf_state)

##Shiny app
# Input
ui <- fluidPage(
  navbarPage("COVID Data",
             tabPanel("Data Explorer",
                      selectInput(inputId = "datastate", label = "State", choices = c("all states", tolower(state.name)), multiple = FALSE),
                      selectInput(inputId = "datadate", label = "Date", choices = as.Date(c(as.Date("1/22/2020", format = "%m/%d/%Y"):as.Date("4/9/2020", format = "%m/%d/%Y")), origin = "1970-01-01"), multiple = FALSE),
                      #selectInput(inputId = "datacity", label = "City", choices = sort(unique(usa_conf[,"Combined_Key"])), multiple = FALSE),
                      dataTableOutput(outputId = "table")),
             tabPanel("Trends", 
                      sidebarLayout(
                        sidebarPanel(width = 4,
                                     selectInput(inputId = "trendstate1", label = "State 1", choices = tolower(state.name), multiple = FALSE),
                                     selectInput(inputId = "trendstate2", label = "State 2", choices = tolower(state.name), multiple = FALSE)
                                     ),
                        mainPanel(plotOutput(outputId = "trend1"), 
                                  plotOutput(outputId = "trend2"))
                      )
                      ),
             tabPanel("Interactive Map",
                      selectInput(inputId = "mapdate", label = "Date", choices = as.Date(c(as.Date("1/22/2020", format = "%m/%d/%Y"):as.Date("4/9/2020", format = "%m/%d/%Y")), origin = "1970-01-01"), multiple = FALSE),
                      leafletOutput(outputId = "map"))
             
  )
  
)

# Server
server <- function(input, output){
  output$table <- renderDataTable({
    if(input$datastate == "all states"){
      usa_conf_g[date == input$datadate, .(Province_State, Combined_Key, confirmed_cases)]
    } else {
      usa_conf_g[Province_State == input$datastate & date == input$datadate,  .(Province_State, Combined_Key, confirmed_cases)]}})
  output$trend1 <- renderPlot({
    trenddata <- usa_conf[Province_State == input$trendstate1]
    trend_g <- trenddata %>% gather(key = "date", value = "confirmed_cases", `1/22/20`:`4/9/20`)
    trend_g$date <- as.Date(trend_g$date, format ="%m/%d/%y")
    trend_g$confirmed_cases <- as.numeric(trend_g$confirmed_cases)
    pop_state <- df_pop_state$value[df_pop_state$region == input$trendstate1]
    trenddata <- trend_g %>% group_by(date) %>% summarise(confirmed_cases = sum(confirmed_cases)) %>% ungroup() %>% mutate(per_cap = confirmed_cases/pop_state * 100000)
    ggplot(data = trenddata, aes(x = as.factor(date), y = as.numeric(per_cap))) + geom_line() + geom_point() + 
      scale_x_discrete(name = "Date", breaks = c("2020-01-22", "2020-02-01", "2020-02-14", "2020-03-01",
                                                 "2020-03-15", "2020-03-19", "2020-04-01", "2020-04-09"),
                       labels = c("January 22", "February 1", "February 14", "March 1",
                                  "March 15", "March 19","April 1", "April 9")) + scale_y_continuous(name = "Number of Confirmed Cases per 100,000 people",
                                                                                                     limits = c(0, 1000)) + labs(title = input$trendstate1)})
  output$trend2 <- renderPlot({
    trenddata <- usa_conf[Province_State == input$trendstate2]
    trend_g <- trenddata %>% gather(key = "date", value = "confirmed_cases", `1/22/20`:`4/9/20`)
    trend_g$date <- as.Date(trend_g$date, format ="%m/%d/%y")
    trend_g$confirmed_cases <- as.numeric(trend_g$confirmed_cases)
    pop_state <- df_pop_state$value[df_pop_state$region == input$trendstate2]
    trenddata <- trend_g %>% group_by(date) %>% summarise(confirmed_cases = sum(confirmed_cases)) %>% ungroup() %>% mutate(per_cap = confirmed_cases/pop_state * 100000)
    ggplot(data = trenddata, aes(x = as.factor(date), y = as.numeric(per_cap))) + geom_line() + geom_point() + 
      scale_x_discrete(name = "Date", breaks = c("2020-01-22", "2020-02-01", "2020-02-14", "2020-03-01",
                                                 "2020-03-15", "2020-03-19", "2020-04-01", "2020-04-09"),
                       labels = c("January 22", "February 1", "February 14", "March 1",
                                  "March 15", "March 19","April 1", "April 9")) + scale_y_continuous(name = "Number of Confirmed Cases per 100,000 people",
                                                                                                     limits = c(0,1000)) + labs(title = input$trendstate2)})
  
  output$map <- renderLeaflet({
    
    states <- geojsonio::geojson_read("http://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_500k.json", what = "sp")
    
    mapdat <- as.data.frame(usa_conf_state[date == input$mapdate ,])
    mapdat$Province_State <- as.factor(mapdat$Province_State)
    states@data$NAME <- tolower(states@data$NAME)
    states@data <- left_join(states@data, mapdat, by = c("NAME" = "Province_State"))
    states@data$NAME <- factor(states@data$NAME)
    bins <- c(0, 100, 500, 1000, 2500, 5000, 7500, 10000, 50000,  Inf)
    pal <- colorBin("Greens", domain = states$h_mean, bins = bins)
    labels <- sprintf(
      "<strong>%s</strong><br/>Total Confirmed Cases: %s<br/>",
      states$NAME, states$confirmed_cases
    ) %>% lapply(htmltools::HTML)
    leaflet(data = states) %>% addTiles(
      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    ) %>% setView(-98.35, 39.7, zoom = 4) %>% addPolygons(fillColor = ~pal(states$confirmed_cases),weight = 2,
                                                          opacity = 1,
                                                          color = "white",
                                                          dashArray = "3",
                                                          fillOpacity = 0.7,
                                                          highlight = highlightOptions(
                                                            weight = 4,
                                                            color = "#666",
                                                            dashArray = "",
                                                            fillOpacity = 0.7,
                                                            bringToFront = TRUE),
                                                          label = labels
    ) %>% addLegend(pal = pal, values = ~states$confirmed_cases, opacity = 0.7, title = "Confirmed Cases",
                    position = "bottomright")
  })
}


# Knit input and server together to launch app
shinyApp(ui = ui, server = server)
