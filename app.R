#-----------------------------------
# Intro to Shiny
#-----------------------------------
# install.package('car)
library(shiny)
library(shinythemes)
library(rgdal)
library(plyr)
library(leaflet)
library(dplyr)
library(data.table)
library(DT)
library(car)
library(tidyverse)




# This very basic shiny app takes our presidents birthplace
# leaflet map and allows us to filter which presidents are
# displayed by party affiliation.It also displays a data 
# table in another tab. 


# This begins the app
server <- function(input, output) {
  # Here we are reading in our birthplace data
  place_df<-read.csv("place_data.csv")
  #reading  in the shapefile
  zips <- readOGR("Zipcodes_Poly", layer = "Zipcodes_Poly",encoding = "UTF-8")
  
  # using a left_join 
  zips@data <- data.frame(zips@data, place_df[match(zips@data$CODE, place_df$CODE),])
  
  zips@data$Risk_cat<-cut(zips@data$Risk, 4, labels = c("Low level","Med level","High level","Very High level"))
  
  # make ACE variable a factor to colour the states 
  factpal <- colorFactor(c("#F9DC9C", "#F2A15E", "#DF5E65", "#C63541"),
                         zips@data$Risk_cat)
  # writing the popup values
  zip_popup <- paste("<strong>Zip: </strong>", 
                     zips@data$CODE, 
                     "<br><br>Risk Index:", 
                     zips@data$Risk,
                     "<br>Poverty:", 
                     zips@data$Poverty,
                     "<br>Education:", 
                     zips@data$Education,
                     "<br>Unemployment:", 
                     zips@data$Unemployment,
                     "<br>Crime:", 
                     zips@data$Crime,
                     "<br>ACEs:", 
                     zips@data$ACEs)
  
  # coloring by factor level, from ACE variable
  # leaflet map works well with shiny
  # Notice I am putting this in "map" object
  map<-leaflet(zips) %>% fitBounds(-75.05, 39.75, -75.2, 40.05) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(stroke = TRUE, 
                smoothFactor = 0.2, 
                fillOpacity = 0.8,  #.8
                fillColor = ~factpal(Risk_cat),
                color = 'white',
                weight = 1, 
                popup = zip_popup) %>%
    addLegend("topright", 
              colors = c("#F9DC9C", "#F2A15E", "#DF5E65", "#C63541"),
              labels = c("0 - 25", "25 - 49", "50 - 75", "75 - 98"),  
              title = "Risk : Lowest to Highest",
              opacity = 1) #1
  
  
  # output the map
  output$map <- renderLeaflet(map)

  #-------------------
  # Data Table
  #-------------------
  
  # Using DT package, we're making a data table
  output$table2 <- DT::renderDataTable(DT::datatable({
    # Choosing just a few columns to display
    data <- zips@data
    
    # Allowing the dropdowns to filter our code
    if (input$CODE2 != "All") {
      data <- data[data$Risk_cat == input$CODE2, ]
    }
    data
  },
 ))
  
  
  
  #----------------------
  # graph  
  #----------------------  
  # Makinc a column that indicates if a state is one of the original areas or not  
  risk_avg <- mean(place_df$Risk)
  poverty_avg <-mean(place_df$Poverty)
  edu_avg <-mean(place_df$Education)
  uemp_avg <-mean(place_df$Unemployment)
  crime_avg <-mean(place_df$Crime)
  ace_avg <- 82.5
  place_df<-place_df %>% add_row(CODE = "City Average", Risk = risk_avg, Poverty = poverty_avg, Education = edu_avg, Unemployment = uemp_avg, Crime = crime_avg, ACEs = ace_avg)
  
  app_df<-reactive({
    
    sub_temp <- subset(place_df, CODE == input$zip_sel1 | CODE == input$zip_sel2)
    sub_temp$CODE <- factor(sub_temp$CODE, levels = c(input$zip_sel1,input$zip_sel2))
    long_df = gather(sub_temp, val1, val2, Risk:ACEs, factor_key = TRUE)
    
    return(long_df)
    
  })

  # And finally lets make the plot
  output$plot  <- renderPlot({
        
    ggplot(data = app_df()) + 
      geom_bar(aes(x = CODE, y = val2, fill = CODE), stat = 'identity', width = 0.8) + 
      facet_wrap(~val1) +
      scale_y_continuous(limits = c(0,100), name = 'Percent')+
      geom_hline(data = subset(long_df, val1 == 'Poverty'), aes(yintercept = 17.6), size= 0.8) +
      geom_hline(data = subset(long_df, val1 == 'Education'), aes(yintercept = 5.8), size= 0.8) +
      geom_hline(data = subset(long_df, val1 == 'Unemployment'), aes(yintercept = 5), size= 0.8) +
      geom_hline(data = subset(long_df, val1 == 'Crime'), aes(yintercept = 3.7), size= 0.8) +
      geom_hline(data = subset(long_df, val1 == 'ACEs'), aes(yintercept = 63.9), size= 0.8) +
      theme_bw()+
      scale_fill_manual(values = c("#554F66", "#C2D1A4")) +
      theme(text = element_text(size=20), legend.position='none') +
      scale_x_discrete(name = 'Zip Code')
  })
  

} # Closes the server function


#-----------------------------------
# UI
#-----------------------------------
place_df<-read.csv("place_data.csv")
risk_avg <- mean(place_df$Risk)
poverty_avg <-mean(place_df$Poverty)
edu_avg <-mean(place_df$Education)
uemp_avg <-mean(place_df$Unemployment)
crime_avg <-mean(place_df$Crime)
ace_avg <- 82.5
place_df<-place_df %>% add_row(CODE = "City Average", Risk = risk_avg, Poverty = poverty_avg, Education = edu_avg, Unemployment = uemp_avg, Crime = crime_avg, ACEs = ace_avg)


ui <- shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  navbarPage(
    "Philly Place Matters App",
  # This applies preset aesthetic decisions to the entire app
  # Check out this page for more themes: https://rstudio.github.io/shinythemes/
    # Welcome tab
  tabPanel(
    "Welcome!",
    headerPanel("Hello there, Dear Reader!"),
    br(),
    h1(""),
    h2("This dashboard is all about GIS analysis of children’s population health needs and resources in Philadelphia", align = "center"),
    h2(""),
    h2(""),
    # the Image of report front page goes here!
    img(src = "report.png", height = 1200, width = 1300, align = 'center'),
    # the absolute panel is the widget that allows you to select by party
    h2(""),
    h2(""),
    h3("Neighborhood level traits such as poverty, lower education, and high crime are linked with worse mental health outcomes. Social capital, or neighbors that watch out for each other, can protect against the negative impact of neighborhood deterioration. Less is known about how perceived neighborhood trust and safety protects against mental illness."),
    h3("This project uses statistical and spatial (mapping) analyses to better understand the impact of changeable neighborhood characteristics on mental health, and proposes a way to use population level risk factors to assess service need and adequacy of community resources.For the first time, a multiple risk factor index is used to determine higher levels of need across the city of Philadelphia. This work has implications for the behavioral health system, as well as can guide policy and planning for other social and city services. "),
  ),
    # Second tab
    tabPanel(
      "Philly ACEs Map",
      br(),
      h1(""),
      h2("This Map depicts GIS analysis of children’s population health needs and resources in Philadelphia", align = "center"),
      h2(""),
      h2(""),
      # the map is called here
      leafletOutput("map",
                    width = "100%",
                    height = "600px"),
     # the absolute panel is the widget that allows you to select by party
       absolutePanel(
        class = "panel panel-default",
        draggable = TRUE,
        top = 300,
        left = 50,
        right = "auto", # fig out by yourself .. shiny 
        bottom = "auto",
        width = 330,
        height = "auto",
       
      ),
     h2(""),
     h2(""),
     h3("Neighborhood level traits such as poverty, lower education, and high crime are linked with worse mental health outcomes. Social capital, or neighbors that watch out for each other, can protect against the negative impact of neighborhood deterioration. Less is known about how perceived neighborhood trust and safety protects against mental illness."),
     h3("This project uses statistical and spatial (mapping) analyses to better understand the impact of changeable neighborhood characteristics on mental health, and proposes a way to use population level risk factors to assess service need and adequacy of community resources.For the first time, a multiple risk factor index is used to determine higher levels of need across the city of Philadelphia. This work has implications for the behavioral health system, as well as can guide policy and planning for other social and city services. ")
     
    ), #comma very important
  tabPanel(
    "Philly ACEs Map Graph representation",
    headerPanel("Interactive graph comparing Risk Index Factors Across Zip Codes"),
    
    # the map is called here
    sidebarLayout(
      sidebarPanel(
        width = 2,
        selectInput(
          "zip_sel1",
          "Zipcode of Interest: ",
          choices = sort(place_df$CODE),
          selected = '19104'
        ),
        selectInput(
          "zip_sel2",
          "Benchmark Zip Code: ",
          choices = sort(place_df$CODE),
          selected = 'City Average'
        ),
        hr(),
        helpText("Horizontal Lines Indicate National Averages")
      ),

    mainPanel(
      width = 10,
      plotOutput('plot'),
      h4(
        strong("Risk"),
        ": A composite index of measures of poverty, education, unemployment, etc."
      ),
      h4(
        strong("Poverty"),
        ": Precentage of families with children below the poverty level."
      ),
      h4(
        strong("Education"),
        ": Percent with less than 9th grade education."
      ),
      h4(strong("Unemployment"), ': Percentage of Unemployed.'),
      h4(strong("Crime"), ': Shooting victims per 10,000.'),
      h4(
        strong("ACEs"),
        ": Percent with at least one Adverse Childhood Experience."
      )
    )
  )
  ), #comma very important
    # Third tab
    tabPanel(
      "Data Table",
      headerPanel("Examine the ACE's Data"),
      # Create a new Row in the UI for selectInputs
      selectInput(
        "CODE2",
        "Risk_cat:",
        c(
          "All",
          "Low level",
          "Med level",
          "High level",
          "Very High level"
        )
      ),
      # Create a new row for the table.
      DT::dataTableOutput("table2")
    )
  )
))

shinyApp(ui = ui, server = server)
