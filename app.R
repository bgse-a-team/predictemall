library(shiny)
library(maps)
library(mapdata)
library(RMySQL)
library(shinythemes)
library(mapproj)
library(ggplot2)
library(leaflet)
library(googleVis)
library(plotrix)
library(sqldf)

options(shiny.error = function(){})

css <- "
.shiny-output-error { visibility: hidden; }
.shiny-output-error:before {
  visibility: visible;
  content: 'Please Select an Option'; }
}
"
con <- dbConnect(MySQL(),user = "trainer", password = "master", host = "127.0.0.1", dbname = "project")
#con <- dbConnect(MySQL(),user = "almysql", password = "pass", host = "127.0.0.1", dbname = "project")
#con <- dbConnect(MySQL(),user = "root", password = "password", host = "127.0.0.1", dbname = "project")

# Define UI for application
ui <- shinyUI(navbarPage(
  title = "Pokemon Go Predictor",
  #tags$style(type="text/css", css),
  theme = shinytheme("slate"),
  tabPanel("Descriptive",
           sidebarLayout(
             sidebarPanel("Select fields to see pokemon spawns",
                          selectInput("select_continent","Select Continent",dbGetQuery(con,"SELECT DISTINCT continent FROM poke_spawns WHERE country IS NOT NULL ORDER BY continent")),
                          uiOutput("selectedContinent"),
                          uiOutput("Types"),
                          selectInput("select_class","Select Pokemon",dbGetQuery(con,"SELECT DISTINCT a.Name FROM (poke_spawns a inner join pkmn_info b on b.id = a.pokemonId)  WHERE a.Name IS NOT NULL ORDER BY a.Name")),
                          uiOutput("pics",align="center"),
                          sliderInput("time", "Period of Time", min = as.Date("2016-08-04"), max = as.Date("2016-10-12"), 
                                      value = c(as.Date("2016-08-01"), Sys.Date())),
                          actionButton("action", label = "(Pokemon) GO")
             ),
             mainPanel(
               "Spawns in selected continent",
               leafletOutput("map_continent"),
               "Spawns in selected country",
               leafletOutput("map_country")
             )
           )        
  ),
  tabPanel("Who to find",
           sidebarLayout(
             sidebarPanel(
               numericInput("select_longitdue","Input Longitude",value=0),
               numericInput("select_latitude","Input Latitude",value=0)
               ),
             mainPanel("The most important variables to find this pokemon type is")
           )),
  tabPanel("Where to find them",
           sidebarLayout(
             sidebarPanel("Find the most important factors for the spawns of each pokemon type",
                          uiOutput("Types2"),
                          actionButton("action2", label = "(Pokemon) GO")
             ),
             
             mainPanel(
               "Spawns in selected continent"
             )
           )        
  )
))

server <- shinyServer(function(input, output) {
  pokemonGO <- read.csv('pokemonGO.csv')
  pokemonGO$Name<-as.character(pokemonGO$Name)
  
  selectedData <- reactive({
    dbGetQuery(con,sprintf("SELECT longitude,latitude FROM poke_spawns WHERE country = \'%s\'",as.character(input$select_country)))
  })
  
  selectedData2 <- reactive({
    dbGetQuery(con,sprintf("SELECT longitude,latitude FROM poke_spawns WHERE continent = \'%s\' and `type1` = \'%s\'",as.character(input$select_continent),as.character(input$select_typeofpoke)))
  })
  
  selectedData3 <- reactive({
    dbGetQuery(con,sprintf("SELECT longitude,latitude FROM poke_spawns WHERE continent = \'%s\' and country = \'%s\' and `type1` = \'%s\'",as.character(input$select_continent),as.character(input$select_country),as.character(input$select_typeofpoke)))
  })
  
  output$selectedContinent <- renderUI({
    countries<-dbGetQuery(con, sprintf("SELECT DISTINCT country FROM poke_spawns WHERE continent = \'%s\' ORDER BY country", as.character(input$select_continent)))
    conditionalPanel("input.select_continent", selectInput("select_country","Select Country", countries))
  })

  output$Types <- renderUI({
    types<-dbGetQuery(con,"SELECT DISTINCT `type1` FROM (poke_spawns inner join pkmn_info on id = pokemonId)  WHERE `type1` IS NOT NULL ORDER BY `type1`")
    conditionalPanel("input.select_country", selectInput("select_typeofpoke","Select Type of Pokemon", types))
  })
  
  output$Types2 <- renderUI({
    Types2<-dbGetQuery(con,"SELECT DISTINCT `type1` FROM (poke_spawns inner join pkmn_info on id = pokemonId)  WHERE `type1` IS NOT NULL ORDER BY `type1`")
    selectInput("select_typeofpoke2","Select Type of Pokemon", Types2)
  })
  
  get_continent <- eventReactive(input$action, {
    as.data.frame(selectedData2())
  })
  
  get_country <- eventReactive(input$action, {
    as.data.frame(selectedData3())
  })
  
  output$map_continent <- renderLeaflet({
    leaflet(get_continent()) %>%
      addTiles() %>%
      addCircles()
  })
  
  output$map_country <- renderLeaflet({
    leaflet(get_country()) %>%
      addTiles() %>%
      addCircles()
  })
  
  output$pics<-renderUI({
    
    tags$img(src = pokemonGO[pokemonGO$Name==input$select_class,7], alt = "photo",height="200", width="200")
    
  })
  
  selectedinfo <- reactive({
    dbGetQuery(con,sprintf("SELECT * FROM poke_spawns WHERE class = \'%s\'",as.character(input$select_class)))
  })
  
  selectedtype <- reactive({
    dbGetQuery(con,sprintf("SELECT * FROM (poke_spawns inner join pkmn_info on id = pokemonId)  WHERE `type1` = \'%s\'",as.character(input$select_type)))
  })
  
  output$hist_poke <-renderPlot(barplot(table(as.vector(selectedinfo()$weather))))
  output$hist_other <-renderPlot(barplot(table(as.vector(selectedinfo()$weather))))

})

# Run the application 
shinyApp(ui = ui, server = server)