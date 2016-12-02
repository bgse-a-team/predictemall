library(shiny)
#library(maps)
#library(mapdata)
library(RMySQL)
library(shinythemes)
#library(mapproj)
library(ggplot2)
library(leaflet)
library(class)
#library(googleVis)
#library(plotrix)
#library(sqldf)
load("./R_Environment.RData")

options(shiny.error = function(){})

# css <- "
# .shiny-output-error { visibility: hidden; }
# .shiny-output-error:before {
#   visibility: visible;
#   content: 'Please Select an Option'; }
# }
# "
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
             sidebarPanel("Select desired fields to see pokemon spawns",
                          selectInput("select_continent","Select Continent",dbGetQuery(con,"SELECT DISTINCT continent FROM poke_spawns WHERE country IS NOT NULL ORDER BY continent")),
                          uiOutput("selectedContinent"),
                          uiOutput("type_or_pkmn"),
                          #radioButtons("selected_choice","Filter by type or pokemon?", c("Pokemon","Type")),
                          uiOutput("by_type"),
                          uiOutput("by_pokemon"),
                          #selectInput("select_class","Select Pokemon",dbGetQuery(con,"SELECT DISTINCT a.Name FROM (poke_spawns a inner join pkmn_info b on b.id = a.pokemonId)  WHERE a.Name IS NOT NULL ORDER BY a.Name")),
                          #uiOutput("pics",align="center"),
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
  tabPanel("Who will I Battle",
           sidebarLayout(
             sidebarPanel(
               sliderInput("u_lati",
                           "Select Latitude",
                           min = -90,
                           max = 90,
                           value = runif(1, min = -85, max = 85),
                           step = 0.01),
               sliderInput("u_longi",
                           "Select Longitude:",
                           min = -180,
                           max = 180,
                           value = runif(1, min = -170, max = 170),
                           step = 0.01),
               #numericInput("select_longitdue","Input Longitude",value=0),
               #numericInput("select_latitude","Input Latitude",value=0),
               actionButton("action2", label = "(Pokemon) GO")
               ),
             mainPanel("The most probable type of pokemon that will appear at this location would be:",
                       textOutput("knn_result"),
                       #imageOutput(type_image),
                       "Some information about the prediction methodology")
                       #textOutput(knn_info))
           )),
  tabPanel("Where are you Pikachu?",
           sidebarLayout(
             sidebarPanel("Optimal conditions for finding a particular type",
                          selectInput("select_typeofpoke2","Select Type of Pokemon",dbGetQuery(con,"SELECT DISTINCT `type1` FROM poke_spawns ORDER BY `type1`")),
                          actionButton("action3", label = "(Pokemon) GO")
             ),
             
             mainPanel("The following plots show the general distribution for the 4 most important variables which determine this pokemon's location "
             )
           )
  )
))

server <- shinyServer(function(input, output) {
  pokemonGO <- read.csv('./pokemonGO.csv')
  pokemonGO$Name<-as.character(pokemonGO$Name)
  
  continent_coords_type <- reactive({
    dbGetQuery(con,sprintf("SELECT longitude,latitude FROM poke_spawns WHERE continent = \'%s\' and type1 = \'%s\'",as.character(input$select_continent),as.character(input$select_typeofpoke)))
  })
  
  continent_coords_pkmn <- reactive({
    dbGetQuery(con,sprintf("SELECT longitude,latitude FROM poke_spawns WHERE continent = \'%s\' and Name = \'%s\'",as.character(input$select_continent),as.character(input$select_pkmn)))
  })
  
  country_coords_type <- reactive({
    dbGetQuery(con,sprintf("SELECT longitude,latitude FROM poke_spawns WHERE continent = \'%s\' and country = \'%s\' and `type1` = \'%s\'",as.character(input$select_continent),as.character(input$select_country),as.character(input$select_typeofpoke)))
  })
  
  country_coords_pkmn <- reactive({
    dbGetQuery(con,sprintf("SELECT longitude,latitude FROM poke_spawns WHERE continent = \'%s\' and country = \'%s\' and Name = \'%s\'",as.character(input$select_continent),as.character(input$select_country),as.character(input$select_pkmn)))
  })
  
  output$selectedContinent <- renderUI({
    countries<-dbGetQuery(con, sprintf("SELECT DISTINCT country FROM poke_spawns WHERE continent = \'%s\' ORDER BY country", as.character(input$select_continent)))
    conditionalPanel("input.select_continent", selectInput("select_country","Select Country", countries))
  })
  
  output$type_or_pkmn <- renderUI ({
    conditionalPanel("input.select_country", radioButtons("selected_choice","Filter by type or pokemon?", c("Type","Pokemon")))
  })

  output$by_type <- renderUI({
    types <- dbGetQuery(con,"SELECT DISTINCT `type1` FROM poke_spawns WHERE `type1` IS NOT NULL ORDER BY `type1`")
    conditionalPanel("input.selected_choice == \"Type\"", selectInput("select_typeofpoke","Select Type of Pokemon", types))
  })
  
  output$by_pokemon <- renderUI ({
    conditionalPanel("input.selected_choice == \"Pokemon\"",
                     selectInput("select_pkmn","Select Pokemon",dbGetQuery(con,"SELECT DISTINCT a.Name FROM (poke_spawns a inner join pkmn_info b on b.id = a.pokemonId)  WHERE a.Name IS NOT NULL ORDER BY a.Name")),
                     uiOutput("pics",align="center"))
  })
  
  output$pics<-renderUI({
    tags$img(src = pokemonGO[pokemonGO$Name==input$select_pkmn,7], alt = "photo",height="200", width="200")
  })
  
  user_choice <- reactive({
    as.character(input$selected_choice)
  })
  
  get_continent_coords <- eventReactive(input$action, {
    if(user_choice() == "Type")
      as.data.frame(continent_coords_type())
    else if (user_choice() == "Pokemon")
      as.data.frame(continent_coords_pkmn())
  })
  
  get_country_coords <- eventReactive(input$action, {
    if(user_choice() == "Type")
      as.data.frame(country_coords_type())
    else if (user_choice() == "Pokemon")
      as.data.frame(country_coords_pkmn())
  })
  
  output$map_continent <- renderLeaflet({
    leaflet(get_continent_coords()) %>%
      addTiles() %>%
      addCircles()
  })
  
  output$map_country <- renderLeaflet({
    leaflet(get_country_coords()) %>%
      addTiles() %>%
      addCircles()
  })
  
  user_coords <- reactive({
    c(as.numeric(input$u_lati), as.numeric(input$u_longi))
  })
  
  predicted_type <- eventReactive(input$action2, {
    #user_coords <- c(as.numeric(input$select_latitude), as.numeric(input$select_longitude))
    #print(user_coords)
    model_knn <- knn(train = data[,c(3,4)], test = user_coords(), cl = data[,"type1"], k = 27)
    as.character(model_knn)
  })
  
  output$knn_result <- renderText({
    as.character(predicted_type())
  })
  # selectedinfo <- reactive({
  #   dbGetQuery(con,sprintf("SELECT * FROM poke_spawns WHERE class = \'%s\'",as.character(input$select_class)))
  # })
  # 
  # selectedtype <- reactive({
  #   dbGetQuery(con,sprintf("SELECT * FROM (poke_spawns inner join pkmn_info on id = pokemonId)  WHERE `type1` = \'%s\'",as.character(input$select_type)))
  # })
  # 
  # output$hist_poke <-renderPlot(barplot(table(as.vector(selectedinfo()$weather))))
  # output$hist_other <-renderPlot(barplot(table(as.vector(selectedinfo()$weather))))

})

# Run the application 
shinyApp(ui = ui, server = server)