library(shiny)
library(RMySQL)
library(shinythemes)
library(ggplot2)
library(leaflet)
library(class)
library(plotly)
#library(googleVis)
#library(plotrix)
#library(sqldf)
load("./R_Environment.RData")
cons <- dbListConnections(MySQL())
for (con in cons) {
  dbDisconnect(con)
}

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
                          uiOutput("region"),
                          uiOutput("type_or_pkmn"),
                          uiOutput("by_type"),
                          uiOutput("by_pokemon"),
                          sliderInput("time", "Period of Time", min = as.Date("2016-08-04"), max = as.Date("2016-10-12"), 
                                      value = c(as.Date("2016-08-01"), Sys.Date())),
                          actionButton("action", label = "(Pokemon) GO")
             ),
             mainPanel(
               fluidRow(
                 column(6,
                        titlePanel("Spawns in selected continent"),
                        leafletOutput("map_continent")
                 ),
                 column(6,
                        titlePanel("Spawns by time of day"),
                        plotlyOutput('daytime_pie'))
               ),
               fluidRow(
                 titlePanel("Spawns in selected country"),
                 leafletOutput("map_country"),
                 uiOutput("region_map")
               )
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
               actionButton("action2", label = "(Pokemon) GO")
             ),
             mainPanel(
               titlePanel("The most probable type of pokemon that will appear at this location would be:"),
               textOutput("knn_result"),
               uiOutput("result_pic"),
               hr(),
               fluidRow(titlePanel("Methodology"),
                        textOutput("knn_info"))
             )
           )),
  tabPanel("Where are you Pikachu?",
           sidebarLayout(
             sidebarPanel("Optimal conditions for finding a particular type",
                          selectInput("select_typeofpoke2","Select Type of Pokemon",dbGetQuery(con,"SELECT DISTINCT `type1` FROM poke_spawns ORDER BY `type1`")),
                          actionButton("action3", label = "(Pokemon) GO"),
                          wellPanel("Methodology used for prediction:",
                                    textOutput("rf_info")
                          )
             ),
             
             mainPanel("The following plots show the general distribution for the 4 most important variables which determine this pokemon's location ",
                       hr(),
                       fluidRow(
                         column(6,
                                plotOutput("plot_imp1")),
                         column(6,
                                plotOutput("plot_imp2"))
                       ),
                       hr(),
                       fluidRow(
                         column(6,
                                plotOutput("plot_imp3")),
                         column(6,
                                plotOutput("plot_imp4"))
                       )
             )
           )
  )
))

server <- shinyServer(function(input, output) {
  pokemonGO <- read.csv('./pokemonGO.csv')
  pokemonGO$Name<-as.character(pokemonGO$Name)
  typeimages <- read.csv('./typepics.csv')
  typeimages$Type <- as.character(typeimages$Type)
  
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
  
  region_coords_type <- reactive({
    dbGetQuery(con,sprintf("SELECT longitude,latitude FROM poke_spawns WHERE continent = \'%s\' and region = \'%s\' and `type1` = \'%s\'",as.character(input$select_continent),as.character(input$select_region),as.character(input$select_typeofpoke)))
  })
  
  region_coords_pkmn <- reactive({
    dbGetQuery(con,sprintf("SELECT longitude,latitude FROM poke_spawns WHERE continent = \'%s\' and region = \'%s\' and Name = \'%s\'",as.character(input$select_continent),as.character(input$select_region),as.character(input$select_pkmn)))
  })
  
  output$selectedContinent <- renderUI({
    countries<-dbGetQuery(con, sprintf("SELECT DISTINCT country FROM poke_spawns WHERE continent = \'%s\' ORDER BY country", as.character(input$select_continent)))
    conditionalPanel("input.select_continent", selectInput("select_country","Select Country", countries))
  })
  
  output$region <- renderUI({
    regions<-dbGetQuery(con, sprintf("SELECT DISTINCT region FROM poke_spawns WHERE country = \'%s\' ORDER BY region", as.character(input$select_country)))
    conditionalPanel("input.select_country == \"usa\"", selectInput("select_region","Select Region", regions))
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
  
  get_region_coords <- eventReactive(input$action, {
    if(user_choice() == "Type")
      as.data.frame(region_coords_type())
    else if (user_choice() == "Pokemon")
      as.data.frame(region_coords_pkmn())
  })
  
  get_region_coords <- eventReactive(input$action, {
    if(user_choice() == "Type")
      as.data.frame(region_coords_type())
    else if (user_choice() == "Pokemon")
      as.data.frame(region_coords_pkmn())
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
  
  output$region_map <- renderUI({
    conditionalPanel("input.select_country == \"usa\"",leafletOutput("map_region"))
  })
  
  output$map_region <- renderLeaflet({
    leaflet(get_region_coords()) %>%
      addTiles() %>%
      addCircles()
  })
  get_daytime_info <- eventReactive(input$action, {
    if(user_choice() == "Type")
      as.data.frame(dbGetQuery(con,sprintf('SELECT appearedTimeOfDay as Period, count(appearedTimeOfDay) as Count from poke_spawns where type1 = \'%s\' group by Period',input$select_typeofpoke)))
    else if (user_choice() == "Pokemon")
      as.data.frame(dbGetQuery(con,sprintf('SELECT appearedTimeOfDay as Period, count(appearedTimeOfDay) as Count from poke_spawns where Name = \'%s\' group by Period',input$select_pkmn)))
  })
  
  output$daytime_pie <- renderPlotly({
    plot_ly(get_daytime_info(), labels = ~Period, values = ~Count, type = 'pie') %>%
      layout(title = 'Proportion of showing up period',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  observeEvent(input$action2, {
    user_coords <- c(as.numeric(input$u_lati), as.numeric(input$u_longi))
    model_knn <- knn(train = data[,c(3,4)], test = user_coords, cl = data[,"type1"], k = 27)
    output$knn_result <- renderText({
      as.character(model_knn)
    })
    output$result_pic <- renderUI({
      tags$img(src = typeimages[typeimages$Type==as.character(model_knn),2], alt = "photo",height="200", width="200")
    })
  })
  
  observeEvent(input$action3, {
    selectedtype <- input$select_typeofpoke2
    imp1 <- as.character(subset(imp_predictors, X==selectedtype)$Importance.1)
    imp2 <- as.character(subset(imp_predictors, X==selectedtype)$Importance.2)
    imp3 <- as.character(subset(imp_predictors, X==selectedtype)$Importance.3)
    imp4 <- as.character(subset(imp_predictors, X==selectedtype)$Importance.4)
    #output$plot_imp1 <- renderPlot(hist(subset(data, type1==selectedtype)[,imp1]))
    #output$plot_imp2 <- renderPlot(hist(subset(data, type1==selectedtype)[,imp2],main = paste("Variable 2:",imp2)))
    #output$plot_imp3 <- renderPlot(hist(subset(data, type1==selectedtype)[,imp3],main = paste("Variable 3:",imp3)))
    #output$plot_imp4 <- renderPlot(hist(subset(data, type1==selectedtype)[,imp4],main = paste("Variable 4:",imp4)))
    
    x1 <- subset(data, type1==selectedtype)[,imp1]
    d1 <- density(x1,na.rm = T)
    plot(d1, main = paste("Variable 1:",imp1))
    polygon(d1, col="red", border="blue")
    output$plot_imp1 <- renderPlot(plot(d1, main = paste("Variable 1:",imp1)))# %>% polygon(d, col="red", border="blue"))
    
    x2 <- subset(data, type1==selectedtype)[,imp2]
    d2 <- density(x2,na.rm = T)
    plot(d2, main = paste("Variable 2:",imp2))
    polygon(d2, col="red", border="blue")
    output$plot_imp2 <- renderPlot(plot(d2, main = paste("Variable 2:",imp2)))
    
    x3 <- subset(data, type1==selectedtype)[,imp3]
    d3 <- density(x3,na.rm = T)
    plot(d3, main = paste("Variable 3:",imp3))
    polygon(d3, col="red", border="blue")
    output$plot_imp3 <- renderPlot(plot(d3, main = paste("Variable 3:",imp3)))
    
    x4 <- subset(data, type1==selectedtype)[,imp4]
    d4 <- density(x4,na.rm = T)
    plot(d4, main = paste("Variable 4:",imp4))
    polygon(d4, col="red", border="blue")
    output$plot_imp4 <- renderPlot(plot(d4, main = paste("Variable 4:",imp4)))
    
  })
  
  output$rf_info <- renderText(
    "We have used random forests algorithm to determine the top 4 most important features which predict a particular pokemon type.
    
    Sampling is done with replacement and we have chosen number of trees = 100 for the computation."
  )
  
  output$knn_info <- renderText(
    "To make this prediction we have used the KNN algorithm with k=27. The algorthim 
    has been trained on our data set of more than 300,000 observations. Given a longitude
    and latitude it finds the closest observations to this point. Using these nearest neighbours 
    we find the most common type of pokemon in that area and make our prediction using this information.
    We chose k=27, after testing error rates usinf 5-fold cross validation. Our data was split into 5 random samples,
    4 were used as training and 1 for testing. We ran this cross validation for values of k=3 
    up to k=30 and concluded that our lowest error rate was at k=27."
  )
})

# Run the application 
shinyApp(ui = ui, server = server)