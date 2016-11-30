library(shiny)
library(maps)
library(mapdata)
library(RMySQL)
library(shinythemes)
library(mapproj)
library(ggplot2)
library(leaflet)

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

# Define UI for application that draws a histogram
ui <- shinyUI(navbarPage(tags$style(type="text/css", css),"Pokemon Go Predictor", theme = shinytheme("slate"),
                         tabPanel("Descriptive",
                                  sidebarLayout(
                                    sidebarPanel("Select country and time period to see pokemon spawns",
                                      selectInput("select_continent","Select Continent",selected=NULL,multiple=TRUE,dbGetQuery(con,"SELECT DISTINCT continent FROM poke_spawns WHERE country IS NOT NULL ORDER BY continent")),
                                      uiOutput("selectedContinent"),
                                      uiOutput("Types"),
                                      sliderInput("time", "Period of Time", min = as.Date("2016-08-04"), max = as.Date("2016-10-12"), 
                                                  value = c(as.Date("2016-08-01"), Sys.Date())),
                                      actionButton("action", label = "(Pokemon) GO ")
                                    ),
                                    mainPanel(
                                      leafletOutput("map_continent"),
                                      leafletOutput("map_country")
                                    )
                                  )        
                         ),
                         tabPanel("More Descriptive",
                                  sidebarLayout(
                                    sidebarPanel(
                                      selectInput("select_class","Select Pokemon",dbGetQuery(con,"SELECT DISTINCT a.Name FROM (poke_spawns a inner join pkmn_info b on b.id = a.pokemonId)  WHERE a.Name IS NOT NULL ORDER BY a.Name")),
                                      uiOutput("pics",align="center"),
                                      selectInput("select_weather","Select Weather",dbGetQuery(con,"SELECT DISTINCT weather FROM poke_spawns WHERE weather IS NOT NULL ORDER BY weather"))
                                    ),
                                    mainPanel(
                                      plotOutput("hist_poke"),
                                      plotOutput("hist_other")
                                    )
                                  )),
                         tabPanel("Analysis",
                                  sidebarLayout(
                                    sidebarPanel(
                                      selectInput("select_type","Select Type of Pokemon",dbGetQuery(con,"SELECT DISTINCT `type1` FROM (poke_spawns inner join pkmn_info on id = pokemonId)  WHERE `type1` IS NOT NULL ORDER BY `type1`")),
                                      selectInput("select_weather","Select Weather",dbGetQuery(con,"SELECT DISTINCT weather FROM poke_spawns WHERE weather IS NOT NULL ORDER BY weather")
                                                  )),
                                      mainPanel("The most important variables to find this pokemon type is")
                                  )),
                         tabPanel("Details",
                                  fluidPage(
                                    # Some custom CSS for a smaller font for preformatted text
                                    tags$head(
                                      tags$style(HTML("
                                                       pre, table.table {
                                                       font-size: smaller;
                                                       }
                                                       "))
                                    ),
                                    
                                    fluidRow(
                                      column(width = 4, wellPanel(
                                        radioButtons("plot_type", "Plot type",
                                                     c("base", "ggplot2")
                                        )
                                      )),
                                      column(width = 4,
                                             # In a plotOutput, passing values for click, dblclick, hover, or brush
                                             # will enable those interactions.
                                             plotOutput("plot1", height = 350,
                                                        # Equivalent to: click = clickOpts(id = "plot_click")
                                                        click = "plot_click",
                                                        dblclick = dblclickOpts(
                                                          id = "plot_dblclick"
                                                        ),
                                                        hover = hoverOpts(
                                                          id = "plot_hover"
                                                        ),
                                                        brush = brushOpts(
                                                          id = "plot_brush"
                                                        )
                                             )
                                      )
                                    ),
                                    fluidRow(
                                      column(width = 3,
                                             verbatimTextOutput("click_info")
                                      ),
                                      column(width = 3,
                                             verbatimTextOutput("dblclick_info")
                                      ),
                                      column(width = 3,
                                             verbatimTextOutput("hover_info")
                                      ),
                                      column(width = 3,
                                             verbatimTextOutput("brush_info")
                                      )
                                    )
                                  ))
))

server <- shinyServer(function(input, output) {
  selectedData <- reactive({
    dbGetQuery(con,sprintf("SELECT * FROM poke_spawns WHERE country = \'%s\'",as.character(input$select_country)))
  })
  
  selectedData2 <- reactive({
    dbGetQuery(con,sprintf("SELECT * FROM poke_spawns WHERE continent = \'%s\'",as.character(input$select_continent)))
  })

  selectedData3 <- reactive({
    dbGetQuery(con,sprintf("SELECT * FROM poke_spawns inner join pkmn_info on id = pokemonId WHERE continent = \'%s\' and country = \'%s\' and `type1` = \'%s\'",as.character(input$select_continent),as.character(input$select_country),as.character(input$select_typeofpoke)))
  })
  
  output$selectedContinent <- renderUI({
    countries<-dbGetQuery(con, sprintf("SELECT DISTINCT country FROM poke_spawns WHERE continent = \'%s\' ORDER BY country", as.character(input$select_continent))) 
    conditionalPanel("input.select_continent", selectInput("select_country","Select Country", countries))
    })

  output$Types <- renderUI({
    types<-dbGetQuery(con,"SELECT DISTINCT `type1` FROM (poke_spawns inner join pkmn_info on id = pokemonId)  WHERE `type1` IS NOT NULL ORDER BY `type1`") 
    conditionalPanel("input.select_country", selectInput("select_typeofpoke","Select Type of Pokemon", types))
  })
  
  output$map_continent <- renderLeaflet({
    input$action
    isolate(
    leaflet(as.data.frame(selectedData2()[,5:4])) %>%
      addTiles() %>%
      addCircles()
    ) 
    })
  
  output$map_country <- renderLeaflet({
    input$action
    isolate(
    leaflet(as.data.frame(selectedData()[,5:4])) %>%
      addTiles() %>%
      addCircles() 
  )
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
  
  output$plot1 <- renderPlot({
    if (input$plot_type == "base") {
      #map(database="world",input$select_country)
      map(database="world",input$select_continent)
      points(selectedData()$longitude[(selectedData()$appearedLocalTime > input$time[1])], selectedData()$latitude[(selectedData()$appearedLocalTime > input$time[1])],pch=18,cex=0.5,col="red")
      
    } else if (input$plot_type == "ggplot2") {
      ggmap(get_map(location = input$select_country, zoom = 4)) + geom_point(aes(selectedData()$longitude[(selectedData()$appearedLocalTime > input$time[1])], selectedData()$latitude[(selectedData()$appearedLocalTime > input$time[1])]))
    }
  })
  
  output$click_info <- renderPrint({
    cat("input$plot_click:\n")
    str(input$plot_click)
  })
  output$hover_info <- renderPrint({
    cat("input$plot_hover:\n")
    str(input$plot_hover)
  })
  output$dblclick_info <- renderPrint({
    cat("input$plot_dblclick:\n")
    str(input$plot_dblclick)
  })
  output$brush_info <- renderPrint({
    cat("input$plot_brush:\n")
    str(input$plot_brush)
  })
  
  
})

# Run the application 
shinyApp(ui = ui, server = server)




