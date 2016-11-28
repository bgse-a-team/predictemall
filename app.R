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

# Define UI for application that draws a histogram
ui <- shinyUI(navbarPage(tags$style(type="text/css", css),"Pokemon Go Predictor", theme = shinytheme("slate"),
                         tabPanel("Descriptive",
                                  sidebarLayout(
                                    sidebarPanel(
                                      selectInput("select_continent","Select Continent",selected=NULL,multiple=TRUE,dbGetQuery(con,"SELECT DISTINCT continent FROM poke_spawns WHERE country IS NOT NULL ORDER BY continent")),
                                      uiOutput("selectedContinent"),
                                      sliderInput("time", "Period of Time", min = as.Date("2016-08-04"), max = as.Date("2016-10-12"), 
                                                  value = c(as.Date("2016-08-01"), Sys.Date()))
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
                                      selectInput("select_class","Select Pokemon",dbGetQuery(con,"SELECT DISTINCT Name FROM (poke_spawns a inner join pkmn_info b on id = pokemonId)  WHERE Name IS NOT NULL ORDER BY Name")),
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
                                      selectInput("select_type","Select Type of Pokemon",dbGetQuery(con,"SELECT DISTINCT `Type 1` FROM (poke_spawns inner join pkmn_info on id = pokemonId)  WHERE `Type 1` IS NOT NULL ORDER BY `Type 1`")),
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
  
  output$selectedContinent <- renderUI({
    countries<-dbGetQuery(con, sprintf("SELECT DISTINCT country FROM poke_spawns WHERE continent = \'%s\' ORDER BY country", as.character(input$select_continent))) 
    conditionalPanel("input.select_continent", selectInput("select_country","Select Country", countries))
    })
  
  output$map_continent <- renderLeaflet({
    leaflet(as.data.frame(selectedData2()[,3:4])) %>%
      addTiles() %>%
      addCircles() 
    })
  
  output$map_country <- renderLeaflet({
    leaflet(as.data.frame(selectedData()[,3:4])) %>%
      addTiles() %>%
      addCircles() 
  })
  selectedinfo <- reactive({
    dbGetQuery(con,sprintf("SELECT * FROM poke_spawns WHERE class = \'%s\'",as.character(input$select_class)))
  })
  
  selectedtype <- reactive({
    dbGetQuery(con,sprintf("SELECT * FROM (poke_spawns inner join pkmn_info on id = pokemonId)  WHERE `Type 1` = \'%s\'",as.character(input$select_type)))
  })
  
  output$hist_poke <-renderPlot(barplot(table(as.vector(selectedinfo()$weather))))
  output$hist_other <-renderPlot(barplot(table(as.vector(selectedinfo()$weather))))
  
  output$plot1 <- renderPlot({
    if (input$plot_type == "base") {
      map(database="world",input$select_country)
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




