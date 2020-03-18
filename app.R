suppressPackageStartupMessages({
library(shiny)
library(shinydashboard)
library(rvest)
library(jsonlite)
library(rgdal)
library(rgeos)
library(maptools)
library(mapproj)
library(leaflet)
library(leaflet.extras)
library(tools)
library(gpclib)
library(DT)
library(tidyverse)
library(testthat)
library(shinytest)
gpclibPermit()
})

# TEST MODE
is_testmode <- function() {
  isTRUE(getOption("shiny.testmode"))
}



# DATA
covid_pl_data <- function(x){
  
  url <- read_html("https://www.gov.pl/web/koronawirus/wykaz-zarazen-koronawirusem-sars-cov-2")

  covid_pl_json <- url %>%
    html_node("pre") %>%
    html_text() %>%
    fromJSON()
  
  covid_pl_json <- covid_pl_json[["parsedData"]] %>%
    fromJSON() %>%
    mutate(Liczba = as.numeric(Liczba)) %>%
    mutate(`Liczba zgonów` = as.numeric(`Liczba zgonów`)) %>%
    rename(Number_of_deaths=`Liczba zgonów`, 
           Number_of_patients = Liczba,
           Voivodeship = Województwo) %>%
    subset(select = -c(Id)) %>%
    replace_na(list(Number_of_patients = 0, Number_of_deaths = 0))
    
  # Remove first row (info for the whole country)
  covid_pl_json <- covid_pl_json[-1, ]
  }

if(isTRUE(getOption("shiny.testmode"))) {
  data <- get(load("tests/data/covid_pl_json_data.RData"))
} else {
  data <- covid_pl_data()
}

data_df <- aggregate(list(Number_of_patients = data$Number_of_patients, Number_of_deaths = data$Number_of_deaths), by = list(Voivodeship = data$Voivodeship), FUN = sum, na.rm=TRUE, na.action=NULL)

covid_pl_sick <- function(x){
  sick <- sum(as.numeric(data$Number_of_patients), na.rm = TRUE)
}
  
covid_pl_death <- function(x){
  death <- sum(as.numeric(data$Number_of_deaths), na.rm = TRUE)
}

##### MAP

# Code for map based on https://blog.prokulski.science/
map_ll <- function(x){
  voiv_map = readOGR(dsn = "map/Województwa.shp", layer = "Województwa")
  
  voivodeship_names <- voiv_map@data %>% select(JPT_KOD_JE, JPT_NAZWA_)
  voivodeship_df <- fortify(voiv_map, region = "JPT_KOD_JE")
  voivodeship_df <- left_join(voivodeship_df, voivodeship_names, by=c("id"="JPT_KOD_JE"))

  centroids <- coordinates(voiv_map) %>%
    as_tibble() %>%
    set_names(c("long", "lat")) %>%
    mutate(nazwa = voiv_map@data$JPT_NAZWA_) %>%
    select(nazwa, long, lat)
  
  centroids_df <- left_join(centroids, data_df, by = c("nazwa" = "Voivodeship")) %>%
    replace_na(list(Number_of_patients = 0, Number_of_deaths = 0))
  }


##### UI

ui <- function(req){
  dashboardPage(
  
  # Theme
  skin = "yellow",
  
  # Header
  dashboardHeader(title = "COVID-19: Poland"),
  
  # Sidebar
  dashboardSidebar(
    
    # Tabs
    sidebarMenu(
      menuItem("Map", tabName = "map", icon = icon("map-marked-alt")),
      menuItem("Data", tabName = "data", icon = icon("table")),
      menuItem("FAQ", tabName = "faq", icon = icon("info")),
      menuItem("Author", tabName = "author", icon = icon("user"))
    ),
    
    br(), br(), br(),
    
    div(style="text-align:center;", 
        img(src = "img/Rlogo.png", height = "25px"),
        HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),
        img(src = "img/shiny.png", height = "25px"),
        HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),
        img(src = "img/RStudio.png", height = "25px"),
        #),
        br(), br(), br(),
        HTML(
          "<table style='margin: auto;'><tr>
            <td style='padding: 5px;'><a href='mailto:justynapawlata@gmail.com?subject=COVID-19: Poland'><i class='fas fa-envelope fa-lg'></i></a></td>
            <td style='padding: 5px;'><a href='https://twitter.com/justynapawlata' target='_blank'><i class='fab fa-twitter fa-lg'></i></a></td>
            <td style='padding: 5px;'><a href='https://github.com/jpawlata/covid-19' target='_blank'><i class='fab fa-github fa-lg'></i></a></td>
          </tr></table>"),
        p(HTML("Copyright &copy; 2020 Justyna Pawlata"))
    )
  ),
  
  # Body
  dashboardBody(
    
    # Height of the map
    tags$style(type = "text/css", "#map {height: calc(100vh - 300px) !important;}"),
    
    # Tabs
    tabItems(
      tabItem(tabName = "map",
              fluidRow(
                # valueBox
                valueBoxOutput("box.sick",
                              width = 6),
                # valueBox
                valueBoxOutput("box.death",
                              width = 6)
              ),
              fluidRow(
                  leafletOutput("map"),
                  width = 12,
                  height = "700px"
              )
    ),
    tabItem(tabName = "data",
            h2("Data Table"),
            div(style = "padding: 0px 15px; font-family: Verdana,Arial,sans-serif;",
            fluidRow(
              # Datatable
                br(),
                dataTableOutput("table", height = "350px"),
                width = 12,
                height = "560px"
            )
          )
        ),
   
    tabItem(tabName = "faq",
            h2("Frequently Asked Questions"),
            br(),
            h3("1. What's the purpose of this website?"),
            p(HTML("<strong>COVID-19: Poland</strong>"), " was created to store all the information about COVID-19 cases in Poland in one place and show it in a readable way."),
            h3("2. What's the data source?"),
            p("Data about the COVID-19 disease in Poland: ", a("Website of the Republic of Poland", href="https://www.gov.pl/web/coronavirus", target="_blank"), br(),
              "Data used to show disease on the map: ", a("Główny Urząd Geodezji i Kartografii", href="http://www.gugik.gov.pl/", target="_blank")),
            h3("3. How often will the page be updated?"),
            p(HTML("<strong>Covid-19: Poland</strong>"), " is updated automatically once the numbers on ", a("Website of the Republic of Poland", href="https://www.gov.pl/web/coronavirus", target="_blank"), " are changed."),
            br(),br(),br(),
            h3("Change log:"),
            tags$ul(
              tags$li(HTML("<strong>1.0.0 - 2020/03/16</strong>"), br(), "First Release",)
              ),
            ),
    tabItem(tabName = "author",
            h2("Author"),
            p("Contact: ", a(href = "mailto:justynapawlata@gmail.com?subject=COVID-19: Poland", "justynapawlata@gmail.com")),
            p("Twitter: ", a(href = "https://twitter.com/justynapawlata", "@justynapawlata", target = "_blank"))
            )
    )
    
  )
  
)
}

##### SERVER

server <- function(input, output) {

  # Reactive 
  live_data <- reactive({
    invalidateLater(120000)
    #covid_pl_data()
    data
  })
  
  live.valueBox.sick <- reactive({
    invalidateLater(120000)    
    covid_pl_sick()        
  })
  
  live.valueBox.death <- reactive({
    invalidateLater(120000)   
    covid_pl_death()       
  })
  
  # Data table
  output$table <- renderDataTable({datatable(
    live_data(),
    colnames = c("Voivodeship", "Number of patients", "Number of deaths")
    )})
  
  # Leaflet plot
  output$map <- renderLeaflet({
    data <- map_ll()
    leaflet(data) %>%
      setView(lng = 19.145136, lat = 51.919438, zoom = 6) %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(
        lng=~long, 
        lat=~lat,
        radius=~Number_of_patients, 
        fillColor= "orange",
        color = "#CC5500",
        stroke=TRUE, 
        fillOpacity=0.5, 
        # Popup
        popup=~paste0(
          "<strong>", toTitleCase(nazwa), "</strong>", br(),
          "Number of patients: ", as.character(Number_of_patients), br(),
          "Number of deaths: ", as.character(Number_of_deaths)
        )
      )
  })
  
  # ValueBoxes
  output$box.sick <- renderValueBox({
    valueBox(
      "Infected:",
      subtitle = tags$p(live.valueBox.sick(), style = "font-weight: bold; font-size: 20px;"),
      icon = icon("stethoscope"),
      color = "red"
    )
  })
  
  # ValueBoxes
  output$box.death <- renderValueBox({
    valueBox(
      "Total deaths:",
      subtitle = tags$p(live.valueBox.death(), style = "font-weight: bold; font-size: 20px;"),
      icon = icon("times-circle"),
      color = "red"
    )
  })
  
}

##### RUN APP
shinyApp(ui = ui, server = server)