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
  library(plotly)
  library(tidyverse)
  # library(shiny.i18n)
  library(testthat)
  library(shinytest)
  library(googlesheets4)
  gpclibPermit()
})

##### TRANSLATIONS

# translator <- Translator$new(translation_csvs_path = "../data")



##### MAP

# Code for map based on https://blog.prokulski.science/
map_ll <- function(data_df) {
  voiv_map <- readOGR(dsn = "map/Województwa.shp", layer = "Województwa")

  voivodeship_names <- voiv_map@data %>% select(JPT_KOD_JE, JPT_NAZWA_)
  voivodeship_df <- fortify(voiv_map, region = "JPT_KOD_JE")
  voivodeship_df <- left_join(voivodeship_df, voivodeship_names, by = c("id" = "JPT_KOD_JE"))

  centroids <- coordinates(voiv_map) %>%
    as_tibble() %>%
    set_names(c("long", "lat")) %>%
    mutate(nazwa = voiv_map@data$JPT_NAZWA_) %>%
    select(nazwa, long, lat)

  centroids_df <- left_join(centroids, data_df, by = c("nazwa" = "Voivodeship")) %>%
    replace_na(list(Number_of_patients = 0, Number_of_deaths = 0))
}


##### UI

ui <- function(req) {
  dashboardPage(

    # Theme
    skin = "yellow",

    # Header
    dashboardHeader(title = "COVID-19: Poland"),

    # Sidebar
    dashboardSidebar(
      collapsed = FALSE,

      # Tabs
      sidebarMenu(
        menuItem("Charts", tabName = "charts", icon = icon("chart-bar"), badgeLabel = "New", badgeColor = "green"),
        menuItem("Map", tabName = "map", icon = icon("map-marked-alt")),
        menuItem("Data", tabName = "data", icon = icon("table")),
        menuItem("FAQ", tabName = "faq", icon = icon("info")),
        menuItem("Author", tabName = "author", icon = icon("user"))
      ),

      br(), br(), br(),

      div(
        style = "text-align:center;",
        img(src = "img/Rlogo.png", height = "25px"),
        HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),
        img(src = "img/shiny.png", height = "25px"),
        HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),
        img(src = "img/RStudio.png", height = "25px"),
        # ),
        br(), br(), br(),
        HTML(
          "<table style='margin: auto;'><tr>
            <td style='padding: 5px;'><a href='mailto:justynapawlata@gmail.com?subject=COVID-19: Poland'><i class='fas fa-envelope fa-lg'></i></a></td>
            <td style='padding: 5px;'><a href='https://twitter.com/justynapawlata' target='_blank'><i class='fab fa-twitter fa-lg'></i></a></td>
            <td style='padding: 5px;'><a href='https://github.com/jpawlata/covid-19' target='_blank'><i class='fab fa-github fa-lg'></i></a></td>
          </tr></table>"
        ),
        p(HTML("Copyright &copy; 2020 Justyna Pawlata"))
      )
    ),

    # Body
    dashboardBody(

      # Height of the map
      tags$style(type = "text/css", "#map {height: calc(100vh - 300px) !important;}"),

      # Tabs
      tabItems(
        tabItem(
          tabName = "map",
          fluidRow(
            leafletOutput("map"),
            width = 12,
            height = "700px"
          )
        ),

        tabItem(
          tabName = "charts",
          #h2("Charts"),
          fluidRow(
            # valueBox sick
            valueBoxOutput("box.sick",
                           width = 4
            ),
            # valueBox death
            valueBoxOutput("box.death",
                           width = 4
            ),
            # valueBox recovered
            valueBoxOutput("box.recovered",
                           width = 4
            )
          ),
          div(style = "padding: 0px 15px; font-family: Verdana,Arial,sans-serif;", 
          fluidRow(
            # Plots
            br(),
            plotlyOutput("chart.sick_per_day", height = "350px"),
            br(),
            plotlyOutput("chart.death_per_day", height = "350px")
            )
          ) 
        ),

        tabItem(
          tabName = "data",
          h2("Data Table"),
          div(
            style = "padding: 0px 15px; font-family: Verdana,Arial,sans-serif;",
            fluidRow(
              # Datatable
              br(),
              dataTableOutput("table", height = "350px"),
              width = 12,
              height = "560px"
            )
          )
        ),

        tabItem(
          tabName = "faq",
          h2("Frequently Asked Questions"),
          br(),
          h3("1. What's the purpose of this website?"),
          p(HTML("<strong>COVID-19: Poland</strong>"), " was created to store all the information about COVID-19 cases in Poland in one place and show it in a readable way."),
          h3("2. What's the data source?"),
          p(
            "Data about the COVID-19 disease in Poland: ", a("Website of the Republic of Poland", href = "https://www.gov.pl/web/coronavirus", target = "_blank"), " & ", a("Michał Rogalski", href = "https://docs.google.com/spreadsheets/d/1ierEhD6gcq51HAm433knjnVwey4ZE5DCnu1bW7PRG3E/htmlview?usp=sharing#", target = "_blank"), br(),
            "Data used to show disease on the map: ", a("Główny Urząd Geodezji i Kartografii", href = "http://www.gugik.gov.pl/", target = "_blank")
          ),
          h3("3. How often will the page be updated?"),
          p(HTML("<strong>Covid-19: Poland</strong>"), " is updated automatically once the numbers on ", a("Website of the Republic of Poland", href = "https://www.gov.pl/web/coronavirus", target = "_blank"), " are changed."),
        ),
        tabItem(
          tabName = "author",
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

  ### Data per Voivodeship
  covid_pl_voiv <- function() {
    url <- read_html("https://www.gov.pl/web/koronawirus/wykaz-zarazen-koronawirusem-sars-cov-2")

    covid_pl_json <- url %>%
      html_node("pre") %>%
      html_text() %>%
      fromJSON()

    covid_pl_json <- covid_pl_json[["parsedData"]] %>%
      fromJSON() %>%
      mutate_at(vars(-Id), funs(gsub('\\s+', '', .))) %>% #remove whitespace
      mutate(Liczba = as.numeric(Liczba)) %>%
      mutate(`Liczba zgonów` = as.numeric(`Liczba zgonów`)) %>%
      rename(
        Number_of_deaths = `Liczba zgonów`,
        Number_of_patients = Liczba,
        Voivodeship = Województwo
      ) %>%
      subset(select = -c(Id)) %>%
      replace_na(list(Number_of_patients = 0, Number_of_deaths = 0))

    # Remove first row (info for the whole country)
    covid_pl_json <- covid_pl_json[-1, ]
  }

  ### Detailed data for Poland

  covid_pl_all <- function() {
  
    gs4_deauth() # googlesheets4 in de-authorized state
    
    ### Get the data:
    #url <- "https://docs.google.com/spreadsheets/d/1ierEhD6gcq51HAm433knjnVwey4ZE5DCnu1bW7PRG3E/htmlview?usp=sharing#"
    url <- "https://docs.google.com/spreadsheets/d/1ierEhD6gcq51HAm433knjnVwey4ZE5DCnu1bW7PRG3E/edit#gid=1400401584"

    data <- read_sheet(url, sheet = "Wzrost")
    
    covid_pl_temp <- data %>%
      select(c(Data, `Nowe przypadki`, `Osoby z wynikiem neg. przy ponownym tescie, osoby zdublowane, osoby błędnie zaraportowane`, `Nowe zgony`, `Nowe wyzdrowienia`)) %>%
      na.omit() %>% # remve rows with no data
      rename(Date = Data,
             Infected_per_Day = `Nowe przypadki`,
             Negative_2nd_Test = `Osoby z wynikiem neg. przy ponownym tescie, osoby zdublowane, osoby błędnie zaraportowane`,
             Deaths_per_Day = `Nowe zgony`,
             Recovered_per_Day = `Nowe wyzdrowienia`) %>%
      mutate(Date = as.Date(Date, "%Y.%m.%d"))
    
    #covid_pl <- read_html(url) %>%
    #  html_nodes("table.waffle") %>%
    #  .[[1]] %>%
    #  html_table(fill = TRUE)

    ### Clean the data:
    #covid_pl_2 <- covid_pl[, -1] %>% # remove first column
    #  select(A, B, C, E, F) %>% # remove unnecessary/empty columns
    #  mutate_if(is.character, list(~ na_if(., ""))) %>% # remove rows with no data
    #  na.omit() # remve rows with no data

    # First row as a colnames
    #covid_pl_temp <- covid_pl_2[-1, ]
    #colnames(covid_pl_temp) <- covid_pl_2[1, ]

    # Colnames:
    #covid_pl_temp <- covid_pl_temp %>%
    #  rename(
    #    Date = Data,
    #    Infected_per_Day = `Próbki pozytywne`,
    #    Negative_2nd_Test = `Osoby z wynikiem neg. przy ponownym tescie, osoby zdublowane*`,
    #    Deaths_per_Day = Zgony,
    #    Recovered_per_Day = Zdrowi
    #  ) %>%
      # Add a year to the date, convert column into date
    #  mutate(Date = paste(Date, ".2020", sep = "")) %>%
    #  mutate(Date = as.Date(Date, "%d.%m.%Y")) %>%
    #  # Remove + / - / whitespace
    #  mutate_at(vars(-Date), funs(gsub("\\+", "", .))) %>%
    #  mutate_at(vars(-Date), funs(gsub("\\-", "", .))) %>%
    #  mutate_at(vars(-Date), funs(gsub("\\%", "", .))) %>%
    #  mutate_at(vars(-Date), funs(as.numeric(gsub(" ", "", .))))

    covid_pl_final <- covid_pl_temp
  }

  ### TO UPDATE TESTS
  if (isTRUE(getOption("shiny.testmode"))) {
    data <- get(load("tests/data/covid_pl_json_data.RData"))
    data_pl <- get(load("tests/data/covid_pl_all_data.RData"))
  } else {
    data <- covid_pl_voiv()
    data_pl <- covid_pl_all()
  }

  data_df <- aggregate(list(Number_of_patients = data$Number_of_patients, Number_of_deaths = data$Number_of_deaths), by = list(Voivodeship = data$Voivodeship), FUN = sum, na.rm = TRUE, na.action = NULL)

  covid_pl_sick <- function(x) {
    sick <- sum(as.numeric(data$Number_of_patients), na.rm = TRUE)
  }

  covid_pl_death <- function(x) {
    death <- sum(as.numeric(data$Number_of_deaths), na.rm = TRUE)
  }

  covid_pl_recovered <- function(x) {
    recovered <- sum(as.numeric(data_pl$Recovered_per_Day), na.rm = TRUE)
  }

  # Reactive
  live_data_voiv <- reactive({
    invalidateLater(120000)
    # covid_pl_voiv()
    data
  })

  live_data_pl_all <- reactive({
    invalidateLater(120000)
    data_pl
  })

  live.valueBox.sick <- reactive({
    invalidateLater(120000)
    covid_pl_sick()
  })

  live.valueBox.death <- reactive({
    invalidateLater(120000)
    covid_pl_death()
  })

  live.valueBox.recovered <- reactive({
    invalidateLater(120000)
    covid_pl_recovered()
  })

  # Data table
  output$table <- renderDataTable({
    datatable(
      live_data_voiv(),
      colnames = c("Voivodeship", "Number of patients", "Number of deaths")
    )
  })

  # Leaflet plot
  output$map <- renderLeaflet({
    data <- map_ll(data_df)
    leaflet(data) %>%
      setView(lng = 19.145136, lat = 51.919438, zoom = 6) %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(
        lng = ~long,
        lat = ~lat,
        # radius=~Number_of_patients,
        radius = ~ (Number_of_patients / covid_pl_sick()) * 100,
        fillColor = "orange",
        color = "#CC5500",
        stroke = TRUE,
        fillOpacity = 0.5,
        # Popup
        popup = ~ paste0(
          "<strong>", toTitleCase(nazwa), "</strong>", br(),
          "Number of patients: ", as.character(Number_of_patients), br(),
          "Number of deaths: ", as.character(Number_of_deaths)
        )
      )
  })

  output$chart.sick_per_day <- renderPlotly({
    data_all <- live_data_pl_all()
    
    data_all %>%
      plot_ly(
        x = ~Date, y = ~Infected_per_Day, type = "scatter", mode = "lines",
        color = I("orange"), name = "Infected per Day"
      ) %>%
      layout(
        title = sprintf("<b>Number of people infected per day</b>"),
        xaxis = list(title = sprintf("<b>Date</b>")),
        yaxis = list(title = sprintf("\n<b>Infected</b>\n\n")),
        margin = list(l = 50, r = 0, b = 50, t = 70)
      )
  })

  output$chart.death_per_day <- renderPlotly({
    data_all <- live_data_pl_all()

    data_all %>%
      plot_ly(
        x = ~Date, y = ~Deaths_per_Day, type = "scatter", mode = "lines",
        color = I("red"), name = "Deaths"
      ) %>%
      layout(
        title = sprintf("<b>Deaths per day</b>"),
        xaxis = list(title = sprintf("<b>Date</b>")),
        yaxis = list(title = sprintf("\n<b>Deaths</b>\n")),
        margin = list(l = 50, r = 0, b = 50, t = 70)
      )
  })

  # ValueBox Sick
  output$box.sick <- renderValueBox({
    valueBox(
      "Infected:",
      subtitle = tags$p(live.valueBox.sick(), style = "font-weight: bold; font-size: 20px;"),
      icon = icon("stethoscope"),
      color = "red"
    )
  })

  # ValueBox Deaths
  output$box.death <- renderValueBox({
    valueBox(
      "Total deaths:",
      subtitle = tags$p(live.valueBox.death(), style = "font-weight: bold; font-size: 20px;"),
      icon = icon("times-circle"),
      color = "red"
    )
  })

  # ValueBox Recovered
  output$box.recovered <- renderValueBox({
    valueBox(
      "Recovered:",
      subtitle = tags$p(live.valueBox.recovered(), style = "font-weight: bold; font-size: 20px;"),
      icon = icon("heartbeat"),
      color = "green"
    )
  })
}

##### RUN APP
options(shiny.trace = FALSE)
options(shiny.fullstacktrace = FALSE)
shinyApp(ui = ui, server = server)
