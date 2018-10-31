library(shiny)
library(shinydashboard)

shinyUI(dashboardPage(
  dashboardHeader(title = "My Web Scraping Project"),
  dashboardSidebar(
    sidebarUserPanel("Raul Vallejo", image = "web-icon.png"),
    sidebarMenu(
      menuItem("Data", tabName = "tables", icon = icon("web")),
      # menuItem("Missingness", tabName = "missing", icon = icon("table")),
      menuItem("Year first published", tabName = "barchart", icon = icon("book")),
      menuItem("Book ratings", tabName = "scatterplot1", icon = icon("globe")),
      menuItem("Book-Film ratings", tabName = "scatterplot2", icon = icon("globe")),
      menuItem("Film ratings", tabName = "scatterplot3", icon = icon("globe"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "tables",
              fluidRow(img(src="goodreads-table.PNG"),
                       img(src="wiki-table.PNG"),
                       img(src="imdb-film.PNG"))
      ),
      # tabItem(tabName = "missing",
      #         fluidRow(titlePanel("Spotify daily top 200 charts 2017")),
      #         fluidRow(valueBoxOutput("distinct.artists"),
      #                  valueBoxOutput("distinct.tracks"),
      #                  valueBoxOutput("total.streams"),
      #                  checkboxInput(inputId = "mapscale",
      #                                         label = "Log scale",
      #                                         value = FALSE)),
      #         fluidRow(htmlOutput("map1"))
      # ),
      tabItem(tabName = "scatterplot1",
              fluidRow(titlePanel("Goodreads data")),
              fluidRow(column(8, offset = 2,plotlyOutput("scatter1")))
      ),
      tabItem(tabName = "scatterplot2",
              fluidRow(titlePanel("Goodreads data + Wikipedia")),
              fluidRow(column(8, offset = 2,plotlyOutput("scatter2")))
      ),
      tabItem(tabName = "scatterplot3",
              fluidRow(titlePanel("Goodreads data + Wikipedia + IMBd")),
              fluidRow(column(8, offset = 2,plotlyOutput("scatter3")))
      )
    )
  )
))

