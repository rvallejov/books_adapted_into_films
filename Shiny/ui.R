library(shiny)
library(shinydashboard)

shinyUI(dashboardPage(
  dashboardHeader(title = "Web Scraping"),
  dashboardSidebar(
    sidebarUserPanel("Raul Vallejo", image = "book-icon.png"),
    sidebarMenu(
      menuItem("Year first published", tabName = "barchart", icon = icon("calendar")),
      menuItem("Book ratings", tabName = "scatterplot1", icon = icon("book")),
      menuItem("Book ratings (adapted films)", tabName = "scatterplot2", icon = icon("wikipedia-w")),
      menuItem("Book-Film data", tabName = "scatter_matrix", icon = icon("table")),
      menuItem("Book-Film data", tabName = "scatterplot3", icon = icon("film")),
      menuItem("Film ratings", tabName = "scatterplot4", icon = icon("film"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "barchart",
              fluidPage(
                fluidRow(titlePanel("Goodreads data")),
                fluidRow(column(8, offset = 2,plotlyOutput("barchart1"))),
                fluidRow(column(8, offset = 1,img(src="Rplot1.jpeg")))
              )
      ),
      tabItem(tabName = "scatterplot1",
              fluidPage(
                fluidRow(titlePanel("Goodreads data")),
                fluidRow(column(8, offset = 2,plotlyOutput("scatter1")))
              )
      ),
      tabItem(tabName = "scatterplot2",
              fluidPage(
                fluidRow(titlePanel("Goodreads data + Wikipedia")),
                fluidRow(column(8, offset = 2,plotlyOutput("scatter2")))
              )
      ),
      tabItem(tabName = "scatter_matrix",
              fluidPage(
                fluidRow(titlePanel("Goodreads data + Wikipedia + IMBd")),
                fluidRow(column(8, offset = 1,img(src="Rplot2.jpeg")))
              )
      ),
      tabItem(tabName = "scatterplot3",
              fluidPage(
                fluidRow(titlePanel("Goodreads data + Wikipedia + IMBd")),
                fluidRow(column(8, offset = 2,plotlyOutput("scatter3")))
              )
      ),
      tabItem(tabName = "scatterplot4",
              fluidPage(
                fluidRow(titlePanel("Goodreads data + Wikipedia + IMBd")),
                fluidRow(column(10, offset = 1,plotlyOutput("scatter4")))
              )
      )
    )
  )
))

