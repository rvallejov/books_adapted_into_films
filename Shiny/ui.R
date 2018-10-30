library(shiny)
library(shinydashboard)

shinyUI(dashboardPage(
  dashboardHeader(title = "My Shiny Project"),
  dashboardSidebar(
    sidebarUserPanel("Raul Vallejo", image = "spotify-icon.png"),
    sidebarMenu(
      menuItem("Top tracks worldwide", tabName = "map", icon = icon("map")),
      menuItem("Song ranking over time", tabName = "timeseries", icon = icon("table")),
      menuItem("Music adoption and retention", tabName = "scatterplot", icon = icon("globe")),
      menuItem("Extreme observations", tabName = "scatter", icon = icon("volume-up")),
      menuItem("Outliers by country", tabName = "boxplots", icon = icon("balance-scale")),
      menuItem("Outlier stream generation", tabName = "barplots", icon = icon("calculator")),
      menuItem("Top 20% stream generation", tabName = "barplots2", icon = icon("arrow-up")),
      menuItem("Adoption vs top 20% streams", tabName = "clusterplot", icon = icon("spotify"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "map",
              fluidRow(titlePanel("Spotify daily top 200 charts 2017")),
              fluidRow(valueBoxOutput("distinct.artists"),
                       valueBoxOutput("distinct.tracks"),
                       valueBoxOutput("total.streams"),
                       checkboxInput(inputId = "mapscale",
                                              label = "Log scale",
                                              value = FALSE)),
              fluidRow(htmlOutput("map1"))
      ),
      tabItem(tabName = "timeseries",
              fluidRow(titlePanel("Individual track adoption and retention")),
              fluidRow(column(2,checkboxGroupInput(inputId = "Top.tracks.1",
                                                   label = "#1 Songs",
                                                   choices = top.songs,
                                                   selected = top.songs[5]),
                                checkboxGroupInput(inputId = "selected.regions",
                                                 label = "Regions",
                                                 choices = my.regions,
                                                 selected = my.regions[6])),
                       column(10,plotlyOutput("line1")))
      ),
      tabItem(tabName = "scatterplot",
              fluidRow(titlePanel("Comparing music adoption and retention by country")),
              fluidRow(valueBoxOutput("distinct.top.ten"),
                       valueBoxOutput("global.reten"),
                       valueBoxOutput("global.adopt")),
              fluidRow(column(8, offset = 2,plotlyOutput("scatter2")))
      ),
      tabItem(tabName = "scatter",
              fluidRow(titlePanel("Understanding extreme values in the data")),
              fluidRow(actionButton(inputId = "resample",
                                             label = "Plot sample",
                                             icon = icon("random"))),
              fluidRow(valueBoxOutput("streams.sample")),
              fluidRow(column(10,offset = 1,plotlyOutput("scatter1")))
      ),
      tabItem(tabName = "boxplots",
              fluidRow(titlePanel("Outlying values by country")),
              fluidRow(radioButtons(inputId = "outliers", label = "Zoom plot",
                                    choices = c("Both","Without outliers","Only outliers"),
                                    selected = "Both",inline = TRUE)),
              fluidRow(column(8, offset = 2,plotOutput("boxplot1")))
      ),
      tabItem(tabName = "barplots",
              fluidRow(titlePanel("Stream generation by outlying data points")),
              fluidRow(radioButtons(inputId = "outlier.groups", label = "Outlier criteria",
                                    choices = c("Graph outlier","Top 20%"),
                                    selected = "Graph outlier", inline = TRUE),
                       column(7,plotlyOutput("outlier.streams")),
                       column(5,plotlyOutput("polarity"))
              )
      ),
      tabItem(tabName = "barplots2",
              fluidRow(titlePanel("Top 20% stream generation worldwide")),
              fluidRow(column(5,plotlyOutput("barplot")),
                       column(7,htmlOutput("map2"))
              )
      ),
      tabItem(tabName = "clusterplot",
              fluidRow(titlePanel("Different country-wide listening habits")),
              fluidRow(valueBoxOutput("global.top20"),
                       valueBoxOutput("global.top20.streams"),
                       valueBoxOutput("global.streams")),
              fluidRow(column(8, offset = 3, plotlyOutput("clusters")))
      )
    )
  )
))


# fluidPage(
#   
#   titlePanel(h1("Spotify Daily Top 200 streaming data wolrdwide")),
#   
#   sidebarLayout(
#     sidebarPanel(
#       checkboxGroupInput(inputId = "Top.tracks",
#                          label = "#1 Songs",
#                          choices = top.songs,
#                          selected = top.songs[5]),
#       checkboxGroupInput(inputId = "selected.regions",
#                          label = "Regions",
#                          choices = my.regions,
#                          selected = my.regions[1]),
#       # selectizeInput(inputId = "scatter.top.tracks",
#       #                label = "Top n tracks",
#       #                choices = c(1:20),
#       #                selected = 10),
#       sliderInput(inputId = "scatter.top.tracks", 
#                   label = "Top n tracks",
#                   min = 1, max = 20,
#                   value = 10)
#     ),
#     # mainPanel(plotOutput("line1"))
#     mainPanel(
#       fluidRow(
#         # column(6, plotOutput("line1")),
#         # column(6, plotOutput("scatter2"))
#         plotlyOutput("line1"),
#         plotlyOutput("scatter2")
#       )
#     )
#   )
  # sidebarLayout(
  #   sidebarPanel(
  #     img(src="AerialOcean.JPG",
  #       width="100%")
  #     ),
  # 
  #   mainPanel(
  #     tags$iframe(src="https://www.youtube.com/embed/5gIhrPGyu6U",
  #               width="640", height="360")
  #   )
  # )
  
# )


