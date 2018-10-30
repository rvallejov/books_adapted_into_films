library(shiny)
library(shinydashboard)

shinyServer(function(input, output){
  
  observe(
    print(input$Top.tracks)
  )
  
  # rv <- reactiveValues()
  
  # observeEvent(input$scatter.top.tracks,{
  #   rv$value <- input$scatter.top.tracks
  # })
  
  # observeEvent(input$mapscale,{
  #   rv$mapscale <- input$mapscale
  #   print(rv$mapscale)
  # })
  
  output$distinct.artists <- renderValueBox({
    valueBox(comma(n_distinct(spotify.data$Artist)),
             "distinct artists",
             icon = icon("music"),
             width = "auto",
             color = "red")
  })
  
  output$distinct.tracks <- renderValueBox({
    valueBox(comma(n_distinct(spotify.data$Track.Name)),
             "distinct tracks",
             icon = icon("headphones"),
             width = "auto",
             color = "blue")
  })
  
  output$total.streams <- renderValueBox({
    valueBox(comma(sum(as.numeric(spotify.data$Streams[which(spotify.data$Region != "global")]))), 
             "total streams", 
             icon = icon("globe"),
             width = "auto",
             color = "green")
  })
  
  output$map1 <- renderGvis({
    gvisGeoChart(most.streamed.2017,
                 locationvar = "Region",
                 colorvar = ifelse(input$mapscale,"log.Streams","Streams"),
                 hovervar = "Track.Name",
                 options=list(colors="['#baffc4', '#00dd20' ,'#009115']",
                              region="world", displayMode="regions",
                              resolution="countries",
                              width="auto", height="auto",
                              backgroundColor = "#d1edf9"))
  })
  
  # output$line.title <-  renderText("Individual track adoption and retention")
  
  output$line1 <- renderPlotly({
    g1 <- spotify.data %>%
      filter(Track.Name %in% input$Top.tracks.1, Region %in% input$selected.regions) %>%
      ggplot(aes(x= Date, y = Position)) +
      geom_point(aes(text=sprintf("Track: %s<br>Artist: %s<br>Position: %s<br>Date: %s<br>Streams: %s",
                                  Track.Name, Artist, Position, Date, comma(Streams)),
                     color = Track.Name), alpha = 0.25) +
      geom_smooth(aes(color = Track.Name), se = FALSE) +
      scale_y_reverse() + 
      facet_grid(.~Region) + 
      theme(legend.position = "bottom") + 
      labs(title = "Song ranking over time")
    ggplotly(g1,tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  output$distinct.top.ten <- renderValueBox({
    valueBox(paste0(comma(n_distinct(top.ten.track$Track.Name))," tracks"), 
             "that reached top 10", 
             icon = icon("music"),
             width = "auto",
             color = "blue")
  })
  
  output$global.adopt <- renderValueBox({
    valueBox(paste0(round(mean(top.ten.region$mean.days.to.top))," days"), 
             "Global adoption average", 
             icon = icon("globe"),
             width = "auto",
             color = "red")
  })
  
  output$global.reten <- renderValueBox({
    valueBox(paste0(round(mean(top.ten.region$mean.days.after.top))," days"), 
             "Global retention average", 
             icon = icon("globe"),
             width = "auto",
             color = "green")
  })
  
  output$scatter2 <- renderPlotly({
    g2 <- top.ten.region %>%
      ggplot(aes(x = mean.days.to.top, y = mean.days.after.top)) +
      geom_hline(aes(yintercept = mean(top.ten.region$mean.days.after.top)),
                 color = "steel blue", linetype = 2, alpha = 0.7) +
      geom_vline(aes(xintercept = mean(top.ten.region$mean.days.to.top)),
                 color = "steel blue", linetype = 2, alpha = 0.7) +
      geom_point(aes(text=sprintf("Continent: %s<br>Region: %s<br>Streams: %s<br>Days to top: %s<br>Days after top: %s", 
                                  Continent, Region, comma(total.streams), round(mean.days.to.top), round(mean.days.after.top)),
                     color = Continent, size = total.streams)) + 
      labs(y = "Days after reaching top 10", x = "Days it took to reach top 10",
           title = "Mean track adoption and retention") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.title = element_blank())
    ggplotly(g2, tooltip="text") %>% 
      config(displayModeBar = FALSE)
  })
  
  randomVals <- eventReactive(input$resample, {
    total.streams.2017 %>%
      sample_n(1000)
  })
  
  output$streams.sample <- renderValueBox({
      valueBox(comma(sum(randomVals()$Streams)),
               "streams from this 1,000 song sample",
               color = "aqua",
               icon = icon("play"),
               width = "auto")
  })
  
  output$scatter1 <- renderPlotly({
    g3 <- randomVals() %>%
      ggplot(aes(x = Track.Name, y = Streams)) +
      geom_point(aes(text=sprintf("Track: %s<br>Streams: %s<br>Proportion of total streams: %s",
                                  Track.Name, comma(Streams), percent(Streams/sum(randomVals()$Streams))),
                     color = Streams), alpha = 0.6) +
      scale_color_gradient(low="blue", high="red") +
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      labs(x = "Individual Tracks", title = "1000 song sample")
    ggplotly(g3,tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  same.four.lines <- reactive({
    spotify.data %>%
      filter(Region %in% top.countries$Region) %>%
      group_by(Region, Track.Name) %>%
      summarise(Streams = sum(as.numeric(Streams))) %>%
      left_join(.,select(descriptive.stats, Region, upper.fence.1, upper.fence.2), by = "Region")
  })
  
  output$boxplot1 <- renderPlot({
    same.four.lines() %>%
      ggplot(aes(x = reorder(Region,Streams,median), y = Streams)) +
      labs(x = "Top streaming countries", title = "Total streams per track in 2017") +
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_boxplot(outlier.color = 'red') +
      if(input$outliers == "Both"){
        coord_flip()
      } else if(input$outliers == "Without outliers"){
        coord_flip(ylim = c(0, 3.1e+07))
      } else if(input$outliers == "Only outliers"){
        coord_flip(ylim = c(1.6e+07, 3.1e+08))
      }
  })
  
  output$outlier.streams <- renderPlotly({
    g4 <- same.four.lines() %>%
      mutate(upper.fence = ifelse(input$outlier.groups == "Graph outlier",
                                  upper.fence.1,
                                  upper.fence.2)) %>%
      mutate(stream.type = ifelse(Streams < upper.fence, "regular","outlier")) %>%
      group_by(Region, stream.type) %>%
      summarise(Streams = sum(Streams),n = n()) %>%
      # spread(key = stream.type, value = Streams, fill = 0) %>%
      ggplot(aes(x = reorder(Region, Streams), y = Streams)) +
      geom_col(aes(text=sprintf("Region: %s<br>Type: %s<br>Streams: %s",
                                Region, stream.type, comma(Streams)),
                   fill = stream.type), position = "stack") +
      labs(title = "Streams generated by outlier and regular tracks",
           x = "Region") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      coord_flip()
    ggplotly(g4, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  output$polarity <- renderPlotly({
    g5 <- same.four.lines() %>%
      mutate(upper.fence = ifelse(input$outlier.groups == "Graph outlier",
                                  upper.fence.1,
                                  upper.fence.2)) %>%
      mutate(stream.type = ifelse(Streams < upper.fence, "s.regular","s.outlier"),
             n.type = ifelse(Streams < upper.fence, "n.regular","n.outlier")) %>%
      group_by(Region, stream.type, n.type) %>%
      summarise(Streams = sum(Streams), n = n()) %>%
      spread(key = stream.type, value = Streams, fill = 0) %>%
      spread(key = n.type, value = n, fill = 0) %>%
      group_by(Region) %>%
      summarise(s.regular = sum(s.regular), s.outlier = sum(s.outlier),
                n.regular = sum(n.regular), n.outlier = sum(n.outlier)) %>%
      mutate(s.outlier.part = s.outlier/(s.regular+s.outlier),
             n.outlier.part = n.outlier/(n.regular+n.outlier)) %>%
      ggplot(aes(x = n.outlier.part, y = s.outlier.part)) +
      geom_point(aes(text=sprintf("Region: %s<br>Proportion of tracks: %s<br>Proportion of streams: %s",
                                  Region, percent(n.outlier.part), percent(s.outlier.part)),
                     color = Region), size = 2) + 
      labs(title = "Proportion of streams generated by outliers",
           x = "Proportion of total tracks",
           y = "Proportion of total streams") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.title = element_blank())
    ggplotly(g5, tooltip = "text") %>% 
      config(displayModeBar = FALSE)
  })
  
  output$barplot <- renderPlotly({
    g6 <- same.four.lines() %>%
      rename(upper.fence = upper.fence.2) %>%
      mutate(stream.type = ifelse(Streams < upper.fence, "s.regular","s.outlier"),
             n.type = ifelse(Streams < upper.fence, "n.regular","n.outlier")) %>%
      group_by(Region, stream.type, n.type) %>%
      summarise(Streams = sum(Streams),n = n()) %>%
      spread(key = stream.type, value = Streams, fill = 0) %>%
      spread(key = n.type, value = n, fill = 0) %>%
      group_by(Region) %>%
      summarise(s.regular = sum(s.regular), s.outlier = sum(s.outlier),
                n.regular = sum(n.regular), n.outlier = sum(n.outlier)) %>%
      mutate(s.outlier.part = s.outlier/(s.regular+s.outlier),
             n.outlier.part = n.outlier/(n.regular+n.outlier)) %>%
      ggplot(aes(x=reorder(Region,s.outlier.part), y = s.outlier.part)) +
      geom_col(aes(text=sprintf("Region: %s<br>Proportion of streams: %s",
                                Region, percent(s.outlier.part)),
                   fill = s.outlier.part)) +
      scale_fill_gradient(low = "blue",high = "red") +
      coord_flip(ylim = c(0.725,0.92)) +
      labs(title = "Proportion of streams generated by Top 20%",
           x = "Region",
           y = "Proportion of total streams") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.title = element_blank())
    ggplotly(g6, tooltip = "text") %>% 
      config(displayModeBar = FALSE)
  })
  
  output$map2 <- renderGvis({
    gvisGeoChart(data = polarity.region,
                 locationvar = "Region",
                 colorvar = "s.outlier.part",
                 # sizevar = "s.outlier.part",
                 #hovervar = "s.outlier.part",
                 options=list(colors="['#443aff', '#c868c9' ,'#f92222']",
                              region="world", displayMode="regions",
                              resolution="countries",
                              width="auto", height="auto",
                              backgroundColor = "#d1edf9"))
  })
  
  output$global.top20 <- renderValueBox({
    valueBox(paste0(percent(sum(polarity.region$s.outlier)/
                       sum(polarity.region$s.outlier + polarity.region$s.regular))," of streams"),
             " were generated by the top 20% tracks",
             color = "red",
             icon = icon("arrow-up"),
             width = "auto")
  })
  
  output$global.top20.streams <- renderValueBox({
    valueBox(comma(sum(polarity.region$s.outlier)),
             " streams generated by the top 20% tracks",
             color = "purple",
             icon = icon("volume-up"),
             width = "auto")
  })
  
  output$global.streams <- renderValueBox({
    valueBox(comma(sum(polarity.region$s.outlier + polarity.region$s.regular)),
             " total streams",
             color = "green",
             icon = icon("spotify"),
             width = "auto")
  })
  
  output$clusters <- renderPlotly({
    g7 <- polarity.region %>%
      left_join(., top.ten.region, by = "Region") %>%
      ggplot(aes(x =mean.days.to.top, y = mean.days.after.top)) +
      geom_point(aes(
        text=sprintf("Region: %s<br>Top 20 percent proportion of streams: %s<br>Days to top: %s<br>Days after top: %s",
                                  Region, percent(s.outlier.part),round(mean.days.to.top),round(mean.days.after.top)),
                     color = s.outlier.part, size = s.outlier)) + 
      scale_color_gradient(low = "blue", high = "red") + 
      labs(y = "Days after reaching top 10", x = "Days it took to reach top 10",
           title = "Mean track adoption and retention") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    ggplotly(g7,tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
})

# function(input, output, session) {
#   
#   observe(
#     print(input$Top.tracks)
#   )
#   
#   rv <- reactiveValues(value = 10)
#   
#   observeEvent(input$scatter.top.tracks,{
#     rv$value <- input$scatter.top.tracks
#   })
#   
#   output$plot1 <- renderPlotly({
#     spotify.data %>%
#       filter(Track.Name %in% input$Top.tracks, Region %in% input$selected.regions) %>%
#       ggplot(aes(x= Date, y = Position)) +
#       geom_point(aes(color = Track.Name), alpha = 0.2) +
#       geom_smooth(aes(color = Track.Name)) +
#       scale_y_reverse() + 
#       facet_grid(.~Region) + 
#       theme(legend.position = "bottom") + 
#       labs(title = "Track adoption and track retention")
#   })
#   
#   scatter.top.tracks.react <- reactive({
#     best.ranks.region <- spotify.data %>%
#       filter(Region != "global", Position == rv$value) %>%
#       group_by(Region, Track.Name) %>%
#       summarise(top.rank.date = min(Date))
#     
#     spotify.data %>%
#       select(-URL) %>%
#       filter(Track.Name != "") %>%
#       left_join(., best.ranks.region, by = c("Region","Track.Name")) %>%
#       filter(!is.na(top.rank.date), Region != "global") %>%
#       group_by(Track.Name, Region) %>%
#       summarise(days.to.top = sum(ifelse(Date < top.rank.date,1,0)),
#                 days.after.top = sum(ifelse(Date > top.rank.date,1,0)),
#                 Streams = sum(as.numeric(Streams))) %>%
#       group_by(Region) %>%
#       summarise(mean.days.to.top = mean(days.to.top),
#                 mean.days.after.top = mean(days.after.top),
#                 number.of.tracks = n(),
#                 total.streams = sum(as.numeric(Streams)))
#   })
#   
#   output$plot2 <- renderPlotly({
#     scatter.top.tracks.react() %>%
#         ggplot(aes(x = mean.days.to.top, y = mean.days.after.top)) +
#         geom_point(aes(color = Region, size = number.of.tracks)) +
#         guides(fill = "none")
#   })
# }