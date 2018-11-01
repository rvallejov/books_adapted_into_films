library(shiny)
library(shinydashboard)

shinyServer(function(input, output){
  
  # output$n.books <- renderValueBox({
  #   valueBox(comma(nrow(goodreads.data)),
  #            "Books",
  #            icon = icon("book"),
  #            width = "auto",
  #            color = "yellow")
  # })
  # 
  # output$n.ratings <- renderValueBox({
  #   valueBox(comma(sum(goodreads.data$ratings)),
  #            "distinct tracks",
  #            icon = icon("headphones"),
  #            width = "auto",
  #            color = "blue")
  # })
  
  output$barchart1 <- renderPlotly({
    p <- books.scatter %>% 
      group_by(book_year) %>%
      summarise(ratings = sum(ratings),
                avg_rating = mean(avg_rating),
                reviews = sum(reviews),
                adapted.film = mean(adapted.film),
                n = n()) %>%
      ggplot(aes(x=as.integer(book_year),y=n)) +
      geom_col(aes(text=sprintf("Year: %s<br>Number of books: %s<br>Avg. rating: %s",
                                book_year, n, avg_rating),
                   fill=log(ratings))) +
      scale_fill_gradient(low = "blue", high = "red") +
      labs(title = paste0(length(books.scatter$book_title)," books"),
           x = "First published",
           y = "Books")
    ggplotly(p,tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  output$scatter1 <- renderPlotly({
    p <- books.scatter %>% 
      ggplot(aes(x=avg_rating,y=log(ratings))) +
      geom_point(aes(text=sprintf("Book: %s<br>Ratings: %s<br>Avg. rating : %s",
                                  book_title, comma(ratings), avg_rating),
                     col=log(reviews)),
                 alpha = 0.6) + 
      scale_color_gradient(low = "blue", high = "red") +
      labs(title = paste0(comma(sum(books.scatter$ratings))," ratings"),
           x = "Avg. book rating",
           y = "Book ratings (log scale)")
    ggplotly(p,tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  output$scatter2 <- renderPlotly({
    p <- books.scatter %>% 
      ggplot(aes(x=avg_rating,y=log(ratings))) +
      geom_point(aes(text=sprintf("Book: %s<br>Ratings: %s<br>Avg. rating : %s",
                                  book_title, comma(ratings), avg_rating),
                     col=as.character(adapted.film)),
                 alpha = 0.8) + 
      labs(title = paste0(percent(mean(books.scatter$adapted.film))," adapted into films"),
           x = "Avg. book rating",
           y = "Book ratings (log scale)")
    ggplotly(p,tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  output$scatter3 <- renderPlotly({
    p <- books.scatter %>% 
      filter(adapted.film == 1) %>%
      ggplot(aes(x=ratings,y=film_ratings)) +
      geom_abline(aes(slope = 1, intercept = 0), color = "grey") +
      geom_point(aes(text=sprintf("Book: %s<br>Book ratings: %s<br>Film ratings: %s",
                                  book_title, ratings, film_ratings),
                     col= avg_rating),
                 alpha = 0.8) + 
      scale_colour_gradient(low = "blue", high = "red") +
      labs(title = paste0(length(unique(films.scatter$book_title)),
                          " books adapted into films"),
           x = "Book ratings",
           y = "Film ratings")
    ggplotly(p,tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  # output$barchart2 <- renderPlot({
  #   films.scatter %>%
  #     group_by(book_year) %>%
  #     summarise(film_ratings = sum(film_ratings),
  #               film_avg_rating = mean(film_avg_rating, na.rm = TRUE),
  #               n = n()) %>% 
  #     ggplot(aes(x=as.numeric(book_year),y=n)) +
  #     geom_col(aes(text=sprintf("Year: %s<br>Number of films: %s<br>Film ratings: %s",
  #                               book_year, n, comma(film_ratings)),
  #                  fill=film_ratings)) + 
  #     scale_fill_gradient(low = "blue", high = "red") +
  #     labs(title = "Films based on books",
  #          x = "First published",
  #          y = "Films")
  # })
  
  output$scatter4 <- renderPlotly({
    p <- films.scatter %>% 
      ggplot(aes(x=book_year,y=film_year)) +
      geom_point(aes(text=sprintf("Book: %s<br>Book year: %s<br>Film year: %s<br>Film ratings: %s",
                                  book_title, book_year, film_year, comma(film_ratings)),
                     col=log(film_ratings)),
                 alpha = 0.6) + 
      scale_color_gradient(low = "blue", high = "red") +
      labs(title = paste0(length(films.scatter$film_title)," film adaptations"),
           x = "Book publish year",
           y = "Film release year")
    ggplotly(p,tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
})
