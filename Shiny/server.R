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
                     col=reviews),
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
  
  output$scatter4 <- renderPlotly({
    p <- films.scatter %>% 
      ggplot(aes(x=avg_rating,y=film_avg_rating/2)) +
      geom_point(aes(text=sprintf("Book: %s<br>Film: %s<br>Film avg. rating: %s<br>Book avg. rating: %s",
                                  book_title, film_title, film_avg_rating/2, avg_rating),
                     col=film_year),
                 alpha = 0.8) + 
      geom_abline(aes(slope = 1, intercept = 0),col = "grey") + 
      scale_color_gradient(low = "blue", high = "red") +
      labs(title = paste0(length(films.scatter$film_title),
                          " films: Only ",
                          sum(films.scatter$avg_rating < films.scatter$film_avg_rating/2, na.rm = TRUE),
                          " better than the book"),
           x = "Book avg. rating",
           y = "Film avg. rating")
    ggplotly(p,tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  output$scatter5 <- renderPlotly({
     
    p <- directors.scatter %>% 
      ggplot(aes(x=avg_rating,y=film_avg_rating/2)) +
      geom_point(aes(text=sprintf("Film director: %s<br>Film avg. rating: %s<br>Book avg. rating: %s<br>Film adaptations: %s",
                                  film_director, film_avg_rating/2, avg_rating, n),
                     col = n, size = n),
                 alpha = 0.5) + 
      geom_abline(aes(slope = 1, intercept = 0),col = "grey") + 
      scale_color_gradient(low = "blue", high = "red") +
      labs(title = paste0(length(directors.scatter$film_director)," directors: Only ",
                          sum(directors.scatter$avg_rating < directors.scatter$film_avg_rating/2, na.rm = TRUE),
                          " better than the book"),
           x = "Book avg. rating",
           y = "Film avg. rating")
    ggplotly(p,tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
})
