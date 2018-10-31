library(shiny)
library(shinydashboard)

shinyServer(function(input, output){
  
  output$n.books <- renderValueBox({
    valueBox(comma(nrow(goodreads.data)),
             "Books",
             icon = icon("book"),
             width = "auto",
             color = "yellow")
  })
  
  output$n.ratings <- renderValueBox({
    valueBox(comma(sum(goodreads.data$ratings)),
             "distinct tracks",
             icon = icon("headphones"),
             width = "auto",
             color = "blue")
  })
  
  output$scatter1 <- renderPlotly({
    p <- books.into.films %>%
      filter(str_count(book_year,"-") == 0, str_count(film_year,"-") == 0 | is.na(film_year)) %>% 
      group_by(author,book_title) %>% 
      summarise(book_year = max(book_year), 
                avg_rating = max(avg_rating), 
                ratings = max(ratings),
                reviews = max(reviews),
                adapted.film = max(adapted.film)) %>% 
      ggplot(aes(x=avg_rating,y=log(ratings))) +
      geom_point(aes(text=sprintf("Book: %s<br>Ratings: %s<br>Avg. rating : %s",
                                  book_title, comma(ratings), avg_rating),
                     col=reviews),
                 alpha = 0.6) + 
      scale_color_gradient(low = "blue", high = "red") +
      labs(title = paste0(length(unique(books.into.films$book_title))," books"))
    ggplotly(p,tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  output$scatter2 <- renderPlotly({
    p <- books.into.films %>%
      filter(str_count(book_year,"-") == 0, str_count(film_year,"-") == 0 | is.na(film_year)) %>% 
      group_by(author,book_title) %>% 
      summarise(book_year = max(book_year), 
                avg_rating = max(avg_rating), 
                ratings = max(ratings),
                reviews = max(reviews),
                adapted.film = max(adapted.film)) %>% 
      ggplot(aes(x=avg_rating,y=log(ratings))) +
      geom_point(aes(text=sprintf("Book: %s<br>Ratings: %s<br>Avg. rating : %s<br>Adapted: %s",
                                  book_title, comma(ratings), avg_rating, adapted.film),
                     col=as.character(adapted.film)),
                 alpha = 0.8) + 
      labs(title = paste0(length(unique(books.into.films$book_title))," books"))
    ggplotly(p,tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  output$scatter3 <- renderPlotly({
    p <- books.into.films %>%
      filter(str_count(book_year,"-") == 0, str_count(film_year,"-") == 0 | is.na(film_year)) %>% 
      group_by(author,book_title) %>% 
      summarise(book_year = max(book_year), 
                avg_rating = max(avg_rating), 
                ratings = max(ratings),
                reviews = max(reviews),
                adapted.film = max(adapted.film),
                film_avg_rating = max(film_avg_rating),
                film_ratings = max(film_ratings)) %>% 
      ggplot(aes(x=avg_rating,y=log(ratings))) +
      geom_point(aes(text=sprintf("Book: %s<br>Ratings: %s<br>Adapted: %s",
                                  book_title, comma(ratings), adapted.film),
                     col=film_avg_rating),
                 alpha = 0.5) + 
      scale_color_gradient(low = "blue", high = "red") +
      facet_grid(.~adapted.film) +
      labs(title = paste0(length(unique(books.into.films$book_title))," books"))
    ggplotly(p,tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
})
