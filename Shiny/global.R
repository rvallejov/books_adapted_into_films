library(data.table)
library(plotly)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(stringr)
# library(car)

# LOAD THE DATA
books.into.films <- fread(file = "books_into_films.csv", stringsAsFactors = FALSE)

books.scatter <- books.into.films %>%
  filter(str_count(book_year,"-") == 0, str_count(film_year,"-") == 0 | is.na(film_year)) %>% 
  group_by(author,book_title) %>% 
  summarise(book_year = max(as.integer(book_year)), 
            avg_rating = max(avg_rating), 
            ratings = max(ratings),
            reviews = max(reviews),
            adapted.film = max(adapted.film),
            film_year = max(as.integer(film_year)),
            film_avg_rating = sum(film_avg_rating),
            film_ratings = sum(film_ratings),
            n = n())

films.scatter <- books.into.films %>%
  filter(str_count(book_year,"-") == 0, str_count(film_year,"-") == 0) %>% 
  group_by(film_title,film_director,book_title) %>% 
  summarise(book_year = max(as.integer(book_year)), 
            avg_rating = mean(avg_rating), 
            ratings = sum(ratings),
            reviews = sum(reviews),
            adapted.film = max(adapted.film),
            film_year = max(as.integer(film_year)),
            film_avg_rating = max(film_avg_rating),
            film_ratings = max(film_ratings))

directors.scatter <- films.scatter %>% 
  filter(film_director != "") %>% 
  group_by(film_director) %>% 
  summarise(book_year = max(as.integer(book_year)), 
            avg_rating = mean(avg_rating), 
            ratings = sum(ratings),
            reviews = sum(reviews),
            adapted.film = max(adapted.film),
            film_year = mean(as.integer(film_year)),
            film_avg_rating = mean(film_avg_rating),
            film_ratings = sum(film_ratings),
            n = n())
