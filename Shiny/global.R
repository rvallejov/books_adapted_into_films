library(data.table)
library(plotly)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
# library(rsconnect)
library(scales)
library(stringr)

# LOAD THE DATA
goodreads.data <- fread(file = "goodreads_awards_backup.csv")
goodreads.data.2 <- fread(file = "goodreads_books.csv")
wiki.data <- fread(file = "wiki_books.csv")

goodreads.data <- bind_rows(select(goodreads.data,-votes),goodreads.data.2)

cat.correction <- data.frame(c("Picture Books","Graphic Novels & Comics","Food & Cooking","Middle Grade & Children's","Debut Author","Goodreads Author"),
                             c("Picture Book","Graphic Novel","Food & Cookbooks","Middle Grade & Children's Books","Debut Goodreads Author","Debut Goodreads Author"),
                             stringsAsFactors = FALSE)
colnames(cat.correction) <- c("category","correct.cat")
goodreads.data <- goodreads.data %>%
  left_join(.,cat.correction, by = "category") %>%
  mutate(category = ifelse(is.na(correct.cat),category,correct.cat),
         book_title = str_replace(book_title," Boxset", "")) %>%
  select(-correct.cat)
goodreads.data <- distinct(goodreads.data,author,book_title, .keep_all = TRUE)
# colnames(goodreads.data)[4] <- c("genre")

books.into.films <- left_join(goodreads.data,
                              wiki.data,
                              copy = TRUE,
                              by = "book_title")

books.into.films <- books.into.films %>% 
  rename(film_title_temp = film_title) %>% 
  mutate(adapted.film = ifelse(is.na(film_year),0,1),
         book_year = ifelse(is.na(book_year),year,book_year),
         film_title = gsub("â€“","-",film_title_temp)) %>% 
  select(-year, -book_author, -film_title_temp, -category, -film_director
         ,-film_user_reviews,-film_critic_reviews, -film_meta_score)

