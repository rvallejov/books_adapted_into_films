library(data.table)
library(plotly)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(stringr)
library(scales)

# goodreads.data <- fread(file = "C:/Users/USUARIO/Documents/NYC DA/Python/NYC DA/goodreads/goodreads_awards.csv")
goodreads.data <- fread(file = "goodreads_awards_backup.csv")
goodreads.data.2 <- fread(file = "goodreads_books.csv")
wiki.data <- fread(file = "wiki_books.csv")

colnames(goodreads.data)[4] <- c("genre")

summary(goodreads.data)
summary(goodreads.data.2)
summary(wiki.data)
head(wiki.data)
tail(wiki.data)

wiki.data %>%
  filter(str_count(book_title,"Harry Potter") > 0)
  # filter(str_count(book_title,"Scarlet Letter") > 0)

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

rm(goodreads.data.2)
rm(cat.correction)

nrow(goodreads.data)
goodreads.data <- distinct(goodreads.data,author,book_title, .keep_all = TRUE)
 
goodreads.data %>%
  filter(str_count(author,"Suzanne Collins") > 0)
  # filter(str_count(book_title,"Harry Potter") > 0)
wiki.data %>%
  filter(str_count(book_author,"Suzanne Collins") > 0)

books.into.films <- left_join(goodreads.data,
                              wiki.data,
                              copy = TRUE,
                              by = "book_title")

books.into.films <- books.into.films %>% 
                      rename(film_title_temp = film_title) %>% 
                      mutate(adapted.film = ifelse(is.na(film_year),0,1),
                             book_year = ifelse(is.na(book_year),year,book_year),
                             film_title = gsub("â€“","-",film_title_temp)) %>% 
                      select(-year, -book_author, -film_title_temp, -genre, -film_director,-film_user_reviews,-film_critic_reviews, -film_meta_score)

head(books.into.films)
summary(books.into.films)
books.into.films %>%
  # filter(adapted.film == 1, str_count(book_year,",") == 0) %>%
  # filter(str_count(book_year,",") == 0) %>%
  distinct(book_title) %>%
  dim(.)
books.into.films %>%
  filter(str_count(author,"Suzanne Collins") > 0)
wiki.data %>%
  filter(str_count(book_author,"Suzanne Collins") > 0)
books.into.films %>%
  filter(str_count(book_title,"Harry Potter") > 0)
books.into.films %>%
  filter(str_count(book_title,"Twilight") > 0)

# Get the top genres of books by number of ratings
top.categories <- goodreads.data %>%
                    group_by(category) %>%
                    summarise(ratings = sum(ratings),avg_rating = mean(avg_rating)) %>%
                    top_n(10, ratings)

# Plot boxplots for the top 10 genres
goodreads.data %>%
  filter(category %in% top.categories$category) %>%
  ggplot(aes(x=category,y=ratings)) +
  geom_boxplot()

# Time series / bar plot showing books over time
g <- books.into.films %>%
  filter(str_count(book_year,"-") == 0, str_count(film_year,"-") == 0 | is.na(film_year)) %>% 
  group_by(author,book_title) %>% 
  summarise(book_year = max(book_year), 
            avg_rating = max(avg_rating), 
            ratings = max(ratings),
            reviews = max(reviews),
            adapted.film = max(adapted.film)) %>% 
  group_by(book_year) %>%
  summarise(ratings = sum(ratings),
            avg_rating = mean(avg_rating),
            reviews = sum(reviews),
            adapted.film = mean(adapted.film),
            n = n()) %>%
  ggplot(aes(x=as.integer(book_year),y=ratings/n)) +
  geom_col(aes(color = adapted.film)) +
  scale_color_gradient(low = "blue", high = "red")
ggplotly(g) %>% config(displayModeBar = FALSE)

# Relationship between avg_rating and log(ratings)
g <- books.into.films %>%
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
ggplotly(g,tooltip = "text") %>% config(displayModeBar = FALSE)

# Relationship between avg_rating and log(ratings)
# by adapted screenplays
g <- books.into.films %>%
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
ggplotly(g,tooltip = "text") %>% config(displayModeBar = FALSE)

# Relationship between avg_rating and log(ratings)
# and film_avg_rating
# by adapted screenplays
g <- books.into.films %>%
  filter(str_count(book_year,"-") == 0, str_count(film_year,"-") == 0 | is.na(film_year)) %>% 
  group_by(author,book_title) %>% 
  summarise(book_year = max(book_year), 
            avg_rating = max(avg_rating), 
            ratings = max(ratings),
            reviews = max(reviews),
            adapted.film = max(adapted.film),
            film_avg_rating = max(film_avg_rating),
            film) %>% 
  ggplot(aes(x=avg_rating,y=log(ratings))) +
  geom_point(aes(text=sprintf("Book: %s<br>Ratings: %s<br>Adapted: %s",
                              book_title, comma(ratings), adapted.film),
                 col=film_avg_rating),
             alpha = 0.5) + 
  scale_color_gradient(low = "blue", high = "red") +
  facet_grid(.~adapted.film) +
  labs(title = paste0(length(unique(books.into.films$book_title))," books"))
ggplotly(g,tooltip = "text") %>% config(displayModeBar = FALSE)

library(VIM) #For the visualization and imputation of missing values.

summary(books.into.films) #Summary information for the sleep dataset.
VIM::aggr(books.into.films)

library(mice) #Load the multivariate imputation by chained equations library.
dim(books.into.films)
sum(is.na(books.into.films))
mice::md.pattern(books.into.films[,c(6:11)])
mice::md.pattern(books.into.films)
