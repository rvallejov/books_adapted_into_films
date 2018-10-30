library(data.table)
library(plotly)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(stringr)
# library(googleVis)
# library(rsconnect)
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

goodreads.data <- bind_rows(select(goodreads.data,-votes),goodreads.data.2)

cat.correction <- data.frame(c("Picture Books","Graphic Novels & Comics","Food & Cooking","Middle Grade & Children's","Debut Author","Goodreads Author"),
                             c("Picture Book","Graphic Novel","Food & Cookbooks","Middle Grade & Children's Books","Debut Goodreads Author","Debut Goodreads Author"),
                             stringsAsFactors = FALSE)

colnames(cat.correction) <- c("category","correct.cat")

goodreads.data <- goodreads.data %>%
                    left_join(.,cat.correction, by = "category") %>%
                    mutate(category = ifelse(is.na(correct.cat),category,correct.cat)) %>%
                    select(-correct.cat)

rm(goodreads.data.2)
rm(cat.correction)

genres <- sort(unique(goodreads.data$genre))

nrow(goodreads.data)
goodreads.data <- distinct(goodreads.data,author,book_title, .keep_all = TRUE)
goodreads.data %>%
  filter(str_count(book_title,"Harry Potter") > 0)

books.into.films <- left_join(goodreads.data,
                              wiki.data,
                              copy = TRUE,
                              by = "book_title")

books.into.films <- books.into.films %>% 
                      rename(film_title_temp = film_title) %>% 
                      mutate(adapted.film = ifelse(is.na(film_year),0,1),
                             book_year = ifelse(is.na(book_year),year,book_year),
                             film_title = gsub("â€“","-",film_title_temp)) %>% 
                      select(-year,-book_author, -film_title_temp)

head(books.into.films)
summary(books.into.films)
books.into.films %>%
  filter(adapted.film == 1) %>%
  head(10)
books.into.films %>%
  filter(str_count(film_title,"Hunger Games") > 0)
books.into.films %>%
  filter(str_count(book_title,"Harry Potter") > 0)

top.categories <- goodreads.data %>%
                    group_by(category) %>%
                    summarise(ratings = sum(ratings),avg_rating = mean(avg_rating)) %>%
                    top_n(10, ratings)

goodreads.data %>%
  filter(category %in% top.categories$category) %>%
  ggplot(aes(x=category,y=ratings)) +
  geom_boxplot()


g <- books.into.films %>%
  group_by(author,book_title) %>% 
  summarise(book_year = max(book_year), 
            avg_rating = max(avg_rating), 
            ratings = max(ratings),
            reviews = max(reviews),
            adapted.film = max(adapted.film)) %>% 
  ggplot(aes(x=book_title,y=avg_rating)) +
  geom_point(aes(text=sprintf("Book: %s<br>Ratings: %s<br>Adapted: %s",
                              book_title, comma(ratings), adapted.film),
                 col=as.character(adapted.film))) + 
  # theme(axis.text.x=element_blank(),
  #       axis.ticks.x=element_blank(),
  #       panel.grid.major = element_blank(),
  #       panel.grid.minor = element_blank()) +
  labs(title = paste0(length(unique(books.into.films$book_title))," books"))
ggplotly(g,tooltip = "text") %>% config(displayModeBar = FALSE)


g <- books.into.films %>%
  group_by(year) %>%
  summarise(ratings = sum(ratings), avg_rating = mean(avg_rating), reviews = sum(reviews)) %>%
  ggplot(aes(x=book_year,y=ratings)) +
  geom_col(aes(fill = reviews)) +
  scale_fill_gradient(low = "blue", high = "red") +
  coord_cartesian(xlim = c(1950, 2018))
ggplotly(g) %>% config(displayModeBar = FALSE)

goodreads.data %>%
  filter(str_sub(book_title,1,1) == "F") %>%
  arrange(book_title)






