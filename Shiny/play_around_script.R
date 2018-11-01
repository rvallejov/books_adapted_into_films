library(data.table)
library(plotly)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(stringr)
library(scales)

wiki.data <- fread(file = "wiki_books.csv")
wiki.data <- wiki.data %>% 
  rename(film_title_temp = film_title) %>% 
  mutate(film_title = gsub("â€“","-",film_title_temp)) %>% 
  select(-film_title_temp)

goodreads.data <- fread(file = "goodreads_awards_backup.csv")
colnames(goodreads.data)[4] <- c("genre")
goodreads.data.2 <- fread(file = "goodreads_books.csv")
goodreads.data <- bind_rows(select(goodreads.data,-votes),goodreads.data.2)

cat.correction <- data.frame(c("Picture Books","Graphic Novels & Comics","Food & Cooking","Middle Grade & Children's","Debut Author","Goodreads Author"),
                             c("Picture Book","Graphic Novel","Food & Cookbooks","Middle Grade & Children's Books","Debut Goodreads Author","Debut Goodreads Author"),
                             stringsAsFactors = FALSE)
colnames(cat.correction) <- c("genre","correct.cat")
goodreads.data <- goodreads.data %>%
                    left_join(.,cat.correction, by = "genre") %>%
                    mutate(genre = ifelse(is.na(correct.cat),genre,correct.cat),
                           book_title = str_replace(book_title," Boxset", "")) %>%
                    select(-correct.cat)

rm(goodreads.data.2)
rm(cat.correction)
nrow(goodreads.data)
goodreads.data <- distinct(goodreads.data,author,book_title, .keep_all = TRUE)

wiki.data %>%
  filter(str_count(book_author,"Suzanne Collins") > 0) %>% 
  select(film_title, book_title, film_ratings)
  
goodreads.data %>% 
  filter(str_count(author,"Suzanne Collins") > 0) %>% 
  select(book_title, ratings)

film.book.title <- c("The Hunger Games: Catching Fire",
                     "The Hunger Games: Mockingjay - Part 1",
                     "The Hunger Games: Mockingjay - Part 2",
                     "Harry Potter and the Philosopher's Stone",
                     "Nineteen Eighty-Four",
                     "The Lord of the Rings: The Fellowship of the Ring",
                     "The Lord of the Rings: The Two Towers",
                     "The Lord of the Rings: The Return of the King")

correct.book.title <- c("Catching Fire",
                        "Mockingjay",
                        "Mockingjay",
                        "Harry Potter and the Sorcerer's Stone",
                        "1984",
                        "The Fellowship of the Ring",
                        "The Two Towers",
                        "The Return of the King")

book.correct <- data.frame(film.book.title,correct.book.title, stringsAsFactors = FALSE)
colnames(book.correct)[1] <- c("film_title")

wiki.data <- wiki.data %>% 
  left_join(., book.correct, by = "film_title") %>% 
  mutate(book_title = ifelse(is.na(correct.book.title),book_title,correct.book.title)) %>%
  # filter(str_count(book_author,"Suzanne Collins") > 0) %>% 
  # head(10) %>% 
  select(-correct.book.title)

books.into.films <- left_join(goodreads.data,
                              wiki.data,
                              copy = TRUE,
                              by = "book_title")

books.into.films <- books.into.films %>% 
                      # rename(film_title_temp = film_title) %>%
                      mutate(adapted.film = ifelse(is.na(film_year),0,1),
                             # film_title = gsub("â€“","-",film_title_temp),
                             book_year = ifelse(is.na(book_year),year,book_year)) %>% 
                      select(-year, -book_author, -genre
                             # -film_title_temp, 
                             )

books.into.films %>% 
  filter(str_count(book_title, "The Great Gatsby") == 0,
         str_count(book_title, "Divergent") == 0,
         str_count(author, "J.K. Rowling") == 0,
         str_count(book_title, "Pride and Prejudice") == 0,
         str_count(book_title, "The Hobbit") == 0,
         str_count(author, "Tolkien") == 0,
         str_count(author, "George Orwell") == 0,
         str_count(author, "C.S. Lewis") == 0,
         str_count(book_title, "Lord of the Flies") == 0,
         str_count(book_title, " The Girl with the Dragon Tattoo") == 0,
         str_count(author, "Suzanne Collins") == 0) %>% 
  arrange(desc(ratings)) %>% 
  select(book_title, film_title, film_ratings, author) %>% 
  head(15)

write.csv(books.into.films, file = "books_into_films.csv")

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
  ggplot(aes(x=as.integer(book_year),y=n)) +
  geom_col(aes(color = adapted.film)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = paste0(length(unique(books.into.films$book_title))," books"))
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
            film_avg_rating = max(film_avg_rating)) %>% 
  ggplot(aes(x=avg_rating,y=log(ratings))) +
  geom_point(aes(text=sprintf("Book: %s<br>Ratings: %s<br>Adapted: %s",
                              book_title, comma(ratings), adapted.film),
                 col=film_avg_rating),
             alpha = 0.5) + 
  scale_color_gradient(low = "blue", high = "red") +
  facet_grid(.~adapted.film) +
  labs(title = paste0(length(unique(books.into.films$book_title))," books"))
ggplotly(g,tooltip = "text") %>% config(displayModeBar = FALSE)

g <- books.scatter %>%
  filter(adapted.film == 1) %>%
  group_by(book_year) %>%
  summarise(ratings = sum(ratings),
            avg_rating = mean(avg_rating),
            reviews = sum(reviews),
            n = n()) %>% 
  ggplot(aes(x=book_year,y=film_year)) +
  geom_point(aes(text=sprintf("Book: %s<br>Book year: %s<br>Film year: %s<br>Ratings: %s<br>Film ratings: %s",
                              book_title, book_year, film_year, comma(ratings), comma(film_ratings)),
                 col=film_avg_rating),
             alpha = 0.5) + 
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = paste0(length(unique(books.into.films$book_title))," books"))
ggplotly(g,tooltip = "text") %>% config(displayModeBar = FALSE)


g <- films.scatter %>% 
  ggplot(aes(x=film_title,y=film_year)) +
  geom_point(aes(text=sprintf("Book: %s<br>Book year: %s<br>Film year: %s<br>Film ratings: %s",
                              book_title, book_year, film_year, comma(film_ratings)),
                 col=log(film_ratings)),
             alpha = 0.3) + 
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = paste0(length(unique(films.scatter$film_title))," films"),
       x = "Book publish year",
       y = "Film release year")
ggplotly(g,tooltip = "text") %>% config(displayModeBar = FALSE)

g <- films.scatter %>% 
  ggplot(aes(x=avg_rating,y=film_avg_rating)) +
  geom_point(aes(text=sprintf("Book: %s<br>Film avg. rating: %s<br>Book avg. rating: %s",
                              book_title, avg_rating, film_avg_rating),
                 col=film_year),
             alpha = 0.8) + 
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = paste0(length(unique(films.scatter$film_title))," films"),
       x = "Book avg. rating",
       y = "Film avg. rating")
ggplotly(g,tooltip = "text") %>% config(displayModeBar = FALSE)

films.scatter %>% 
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
            n = n()) %>% 
  select(film_director, film_avg_rating, n) %>% 
  top_n(10,n)

g <- films.scatter %>% 
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
            n = n()) %>% 
  ggplot(aes(x=avg_rating,y=film_avg_rating)) +
  geom_point(aes(text=sprintf("Film director: %s<br>Film avg. rating: %s<br>Book avg. rating: %s<br>Film adaptations: %s",
                              film_director, avg_rating, film_avg_rating, n),
                 col=n),
             alpha = 0.8) + 
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = paste0(length(unique(films.scatter$film_title))," films"),
       x = "Book avg. rating",
       y = "Film avg. rating")
ggplotly(g,tooltip = "text") %>% config(displayModeBar = FALSE)

##################### Scatter matrix ################

library(car)

as.data.frame(books.scatter) %>% 
  select(-author, -book_title, -adapted.film, -film_year, -film_avg_rating, -film_ratings) %>% 
  scatterplotMatrix(., diagonal=list(method ="histogram"), col= "steel blue",
                    ellipse=FALSE, smooth = FALSE,var.labels = colnames(.))

as.data.frame(films.scatter) %>% 
  select(-film_title, -book_title, -adapted.film) %>% 
  scatterplotMatrix(., diagonal=list(method ="histogram"), col= "steel blue",
                    ellipse=FALSE, smooth = FALSE,var.labels = colnames(.))

################ MISSINGNESS ##############

library(VIM) #For the visualization and imputation of missing values.

summary(books.into.films) #Summary information for the sleep dataset.
VIM::aggr(books.into.films)

library(mice) #Load the multivariate imputation by chained equations library.
dim(books.into.films)
sum(is.na(books.into.films))
mice::md.pattern(books.into.films[,c(6:11)])
mice::md.pattern(books.into.films)
