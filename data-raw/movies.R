library(dplyr)
library(readr)
library(usethis)
library(tidyr)
library(stringr)

# Data source
# http://grouplens.org/datasets/movielens/20m/

# Load data
movies <- read_csv("data-raw/movies.csv")
ratings <- read_csv("data-raw/ratings.csv")
links <- read_csv("data-raw/links.csv")

# Summarize ratings
ratingsSummary <-
  ratings %>%
  group_by(movieId) %>%
  summarize(avgRating = mean(rating),
            nRating = n())

# Transform data to a wide form for sets analysis
movieSets <-
  movies %>%
  mutate(year = as.numeric(str_extract(title, "(?<=\\()[:digit:]{4}(?=\\))")),
         title = str_remove(title, "\\([:digit:]{4}\\)"),
         nGenre = str_count(genres, "\\|") + 1) %>%
  separate(genres,
           into = as.character(1:max(.$nGenre)),
           sep = "\\|",
           fill = "right",
           remove = FALSE) %>%
  gather(genreIndex, genre, -movieId, -title, -year, -nGenre, -genres) %>%
  filter(!is.na(genre)) %>%
  mutate(indicator = 1) %>%
  select(movieId, title, genre, genres, year, indicator) %>%
  spread(genre, indicator, fill = 0) %>%
  select(-`(no genres listed)`) %>%
  left_join(ratingsSummary, by = "movieId") %>%
  left_join(links, by = "movieId") %>%
  select(movieId, title, year, imdbId, tmdbId, avgRating, nRating, everything())

# Save data
write_csv(movies, "data-raw/movieSets.csv")
usethis::use_data(movieSets, overwrite = TRUE)
