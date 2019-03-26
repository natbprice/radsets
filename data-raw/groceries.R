library(arules)
library(dplyr)
library(readr)

data("Groceries")
summary(Groceries)

groceries <-
  as_tibble(as(Groceries, "matrix")) %>%
  mutate(id = row_number())

# Save data
write_csv(groceries, "data-raw/groceries.csv")
usethis::use_data(groceries, overwrite = TRUE)
