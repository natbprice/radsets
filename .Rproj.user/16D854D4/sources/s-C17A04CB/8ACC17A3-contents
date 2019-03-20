library(dplyr)
library(readr)
library(usethis)
library(tidyr)
library(stringr)

# https://www.kaggle.com/carrie1/ecommerce-data

# Load data
ecommerceRaw <- read_csv("data-raw/data.csv")

itemCounts <-
  ecommerceRaw %>%
  group_by(StockCode) %>%
  summarize(N = n_distinct(CustomerID)) %>%
  arrange(desc(N))

ecommerce <-
  ecommerceRaw %>%
  mutate(year = as.numeric(str_extract(InvoiceDate, "[:digit:]{4}")),
         month = str_extract(InvoiceDate, "^[:digit:]+")) %>%
  filter(year == 2011, month == "8") %>%
  distinct(CustomerID, StockCode) %>%
  mutate(purchase = 1) %>%
  spread(StockCode, purchase, fill = 0)

# Save data
write_csv(ecommerce, "data-raw/ecommerce.csv")
usethis::use_data(ecommerce, overwrite = TRUE)
