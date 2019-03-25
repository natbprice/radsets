library(readr)
library(usethis)

# Use data from UpSetR package
upsetDataDir <- system.file("extdata", package = "UpSetR")
mutations <- read_delim(paste0(upsetDataDir,"/mutations.csv"), delim = ",")

# Save data
write_csv(mutations, "data-raw/mutations.csv")
usethis::use_data(mutations, overwrite = TRUE)
