library(janitor)
library(tidyverse)
library(usethis)

# Reading and cleaning the GOI.csv data set, then removing the first row
GOI_data <- read_csv("data-raw/GOI.csv") %>%
  clean_names() %>%
  slice(-1)

# Saving the prepared data set
usethis::use_data(GOI_data, overwrite = TRUE)

