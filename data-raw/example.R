#--pull in example data

rm(list = ls())

library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)

d1 <- readxl::read_excel("data-raw/Example questionnaire.xlsx",
                         skip = 5)

#--process into tidy data
d2 <-
  d1 %>%
  tidyr::fill(title, pesticide_load) |>
  dplyr::mutate(pesticide_load = as.numeric(pesticide_load)) |>
  dplyr::select(-notes)

opat_example <- d2

usethis::use_data(opat_example, overwrite = TRUE)
