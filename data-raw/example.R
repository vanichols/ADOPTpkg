#--pull in example data

rm(list = ls())


d1 <- readxl::read_excel("data-raw/Example questionnaire.xlsx",
                         skip = 5)

#--process into tidy data
d2 <-
  d1 %>%
  tidyr::fill(title, pesticide_load) |>
  dplyr::mutate(pesticide_load = as.numeric(pesticide_load),
                weight = as.numeric(weight)) |>
  dplyr::select(-notes)

adopt_example <- d2

usethis::use_data(adopt_example, overwrite = TRUE)
