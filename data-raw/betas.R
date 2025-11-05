#--take binned beta distributions from Adrian
rm(list = ls())

library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)

d1 <- readxl::read_excel("data-raw/beta-distribution-cheat-sheet-impact.xlsx",
                         skip = 5)

#--process into tidy data
#--the values listed are for impact, not value
#--create a mirror image
d2 <-
  d1 %>%
  dplyr::select(-tot) %>%
  tidyr::fill(confidence) %>%
  janitor::clean_names() %>%
  tidyr::pivot_longer(x1:x5) %>%
  dplyr::mutate(name = readr::parse_number(name),
                value_bin = dplyr::case_when(
                  name == 1 ~ 5,
                  name == 2 ~ 4,
                  name == 3 ~ 3,
                  name == 4 ~ 2,
                  name == 5 ~ 1,
                  TRUE ~ 9999
                )) %>%
  dplyr::rename(score = value) %>%
  dplyr::select(-name) %>%
  dplyr::mutate_if(is.character, stringr::str_to_lower)


#--check it
d2 %>%
  dplyr::mutate(ratingF = forcats::fct_inorder(rating),
                confidenceF = forcats::fct_inorder(confidence)) %>%
  ggplot2::ggplot(ggplot2::aes(value_bin, score)) +
  ggplot2::geom_col() +
  ggplot2::facet_grid(ratingF ~confidenceF)

#--rename the ratings
d3 <-
  d2 |>
  mutate(rating = case_when(
    rating == "very low impact" ~ "very high value",
    rating == "low impact" ~ "high value",
    rating == "medium impact" ~ "neutral value",
    rating == "high impact" ~ "low value",
    rating == "very high impact" ~ "very low value"
  ))

#--check it
d3 %>%
  dplyr::mutate(ratingF = forcats::fct_inorder(rating),
                confidenceF = forcats::fct_inorder(confidence)) %>%
  ggplot2::ggplot(ggplot2::aes(value_bin, score)) +
  ggplot2::geom_col() +
  ggplot2::facet_grid(confidenceF~ratingF , labeller = label_wrap_gen(5))


opat_betas <- d3

usethis::use_data(opat_betas, overwrite = TRUE)
