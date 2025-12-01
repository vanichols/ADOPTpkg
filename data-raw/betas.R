#--take binned beta distributions from Adrian
library(readr)
library(ggplot2)
library(scales)

rm(list = ls())

source("R/palette.R")


# internal file - good input file ---------------------------------------------------------

d1 <- readxl::read_excel("data-raw/beta-distribution-cheat-sheet-impact.xlsx",
                         skip = 5)

#--process into tidy data
#--the values listed are for impact, not value
#--create a mirror image
d2 <-
  d1 |>
  dplyr::select(-tot) |>
  tidyr::fill(confidence) |>
  janitor::clean_names() |>
  tidyr::pivot_longer(x1:x5) |>
  dplyr::mutate(name = readr::parse_number(name),
                value_bin = dplyr::case_when(
                  name == 1 ~ 5,
                  name == 2 ~ 4,
                  name == 3 ~ 3,
                  name == 4 ~ 2,
                  name == 5 ~ 1,
                  TRUE ~ 9999
                )) |>
  dplyr::rename(score = value) |>
  dplyr::select(-name) |>
  dplyr::mutate_if(is.character, stringr::str_to_lower)


#--check it
d2 |>
  dplyr::mutate(ratingF = forcats::fct_inorder(rating),
                confidenceF = forcats::fct_inorder(confidence)) |>
  ggplot(aes(value_bin, score)) +
  geom_col() +
  facet_grid(ratingF ~confidenceF)

#--rename the ratings
d3 <-
  d2 |>
  dplyr::mutate(rating = dplyr::case_when(
    rating == "very low impact" ~ "very high value",
    rating == "low impact" ~ "high value",
    rating == "medium impact" ~ "neutral value",
    rating == "high impact" ~ "low value",
    rating == "very high impact" ~ "very low value"
  ))

#--check it
d3 |>
  dplyr::mutate(ratingF = forcats::fct_inorder(rating),
                confidenceF = forcats::fct_inorder(confidence)) |>
  ggplot(aes(value_bin, score)) +
  geom_col() +
  facet_grid(confidenceF~ratingF , labeller = label_wrap_gen(5))

#--make a nice one for supplemental figures
d3 |>
  dplyr::mutate(score = score/100) |>
  #--fix rating labels
  dplyr::mutate(
    rating = stringr::str_to_sentence(rating),
    ratingF = forcats::fct_inorder(rating),
    ratingF2 = forcats::fct_rev(ratingF),
    rating_numeric = as.numeric(ratingF2),
    rating = paste0(rating, " (", rating_numeric, ")"),
    ratingF = forcats::fct_inorder(rating),
    ratingF = forcats::fct_rev(ratingF)) |>
  #--fix confidence labels
  dplyr::mutate(
    confidence = dplyr::case_when(
      confidence == "l" ~ "Low confidence",
      confidence == "m" ~ "Medium confidence",
      confidence == "h" ~ "High confidence",
      confidence == "vh" ~ "Very high confidence",
      TRUE ~ "XX"),
    confidenceF = forcats::fct_inorder(confidence)) |>
  ggplot(aes(value_bin, score)) +
  geom_col(aes(fill = ratingF), show.legend = F, color = "black") +
  scale_y_continuous(labels = label_percent(),
                     limits = c(0, 1)) +
  scale_fill_discrete(palette = value_colors) +
  facet_grid(confidenceF~ratingF , labeller = label_wrap_gen(10)) +
  labs(x = "Value",
       y = "Weight") +
  theme_bw()

ggsave("data-raw/fig_beta-distributions.png",
       width = 6, height = 5)

#--assign numeric values to ratings
d4 <-
  d3 |>
  dplyr::mutate(rating_numeric = dplyr::case_when(
    rating == "very high value" ~ 5,
    rating == "high value" ~ 4,
    rating == "neutral value" ~ 3,
    rating == "low value" ~ 2,
    rating == "very low value" ~ 1
  )) |>
  dplyr::select(rating, rating_numeric, everything())

adopt_betas <- d4

usethis::use_data(adopt_betas,
                  overwrite = TRUE)
