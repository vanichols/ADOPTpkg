library(dplyr)
library(ggplot2)
library(readxl)

vera <-
  read_excel("data-raw/Copy of final_PL_all_24.11.xlsx") |>
  rename(cas = 2,
         compound = 4,
         load_score_vera = PL_total)


dat <-
  adopt_hpli |>
  select(compound, cas, load_score_noe = load_score) |>
  left_join(vera |>
              select(cas, compound, load_score_vera)
  )

dat |>
  ggplot(aes(load_score_vera, load_score_noe)) +
  geom_point() +
  geom_abline() +
  scale_x_continuous(limits = c(0, 1.25)) +

  scale_y_continuous(limits = c(0, 1.25))


