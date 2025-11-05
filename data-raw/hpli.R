#--use supplemental material from Noe

library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)

d1 <- read_excel("data-raw/Supplementary material Table S2 (V2).xlsx",
                sheet = "Pesticide Load (by substance)",
                skip = 3) |>
  rename(unknown = 8)

#--the unknown column is empty
d2 <-
  d1 |>
  janitor::remove_empty("cols")

#--in theory there should be no substances with <60% coverage, right?
#--ask Noe about this...
d2 |>
  mutate(data_coverage = 1 - missing_share) |>
  ggplot2::ggplot(aes(data_coverage)) +
  ggplot2::geom_histogram()

#--the ind compartments add up to the total load_score
#--to get the 'original' scores for each compartment,
#  undo the weighting by multiplying by 3, 6, 6, and 3

d3 <-
  d2 |>
  mutate(env_raw = env*3,
         eco.terr_raw = eco.terr * 6,
         eco.aqua_raw = eco.aqua * 6,
         hum_raw = hum * 3) |>
  select(compound,
         env_raw:hum_raw,
         env_sc = env,
         eco.terr_sc = eco.terr,
         eco.aqua_sc = eco.aqua,
         hum_sc = hum,
         everything())

opat_hpli <- d3

usethis::use_data(opat_hpli, overwrite = TRUE)
