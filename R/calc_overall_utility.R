#' Calcuate the overall utility of the strategies with user-defined weighting
#'
#' @param data The questionaire data template, filled in.
#' @param betas A dataset defining the beta distributions for confidence levels.
#' @returns A dataframe
#' @import stringr
#' @export

calc_overall_utility <- function(data = opat_example,
                             betas = opat_betas,
                             nsim = 100) {


  #-- vector of strategies
  v.strat <-
    data |>
    dplyr::pull(title) |>
    unique()

  #-- vector of metrics
  v.met <-
    data |>
    dplyr::pull(metric) |>
    unique()

  #--data with confidence intervals
  data_conf <-
    data |>
    dplyr::rename(rating_numeric = rating_1to5) |>
     #--join with confidence bins
    dplyr::left_join(betas,
              relationship =
                "many-to-many") |>
    dplyr::select(title, metric, weight, score, value_bin)


  value_bin_options <-
    data_conf %>%
    dplyr::select(value_bin) %>%
    dplyr::distinct()

  #--first scenario-------------------------
  data_1 <-
    data_conf %>%
    dplyr::filter(title == v.strat[1])

  bayes.value.vector1 <- NULL

  for(k in 1:length(v.met)){

    tmp.impact <- v.met[k]

    tmp.df <-
      data_1 %>%
      dplyr::select(metric, value_bin, score) %>%
      dplyr::filter(metric == tmp.impact)

    tmp.wt <-
      data_1 %>%
      dplyr::select(weight, metric) %>%
      dplyr::filter(metric == tmp.impact) %>%
      dplyr::distinct() %>%
      dplyr::pull(weight)

    tmp.samp <-
      sample(x = tmp.df$value_bin,
             prob = tmp.df$score,
             size = nsim*tmp.wt,
             replace = TRUE)

    bayes.value.vector1 <- c(bayes.value.vector1, tmp.samp)

    k <- k + 1

  }

  datares_1 <-
    value_bin_options %>%
    dplyr::left_join(
      tibble::tibble(value_bin = bayes.value.vector1) %>%
        dplyr::group_by(value_bin) %>%
        dplyr::summarise(score = dplyr::n()/nsim)
    ) %>%
    dplyr::mutate(
      score = ifelse(is.na(score), 0, score),
      metric  = "Weighted combo",
      title = v.strat[1])

  #--second scenario-------------------------
  data_2 <-
    data_conf %>%
    dplyr::filter(title == v.strat[2])

  bayes.value.vector2 <- NULL

  for(j in 1:length(v.met)){

    tmp.impact <- v.met[j]

    tmp.df <-
      data_2 %>%
      dplyr::select(metric, value_bin, score) %>%
      dplyr::filter(metric == tmp.impact)

    tmp.wt <-
      data_2 %>%
      dplyr::select(weight, metric) %>%
      dplyr::filter(metric == tmp.impact) %>%
      dplyr::distinct() %>%
      dplyr::pull(weight)

    tmp.samp <-
      sample(x = tmp.df$value_bin,
             prob = tmp.df$score,
             size = nsim*tmp.wt,
             replace = TRUE)

    bayes.value.vector2 <- c(bayes.value.vector2, tmp.samp)

    j <- j + 1

  }

  datares_2 <-
    value_bin_options %>%
    dplyr::left_join(
      tibble::tibble(value_bin = bayes.value.vector2) %>%
        dplyr::group_by(value_bin) %>%
        dplyr::summarise(score = dplyr::n()/nsim)
    ) %>%
    dplyr::mutate(
      score = ifelse(is.na(score), 0, score),
      metric  = "Weighted combo",
      title = v.strat[2])


  datares <-
    dplyr::bind_rows(datares_1, datares_2) |>
    select(title, metric, value_bin, score)



  #--calculate utility
  data_util<-
    datares |>
    group_by(title, metric) |>
    summarise(utility = weighted.mean(value_bin, w = score))

  #--calculate standard deviation of distribution
  data_sd <-
    datares |>
    select(title, metric, score, value_bin) |>
    left_join(data_util |>
                select(title, metric, utility)) |>
    mutate(term = (value_bin-utility)^2 * score/100) |>
    group_by(title, metric) |>
    summarise(mysd = sum(term)^0.5)

  #--check if the values make sense, they do I guess
  datares |>
    left_join(data_util) |>
    left_join(data_sd) |>
    ggplot(aes(value_bin, score)) +
    geom_col() +
    geom_label(aes(3, 100, label = utility)) +
    geom_label(aes(3, 80, label = round(mysd, 2))) +
    facet_grid(title~ metric)

  #--calculate differences in mean utility
  data_util_change <-
    data_util |>
      #--calculate change from baseline to new approach
    mutate(title = as.factor(title),
           title = paste0("S", as.numeric(title))) |>
      pivot_wider(names_from = title,
                  values_from = utility) |>
      rename (base = S1, new = S2) |>
      mutate(diff_pct = (new - base)/base*100)

  #--calculate uncertainty in differences
  data_util_change_sd <-
    data_sd |>
    #--calculate change from baseline to new approach
    mutate(title = as.factor(title),
           title = paste0("S", as.numeric(title))) |>
    pivot_wider(names_from = title,
                values_from = mysd) |>
    rename (base = S1, new = S2) |>
    mutate(diff_sd = sqrt(base*base + new*new))

  tibble(
    sd_50p = rnorm(1000, 50, 25),
    sd_100p = rnorm(1000, 50, 50),
    sd_20p = rnorm(1000, 50, 10),
    sd_5p = rnorm(1000, 50, 5)
  ) |>
    pivot_longer(1:4) |>
    ggplot(aes(value)) +
    geom_histogram(aes(fill = name)) +
  scale_x_continuous(limits = c(0, 100)) +
  facet_grid(.~name)

#final_data <-
  data_util |>
  left_join(data_sd) |>
  pivot_longer(utility:mysd) |>
  mutate(value = value*20) |>
    pivot_wider(names_from = name,
                values_from = value) |>
    mutate(conf = case_when(
      (mysd < 5) ~ "vh",
      ((mysd >= 5) & (mysd < 50)) ~ "h",
      ((mysd >= 50) & (mysd < 75)) ~ "m",
      (mysd >= 75) ~ "l",
      TRUE ~ "XXX"
    ))


   }

# #--testing function

