#' Calcuate the overall utility of the strategies with user-defined weighting
#'
#' @param data The questionaire data template, filled in.
#' @param betas A dataset defining the beta distributions for confidence levels.
#' @returns A dataframe
#' @import stringr
#' @export

#--for testing
data <- adopt_example
betas <- adopt_betas
nsim <- 10000

calc_overall_utility <- function(data = adopt_example,
                                 betas = adopt_betas,
                                 nsim = 10000) {
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
                       by = c("rating_numeric", "confidence"),
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

    for (k in 1:length(v.met)) {
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
        sample(
          x = tmp.df$value_bin,
          prob = tmp.df$score,
          size = nsim * tmp.wt,
          replace = TRUE
        )

      bayes.value.vector1 <- c(bayes.value.vector1, tmp.samp)

      k <- k + 1

    }

    datares_1 <-
      value_bin_options %>%
      dplyr::left_join(
        tibble::tibble(value_bin = bayes.value.vector1) %>%
          dplyr::group_by(value_bin) %>%
          dplyr::summarise(score = dplyr::n() / nsim),
      by = "value_bin") %>%
      dplyr::mutate(
        score = ifelse(is.na(score), 0, score),
        metric  = "Weighted combo",
        title = v.strat[1]
      )

    #--second scenario-------------------------
    data_2 <-
      data_conf %>%
      dplyr::filter(title == v.strat[2])

    bayes.value.vector2 <- NULL

    for (j in 1:length(v.met)) {
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
        sample(
          x = tmp.df$value_bin,
          prob = tmp.df$score,
          size = nsim * tmp.wt,
          replace = TRUE
        )

      bayes.value.vector2 <- c(bayes.value.vector2, tmp.samp)

      j <- j + 1

    }

    datares_2 <-
      value_bin_options %>%
      dplyr::left_join(
        tibble::tibble(value_bin = bayes.value.vector2) %>%
          dplyr::group_by(value_bin) %>%
          dplyr::summarise(score = dplyr::n() / nsim),
        by = c("value_bin")
      ) %>%
      dplyr::mutate(
        score = ifelse(is.na(score), 0, score),
        metric  = "Weighted combo",
        title = v.strat[2]
      )


    datares <-
      dplyr::bind_rows(datares_1, datares_2) |>
      dplyr::select(title, metric, value_bin, score)


    #--calculate utility
    suppressMessages(
    data_util <-
      datares |>
      dplyr::group_by(title, metric) |>
      dplyr::summarise(utility = weighted.mean(value_bin, w = score))
    )

    #--calculate standard deviation of distribution
    suppressMessages(
    data_sd <-
      datares |>
      dplyr::select(title, metric, score, value_bin) |>
      dplyr::left_join(data_util |>
                         dplyr::select(title, metric, utility)) |>
      dplyr::mutate(term = (value_bin - utility)^2 * score) |>
      dplyr::group_by(title, metric) |>
      dplyr::summarise(mysd = sum(term)^0.5)
    )

    #--what is the maximum sd possible? I think it is 20
    #--if they were split
    #sqrt((3 - 5)^2 * 50 + (3 - 1)^2 * 50)

    #--check if the values make sense, they do I guess
    # datares |>
    #   dplyr::left_join(data_util) |>
    #   dplyr::left_join(data_sd) |>
    #   ggplot(aes(value_bin, score)) +
    #   geom_col() +
    #   geom_label(aes(3, 100, label = utility)) +
    #   geom_label(aes(3, 80, label = round(mysd, 2))) +
    #   facet_grid(title ~ metric)

    suppressMessages(
    final_data <-
      data_util |>
      dplyr::left_join(data_sd) |>
      dplyr::mutate(conf = dplyr::case_when(
        (mysd < 5) ~ "vh",
        ((mysd >= 5) & (mysd < 10)) ~ "h",
        ((mysd >= 10) & (mysd < 15)) ~ "m",
        (mysd >= 15) ~ "l",
        TRUE ~ "XXX"
      ))
    )

    return(final_data)


}

# #--testing function
calc_overall_utility()
