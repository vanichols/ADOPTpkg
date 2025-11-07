#' Create a ridge plot of the six metrics for a given strategy
#'
#' @param data The questionaire data template, filled in.
#' @param betas A dataset defining the beta distributions for confidence levels.
#' @returns A plot
#' @import ggplot2
#' @import stringr
#' @import forcats
#' @import ggridges
#' @export


make_ridge_plots <- function(data = opat_example,
                             betas = opat_betas) {

  metric_colors <- c(
    "Crop value" =  "#c2e699",
    "Costs" = "#ffffcc",
    "Time/management\ncomplexity" = "#fa9fb5",
    "Immediate usability" = "#fdbe85",
    "Environmental impact" = "#c51b8a",
    "User health and safety" = "#6baed6"
  )

  metric_names <-
    data |>
    dplyr::pull(metric) |>
    unique()

  metric_names_nice <-
    c(
      "Crop value",
      "Costs",
      "Time/management\ncomplexity",
      "Immediate usability",
      "Environmental impact",
      "User health and safety"
    )

  #--get names of approaches
  strategy_name <-
    data |>
    dplyr::pull(title) |>
    unique()

  if(length(strategy_name) > 0){

    m1 <- paste("Hi! The dataset you provided has more than one strategy.\nThe plots will reflect the first one listed:",
                strategy_name[1])
    warning(m1)
  }

  plot_data <-
    data |>
    dplyr::filter(title == strategy_name[1]) |>
    dplyr::rename(rating_numeric = rating_1to5) |>
    #--make metric into a factor
    dplyr::mutate(
      metric2 = dplyr::case_when(
        metric == metric_names[1] ~ metric_names_nice[1],
        metric == metric_names[2] ~ metric_names_nice[2],
        metric == metric_names[3] ~ metric_names_nice[3],
        metric == metric_names[4] ~ metric_names_nice[4],
        metric == metric_names[5] ~ metric_names_nice[5],
        metric == metric_names[6] ~ metric_names_nice[6]),
      metricF = factor(metric2, levels = (metric_names_nice))) |>
    #--join with confidence bins
    dplyr::left_join(betas,
              relationship =
                "many-to-many") |>
    #--make some things for the figure
    dplyr::arrange(metricF) |>
    dplyr::mutate(metric_label = paste0(metricF, " (", round(weight, 2), "%)"),
                  metricF = as.factor(metric_label),
                  metricF = forcats::fct_inorder(metricF)) |>
    dplyr::mutate(score = as.integer(score)) |>
    dplyr::select(title, metricF, weight, value_bin, score)

  #--ridge plot

   plot_data |>
     tidyr::uncount(score) |>
     ggplot() +
     geom_density(
       (aes(x = value_bin,
            fill = metricF)),
       bw = 0.5,
       show.legend = F
     ) +
     geom_col(data = plot_data,
              aes(value_bin, score/100),
              alpha = 0.5) +
   facet_wrap(~metricF, labeller = label_wrap_gen(width = 20)) +
     scale_fill_manual(values = unname(metric_colors)) +
     scale_x_continuous(
       breaks = c(1, 2, 3, 4, 5),
       labels = c("Unacceptable",
                  "Disuaded",
                  "Is a consideration",
                  "Acceptable",
                  "Highly acceptable")
     ) +
     scale_y_continuous(
       breaks = c(0, .2, .4, .6, .8, 1),
       limits = c(0, 1),
       labels = scales::label_percent(),
     ) +
    labs(
      title = paste(strategy_name[1]),
      y = NULL,
      fill = NULL,
      x = NULL
    ) +
    # Theme
    theme_minimal() +
    theme(
      legend.title = element_blank(),
      legend.position = "top",
      legend.justification = "center",
      legend.box = "horizontal",
      legend.key = element_blank(),
      legend.box.margin = margin(),
      legend.margin = margin(),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.caption = element_text(hjust = 0),
      legend.location = "plot",
      #--get rid of minor gridlines
      panel.grid.minor = element_blank(),
      #--ratings text
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    )


   }

# #--testing function
#make_ridge_plots()
