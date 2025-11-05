#' Create a ridge plot of the six metrics for a given strategy
#'
#' @param data The questionaire data template, filled in.
#' @param betas A dataset defining the beta distributions for confidence levels.
#' @returns A plot
#' @import ggplot2
#' @import stringr
#' @import ggridges
#' @export


make_ridge_plots <- function(data = opat_example,
                             betas = opat_betas) {

  metric_colors6 <- c(
    "Crop value" =  "#c2e699",
    "Costs" = "#fd8d3c",
    "Time/management complexity" = "#f768a1",
    "Immediate usability" = "#fdbe85",
    "Environmental impact" = "#7a0177",
    "User health and safety" = "#6baed6"
  )

  metric_names6 <-
    c(
      "Crop value",
      "Costs",
      "Time/management\ncomplexity",
      "Immediate usability",
      "Environmental impact",
      "User health and safety"
    )


  #--get names of approaches
  strategy_names <-
    data |>
    dplyr::select(title) |>
    dplyr::distinct()


  plot_data <-
    data |>
    dplyr::rename(rating_numeric = rating_1to5) |>
    dplyr::mutate(metric = c(metric_names6, metric_names6),
           metricF = factor(metric, levels = rev(metric_names6))) |>
    #--join with confidence bins
    dplyr::left_join(betas,
              relationship =
                "many-to-many") |>
    dplyr::select(title, metricF, value_bin, score)

  #--ridge plot

   plot_data |>
     dplyr::mutate(score = as.integer(score)) |>
     tidyr::uncount(score) |>
     ggplot(aes(x = value_bin,
                y = metricF)) +
     ggridges::geom_density_ridges2(
       aes(fill = title),
       alpha = 0.6,
       bandwidth = 0.5,
       scale = 0.9 #--height of distributions
       ) +
     scale_fill_manual(values = c("gray10", "#ffffcc"),
                       #values = c("#fdbe85", "#08519c"),
                       guide = guide_legend(ncol = 2)) +
     scale_x_continuous(
       breaks = c(1, 2, 3, 4, 5),
       labels = c("Unacceptable",
                  "Disuaded",
                  "Is a consideration",
                  "Acceptable",
                  "Highly acceptable")
     ) +
    labs(
      title = "Performance",
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
