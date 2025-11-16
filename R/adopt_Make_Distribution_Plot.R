#' Create a rose plot for two compounds and distributions
#'
#' @param compound_names Vector of desired compounds, length one or more.
#' @param data The adopt_hpli dataset.
#' @returns A paired rose plot and distribution
#' @import dplyr
#' @import tidyr
#' @import ggnewscale
#' @import ggplot2
#' @import stringr
#' @import ggrepel
#' @import patchwork
#' @export

adopt_Make_Distribution_Plot <- function(
    compound_names = c("diquat", "glyphosate"),
    data = adopt_hpli) {

  metric_colors2 <- c(
    "Environmental fate" =  "#31a354",
    "Ecotoxicity (terrestrial)" = "#fd8d3c",
    "Ecotoxicity (aquatic)" = "#08519c",
    "Human health" = "#7a0177"
  )

  metric_names <-
    c(
      "Environmental fate",
      "Ecotoxicity (terrestrial)",
      "Ecotoxicity (aquatic)",
      "Human health"
    )

  plot_compounds <- compound_names

  #--distribution of data for all compounds
  plot_data <-
    data |>
    #dplyr::group_by(compound_category) |>
    dplyr::arrange(load_score) |>
    dplyr::mutate(n = 1:dplyr::n(),
                  n = n/max(n))

  #--get
  data_compounds <-
    plot_data |>
    dplyr::filter(compound %in% plot_compounds) |>
    dplyr::select(compound, n, load_score) |>
    dplyr::mutate(load_score = round(load_score, 2))


    #--rectangle for load levels
  background <- data.frame(
    xmin = 0,
    xmax = 1,
    ymin = c(0, 0.5, 1.0),
    ymax = c(0.5, 1.0, 1.5),
    band = factor(
      c("Low to moderate load",
        "Moderate to high load",
        "High to very high load"),
      levels = c("Low to moderate load",
                 "Moderate to high load",
                 "High to very high load")
    )
  )

    ggplot2::ggplot() +
    #--rectangles of load division
    ggplot2::geom_rect(
      data = background,
      ggplot2::aes(xmin = xmin,
                   xmax = xmax,
                   ymin = ymin,
                   ymax = ymax,
                   fill = band),
      show.legend = F) +
    ggplot2::scale_fill_manual(
      name = " ",
      # breaks = c(0, 0.5, 1.0, 1.5),
      values = c(
        "Low to moderate load" = "white",
        "Moderate to high load" = "gray85",
        "High to very high load" = "gray70"
      ),
      guide = ggplot2::guide_legend(override.aes = list(
        color = "gray70", size  = 0.5
      ))
    ) +
    #--line of all compounds
    ggplot2::geom_line(
      data = plot_data,
      ggplot2::aes(n, load_score),
      color = "black") +
    #--substance 1
      ggplot2::geom_point(
        data = data_compounds |>
          dplyr::filter(compound == plot_compounds[1]),
        ggplot2::aes(n, load_score),
        fill = "red",
        pch = 21,
        size = 5) +
      ggrepel::geom_label_repel(
      data = data_compounds |>
        dplyr::filter(compound == plot_compounds[1]),
      ggplot2::aes(n, load_score, label = paste(plot_compounds[1], load_score)),
      size = 5) +
      #--substance 2
      ggplot2::geom_point(
        data = data_compounds |>
          dplyr::filter(compound == plot_compounds[2]),
        ggplot2::aes(n, load_score),
        fill = "red",
        pch = 21,
        size = 5) +
      ggrepel::geom_label_repel(
        data = data_compounds |>
          dplyr::filter(compound == plot_compounds[2]),
        ggplot2::aes(n, load_score, label = paste(plot_compounds[2], load_score)),
                     size = 5) +
    ggplot2::scale_x_continuous(
      breaks = c(0, 0.5, 1),
      labels = c("Lowest\ntoxicity", "Median\ntoxicity", "Highest\ntoxicity")
    ) +
    ggplot2::labs(
      title = NULL,
      subtitle = NULL,
      x = NULL,
      y = "Toxicity load"
    ) +
    # Theme
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold"),
      #panel.grid.major.x = element_blank(),
      #panel.grid.major = element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      #axis.text.x = element_blank(),
      #axis.text.y = element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      plot.margin = ggplot2::margin(t = 0,  # Top margin
                           r = 0,  # Right margin
                           b = 0,  # Bottom margin
                           l = 20)
    )



}
