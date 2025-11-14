#' Create a rose plot for two compounds and distributions
#'
#' @param compound_name1 Name of desired compound.
#' @param compound_name2 Name of desired compound to compare.
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

adopt_Make_Distribution_Plot <- function(compound_name1 = "diquat",
                           compound_name2 = "glyphosate",
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

  plot_compounds <- c(compound_name1, compound_name2)

  # Data to plot
  plot_data <-
    data |>
    dplyr::filter(compound %in% plot_compounds) |>
    dplyr::select(compound, env_raw, eco.terr_raw, eco.aqua_raw, hum_raw) |>
    tidyr::pivot_longer(env_raw:hum_raw) |>
    dplyr::mutate(
      attribute = c(metric_names, metric_names),
      attributeF = factor(attribute, levels = metric_names),
      attribute_num = as.numeric(attributeF)
    ) |>
    #--make dummy x values
    dplyr::mutate(
      xmin = c(0, 120, 180, 240, 0, 120, 180, 240),
      xmid = c(60, 150, 210, 300, 60, 150, 210, 300),
      xmax = c(120, 180, 240, 360, 120, 180, 240, 360)
    )



  #--distribution of data for all compounds
  plot_data2 <-
    data |>
    #dplyr::group_by(compound_category) |>
    dplyr::arrange(load_score) |>
    dplyr::mutate(n = 1:dplyr::n(),
                  n = n/max(n))

  #--create facet titles from compounds and loads
  data_loads <-
    plot_data2 |>
    dplyr::filter(compound %in% plot_compounds) |>
    dplyr::select(compound, n, load_score) |>
    dplyr::mutate(load_score = round(load_score, 2))


    #--rectangle for load levels
  background2 <- data.frame(
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
      data = background2,
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
      data = plot_data2,
      ggplot2::aes(n, load_score),
      color = "black") +
    #--substance 1
      ggplot2::geom_point(
        data = data_loads |>
          dplyr::filter(compound == compound_name1),
        ggplot2::aes(n, load_score),
        fill = "red",
        pch = 21,
        size = 5) +
      ggrepel::geom_label_repel(
      data = data_loads |>
        dplyr::filter(compound == compound_name1),
      ggplot2::aes(n, load_score, label = paste(compound, load_score),
      size = 5)) +
      #--substance 2
      ggplot2::geom_point(
        data = data_loads |>
          dplyr::filter(compound == compound_name2),
        ggplot2::aes(n, load_score),
        fill = "red",
        pch = 21,
        size = 5) +
      ggrepel::geom_label_repel(
        data = data_loads |>
          dplyr::filter(compound == compound_name2),
        ggplot2::aes(n, load_score, label = paste(compound, load_score),
                     size = 5)) +#--general fig settings
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

# #--testing function
#make_rose_plot_pair()
