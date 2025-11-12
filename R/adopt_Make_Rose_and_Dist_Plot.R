#' Create a rose plot of the four categories for a given compound
#'
#' @param compound_name Name of desired compound.
#' @param data The adopt_hpli dataset.
#' @returns A rose plot
#' @import ggnewscale
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import stringr
#' @import patchwork
#' @export

# #--for testing
# compound_name <- "diquat"
# data <- adopt_hpli
# #---

adopt_Make_Rose_and_Dist_Plot <- function(
    compound_name = "diquat",
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

  #--extract category of compound for title
  data_compound_category <-
    data |>
    dplyr::filter(compound == compound_name) |>
    dplyr::pull(compound_category)


  #--extract load for title
  data_load <-
    data |>
    dplyr::filter(compound == compound_name) |>
    dplyr::pull(load_score) |>
    round(2)

  plot_title <-
    paste0("Compound: ",
         compound_name,
         " (",
         data_compound_category,
         ")")

  plot_subtitle <- paste("Overall load:", data_load)

  # Plot1, rose plot
  # Data to plot
  plot_data1 <-
    data |>
    dplyr::filter(compound == compound_name) |>
    dplyr::select(compound, env_raw, eco.terr_raw, eco.aqua_raw, hum_raw) |>
    tidyr::pivot_longer(env_raw:hum_raw) |>
    dplyr::mutate(
      attribute = metric_names,
      attributeF = factor(attribute, levels = metric_names),
      attribute_num = as.numeric(attributeF)
    ) |>
    #--make dummy x values
    dplyr::mutate(
      xmin = c(0, 120, 180, 240),
      xmid = c(60, 150, 210, 300),
      xmax = c(120, 180, 240, 360)
    )

  # Dummy data for background concentric circles
  background1 <- data.frame(
    xmin = 0,
    xmax = 360,
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


  plot1 <-
    ggplot2::ggplot(plot_data1,
                    ggplot2::aes(
    x = 0,
    #attribute,
    y = value,
    fill = attribute
  )) +
    # Concentric circles
    ggplot2::geom_rect(
      data = background1,
      ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        fill = band
      ),
      #show.legend = F,
      alpha = 0.5,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_manual(
      name = " ",
      # breaks = c(0, 0.5, 1.0, 1.5),
      values = c(
        "Low to moderate load" = "white",
        "Moderate to high load" = "gray85",
        "High to very high load" = "gray70"
      ),
      guide = guide_legend(override.aes = list(
        color = "gray70", size  = 0.5
      ))
    ) +
    # Compartment divisions
    ggplot2::geom_segment(
      data = data.frame(x = c(0, 120, 180, 240)),
      ggplot2::aes(
        x = x,
        xend = x,
        y = 0,
        yend = 1.5
      ),
      colour = "gray65",
      linewidth = 0.5,
      inherit.aes = FALSE
    ) +
    # # New fill layer for the metrics
    ggnewscale::new_scale_fill() +
    # Metrics (existing values under 1.5)
    ggplot2::geom_rect(
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = 0,
        ymax = value,
        fill = attribute
      ),
      show.legend = F,
      color = "black",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_text(
      aes(x = xmid,
          y = 2,
          label = stringr::str_wrap(attribute, 8),
          color = attribute),
      show.legend = F,
      size = 3.5,
      #color = "#8B0000",
      fontface = "italic"
    ) +
    # Legend
    ggplot2::scale_fill_manual(values = metric_colors2, guide = guide_legend(ncol = 1)) +
    ggplot2::scale_color_manual(values = metric_colors2, guide = guide_legend(ncol = 1)) +
    ggplot2::labs(
      title = NULL,
      subtitle = NULL,
      x = NULL,
      y = NULL,
      #fill = "Metrics",
      #color = "Metrics"
    ) +
    # Theme
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      panel.grid.major.x = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    ) +
    # axis.ticks.y = element_line(color = "gray33")) +
    # Turn the barplot into a roseplot
    ggplot2::coord_polar(start = 0,
                clip = "off")

  # Plot 2, distribution plot

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

  #--distribution of data for this category
  plot_data2 <-
    data |>
    dplyr::group_by(compound_category) |>
    dplyr::arrange(load_score) |>
    dplyr::mutate(n = 1:dplyr::n(),
           n = n/max(n))

  plot2 <-
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
        guide = guide_legend(override.aes = list(
          color = "gray70", size  = 0.5
        ))
      ) +
    #--line of all compounds
    ggplot2::geom_line(
      data = plot_data2 |>
        dplyr::filter(compound_category == data_compound_category),
      ggplot2::aes(n, load_score),
      color = "black") +
    #--point representing selected compound
    ggplot2::geom_point(
      data = plot_data2 |>
        dplyr::filter(compound == compound_name),
      ggplot2::aes(n, load_score),
      pch = 24,
      fill = "#d94701",
      size = 5) +
    #--general fig settings
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
        legend.title = element_text(face = "bold"),
        #panel.grid.major.x = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_blank(),
        #axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 20)
      )


  patchwork::wrap_plots(plot1, plot2, ncol = 2) +
    patchwork::plot_layout(guides = "collect") &
    patchwork::plot_annotation(
      title = plot_title,
      subtitle = plot_subtitle,
      theme = theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                    plot.subtitle = element_text(hjust = 0.5),
                    legend.position = "bottom",
                    legend.title = element_text(face = "bold"))
    )


}


