#' Create a ridge plot of the six metrics for a given strategy
#'
#' @param data The questionaire data template, filled in.
#' @returns A plot
#' @import ggplot2
#' @import stringr
#' @export

make_rose_plot_pair <- function(data = opat_hpli) {

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



  #--create facet titles from compounds and loads
  data_loads <-
    data |>
    dplyr::filter(compound %in% plot_compounds) |>
    dplyr::select(compound, load_score) |>
    dplyr::mutate(load_score = round(load_score, 2))

  plot_data_pair <-
    plot_data |>
    dplyr::left_join(data_loads) |>
    dplyr::mutate(facet_name = paste("Compound:", compound, "\n Overall load:",
                              load_score))


  # Dummy data for background concentric circles
  background <-
    tidyr::crossing(
    data.frame(
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
                 "High to very high load"),
    )),
    plot_data_pair |>
      dplyr::select(facet_name) |>
      dplyr::distinct()
  )


  ggplot(plot_data_pair, aes(
    x = 0,
    #attribute,
    y = value,
    fill = attribute
  )) +
    # Concentric circles
    geom_rect(
      data = background,
      aes(
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
    scale_fill_manual(
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
    geom_segment(
      data = data.frame(x = c(0, 120, 180, 240)),
      aes(
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
    geom_rect(
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
    geom_text(
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
    scale_fill_manual(values = metric_colors2, guide = guide_legend(ncol = 1)) +
    scale_color_manual(values = metric_colors2, guide = guide_legend(ncol = 1)) +
    labs(
      x = NULL,
      y = NULL,
      fill = "Metrics",
      color = "Metrics"
    ) +
    # Theme
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      panel.grid.major.x = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      strip.text = element_text(face = "bold", size = rel(1.1)),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    ) +
    # axis.ticks.y = element_line(color = "gray33")) +
    # Turn the barplot into a roseplot
    coord_polar(start = 0,
                clip = "off") +
    facet_grid(.~facet_name)
}

# #--testing function
#make_rose_plot_pair()
