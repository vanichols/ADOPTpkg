#' Create a rose plot of the four categories for a given compound
#'
#' @param compound_name Name of desired compound.
#' @param data The opat_hpli dataset.
#' @returns A rose plot
#' @import ggnewscale
#' @import ggplot2
#' @import stringr
#' @export

# #--for testing
# compound_name <- "diquat"
# data <- opat_hpli
# #---

make_rose_plot <- function(compound_name = "diquat",
                           data = opat_hpli) {

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

  # Data to plot
  plot_data <-
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
  background <- data.frame(
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


  # Plot
  ggplot(plot_data, aes(
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
      title = paste("Compound:", compound_name),
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
      plot.title = element_text(hjust = 0.5, face = "bold")
    ) +
    # axis.ticks.y = element_line(color = "gray33")) +
    # Turn the barplot into a roseplot
    coord_polar(start = 0,
                clip = "off")
}

# #--testing function
# make_rose_plot(compound_name = "2,4-D",
#                data = opat_hpli)

