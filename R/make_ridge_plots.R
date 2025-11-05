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

  metric_colors <- c(
    # Environmental fate
    "Soil persistence (DT50 soil)"         = "#ffffcc",
    "Water persistence (DT50 water)"       = "#c2e699",
    "Surface water transfer (Kfoc)"        = "#78c679",
    "Groundwater transfer (GUS)"           = "#31a354",
    "Aquatic biome transfer (BCF)"         = "#006837",
    # Ecotoxicity (terrestrial)
    "Birds (acute oral)"                   = "#feedde",
    "Earthworms (acute soil)"              = "#fdbe85",
    "Honeybees (acute oral/contact/other)" = "#fd8d3c",
    "Mammals (acute oral)"                 = "#d94701",
    # Ecotoxicity (aquatic)
    "Algae (acute aqueous)"                = "#eff3ff",
    "Aquatic invertebrates (acute aq.)"    = "#bdd7e7",
    "Aquatic invertebrates (chronic aq.)"  = "#6baed6",
    "Fish (acute aqueous)"                 = "#3182bd",
    "Fish (chronic aqueous)"               = "#08519c",
    # Human health
    "Mammals (acute dermal)"               = "#feebe2",
    "Mammals (acute inhalation)"           = "#fcc5c0",
    "Humans (carcinogenicity)"             = "#fa9fb5",
    "Humans (cholinesterase inhibition)"   = "#f768a1",
    "Humans (neurotoxicity)"               = "#c51b8a",
    "Humans (reprotoxicity)"               = "#7a0177"
  )


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
      "Time/management complexity",
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
    rename(rating_numeric = rating_1to5) |>
    mutate(metric = c(metric_names6, metric_names6),
           metricF = factor(metric, levels = rev(metric_names6))) |>
    #--join with confidence bins
    left_join(betas,
              relationship =
                "many-to-many") |>
    select(title, metricF, value_bin, score)

  #--experimenting

   plot_data |>
     mutate(score = as.integer(score)) |>
     uncount(score) |>
     ggplot(aes(x = value_bin,
                y = metricF)) +
     geom_density_ridges2(
       aes(fill = title),
       alpha = 0.6,
       bandwidth = 0.5,
       scale = 0.9 #--height of distributions
       ) +
     scale_fill_manual(values = c("#fdbe85", "#08519c"),
                       guide = guide_legend(ncol = 1)) +
     scale_x_continuous(
       breaks = c(1, 2, 3, 4, 5),
       labels = c("Unacceptable",
                  "Disuaded",
                  "Is a consideration",
                  "Acceptable",
                  "Highly acceptable")
     ) +
    labs(
      x = "Performance",
      y = NULL,
      fill = NULL
    ) +
    # Theme
    theme_minimal() +
    theme(
      legend.position = "top",
      legend.title = element_text(face = "bold"),
      #panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      # axis.text.y = element_blank(),
      # strip.text = element_text(face = "bold", size = rel(1.1)),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    )


   }

# #--testing function
#make_rose_plot_pair()
