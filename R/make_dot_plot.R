#' Create a dot plot summarizing change in the six metrics
#'
#' @param data The questionaire data template, filled in.
#' @param betas A dataset defining the beta distributions for confidence levels.
#' @returns A plot
#' @import ggplot2
#' @import stringr
#' @export

make_dot_plot <- function(data = opat_example,
                             betas = opat_betas) {

  plot_colors <- c(
    "Really bad" =  "#d94701",
    "Bad" = "#fdbe85",
    "Neutral" = "gray",
    "Good" = "#bdd7e7",
    "Really good" = "#08519c"
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

cat_range <- 7.5

  plot_data <-
    data |>
    rename(rating_numeric = rating_1to5) |>
    mutate(metric = c(metric_names6, metric_names6),
           metricF = factor(metric, levels = rev(metric_names6)),
           weights = c(50, 12.5, 6.25, 6.25, 12.5, 12.5,
                       50, 12.5, 6.25, 6.25, 12.5, 12.5)) |>
    #--join with confidence bins
    left_join(betas,
              relationship =
                "many-to-many") |>
    #--calculate utility
    group_by(title, metricF, weights) |>
    summarise(utility = weighted.mean(value_bin, w = score)) |>
      #--calculate change from baseline to new approach
      pivot_wider(names_from = title,
                  values_from = utility) |>
      rename (base = 3, new = 4) |>
      mutate(diff_pct = (new - base)/base*100,
             diff_cat = case_when(
               (diff_pct <= -cat_range - cat_range*2) ~ "Really bad",
               (diff_pct > -cat_range - cat_range*2) & (diff_pct <= -cat_range) ~ "Bad",
               (diff_pct > -cat_range) & (diff_pct <= cat_range) ~ "Neutral",
               (diff_pct > cat_range) & (diff_pct <= cat_range + cat_range*2) ~ "Good",
               diff_pct > cat_range + cat_range*2 ~ "Really good")
      )

  #--experimenting

   plot_data |>
     mutate(diff_pct_abs = abs(diff_pct),
            dummy = "hi") |>
     ggplot(aes(x = metricF,
                y = dummy)) +
     geom_point(
       aes(fill = diff_pct,
           size = weights),
       pch = 21) +
     scale_fill_gradient2() +
     scale_size_continuous(range = c(6, 20), guide = "none") +
     coord_flip() +
     labs(
      x = NULL,
      y = NULL,
      fill = "Change in performance"
    ) +
    # Theme
    theme_minimal() +
    theme(
      legend.position = "top",
      legend.title = element_text(face = "bold"),
      legend.background = element_rect(fill='transparent'),
      legend.box.background = element_rect(fill='transparent'),
      #panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      #axis.text.y = element_blank(),
      # strip.text = element_text(face = "bold", size = rel(1.1)),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    )


   }

# #--testing function
#make_rose_plot_pair()
