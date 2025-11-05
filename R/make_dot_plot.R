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

cat_range <- 7.5

  plot_data1 <-
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
    summarise(utility = weighted.mean(value_bin, w = score),
              utility_sd = (sd(score))^2/(5))

  #--calculate differences in mean utility
  plot_data2 <-
    plot_data1 |>
    select(-utility_sd) |>
      #--calculate change from baseline to new approach
      pivot_wider(names_from = title,
                  values_from = utility) |>
      rename (base = 3, new = 4) |>
      mutate(diff_pct = (new - base)/base*100,diff_pct_abs = round(abs(diff_pct), -1),
             diff_pct_label = ifelse(diff_pct < 0, paste0("-", diff_pct_abs, "%"),
                                     paste0("+", diff_pct_abs, "%")))

  #--calculate uncertainty in differences
  plot_data3 <-
    plot_data1 |>
    select(-utility) |>
    #--calculate change from baseline to new approach
    pivot_wider(names_from = title,
                values_from = utility_sd) |>
    rename (base = 3, new = 4) |>
    mutate(diff_sd = sqrt(base + new)) |>
    select(metricF, diff_sd)

  #--experimenting
  #--be cool to have size relative to certainty?

   plot_data2 |>
     left_join(plot_data3) |>
     mutate(dummy = "hi") |>
     ggplot(aes(x = metricF,
                y = dummy)) +
     # geom_point(
     #   aes(fill = diff_pct,
     #       size = weights),
     #   pch = 21) +
     geom_label(aes(y = dummy,
                   label = diff_pct_label,
                   size = diff_sd,
                   fill = diff_pct),
                show.legend = F) +
     scale_fill_gradient2(mid = "white",
                          #high = "#08519c",
                          high = "#06386b",
                          low = "#8d2e01",
                          ) +
     scale_size_continuous(range = c(1, 5), guide = "none") +
     # scale_y_continuous(limits = c(-0.5, 1.5),
     #                    expand = expansion(0.001)) +
     coord_flip() +
     labs(
      x = NULL,
      y = NULL,
      title = "Change in performance with new strategy"
    ) +
     guides(fill = guide_legend(title.position = "top")) +
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
       panel.grid = element_blank(),
       #--ratings text
       axis.text.x = element_blank(),
       plot.title = element_text(hjust = 0.5, face = "bold"),
       plot.subtitle = element_text(hjust = 0.5)
     )


   }

# #--testing function
#make_rose_plot_pair()
