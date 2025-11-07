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

  baseline_color <- "gray10"
  strategy_color <- "#ffffcc"

  # plot_colors <- c(
  #   "Really bad" =  "#d94701",
  #   "Bad" = "#fdbe85",
  #   "Neutral" = "gray",
  #   "Good" = "#bdd7e7",
  #   "Really good" = "#08519c"
  # )

  metric_names <-
    data |>
    pull(metric) |>
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
  strategy_names <-
    data |>
    dplyr::select(title) |>
    dplyr::distinct()

  data1 <-
    data |>
    rename(rating_numeric = rating_1to5) |>
    #--make metric into a factor
    mutate(
      metric2 = case_when(
        metric == metric_names[1] ~ metric_names_nice[1],
        metric == metric_names[2] ~ metric_names_nice[2],
        metric == metric_names[3] ~ metric_names_nice[3],
        metric == metric_names[4] ~ metric_names_nice[4],
        metric == metric_names[5] ~ metric_names_nice[5],
        metric == metric_names[6] ~ metric_names_nice[6])) |>
     #--join with confidence bins
    left_join(betas,
              relationship =
                "many-to-many")


  #--calculate utility (mean of probabilty distribution)
  data2<-
    data1 |>
    group_by(title, metric2, weight) |>
    summarise(utility = weighted.mean(value_bin, w = score))

  #--calculate standard deviation of distribution
  data3 <-
    data1 |>
    select(title, metric2, score, value_bin) |>
    left_join(data2 |>
                select(title, metric2, utility)) |>
    mutate(term = (value_bin-utility)^2 * score/100) |>
    group_by(title, metric2) |>
    summarise(mysd = sum(term)^0.5)

  #--check if the values make sense, they do!
  data1 |>
    left_join(data2) |>
    left_join(data3) |>
    ggplot(aes(value_bin, score)) +
    geom_col() +
    geom_label(aes(3, 100, label = utility)) +
    geom_label(aes(3, 80, label = round(mysd, 2))) +
    facet_grid(title~ metric2)

  #--calculate differences in mean utility
  data4 <-
    data2 |>
      #--calculate change from baseline to new approach
    mutate(title = as.factor(title),
           title = paste0("S", as.numeric(title))) |>
      pivot_wider(names_from = title,
                  values_from = utility) |>
      rename (base = S1, new = S2) |>
      mutate(diff_pct = (new - base)/base*100)

  #--calculate uncertainty in differences
  data5 <-
    data3 |>
    #--calculate change from baseline to new approach
    mutate(title = as.factor(title),
           title = paste0("S", as.numeric(title))) |>
    pivot_wider(names_from = title,
                values_from = mysd) |>
    rename (base = S1, new = S2) |>
    mutate(diff_sd = sqrt(base*base + new*new))


plot_data1 <-
  data1 |>
  select(title, metric2, score, value_bin) |>
  left_join(data2) |>
  left_join(data3) |>
  left_join(data4) |>
    #--make a label of % change and assign color
    mutate(dabs = round(abs(diff_pct), -1),
           dabs_lab = ifelse(diff_pct < 0,
                             paste0("-", dabs, "%"),
                          paste0("+", dabs, "%")),
           dabs_lab_color = ifelse(diff_pct < 0,
                                   "basewins",
                                   "stratwins"))

  #--experimenting
  #--be cool to have size relative to certainty?

   plot_data1 |>
     mutate(dummy = 1,
            metricF = factor(metric2, levels = metric_names_nice),
            metric2 = fct_rev(metricF)) |>
     ggplot(aes(x = metric2,
                y = dummy)) +
     geom_point(
       aes(fill = dabs_lab_color,
           size = mysd),
       pch = 21) +
     geom_text(aes(y = dummy*.5,
                   label = dabs_lab,
                   ),
                show.legend = F) +
     scale_size_continuous(range = c(2, 11),
                           breaks = c(2, 5, 8, 11),
                           labels = c("Low",
                                      "Medium",
                                      "High",
                                      "Very high"),
                           guide = "legend") +
     scale_y_continuous(limits = c(0, 1.5)) +
     scale_fill_manual(values = c(baseline_color, strategy_color)) +
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
