# needs tidyverse and patchwork to run

turbo_col = turbo_10[4] # set global colour

bp$metrics_list <- purrr::map(bp$metrics_list, dplyr::ungroup)

plot_select <- function(metric_df, label, colname, cutoff_lower = NA, cutoff_upper = NA){
  
  if(is.na(cutoff_lower)){cutoff_lower <- min(metric_df[[colname]], na.rm = TRUE)}
  else(cutoff_lower = cutoff_lower)
  
  if(is.na(cutoff_upper)){cutoff_upper <- max(metric_df[[colname]], na.rm = TRUE)}
  else(cutoff_upper = cutoff_upper)
  
  metric_df |> dplyr::transmute(
    metric = label, 
    value = !!sym(colname), 
    cutoff_lower = cutoff_lower, cutoff_upper = cutoff_upper)
}

bp$metrics_list$qli <- bp$metrics_list$qli |> 
  dplyr::mutate(abs_quantreg_x  = abs(quantreg_x))

qli_x2_pos <- bp$metrics_list$qli |> dplyr::filter(quantreg_x2_sign == "pos")
qli_x2_neg <- bp$metrics_list$qli |> dplyr::filter(quantreg_x2_sign == "neg")

all_metrics_df <- rbind(
  
  bp$metrics_list$es |> plot_select("Effect size", "abs_effect_size"), 
  bp$metrics_list$n_bw |> plot_select("N (between-subjects)", "n_total", cutoff_upper = 2000), 
  bp$metrics_list$n_rm |> plot_select("N (within/mixed-designs)", "n_rm"), 
  bp$metrics_list$n_ratio_bw |> plot_select("N ratio (between-subjects)", "n_ratio", cutoff_upper = 5), 
  bp$metrics_list$n_ratio_bw_rm |> plot_select("N ratio (mixed designs)", "n_ratio_bw_rm", cutoff_upper = 5), 
  bp$metrics_list$pn |> plot_select("p/n ratio", "pn_ratio"), 
  
  bp$metrics_list$mode |> plot_select("Number of modes", "n_modes"), 
  bp$metrics_list$skew |> plot_select("Skewness", "abs_skewness"), 
  bp$metrics_list$kurt |> plot_select("Excess kurtosis", "excess_kurtosis", cutoff_upper = 10), 
  
  bp$metrics_list$vr |> plot_select("Variance ratio", "vr", cutoff_upper = 40), 
  bp$metrics_list$qli |> plot_select("Linear QLI trend", "abs_quantreg_x", cutoff_upper = 4),
  qli_x2_pos |> plot_select("Quadratic QLI trend (+)", "quantreg_x2", cutoff_upper = 4), 
  qli_x2_neg |> plot_select("Quadratic QLI trend (-)", "quantreg_x2", cutoff_lower = -4), 
  bp$metrics_list$qli |> plot_select("Cubic QLI trend", "quantreg_x3", cutoff_lower = -10, cutoff_upper = 10),
  bp$metrics_list$qli |> plot_select("Quaritc QLI trend", "quantreg_x4", cutoff_lower = -20, cutoff_upper = 20),
  
  bp$metrics_list$outliers_1.96 |> plot_select("Cases above Z = 1.96", "resid_prop_1.96"),
  bp$metrics_list$outliers_2.58 |> plot_select("Cases above Z = 2.58", "resid_prop_2.58"),
  bp$metrics_list$outliers_3.29 |> plot_select("Cases above Z = 3.29", "resid_prop_3.29")
  
) |> 
  dplyr::mutate(
    metric = factor(metric, levels = unique(metric)
    )
  ) 

all_metrics_list <- all_metrics_df |> dplyr::group_by(metric) |>  dplyr::group_split()

all_plots <- purrr::map(
  .x = all_metrics_list, 
  .f = \(x) {
    x |> 
      ggplot2::ggplot(aes(x = value)) + 
      geom_density(colour = turbo_col, fill = turbo_col, alpha = 0.3) + 
      labs(y = "", x = "") + 
      ggtitle(label = x$metric[1]) + 
      coord_cartesian(xlim = c(x$cutoff_lower[1], x$cutoff_upper[1])) + 
      theme_minimal(base_family = "Times", base_size = 9) + 
      theme(
        plot.title = element_text(size = 8, hjust = 0.5)
      )
  }
)

# tweaking modes (poisson) to show bars instead of smooth density

all_plots[[7]] <- all_plots[[7]]$data |> 
  ggplot2::ggplot(aes(x = value)) + 
  stat_function(fun = dpois, n = 8, args = list(lambda =  0.45), 
                geom = "point", size = 0.75, col = turbo_col) +
  stat_function(fun = dpois, n = 8, args = list(lambda =  0.45), 
                geom = "line", col = turbo_col) + 
  stat_function(fun = dpois, n = 8, args = list(lambda =  0.45), 
                geom = "area", fill = turbo_col, alpha = 0.3) +
  scale_x_continuous(breaks = seq(1, 8, 1)) + 
  labs(y = "", x = "") + 
  ggtitle(label = "Number of modes") + 
  coord_cartesian(xlim = c(1, 8)) + 
  theme_minimal(base_family = "Times", base_size = 9) + 
  theme(
    plot.title = element_text(size = 8, hjust = 0.5)
  )

y_label = ggplot2::ggplot() + 
  labs(y = "Density") + 
  theme_minimal(base_family = "Times", base_size = 9)


uni_dist_gg <- y_label + ((all_plots[[7]] | all_plots[[8]] | all_plots[[9]])) +
  plot_layout(widths = c(0, 6))

uni_hetro_gg <- y_label + ((all_plots[[10]] | all_plots[[11]] | all_plots[[12]]) / 
                             (all_plots[[13]] | all_plots[[14]] | all_plots[[15]])) +
  plot_layout(widths = c(0, 6))

uni_z_gg <- y_label + ((all_plots[[16]] | all_plots[[17]] | all_plots[[18]])) +
  plot_layout(widths = c(0, 6))



## ----TABLE 5.5

quantiles_sum <- purrr::map(
  .x = bp$quantiles_list, 
  .f = \(x) {
    x |> 
      tibble::rowid_to_column(var = "quantile") |> 
      dplyr::filter(quantile %in% c(2, 7)) |> 
      dplyr::mutate(quantile = ifelse(quantile == 2, "q_10", "q_90")) |> 
      tidyr::pivot_wider(id_cols = metric, values_from = value, names_from = quantile)
  }
) |> 
  purrr::reduce(rbind) |> 
  dplyr::mutate(
    category = ifelse(
      metric %in% c("n_bw", "n_rm", "n_ratio_bw_rm", "n_ratio_bw", "pn", "effect_size"), 
      "General metrics", "Residual metrics"), 
    metric_label = c(
      "N (between-subjects designs)", 
      "N (within/mixed-designs)", 
      "N ratio (mixed designs)",
      "N ratio (between-subjects designs)", 
      "p/n ratio", 
      "Effect size", 
      "Skewness", 
      "Excess kurtosis", 
      "Number of modes", 
      "Z > 1.96", 
      "Z > 2.58",
      "Z > 3.29",
      "Variance ratio", 
      "Linear QLI", 
      "Quadratic QLI (pos)", 
      "Quadratic QLI (neg)", 
      "Cubic QLI", 
      "Quartic QLI"
    ), 
    metric = dplyr::case_when(
      .default = metric,
      metric == "qh_x3_neg" ~ "qh_x3", 
      metric == "qh_x4_neg" ~ "qh_x4"
    )
  )

estimates_sum <- 
  bp$estimates_list |> 
  purrr::reduce(rbind) |> 
  dplyr::mutate(metric = names(bp$estimates_list)) |> 
  dplyr::mutate(
    metric = stringr::str_remove_all(metric, "estimates_metrics_") |> 
      stringr::str_remove_all("brm_metrics_") |> 
      stringr::str_remove_all("simple_") |> 
      stringr::str_replace_all("outliers", "z") |> 
      stringr::str_replace_all("quantreg", "qh"),
    metric =  
      dplyr::case_when(
        .default = metric,
        metric == "n_ratio" ~ "n_ratio_bw", 
        metric == "skew" ~ "abs_skewness", 
        metric == "excess_kurt_simple" ~ "excess_kurtosis", 
        metric == "effect_sizes" ~ "effect_size"
      )
  )

eq_join <- dplyr::full_join(
  quantiles_sum, estimates_sum, by = "metric"
) |> 
  dplyr::mutate(
    category = ifelse(metric %in% c("z_1.96_zi", "z_2.58_zi", "z_3.29_zi"), "Residual metrics", category), 
    metric_label = ifelse(
      metric %in% c("z_1.96_zi", "z_2.58_zi", "z_3.29_zi"), 
      c("Z > 1.96 (zero inflation)", 
        "Z > 2.58 (zero inflation)", 
        "Z > 3.29 (zero inflation)"), 
      metric_label)
  ) |> 
  dplyr::arrange(category, metric) |> 
  dplyr::mutate(
    order = c(
      c(0, 1, 3, 4, 2, 6), 
      c(8,9,7), 
      c(11, 11.5, 11, 11.5, 11.5), 
      10, 
      rep(12, 6)
    )
  ) |> 
  dplyr::arrange(order) |> 
  dplyr::select(
    category,
    metric,
    metric_label, 
    q_10,
    lower_hpd, 
    estimate, 
    upper_hpd,
    q_90
  ) |> 
  dplyr::mutate(
    dplyr::across(
      .cols = where(is.numeric),
      .fns = \(x) round(x, 3)
      #.fns = \(x) ifelse(is.na(x), "", format(x, digits = 2, nsmall = 3))
    )
  )

eq_format <- eq_join |> 
  dplyr::mutate(
    dplyr::across(
      .cols = where(is.numeric),
      .fns = \(x) ifelse(is.na(x), "", format(x, digits = 3, nsmall = 3))
    )
  ) |> 
  dplyr::filter(category == "Residual metrics") |> 
  dplyr::select(-c(metric, category))
  

eq_col_names <- c(
  "Metric", 
  "$Q_{10}$", 
  #"2.5\\% HPD", 
  "$2.5\\% \\atop \\text{HPD}$",
  "$\\hat\\beta_0$", 
  #"97.5\\% HPD",
  "$97.5\\% \\atop \\text{HPD}$",
  "$Q_{90}$")

uni_dist_tbl <- eq_format[1:3, ] |>
  knitr::kable(col.names = eq_col_names)
  
uni_hetro_tbl <- eq_format[4:9, ] |>
  knitr::kable(col.names = eq_col_names)

uni_z_tbl <- eq_format[10:15, ] |>
  knitr::kable(col.names = eq_col_names)


