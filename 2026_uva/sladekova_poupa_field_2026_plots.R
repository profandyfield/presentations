turbo_5 <- scales::viridis_pal(option = "turbo")(5)



raw_sum <- env$data |>
  select(id, scenario, cat_norm, cat_het, cat_out, cat_inf) |> 
  pivot_longer(
    cols = -c(scenario, id),
    values_to = "practice_cat", 
    names_to = "assumption"
  ) |> 
  group_by(scenario, assumption, practice_cat) |> 
  summarise(
    n = n()
  ) |> 
  filter(!is.na(practice_cat)) |> 
  group_by(scenario, assumption) |> 
  mutate(
    assumption = factor(assumption, 
                        levels = c("cat_norm", "cat_het", "cat_out", "cat_inf"), 
                        labels = c("Normality", "Homoscedasticity", "Basic checks and outliers", "Influential cases")),
    percent = round((n / sum(n)) * 100, 2)
  )

raw_plot <- raw_sum |> 
  ggplot(aes(y = scenario, fill = factor(practice_cat, levels = c(5,4,3,2,1)), x = percent)) + 
  geom_col(width = 0.75) + 
  facet_wrap(~assumption, nrow = 4) + 
  scale_fill_manual(values = (c(turbo_5[1], turbo_5[2], turbo_5[3], turbo_5[4], turbo_5[5]))) + 
  labs(x = "\n Percent (%)", y = "Scenario\n") + 
  theme_minimal() + 
  theme(
    legend.position = "none", 
    text = element_text(family = "Times")
  )


raw_plot_legend <- raw_sum |> 
  ggplot(aes(y = scenario, x = percent, 
                      fill = factor(practice_cat, 
                                    levels = c(1,2,3,4,5), 
                                    labels = c("(1) No checks performed", 
                                               "(2) Some, but insufficient checks", 
                                               "(3) Checks relying on arbitrary cut-offs or subjective judgement", 
                                               "(4) Checks from previous category followed by a corrective action", 
                                               "(5) Use of robust methods and/or sensitivity analyses")))) + 
  geom_col(width = 0.75) + 
  #facet_wrap(~assumption, nrow = 4) + 
  scale_fill_manual(values = rev(c(turbo_5[1], turbo_5[2], turbo_5[3], turbo_5[4], turbo_5[5]))) + 
  labs(fill = "Practice category", y = "", x = "") + 
  coord_cartesian(xlim = c(500, 1000)) + 
  theme_void() + 
  theme(
    text = element_text(family = "Times")
  )

analytic_practice_gg <- raw_plot + raw_plot_legend

