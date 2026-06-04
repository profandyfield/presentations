library("ggdist")


# mean summaries ----------------------------------------------------------

know_sum <- imp_data_0 |>
  filter(scenario == "s1") |> 
  select(id, contains("mean")) |> 
  pivot_longer(-id, names_to = "assumption", values_to = "know") |> 
  group_by(assumption) |> 
  summarise(
    mean = mean(know, na.rm = T), 
    trimmed = mean(know, na.rm = T, trim = 0.2), 
    sd = sd(know, na.rm = T), 
    min = min(know, na.rm = T),
    max = max(know, na.rm = T) 
    #n_missing = sum(is.na(know)), 
    #percent_missing = (n_missing / length(know))*100
  ) |> 
  mutate(assumption = c("Knowledge: Homoscedasticity", "Knowledge: Independence", 
                        "Knowledge: Linearity", "Knowledge: Normality", "Knowledge: Outliers", "Knowledge: Overall"), 
         across(
           where(is.numeric), ~round(.x, 2)
         ))


# individual statements  --------------------------------------------------

tf_full <- imp_data_0 |>
  filter(scenario == "s1") |>
  select(contains("tf")) |>
  select(-contains("mean")) |>
  pivot_longer(cols = everything(), names_to = "statement", values_to = "score") |>
  separate(statement, into = c("statement", "statement_id"), sep = "_(?=[^_]+$)") |> 
  left_join(x = _, y = tf_ids, by = "statement_id") |> 
  transmute(
    statement = new_statement, 
    tf = case_when(new_correct == 1 ~ TRUE, new_correct == -1 ~ FALSE), 
    mean = case_when(
      tf == FALSE ~ reverse_score_knowledge(as.double(score)), 
      TRUE ~ as.double(score)
    ), 
    assumption = assumption
  )|> 
  mutate(
    statement = str_replace_all(statement, "Cook’s", "Cook's")
  ) |>
  group_by(assumption) |> 
  group_split()

colour_scale = "mako"

# heteroscedasticity ------------------------------------------------------

statement_width = 39

p_het_true <-  ggplot(tf_full[[1]] |> filter(tf == T), aes(y = statement, x = mean)) |>  
  plot_settings(x = _, "left", colour_scale, statement_width = statement_width) 

p_het_false <-  ggplot(tf_full[[1]] |> filter(tf == F), aes(y = statement, x = mean)) |> 
  plot_settings(x = _, "right", colour_scale, statement_width = statement_width)

# independence ------------------------------------------------------------

statement_width = 30

p_ind_true <-  ggplot(tf_full[[2]] |> filter(tf == T), aes(y = statement, x = mean)) |>  
  plot_settings(x = _, "left", colour_scale, statement_width = statement_width) 

p_ind_false <-  ggplot(tf_full[[2]] |> filter(tf == F), aes(y = statement, x = mean)) |> 
  plot_settings(x = _, "right", colour_scale, statement_width = statement_width)

# linearity ---------------------------------------------------------------

statement_width = 40

p_lin_true <-  ggplot(tf_full[[3]] |> filter(tf == T), aes(y = statement, x = mean)) |>  
  plot_settings(x = _, "left", colour_scale, statement_width = statement_width) 

p_lin_false <-  ggplot(tf_full[[3]] |> filter(tf == F), aes(y = statement, x = mean)) |> 
  plot_settings(x = _, "right", colour_scale, statement_width = statement_width)

# normality ---------------------------------------------------------------

statement_width = 42

p_norm_true <-  ggplot(tf_full[[5]] |> filter(tf == T), aes(y = statement, x = mean)) |>  
  plot_settings(x = _, "left", colour_scale, statement_width = statement_width) 

p_norm_false <-  ggplot(tf_full[[5]] |> filter(tf == F), aes(y = statement, x = mean)) |> 
  plot_settings(x = _, "right", colour_scale, statement_width = statement_width)

# outliers ----------------------------------------------------------------

statement_width = 42

p_out_true <-  ggplot(tf_full[[4]] |> filter(tf == T), aes(y = statement, x = mean)) |>  
  plot_settings(x = _, "left", colour_scale, statement_width = statement_width) 

p_out_false <-  ggplot(tf_full[[4]] |> filter(tf == F), aes(y = statement, x = mean)) |> 
  plot_settings(x = _, "right", colour_scale, statement_width = statement_width)


# --------------
# PRACTICE
# -------

# scenario ----------------------------------------------------------------

practice_overall_scenario_raw <- imp_data_0  |> 
   select(id, scenario, check_sum)  |> 
   filter(complete.cases(check_sum))  |> 
   group_by(check_sum, scenario)  |> 
   mutate(n = n()/1000/1.5)  |> 
   ungroup()  |> 
   mutate(scenario = factor(scenario, labels = c("Experimental", "Cross-sectional")))

p_practice_scenario <- 
   ggplot(data = practice_overall_scenario_raw, aes(x = scenario, y = check_sum, fill = scenario)) + 
  geom_boxplot(alpha = 0.75, linewidth = 0.3, width = 0.05) + 
  ggrain::geom_rain(rain.side = "f1x1", 
                    point.args = list(alpha = 00),
                    violin.args = list(size = 0.3, alpha = 0.75),
                    boxplot.args = list(width = 0, linewidth = 0, alpha = 0, colour = "white"),
                    alpha = 0.75) + 
  geom_point(
    data = practice_mod_overall_list$means_scenario,
    aes(x = scenario, y = Estimate),
    position = position_nudge(x = c(0.12, -0.12)),
    size = 2) +
  geom_errorbar(
    inherit.aes = FALSE,
    data = practice_mod_overall_list$means_scenario,
    aes(
      x = scenario,
      ymin = lower,
      ymax = upper
    ),
    position = position_nudge(x = c(0.12, -0.12)),
    width = 0.05,
    size = .5,
  ) +
  scale_fill_viridis_d(option = "mako", direction = -1, begin = 0.3, end =0.6) + 
  scale_colour_viridis_d(option = "mako", direction = -1, begin = 0.3, end =0.6) + 
  labs(x = "\n Scenario\n", y = "Self-reported practice (0-10) \n") + 
  coord_cartesian(ylim = c(0, 10)) + 
  scale_y_continuous(breaks = seq(0, 10, 1)) + 
  theme_light() + 
  theme(
    text = element_text(family = "serif"),
    legend.position = "none", 
    panel.grid.major = element_line(colour = "#ededed"), 
    panel.grid.minor = element_blank()
  )

# knowledge ---------------------------------------------------------------

practice_overall_knowledge_raw <- imp_data_0 |> 
   select(scenario, ri_teach, ri_subfield, uni_rank_cat, tf_overall_mean, check_sum) |> 
   filter(complete.cases(tf_overall_mean), complete.cases(check_sum))
# filter(uni_rank_cat == 1, scenario == "s1", ri_teach == "No", ri_subfield == "Methods")

p_practice_knowledge <- 
  practice_mod_overall_list$means_knowledge  |> 
   filter( between(tf_overall_mean, 4, 9))  |> 
   ggplot(data = _, aes(x = tf_overall_mean, y = estimate)) +
  #geom_rect(xmin = 0, xmax = 10, ymin = -5, ymax = -0.05, colour = NA, fill = "#ededed", alpha = 0.1) + 
  geom_point(inherit.aes = FALSE, 
             data = practice_overall_knowledge_raw, 
             aes(x = tf_overall_mean, y = check_sum), 
             alpha = 0.05, 
             colour = mako$m3,
             size = 1
  ) + 
  
  geom_ribbon(aes(ymin = lower_hpd, ymax = upper_hpd), 
              alpha = 0.75, colour = NA, linewidth = 0.05, fill = mako$m3) + 
  geom_path(colour = "black") + 
  labs(x = "\nOverall knowledge (0-10)", y = "\n") + 
  coord_cartesian(ylim = c(0,10), xlim = c(0, 10)) + 
  scale_x_continuous(breaks = seq(0, 10, 1)) + 
  scale_y_continuous(breaks = seq(0, 10, 1)) + 
  theme_minimal() + 
  theme(
    legend.position = "top",
    panel.grid.major = element_line(colour = "#ededed"), 
    panel.grid.minor = element_blank(),
    text = element_text(family = "serif")
  )

# ri_teach ----------------------------------------------------------------

practice_overall_ri_teach_raw <- imp_data_0  |> 
   select(id, ri_teach, check_sum)  |> 
   filter(!is.na(check_sum), !is.na(ri_teach))  |> 
   group_by(check_sum, ri_teach)  |> 
   mutate(n = n()/1000/1.5)

p_practice_ri_teach <-
   ggplot(data = practice_overall_ri_teach_raw, aes(x = ri_teach, y = check_sum, fill = ri_teach)) + 
  geom_boxplot(alpha = 0.75, linewidth = 0.3, width = 0.05) + 
  ggrain::geom_rain(rain.side = "f1x1", 
                    point.args = list(alpha = 00),
                    violin.args = list(size = 0.3, alpha = 0.75),
                    boxplot.args = list(width = 0, size = 0, alpha = 0, colour = "white"),
                    alpha = 0.75) + 
  geom_point(
    data = practice_mod_overall_list$means_ri_teach, 
    aes(x = ri_teach, y = Estimate), 
    position = position_nudge(x = c(0.12, -0.12)),
    size = 2) +
  geom_errorbar(inherit.aes = FALSE,
                data = practice_mod_overall_list$means_ri_teach,
                aes(
                  x = ri_teach,
                  ymin = lower, 
                  ymax = upper
                ),
                position = position_nudge(x = c(0.12, -0.12)),
                width = 0.05,
                linewidth = .5,
  ) +
  scale_fill_viridis_d(option = "mako", direction = -1, begin = 0.3, end =0.6) + 
  scale_colour_viridis_d(option = "mako", direction = -1, begin = 0.3, end =0.6) + 
  labs(x = "\n Research methods teaching\n", y = "Self-reported practice (0-10) \n") + 
  #coord_cartesian(ylim = c(0, 10), xlim = c(0.80,2.5)) + 
  scale_y_continuous(breaks = seq(0, 10, 1)) + 
  theme_light() + 
  theme(
    text = element_text(family = "serif"),
    legend.position = "none", 
    panel.grid.major = element_line(colour = "#ededed"), 
    panel.grid.minor = element_blank()
  )

# ri_subfield -------------------------------------------------------------

practice_overall_ri_subfield_raw <- imp_data_0  |> 
   select(id, ri_subfield, check_sum)  |> 
   filter(!is.na(check_sum), !is.na(ri_subfield))  |> 
   group_by(check_sum, ri_subfield)  |> 
   mutate(n = n()/100/3)  |> 
   ungroup()  |> 
   mutate(ri_subfield = factor(ri_subfield, labels = 
                                       c("Methods", "\nBiological", "Clinical", "\nCognitive",
                                         "Developmental", "\nSocial"))
  )


p_practice_ri_subfield <-
   ggplot(data = practice_overall_ri_subfield_raw, aes(x = ri_subfield, y = check_sum, fill = ri_subfield)) + 
  ggrain::geom_rain(
    point.args = list(alpha = 0), 
    violin.args = list(linewidth = 0.3, alpha = 0.75),
    boxplot.args = list(linewidth = 0.3, alpha = 0.75),
    boxplot.args.pos = rlang::list2(width = 0.05, position = position_nudge(x = 0.06)),
    alpha = 0.75) + 
  geom_point(
    data = practice_mod_overall_list$means_ri_subfield, 
    aes(x = ri_subfield, y = Estimate), 
    position = position_nudge(x = -0.06),
    size = 2) +
  geom_errorbar(inherit.aes = FALSE,
                data = practice_mod_overall_list$means_ri_subfield,
                position = position_nudge(x = -0.06),
                aes(
                  x = ri_subfield,
                  ymin = lower, 
                  ymax = upper
                ),
                width = 0.05,
                linewidth = .5,
  ) +
  scale_fill_viridis_d(option = "mako", direction = -1, begin = 0.3, end = 1) + 
  labs(x = "\n Psychology subfield", y = "\n") + 
  coord_cartesian(ylim = c(0, 10), xlim = c(1.25, 6.1)) + 
  scale_y_continuous(breaks = seq(0, 10, 1)) + 
  theme_light() + 
  theme(
    text = element_text(family = "serif"),
    legend.position = "none", 
    panel.grid.major = element_line(colour = "#ededed"), 
    panel.grid.minor = element_blank()
  )

## ORDINAL MODELS

panel_order <- c("Linearity", "Independence", "Homoscedasticity", 
                 "Normal \nerrors", "Normal \nsampling \ndistribution", 
                 "Outliers", "Influential \ncases")

practice_probs_scenario <-  rbind.data.frame(
  practice_mod_norm_sam_list$probs$probs_scenario, 
  practice_mod_norm_err_list$probs$probs_scenario, 
  practice_mod_out_list$probs$probs_scenario, 
  practice_mod_lin_list$probs$probs_scenario, 
  practice_mod_ind_list$probs$probs_scenario, 
  practice_mod_het_list$probs$probs_scenario, 
  practice_mod_inf_list$probs$probs_scenario
)  |> 
   ungroup()  |> 
   mutate(
    assumption = case_when(
      assumption == "Normal sampling distribution" ~ "Normal \nsampling \ndistribution", 
      assumption == "Normal errors" ~ "Normal \nerrors", 
      assumption == "Influential cases" ~ "Influential \ncases", 
      TRUE ~ assumption
    ), 
    assumption = factor(assumption, levels = panel_order), 
    scenario = factor(scenario, labels = c("Experimental", "Cross-sectional")),
    .category = factor(.category, labels = c("1", "2", "3")), 
    #across(where(is.numeric), to_percent),
    across(where(is.numeric), ~round(.x, 2))
  )  |>  arrange(assumption)

dodge_width = 0.3


practice_plot_scenario <-  ggplot(data = practice_probs_scenario, 
                                          aes(x = .category, y = estimate, colour = scenario)) + 
  #geom_path(position = position_dodge(width = dodge_width), alpha = 0.5) +
  geom_errorbar(aes(ymin = lower_hpd, ymax = upper_hpd),
                width = 0.2, linewidth = .65, position = position_dodge(width = dodge_width)) + 
  geom_point(position = position_dodge(width = dodge_width)) +
  facet_wrap(~assumption, ncol = 7) + 
  labs(x = "\nPractice category: (1) Not attentive (2) Partially attentive (3) Fully attentive", y = "Estimated proportion \n", colour = "Scenario") + 
  coord_cartesian(ylim = c(0,1)) + 
  scale_y_continuous(breaks = seq(0, 1, 0.1)) + 
  scale_colour_manual(values = c(mako$m2, mako$m5)) + 
  #scale_colour_viridis_d(option = "mako", direction = -1, begin = 0.5, end = 1) + 
  theme_light() + 
  theme(
    legend.position = "top", 
    text = element_text(family = "serif"),
  )  + 
  guides(colour = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 1))

# knowledge ---------------------------------------------------------------

practice_probs_continuous <- 
  rbind.data.frame(
    practice_mod_norm_sam_list$probs$probs_tf_norm_sam_mean, 
    practice_mod_norm_err_list$probs$probs_tf_norm_err_mean,
    practice_mod_het_list$probs$probs_tf_het_mean,
    practice_mod_out_list$probs$probs_tf_out_mean,
    practice_mod_inf_list$probs$probs_tf_inf_mean,
    practice_mod_lin_list$probs$probs_tf_lin_mean,
    practice_mod_ind_list$probs$probs_tf_ind_mean
  )  |>  ungroup()  |> 
   mutate(
    assumption = case_when(
      assumption == "Normal sampling distribution" ~ "Normal \nsampling \ndistribution", 
      assumption == "Normal errors" ~ "Normal \nerrors", 
      assumption == "Influential cases" ~ "Influential \ncases", 
      TRUE ~ assumption
    ), 
    assumption = factor(assumption, levels = panel_order),
    .category = factor(.category, labels = c("1", "2", "3")), 
    continuous_predictor = continuous_predictor*100,
    #across(where(is.numeric), to_percent),
    across(where(is.numeric), ~round(.x, 2)), 
    continuous_predictor = continuous_predictor / 100
  )

practice_plot_knowledge <- practice_probs_continuous  |> 
   mutate(.category = factor(.category, labels = c("(1) Not attentive", "(2) Partially attentive", "(3) Fully attentive")))  |> 
   ggplot(data = _, aes(x = continuous_predictor, y = estimate, colour = .category, group = .category, fill = .category)) + 
  geom_ribbon(aes(ymin = lower_hpd, ymax = upper_hpd), alpha = 0.2, colour = NA) + 
  geom_path() + 
  labs(x = "\nKnowledge of assumption (0-10)", y = "Estimated proportion \n", colour = "Practice category", fill = "Practice category") + 
  scale_x_continuous(breaks = seq(0, 10, 2)) + 
  scale_y_continuous(breaks = seq(0, 1, 0.1)) + 
  scale_colour_manual(values = c(mako$m5, mako$m3, mako$m2)) + 
  scale_fill_manual(values = c(mako$m5, mako$m3, mako$m2)) + 
  facet_wrap(~assumption, ncol = 7) + 
  theme_light() + 
  theme(
    legend.position = "top",
    panel.grid.major = element_line(colour = "#ededed"), 
    panel.grid.minor = element_blank(),
    text = element_text(family = "serif")
  ) + 
  guides(colour = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 1))

# ri_teach ----------------------------------------------------------------

practice_probs_ri_teach <-  rbind.data.frame(
  practice_mod_norm_sam_list$probs$probs_ri_teach, 
  practice_mod_norm_err_list$probs$probs_ri_teach, 
  practice_mod_out_list$probs$probs_ri_teach, 
  practice_mod_lin_list$probs$probs_ri_teach, 
  practice_mod_ind_list$probs$probs_ri_teach, 
  practice_mod_het_list$probs$probs_ri_teach, 
  practice_mod_inf_list$probs$probs_ri_teach
  
)  |>  ungroup()  |> 
   mutate(
    assumption = case_when(
      assumption == "Normal sampling distribution" ~ "Normal \nsampling \ndistribution", 
      assumption == "Normal errors" ~ "Normal \nerrors", 
      assumption == "Influential cases" ~ "Influential \ncases", 
      TRUE ~ assumption
    ), 
    assumption = factor(assumption, levels = panel_order),
    ri_teach = factor(ri_teach, labels = c("Yes", "No")),
    .category = factor(.category, labels = c("1", "2", "3")), 
    # across(where(is.numeric), to_percent),
    across(where(is.numeric), ~round(.x, 2))
  )  |>  arrange(assumption)

dodge_width = 0.3

practice_plot_teach <-  ggplot(data = practice_probs_ri_teach, aes(x = .category, y = estimate, colour = ri_teach)) + 
  #geom_path(position = position_dodge(width = dodge_width), alpha = 0.5) +
  geom_errorbar(aes(ymin = lower_hpd, ymax = upper_hpd),
                width = 0.2, linewidth = .65, position = position_dodge(width = 0.3)) + 
  geom_point(position = position_dodge(width = 0.3)) +
  facet_wrap(~assumption, ncol = 7) + 
  labs(x = "\nPractice category: (1) Not attentive (2) Partially attentive (3) Fully attentive", y = "Estimated proportion\n", colour = "Research methods teaching") + 
  coord_cartesian(ylim = c(0,1)) + 
  scale_y_continuous(breaks = seq(0, 1, 0.1)) + 
  #scale_colour_manual(values = c(mon$m5, mon$m9)) + 
  scale_colour_manual(values = c(mako$m2, mako$m5)) + 
  theme_light() + 
  theme(
    legend.position = "top", 
    text = element_text(family = "serif"),
  )  + 
  guides(colour = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 1))

# psych_subfield ----------------------------------------------------------

practice_probs_ri_subfield <-  rbind.data.frame(
  practice_mod_norm_sam_list$probs$probs_ri_subfield, 
  practice_mod_norm_err_list$probs$probs_ri_subfield, 
  practice_mod_out_list$probs$probs_ri_subfield, 
  practice_mod_lin_list$probs$probs_ri_subfield, 
  practice_mod_ind_list$probs$probs_ri_subfield, 
  practice_mod_het_list$probs$probs_ri_subfield, 
  practice_mod_inf_list$probs$probs_ri_subfield
  
)  |>  ungroup()  |> 
   mutate(
    assumption = case_when(
      assumption == "Normal sampling distribution" ~ "Normal \nsampling \ndistribution", 
      assumption == "Normal errors" ~ "Normal \nerrors", 
      assumption == "Influential cases" ~ "Influential \ncases", 
      TRUE ~ assumption
    ), 
    assumption = factor(assumption, levels = panel_order),
    ri_subfield = factor(ri_subfield)  |> forcats::fct_relevel("Methods", after = 0L),
    .category = factor(.category, labels = c("1", "2", "3")), 
    #  across(where(is.numeric), to_percent),
    across(where(is.numeric), ~round(.x, 2))
  )  |>  arrange(assumption, ri_subfield)

dodge_width = 0.8
practice_plot_subfield <-  ggplot(data = practice_probs_ri_subfield, aes(x = .category, y = estimate, colour = ri_subfield)) + 
  #geom_path(position = position_dodge(width = dodge_width), alpha = 0.5) +
  geom_errorbar(aes(ymin = lower_hpd, ymax = upper_hpd),
                width = 0.2, linewidth = .65, position = position_dodge(width = dodge_width)) + 
  geom_point(position = position_dodge(width = dodge_width)) +
  facet_wrap(~assumption, ncol = 7) + 
  labs(x = "\nPractice category: (1) Not attentive (2) Partially attentive (3) Fully attentive", y = "Estimated proportion\n", y = "Estimated proportion\n", colour = "Psychology subfield") + 
  coord_cartesian(ylim = c(0,1)) + 
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +  
  # scale_colour_manual(values = c(mon$m9, mon$m8, mon$m7, mon$m6, mon$m5, mon$m4)) + 
  scale_colour_viridis_d(option = "mako", direction = -1, end = 0.9) +
  theme_light() + 
  theme(
    legend.position = "top", 
    text = element_text(family = "serif"),
  ) + 
  guides(colour = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 1))

