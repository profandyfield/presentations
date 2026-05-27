

#### helper functions for data cleaning ####

# Extract numeric info (returns NA if no numeric in the string)

extract_num <- function(vector){
  as.numeric(gsub("[^0-9.\\-]+", "", as.character(vector)))
}

# Reverse score (for vars where 0 is part of the scale, else you need to add 1 to max dcore)

reverse_score_knowledge <- function(var){
  return((var - (10)) |> abs())
}



#### formatting functions ####

output_lm <- function(mod){
  
  mod_summary <- summary(mod)
  ci <- confint(mod) |> as.data.frame()
  
  resid_se = mod_summary$sigma
  df = mod_summary$df
  r2 = mod_summary$r.squared |> round(digits = 2)
  adj_r2 = mod_summary$adj.r.squared
  f_test = mod_summary$fstatistic |> round(digits = 3)
  p = pf(f_test[1], f_test[2], f_test[3], lower.tail = F) |> round(digits = 3)
  attributes(p) <- NULL
  
  note_1 = paste0(" ", " The model explains ", r2*100, "% of variance.")
  note_2 = paste0("\n F(", f_test[2], ",", f_test[3], ") = ", f_test[1], ")", ", p = ", p)
  
  coefs = mod_summary$coefficients[ ,c(1,2)] |> as.data.frame() |> 
    bind_cols( ., ci) |>
    knitr::kable(digits = 3) |>
    kableExtra::kable_styling() |>
    kableExtra::add_footnote(
      paste(note_1, note_2)
    )
  
  return(coefs)
  
}

#### z scores and outliers #####

z <- function(x){(x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)}

z_percent <- function(x, z){(which(abs(x) >= z) |> length()) / length(!is.na(x)) * 100}

load_z_table <- function(){readr::read_csv("../data/z_table_long.csv")}

# z_table <- readr::read_csv("../data/z_table.csv") |> 
#   filter(!is.na(Z)) |> 
#   pivot_longer(., names_to = "dec", -Z) |> 
#   transmute(
#     z = Z - as.numeric(dec), 
#     prob = value
#   ) |> 
#   arrange(prob)

#write.csv(z_table, "../data/z_table_long.csv", row.names = FALSE)

#### working with models ####

get_coef <- function(model, label, paper = "Author Name, YEAR") {
  summary(model)$coefficients |> 
    tibble::as_tibble(., rownames = "coefficient") |> 
    transmute(
      paper = paper, 
      model_type = label,
      coefficient = coefficient,
      b = round(Estimate, digits = 5),
      se = `Std. Error`, 
      t = `t value`,
      p = `Pr(>|t|)`, 
    )
}


#### imputation ####

ordered_factor <- function(x){factor(x, ordered = TRUE, levels = c(1,2,3,4,5))}

index <- function(pred_matrix_df, var_name){
  index = which(colnames(pred_matrix_df) == var_name)
  return(index)
}

batch_adjust_matrix <- function(predictor_matrix_df, 
                                dplyr_select_query_row, 
                                dplyr_select_query_col,
                                assign = 0){
  
  # process rows
  selected_var_names_list_row = NULL
  for(i in 1:length(dplyr_select_query_row)){
    
    selected_var_names_list_row_i <- predictor_matrix_df |> 
      select(eval(parse(text = dplyr_select_query_row[[i]]))) |> 
      names()
    
    selected_var_names_list_row[[i]] = selected_var_names_list_row_i
  }
  selected_var_names_row <- unlist(selected_var_names_list_row)
  selected_var_names_row <- selected_var_names_row[!duplicated(selected_var_names_row)]
  
  # process col
  selected_var_names_list_col = NULL
  for(i in 1:length(dplyr_select_query_col)){
    
    selected_var_names_list_col_i <- predictor_matrix_df |> 
      select(eval(parse(text = dplyr_select_query_col[[i]]))) |> 
      names()
    
    selected_var_names_list_col[[i]] = selected_var_names_list_col_i
  }
  selected_var_names_col <- unlist(selected_var_names_list_col)
  selected_var_names_col <- selected_var_names_col[!duplicated(selected_var_names_col)]
  
  new_predictor_matrix <- predictor_matrix_df
  new_predictor_matrix[selected_var_names_row, selected_var_names_col] <- assign
  
  return(new_predictor_matrix)
  
}

#### brms model summaries and plots ####

lazy_kable <- function(x){
  x |> 
    knitr::kable(digits = 2) |>
    kableExtra::kable_classic(html_font = "Times", full_width = F)
}



generate_summary_objects <- function(knowledge_mod, assumption){
  
  knowledge_mod_sum <- summary(knowledge_mod)
  
  knowledge_mod_df <- as.data.frame(knowledge_mod)
  
  knowledge_mod_bs <- knowledge_mod_sum$fixed[, c("Estimate", "l-95% CI", "u-95% CI")] |> 
    as.data.frame() |>
    select(!contains("95%"))  |> 
    tibble::rownames_to_column("term")
  
  knowledge_mod_hpd <- 
    HDInterval::hdi(knowledge_mod_df, .95) |> 
    t() |> as.data.frame() |>
    tibble::rownames_to_column("term") |>
    mutate(term = stringr::str_remove_all(string = term, pattern = "b_"))
  
  knowledge_mod_bs <- 
    left_join(knowledge_mod_bs, knowledge_mod_hpd, by = "term") |>
    mutate(assumption = assumption)
  
  return(
    list(sum = knowledge_mod_sum, 
         #hpd = knowledge_mod_df,
         b = knowledge_mod_bs)
  )
  
}



estimated_means <- function(summary_object_bs, predictor_string, levels, assumption){
  
  summary_object_bs |>
    filter(stringr::str_detect(eval(term), pattern = (paste0("Intercept|", predictor_string)))) |>
    mutate(
      Estimate = if_else(term != "Intercept", Estimate + Estimate[1], Estimate), 
      lower = if_else(term != "Intercept", lower + Estimate[1], lower), 
      upper = if_else(term != "Intercept", upper + Estimate[1], upper), 
      term = levels, 
      assumption = assumption
    )
}


lower_hpd <- function(x){HDInterval::hdi(x)[1]}
upper_hpd <- function(x){HDInterval::hdi(x)[2]}



c_probs <- function(x) {
  
  x |> 
    summarise(
      
      estimate = case_when(
        .category == 0 ~ median(.epred),
        .category == 1 ~ median(1 - .epred)
      )[1],
      
      lower_hpd = case_when(
        .category == 0 ~ lower_hpd(.epred),
        .category == 1 ~ lower_hpd(1 - .epred)
      )[1],
      
      upper_hpd = case_when(
        .category == 0 ~ upper_hpd(.epred),
        .category == 1 ~ upper_hpd(1 - .epred)
      )[1]
    )
}



conditional_c_probs <- function(ordinal_practice_mod, original_data, continuous_predictor, assumption_label){
  
  # ordinal_practice_mod = practice_mod_het
  # original_data = imp_data_0
  # continuous_predictor = "tf_het_mean"
  
  predictor_mean = mean(unlist(original_data[continuous_predictor]), na.rm = T)
  
  newdata <- expand_grid(scenario = c("s1", "s2"),
                                continuous_predictor = c(predictor_mean),
                                ri_teach = c("Yes", "No"), 
                                ri_subfield = c("Methods", "Biological", "Clinical", "Cognitive", "Developmental", "Social"), 
                                uni_rank_cat = c(1, 2, 3, 4), 
                                id = as.logical(NA))
  
  names(newdata)[2] <- continuous_predictor
  
  epreds <- tibble::tibble(newdata) |> 
    tidybayes::add_epred_draws(ordinal_practice_mod, re_formula = NA)
  
  # c_probs <- function(x) {
  #   
  #   x |> 
  #     summarise(
  #       
  #       estimate = case_when(
  #         .category == 0 ~ median(.epred),
  #         .category == 1 ~ median(1 - .epred)
  #       )[1],
  #       
  #       lower_hpd = case_when(
  #         .category == 0 ~ lower_hpd(.epred),
  #         .category == 1 ~ lower_hpd(1 - .epred)
  #       )[1],
  #       
  #       upper_hpd = case_when(
  #         .category == 0 ~ upper_hpd(.epred),
  #         .category == 1 ~ upper_hpd(1 - .epred)
  #       )[1]
  #     )
  # }
  
  c_probs_scenario <- epreds |> 
    filter(ri_teach == "Yes", ri_subfield == "Methods", uni_rank_cat == 1, .category %in% c(0,1)) |> 
    group_by(scenario, .category) |> 
    c_probs(.) |> 
    mutate(assumption = assumption_label)
  
  c_probs_ri_teach <- epreds |> 
    filter(scenario == "s1", ri_subfield == "Methods", uni_rank_cat == 1, .category %in% c(0,1)) |> 
    group_by(ri_teach, .category) |> 
    c_probs(.) |> 
    mutate(assumption = assumption_label)
  
  c_probs_ri_subfield <- epreds |> 
    filter(scenario == "s1", ri_teach == "Yes", uni_rank_cat == 1, .category %in% c(0,1)) |> 
    group_by(ri_subfield, .category) |> 
    c_probs(.) |> 
    mutate(assumption = assumption_label)
  
  c_probs_uni_rank_cat <- epreds |> 
    filter(scenario == "s1", ri_teach == "Yes", ri_subfield == "Methods", .category %in% c(0,1)) |> 
    group_by(uni_rank_cat, .category) |> 
    c_probs(.) |> 
    mutate(assumption = assumption_label)
  
  conditional_c_probs <- list(
   # epreds = epreds, 
    c_probs_scenario = c_probs_scenario, 
    c_probs_ri_teach = c_probs_ri_teach, 
    c_probs_ri_subfield = c_probs_ri_subfield, 
    c_probs_uni_rank_cat = c_probs_uni_rank_cat
  )
  
  return(conditional_c_probs)
  
}



# assumption_label = "Homoscedastcity"
# continuous_predictor = "tf_het_mean"
# ordinal_practice_mod = practice_mod_het

conditional_c_probs_cont <- function(ordinal_practice_mod, continuous_predictor, assumption_label){
  
  newdata <- expand_grid(scenario = c("s1"),
                                continuous_predictor = c(0:10),
                                ri_teach = c("Yes"), 
                                ri_subfield = c("Methods"), 
                                uni_rank_cat = c(1), 
                                id = as.logical(NA))
  
  names(newdata)[2] <- continuous_predictor
  
  epreds <- tibble::tibble(newdata) |> 
    tidybayes::add_epred_draws(ordinal_practice_mod, re_formula = NA) 
  
  epreds %<>% 
    filter(.category != 2) |> 
    group_by(get(continuous_predictor), .category) |> 
    c_probs() |> 
    mutate(assumption = assumption_label)
  
  names(epreds)[1] =  "continuous_predictor"
  
  return(epreds)
  
}


conditional_c_probs_cont <- function(ordinal_practice_mod, continuous_predictor, assumption_label){
  
  newdata <- expand_grid(scenario = c("s1"),
                                continuous_predictor = c(0:10),
                                ri_teach = c("Yes"), 
                                ri_subfield = c("Methods"), 
                                uni_rank_cat = c(1), 
                                id = as.logical(NA))
  
  names(newdata)[2] <- continuous_predictor
  
  epreds <- tibble::tibble(newdata) |> 
    tidybayes::add_epred_draws(ordinal_practice_mod, re_formula = NA) 
  
  epreds %<>% 
    filter(.category != 2) |> 
    group_by(get(continuous_predictor), .category) |> 
    c_probs() |> 
    mutate(assumption = assumption_label)
  
  names(epreds)[1] =  "continuous_predictor"
  
  return(epreds)
  
}

conditional_probs <- function(ordinal_practice_mod, original_data, continuous_predictor, assumption_label){
  
  # ordinal_practice_mod = practice_mod_norm_sam
  # original_data = imp_data_0
  # continuous_predictor = "tf_norm_mean"
  # assumption_label = "Normal sampling distribution"
  
  predictor_mean = mean(unlist(original_data[continuous_predictor]), na.rm = T)
  
  newdata <- expand_grid(scenario = c("s1", "s2"),
                                continuous_predictor = c(predictor_mean),
                                ri_teach = c("Yes", "No"), 
                                ri_subfield = c("Methods", "Biological", "Clinical", "Cognitive", "Developmental", "Social"), 
                                uni_rank_cat = c(1, 2, 3, 4), 
                                id = as.logical(NA))
  
  names(newdata)[2] <- continuous_predictor
  
  epreds <- tibble::tibble(newdata) |> 
    tidybayes::add_epred_draws(ordinal_practice_mod, re_formula = NA)
  
  
  probs_scenario <- epreds |> 
    filter(ri_teach == "Yes", ri_subfield == "Methods", uni_rank_cat == 1) |> 
    group_by(scenario, .category) |> 
    summarise(estimate = median(.epred),
                     lower_hpd = lower_hpd(.epred), 
                     upper_hpd = upper_hpd(.epred)) |>
    mutate(assumption = assumption_label)
  
  probs_ri_teach <- epreds |> 
    filter(scenario == "s1", ri_subfield == "Methods", uni_rank_cat == 1) |> 
    group_by(ri_teach, .category) |> 
    summarise(estimate = median(.epred),
                     lower_hpd = lower_hpd(.epred), 
                     upper_hpd = upper_hpd(.epred)) |>
    mutate(assumption = assumption_label)
  
  probs_ri_subfield <- epreds |> 
    filter(scenario == "s1", ri_teach == "Yes", uni_rank_cat == 1) |> 
    group_by(ri_subfield, .category) |> 
    summarise(estimate = median(.epred),
                     lower_hpd = lower_hpd(.epred), 
                     upper_hpd = upper_hpd(.epred)) |>
    mutate(assumption = assumption_label)
  
  probs_uni_rank_cat <- epreds |> 
    filter(scenario == "s1", ri_teach == "Yes", ri_subfield == "Methods") |> 
    group_by(uni_rank_cat, .category) |> 
    summarise(estimate = median(.epred),
                     lower_hpd = lower_hpd(.epred), 
                     upper_hpd = upper_hpd(.epred)) |>
    mutate(assumption = assumption_label)
  
  conditional_probs <- list(
    probs_scenario = probs_scenario, 
    probs_ri_teach = probs_ri_teach, 
    probs_ri_subfield = probs_ri_subfield, 
    probs_uni_rank_cat = probs_uni_rank_cat
  )
  
  return(conditional_probs)
  
}


conditional_means_cont <- function(overall_practice_mod, continuous_predictor, assumption_label){
  
  newdata <- expand_grid(scenario = c("s1"),
                                continuous_predictor = c(0:10),
                                ri_teach = c("Yes"), 
                                ri_subfield = c("Methods"), 
                                uni_rank_cat = c(1), 
                                id = as.logical(NA))
  
  names(newdata)[2] <- continuous_predictor
  
  epreds <- tibble::tibble(newdata) |> 
    tidybayes::add_epred_draws(practice_mod_overall, re_formula = NA) 
  
  epreds %<>% 
    group_by(get(continuous_predictor)) |> 
    summarise(estimate = median(.epred), 
                     lower_hpd = lower_hpd(.epred), 
                     upper_hpd = upper_hpd(.epred)) |> 
    mutate(assumption = assumption_label)
  
  names(epreds)[1] =  continuous_predictor
  
  return(epreds)
  
}

tf_intercept_estimates <- function(tf_intercept_model){
  
  post = as.data.frame(tf_intercept_model)$b_Intercept
  
  post_df = data.frame(
    estimate = mean(post), 
    lower = lower_hpd(post), 
    upper = upper_hpd(post)
  )
  
  rownames(post_df) = deparse(substitute(tf_intercept_model))
  
  return(post_df)
}

tf_posterior_draws <- function(tf_intercept_model){
  
  data.frame(
    b = sample(as.data.frame(tf_intercept_model)$b_Intercept, size = 1000), 
    model = deparse(substitute(tf_intercept_model))
  )
  
}

conditional_probs_cont <- function(ordinal_practice_mod, continuous_predictor, assumption_label){
  
  newdata <- expand_grid(scenario = c("s1"),
                                continuous_predictor = c(0:10),
                                ri_teach = c("Yes"), 
                                ri_subfield = c("Methods"), 
                                uni_rank_cat = c(1), 
                                id = as.logical(NA))
  
  names(newdata)[2] <- continuous_predictor
  
  epreds <- tibble::tibble(newdata) |> 
    tidybayes::add_epred_draws(ordinal_practice_mod, re_formula = NA) 
  
  epreds %<>% 
    group_by(get(continuous_predictor), .category) |> 
    summarise(estimate = median(.epred),
                     lower_hpd = lower_hpd(.epred), 
                     upper_hpd = upper_hpd(.epred)) |>
    mutate(assumption = assumption_label)
  
  names(epreds)[1] =  "continuous_predictor"
  
  return(epreds)
  
}


#### colour pals ####

ryb <- data.frame(red4 = "#d73027", 
                  red3 = "#f46d43", 
                  red2 = "#fdae61", 
                  red1 = "#fee090",
                  yel0 = "#ffffbf", 
                  blu1 = "#e0f3f8", 
                  blu2 = "#abd9e9", 
                  blu3 = "#74add1", 
                  blu4 = "#4575b4")


mon <- data.frame(
  m1 = "#ffffe5", 
  m2 = "#fff7bc", 
  m3 = "#fee391", 
  m4 = "#fec44f", 
  m5 = "#fe9929", 
  m6 = "#ec7014", 
  m7 = "#cc4c02", 
  m8 = "#993404", 
  m9 = "#662506"
)

mako <- data.frame(
  m1 = "#a8e1bc", 
  m2 = "#40baac", 
  m3 = "#3587a5", 
  m4 = "#3c5396", 
  m5 = "#34264a", 
  m6 = "#0c0304"
)

#### plotting functions #### 

sd_min <- function(x){mean(x, na.rm = T) - sd(x, na.rm = T)}
sd_max <- function(x){mean(x, na.rm = T) + sd(x, na.rm = T)}

plot_settings <- function(x, side = "left", colour_scale = "mon", statement_width = 40){
  
  plot <- x + ggridges::geom_density_ridges_gradient(
    aes(x = mean, fill = stat(x)),
    scale = 0.5, colour = NA
  ) +
    
    stat_summary(geom = "errorbar", fun.min = "sd_min", fun.max = "sd_max", 
                 width = 0.1, size = 0.5) + 
    
    stat_summary(geom = "point", fun = "mean", 
                 size = 1.5) + 
    
    
    scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = statement_width), 
                     position = side) +
    scale_x_continuous(breaks = c(0, 10), 
                       labels = c("0 \nDefinitely\nFalse", "10\nDefinitely\nTrue")) + 
    labs(x = "", y = "") + 
    coord_cartesian(xlim = c(-0.5, 10.5)) +  
    
    
    facet_wrap(~tf) + 
    theme_light() + 
    theme(
      legend.position = "none", 
      strip.text = element_text(face = "bold"),
      text = element_text(family = "serif", size = 12),
      axis.text.x = element_text(hjust = c(0.1, 0.9)), 
    )
  
  if(colour_scale == "ryb"){
    
    if(side == "right"){
      plot + 
        scale_fill_gradient2(
          low = "#009e6c", mid = ryb$yel0, high = ryb$red4, midpoint = 5)
    } else {
      plot + 
        scale_fill_gradient2(
          low = ryb$red4, mid = ryb$yel0, high = "#009e6c" , midpoint = 5)
    }
  }
  
  if(colour_scale == "mako"){
    
    if(side == "right"){
      plot + 
        scale_fill_gradient2(
          low = mako$m5, mid = mako$m3, high = mako$m1, midpoint = 5)
    } else {
      plot + 
        scale_fill_gradient2(
          low = mako$m1, mid = mako$m3, high = mako$m5 , midpoint = 5)
    }
  }
  
  else if(colour_scale == "mon"){
    
    if(side == "right"){
      plot + 
        scale_fill_gradient2(
          low = mon$m9, mid = mon$m5, high = mon$m1, midpoint = 5)
    } else {
      plot + 
        scale_fill_gradient2(
          low = mon$m1, mid = mon$m5, high = mon$m9, midpoint = 5)
    }
  }
}

to_percent <- function(x) x*100

#### other ####

loop_progress <- function(){
  
  Sys.sleep(0.001)
  print(l)
  flush.console()
  
}

#___________________
# Helpers for Sladekova & Field (2026)
#-----------

lazy_kable <- function(x, full_width = F){
  x |> 
    knitr::kable() |> 
    kableExtra::kable_styling(bootstrap_options = "striped", full_width = full_width)
}

exp_c <- function(x) exp(x)/(1 + exp(x))

lower_hpd <- function(x){HDInterval::hdi(x)[1]}
upper_hpd <- function(x){HDInterval::hdi(x)[2]}

conditional_probs <- function(ordinal_practice_mod, assumption_label){
  
  newdata <- expand_grid(scenario = c("s1", "s2"),
                                demo_level = c("Faculty who teach methods", "Faculty", "Postdoctoral researcher", "PhD researcher"), 
                                uni_rank_cat = c(1, 2, 3, 4), 
                                id = as.logical(NA))
  
  epreds <- tibble::tibble(newdata) |> 
    tidybayes::add_epred_draws(ordinal_practice_mod, re_formula = NA)
  
  probs_scenario <- epreds |> 
    filter(demo_level == "Faculty who teach methods", uni_rank_cat == 1) |> 
    group_by(scenario, .category) |> 
    summarise(estimate = median(.epred) * 100,
                     lower_hpd = lower_hpd(.epred) * 100, 
                     upper_hpd = upper_hpd(.epred * 100)) |>
    mutate(assumption = assumption_label)
  
  probs_demo_level <- epreds |> 
    filter(scenario == "s1", uni_rank_cat == 1) |> 
    group_by(demo_level, .category) |> 
    summarise(estimate = median(.epred) * 100,
                     lower_hpd = lower_hpd(.epred) * 100, 
                     upper_hpd = upper_hpd(.epred)* 100) |>
    mutate(assumption = assumption_label)
  
  probs_uni_rank_cat <- epreds |> 
    filter(scenario == "s1", demo_level == "Faculty who teach methods") |> 
    group_by(uni_rank_cat, .category) |> 
    summarise(estimate = median(.epred)* 100,
                     lower_hpd = lower_hpd(.epred)* 100, 
                     upper_hpd = upper_hpd(.epred)* 100) |>
    mutate(assumption = assumption_label)
  
  conditional_probs <- list(
    probs_scenario = probs_scenario, 
    probs_demo_level = probs_demo_level, 
    probs_uni_rank_cat = probs_uni_rank_cat
  )
  
  return(conditional_probs)
}


conditional_probs_inf <- function(ordinal_practice_mod_s1, ordinal_practice_mod_s2, assumption_label){
  
  # assumption_label = "Influential cases"
  # ordinal_practice_mod_s1 = mod_inf_mi_s1
  # ordinal_practice_mod_s2 = mod_inf_mi_s2
  
  newdata <- expand_grid(demo_level = c("Faculty who teach methods", "Faculty", "Postdoctoral researcher", "PhD researcher"), 
                                uni_rank_cat = c(1, 2, 3, 4), 
                                id = as.logical(NA))
  
  epreds_s1 <- tibble::tibble(newdata) |> 
    tidybayes::add_epred_draws(ordinal_practice_mod_s1, re_formula = NA)
  
  epreds_s2 <- tibble::tibble(newdata) |> 
    tidybayes::add_epred_draws(ordinal_practice_mod_s2, re_formula = NA)
  
  probs_scenario_s1 <- epreds_s1 |> 
    filter(demo_level == "Faculty who teach methods", uni_rank_cat == 1) |> 
    group_by(.category) |> 
    summarise(estimate = median(.epred)* 100,
                     lower_hpd = lower_hpd(.epred)* 100, 
                     upper_hpd = upper_hpd(.epred)* 100) |>
    mutate(
      scenario = "s1",
      assumption = assumption_label)
  
  probs_scenario_s2 <- epreds_s2 |> 
    filter(demo_level == "Faculty who teach methods", uni_rank_cat == 1) |> 
    group_by(.category) |> 
    summarise(estimate = median(.epred)* 100,
                     lower_hpd = lower_hpd(.epred)* 100, 
                     upper_hpd = upper_hpd(.epred)* 100) |>
    mutate(
      scenario = "s2",
      assumption = assumption_label)
  
  probs_demo_level <- epreds_s1 |> 
    filter(uni_rank_cat == 1) |> 
    group_by(demo_level, .category) |> 
    summarise(estimate = median(.epred)* 100,
                     lower_hpd = lower_hpd(.epred)* 100, 
                     upper_hpd = upper_hpd(.epred)* 100) |>
    mutate(
      assumption = assumption_label)
  
  probs_uni_rank_cat_s1 <- epreds_s1 |> 
    filter(demo_level == "Faculty who teach methods") |> 
    group_by(uni_rank_cat, .category) |> 
    summarise(estimate = median(.epred)* 100,
                     lower_hpd = lower_hpd(.epred)* 100, 
                     upper_hpd = upper_hpd(.epred)* 100) |>
    mutate(assumption = assumption_label)
  
  conditional_probs <- list(
    probs_scenario = rbind(probs_scenario_s1, probs_scenario_s2), 
    probs_demo_level = probs_demo_level, 
    probs_uni_rank_cat = probs_uni_rank_cat
  )
  
}

process_method_sum <- function(method_df, scenario = scenario, method = method, assumption){
  
  method_df |>
    group_by(scenario, method) |>
    summarise(n = n()) |>
    group_by((scenario)) |>
    mutate(percent = ((n / sum(n)) * 100) |> round(2)) |>
    pivot_wider(
      id_cols = (method),
      names_from = (scenario),
      values_from = c(n, percent)
    ) |> 
    arrange((method)) |> 
    transmute(
      Assumption = c(assumption, rep("", times = nrow(.)-1)), 
      method = method,
      n_exp = `n_Experimental`, percent_exp = `percent_Experimental`, 
      n_cs = `n_Cross-sectional`, percent_cs = `percent_Cross-sectional`
    )
}

