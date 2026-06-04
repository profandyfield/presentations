

performance_summary_bw <- readxl::read_excel("../shared_data/simulation_design_planning.xlsx", sheet = 5) |> 
  dplyr::mutate(
    dplyr::across(
      .cols = everything(), .fns = ~ifelse(is.na(.x), "", .x)
    )
  ) |> 
  dplyr::select(-Design)

performance_summary_rm <- readxl::read_excel("../shared_data/simulation_design_planning.xlsx", sheet = 6) |> 
  dplyr::mutate(
    dplyr::across(
      .cols = everything(), .fns = ~ifelse(is.na(.x), "", .x)
    )
  ) |> 
  dplyr::select(-Design)
