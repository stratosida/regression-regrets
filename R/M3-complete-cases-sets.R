# ME1: Patterns of missing values
# Patterns of missing values will be investigated by:
# computing a table of complete cases (for the three predictor sets described above) 
# for strata defined by the structural variables age and sex,



# Based on domain expertise, the predictors are grouped by their assumed importance to predict bacteremia. 
# (1) Variables with known strong associations with bacteremia are age (AGE), leukocytes (WBC), 
# blood urea neutrogen (BUN), creatinine (CREA), thrombocytes (PLT), and neutrophiles (NEU) and these predictors will be included in the model as key predictors. 
# (2) Predictors of medium importance are potassium (POTASS), and some acute-phase related parameters such as fibrinogen (FIB), C-reactive protein (CRP), aspartate transaminase (ASAT), alanine transaminase (ALAT), and gamma-glutamyl transpeptidase (GGT). 
# (3) All other predictors are of minor importance.

m3_analysis <- function(ADSL, ADLB){
  
  final_ard <- NULL
  
  ## join structural variables on
  ADLB <- ADLB |> left_join(ADSL |> select(USUBJID, SEXC, AGEGR01, AGEGR01C), by = "USUBJID")
  
  
  ## all predictors
  ard_all <- ADLB |> 
    #filter(KEY_PRED_FL01 == "Y") |>
    select(SUBJID, PARAMCD, AVAL, SEXC, AGEGR01, AGEGR01C) |>
    pivot_wider(names_from = PARAMCD, values_from = AVAL, values_fill = NA) |>
    select(-SUBJID) |>
    group_by(SEXC, AGEGR01, AGEGR01C) |>
    miss_case_table() |> 
    filter(n_miss_in_case == 0) |>
    mutate(Set = "All predictors", 
           row_order = 1) |>
    ungroup()
    
  ## key predictors
  ard_key <- ADLB |> 
    filter(KEY_PRED_FL01 == "Y") |>
    select(SUBJID, PARAMCD, AVAL, SEXC, AGEGR01, AGEGR01C) |>
    pivot_wider(names_from = PARAMCD, values_from = AVAL, values_fill = NA) |>
    select(-SUBJID) |>
    group_by(SEXC, AGEGR01, AGEGR01C) |>
    miss_case_table() |> 
    filter(n_miss_in_case == 0) |>
    mutate(Set = "Key predictors", 
           row_order = 2) |>
    ungroup()
  
  
  ## medium predictors
  ard_med <- ADLB |> 
    filter(MED_PRED_FL01 == "Y") |>
    select(SUBJID, PARAMCD, AVAL, SEXC, AGEGR01, AGEGR01C) |>
    pivot_wider(names_from = PARAMCD, values_from = AVAL, values_fill = NA) |>
    select(-SUBJID) |>
    group_by(SEXC, AGEGR01, AGEGR01C) |>
    miss_case_table() |> 
    filter(n_miss_in_case == 0) |>
    mutate(Set = "Medium importance predictors", 
           row_order = 3) |>
    ungroup()
  
  
  marginals <- ADSL |> group_by(AGEGR01C, SEXC) |> tally()
  
  ### create a final analysis results table for display
  final_ard <- bind_rows(ard_all, ard_key, ard_med) |> 
    left_join(marginals, by = c("AGEGR01C", "SEXC"))
  
 
  return(final_ard)
}