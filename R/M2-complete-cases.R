############ M2 ###########################
#2.2.2 M2: Complete cases
#The number of available complete cases (outcome and predictors) will be reported when considering:
#  
# 1. the outcome variable (BC)
# 2. outcome and structural variables (BC, AGE, SEX)
# 3. outcome and key predictors only (BC, AGE, WBC, BUN, CREA, PLT, NEU)
# 4. outcome, key predictors and predictors of medium importance (BC, AGE, WBC, BUN, CREA, PLT, NEU, POTASS, FIB, CRP, ASAT, ALAT, GGT)
# 5. outcome and all predictors.
############ M2 ###########################

## TODO: This code could be refactored ALOT
## USe the pre-defined analysis flags rather than hardcoded filter 

m2_analysis <- function(ADSL, ADLB){

  
## step 1
outcome <- ADSL |>
  select(BACTEREMIA) |>
  miss_case_table() |>
  mutate(Set = "Outcome", 
         row_order = 1)


## step 2
outcome_structural <- ADSL |>
  select(BACTEREMIA, AGE, SEX) |>
  miss_case_table() |>
  mutate(Set = "Outcome and structural variables",
         row_order = 2)




# step 3# 3. outcome and key predictors only (BC, AGE, WBC, BUN, CREA, PLT, NEU)

outcome_key_p01 <- ADLB |>
  filter(PARAMCD %in% c("WBC", "BUN", "CREA", "PLT", "NEU")) |>
  select(SUBJID, PARAMCD, AVAL) |>
  pivot_wider(names_from = PARAMCD, values_from = AVAL, values_fill = NA)

outcome_key_p02 <- ADSL |>
  select(SUBJID, BACTEREMIA, AGE) |>
  left_join(outcome_key_p01, by = "SUBJID") 

outcome_key_p03 <- outcome_key_p02 |>
  select(-SUBJID) |>
  miss_case_table()

## we are only interested in complete cases 
outcome_key_predictors <- outcome_key_p03 |>
  filter(n_miss_in_case == 0) |>
  mutate(Set = "Outcome and key predictors only", 
         row_order = 3)
  

#### analysis M.2.4
## 4. outcome, key predictors and predictors of medium importance 
# (BC, AGE, WBC, BUN, CREA, PLT, NEU, POTASS, FIB, CRP, ASAT, ALAT, GGT)


outcome_key_m_p01 <- ADLB |>
  filter(PARAMCD %in% c("WBC", "BUN", "CREA", "PLT", "NEU", "POTASS", "FIB", "CRP", 
                        "ASAT", "ALAT", "GGT")) |>
  select(SUBJID, PARAMCD, AVAL) |>
  pivot_wider(names_from = PARAMCD, values_from = AVAL, values_fill = NA)


outcome_key_m_p02 <- ADSL |>
  select(SUBJID, BACTEREMIA, AGE) |>
  left_join(outcome_key_m_p01, by = "SUBJID") 

outcome_key_m_p03 <- outcome_key_m_p02 |>
  select(-SUBJID) |>
  miss_case_table()

outcome_key_med_predictors <-
  outcome_key_m_p03 |> filter(n_miss_in_case == 0) |>
  mutate(Set = "Outcome key predictors and predictors of medium importance", 
         row_order = 4)


### analysis M.2.5 Outcome_and_all_predictors

all_01 <- ADLB %>%
  select(SUBJID, PARAMCD, AVAL) |>
  pivot_wider(names_from = PARAMCD, values_from = AVAL, values_fill = NA)

all_02 <- ADSL |>
  select(SUBJID, BACTEREMIA, AGE, SEX) |>
  left_join(all_01, by = "SUBJID") 

all_03 <- all_02 |>
  select(-SUBJID) |>
  miss_case_table()

outcome_all_predictors <-
  all_03 |> filter(n_miss_in_case == 0) |>
  mutate(Set = "Outcome and all predictors", 
         row_order = 5)



### create a final analysis results table for display
final_ard <- bind_rows(outcome, outcome_structural, outcome_key_predictors, outcome_key_med_predictors, outcome_all_predictors)


return(final_ard)
}