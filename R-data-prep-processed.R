## Read in the source data dictionary and display 
bact_dd <- read_csv(here("data-raw", "Bacteremia_public_S2_Data_Dictionary.csv")) %>%
  arrange(VariableNr) |>
  clean_names() |>
  mutate(PARAMCD = variable )

bact_dd |> glimpse()

## display as a table 
#bact_dd |> 
#  datatable(options = list(pageLength = 5))


## Read in the source data 
bact_data <- read_csv(here("data-raw", "Bacteremia_public_S2.csv")) 

bact_data %>% spec()



## transform 
bact_data01 <-
  bact_data |> 
  pivot_longer(
    cols = c(-ID, -SEX, -AGE, -BloodCulture),
    names_to = "PARAMCD",
    #names_prefix = "PARAMCD",
    values_to = "AVAL",
    values_drop_na = FALSE
  )


bact_data02 <- 
  bact_data01 |> 
  left_join(bact_dd, by = "PARAMCD") |>
  mutate(
    UNIT = units,
    TYPE = if_else(scale_of_measurement == "continuous", "numeric", "NA"), 
    NOTE = remark, 
    NOTE2 = from_paper,
    SEXC = case_when(
      SEX == 1 ~ "male",
      SEX == 2 ~ "female",
      TRUE ~ "NA"
    ),
    SUBJID = ID + 100000, 
    USUBJID = paste0("DC-2019-0054-", SUBJID),
    PARAM = paste0(label, " (", UNIT, ")"),
    BACTEREMIA = BloodCulture,
    BACTEREMIAN = case_when(
      BloodCulture == "no" ~ 0,
      BloodCulture == "yes" ~ 1)
  )


bact_data02 |> glimpse()

# Should come from the data set specification
bact_data03 <- 
  bact_data02 %>%
  relocate(USUBJID, SUBJID, AGE, SEX, SEXC, BACTEREMIA, BACTEREMIAN, PARAM, PARAMCD, AVAL, UNIT, TYPE, NOTE, NOTE2) |>
  select(USUBJID, SUBJID, AGE, SEX, SEXC, BACTEREMIA, BACTEREMIAN, PARAM, PARAMCD, AVAL, UNIT, TYPE, NOTE, NOTE2)



bact_data03 |> glimpse()

names(bact_data03)

## Takes me up to end of BACT_INTRO

attr(bact_data03$SUBJID, "label") <- "Patient Identifer"  
attr(bact_data03$USUBJID, "label") <- "Unique Patient Identifer with data souce reference: DC 2019-0054"  
attr(bact_data03$SEX, "label") <- "Patient gender"  
attr(bact_data03$SEX, "label") <- "Patient gender (Numeric coding, 1: male, 2: female)"  
attr(bact_data03$SEXC, "label") <- "Patient gender (Character coding)"  
attr(bact_data03$AGE, "label") <- "Patient age (years)"
attr(bact_data03$BACTEREMIA, "label") <- "Blood culture result for bacteremia (Character coding)"
attr(bact_data03$BACTEREMIAN, "label") <- "Blood culture result for bacteremia (Numeric coding, 0: No, 1: Yes)"
attr(bact_data03$PARAM, "label") <- "Parameter name"
attr(bact_data03$PARAMCD, "label") <- "Parameter code"
attr(bact_data03$AVAL, "label") <- "Parameter analysis value (Numeric)"
attr(bact_data03$UNIT, "label") <- "Parameter unit"
attr(bact_data03$TYPE, "label") <- "Parameter: variable type (Numeric, Character)"
attr(bact_data03$NOTE, "label") <- "Notes from source data dictionary - free text"
attr(bact_data03$NOTE2, "label") <- "Remarks from source data dictionary - free text"


table(bact_data02$BloodCulture)
table(bact_data03$BACTEREMIA)


bact_data03 %>% glimpse()


#### IDA PLAN

# age (AGE), leukocytes (WBC), blood urea neutrogen (BUN), creatinine (CREA), 
# thrombocytes (PLT), and neutrophiles (NEU) and these predictors will be included in the model as key predictors

# Predictors of medium importance are potassium (POTASS), 
# and some acute-phase related parameters such as fibrinogen (FIB), 
# C-reactive protein (CRP), aspartate transaminase (ASAT), 
# alanine transaminase (ALAT), and gamma-glutamyl transpeptidase (GGT).


bact_data04 <-
  bact_data03 |>
  mutate(
    KEY_PRED_FL = case_when(
      PARAMCD %in% c("WBC", "BUN", "CREA", "PLT", "NEU") ~ "Y"
    ),
    MED_PRED_FL = case_when(
      PARAMCD %in% c("POTASS", "FIB", "CRP", "ASAT", "ALAT", "GGT") ~ "Y"
    )
  )

attr(bact_data04$KEY_PRED_FL, "label") <- "Key Predictor Flag"
attr(bact_data04$MED_PRED_FL, "label") <- "Predictors of medium importance Flag"

bact_data04 |> glimpse()

## DO something with this? ###############

# For example, leukocytes consist of five different types of blood cells 
# (BASO, EOS, NEU, LYM and MONO), and the sum of the concentration of these types 
# approximately (but not exactly) gives the leukocyte count, which is recorded 
# in the variable WBC. Moreover, these variables are given as absolute counts 
# and as percentages of the sum of the five variables, which creates some correlation. 
# Some laboratory variables differ by sex and age, but the special selection of patients 
# for this study (suspicion of bacteremia) may distort or alter the expected correlations with sex and age.

## Age group
# For the purpose of stratifying IDA results by age, 
# age will be categorized into the following 
# three groups: [16, 50], (50, 65], (65, 101].

bact_data05 <-
  bact_data04 |>
  mutate(AGEGR01 = case_when(AGE >= 16  & AGE <= 50 ~ 1,
                             AGE > 50 & AGE <= 65 ~ 2,
                             AGE > 65 & AGE <= 101 ~ 3),
         AGEGR01C = case_when(AGEGR01 == 1 ~ "(16, 50]",
                              AGEGR01 == 2 ~ "(50, 65]",
                              AGEGR01 == 3 ~ "(65, 101]"))


attr(bact_data05$AGEGR01, "label") <- "Age group 01 (Numeric coding)"
attr(bact_data05$AGEGR01C, "label") <- "Age group 01 (Character coding)"

################### end of IDA plan

#-------------------- IDA missing ness

library(naniar)


ADSL <- bact_data05 %>%
  filter(PARAMCD == "WBC") %>%
  select(USUBJID, SUBJID, AGE, AGEGR01, AGEGR01C, SEX, SEXC, BACTEREMIA, BACTEREMIAN)


saveRDS(ADSL, file = "adsl.rds")


## keep age in ? as its a key predictor 
ADLB <- bact_data05 %>%
  select(-AGE, -AGEGR01, -AGEGR01C, -SEX, -SEXC, -BACTEREMIA, -BACTEREMIAN)

saveRDS(ADLB, file = "adlb.rds")






################## M1 ###########################
## prevelance of missing 

# structural variables 
ADSL %>%
  select(AGE, AGEGR01, AGEGR01C, SEX) %>%
  miss_var_summary()



describe_structural_cont_vars <-
  ADSL %>%
  select(AGE) %>%
  miss_var_summary()


describe_structural_cont_vars <-
  describe_structural_cont_vars |>
  rename(PARAMCD = variable) |>
  mutate(PARAM = "AGE (years)") |>
  pivot_longer(cols = c(-PARAM, -PARAMCD), names_to = "STATISTIC", values_to = "VALUE")

describe_structural_cont_vars


#bact_data05 |>
#  select(AGE, AGEGR01, SEX) |>
#  skimr::skim()

#bact_data05 %>%
#  select(PARAM, PARAMCD, AVAL) %>%
#  group_by(PARAM, PARAMCD) %>%
#  miss_var_table()

describe_predictor_cont_vars <-
  ADLB %>%
  select(PARAM, PARAMCD, AVAL) %>%
  group_by(PARAM, PARAMCD) %>%
  miss_var_summary() %>%
  select(-variable) %>%
  pivot_longer(cols = c(-PARAM, -PARAMCD), names_to = "STATISTIC", values_to = "VALUE")



############3 M2 ###########################33
#2.2.2 M2: Complete cases
#The number of available complete cases (outcome and predictors) will be reported when considering:
#  
# 1. the outcome variable (BC)
# 2. outcome and structural variables (BC, AGE, SEX)
# 3. outcome and key predictors only (BC, AGE, WBC, BUN, CREA, PLT, NEU)
# 4. outcome, key predictors and predictors of medium importance (BC, AGE, WBC, BUN, CREA, PLT, NEU, POTASS, FIB, CRP, ASAT, ALAT, GGT)
# 5. outcome and all predictors.

## step 1

outcome <- ADSL %>%
  select(BACTEREMIA) %>%
  miss_case_table()

outcome

## step 3
outcome_structural <- ADSL %>%
  select(BACTEREMIA, AGE, SEX) %>%
  miss_case_table()

outcome_structural


# step 3# 3. outcome and key predictors only (BC, AGE, WBC, BUN, CREA, PLT, NEU)

step1 <- ADLB %>%
  filter(PARAMCD %in% c("WBC", "BUN", "CREA", "PLT", "NEU")) %>%
  select(SUBJID, PARAMCD, AVAL) |>
  pivot_wider(names_from = PARAMCD, values_from = AVAL, values_fill = NA)


step2 <- ADSL %>%
  select(SUBJID, BACTEREMIA, AGE) %>%
  left_join(step1, by = "SUBJID") 

step3 <- step2 %>%
  select(-SUBJID) %>%
  miss_case_table()

step3 %>% filter(n_miss_in_case == 0)



#### analysis M.2.4
## 4. outcome, key predictors and predictors of medium importance 
# (BC, AGE, WBC, BUN, CREA, PLT, NEU, POTASS, FIB, CRP, ASAT, ALAT, GGT)


step1 <- ADLB %>%
  filter(PARAMCD %in% c("WBC", "BUN", "CREA", "PLT", "NEU", "POTASS", "FIB", "CRP", 
                        "ASAT", "ALAT", "GGT")) %>%
  select(SUBJID, PARAMCD, AVAL) |>
  pivot_wider(names_from = PARAMCD, values_from = AVAL, values_fill = NA)


step2 <- ADSL %>%
  select(SUBJID, BACTEREMIA, AGE) %>%
  left_join(step1, by = "SUBJID") 

step3 <- step2 %>%
  select(-SUBJID) %>%
  miss_case_table()

step3 %>% filter(n_miss_in_case == 0)



### analysis M.2.5 Outcome_and_all_predictors



step1 <- ADLB %>%
  select(SUBJID, PARAMCD, AVAL) |>
  pivot_wider(names_from = PARAMCD, values_from = AVAL, values_fill = NA)

step2 <- ADSL %>%
  select(SUBJID, BACTEREMIA, AGE, SEX) %>%
  left_join(step1, by = "SUBJID") 

step3 <- step2 %>%
  select(-SUBJID) %>%
  miss_case_table()

step3 %>% filter(n_miss_in_case == 0)


##2.2.3 M3: Patterns of missing values
##Patterns of missing values will be investigated by:
## computing a table of complete cases (for the three predictor sets described above) for strata defined by the structural variables age and sex,

#1 All predictors	411	26
#2 Structural variables	1579	100
#3 Key predictors	1468	93
#4 Key and medium importance predictors


### 1

step1 <- ADLB %>%
  select(SUBJID, PARAMCD, AVAL) |>
  pivot_wider(names_from = PARAMCD, values_from = AVAL, values_fill = NA)

step2 <- ADSL %>%
  select(SUBJID, -BACTEREMIA, AGEGR01, AGEGR01C, SEXC) %>%
  left_join(step1, by = "SUBJID") 

step3 <- step2 %>%
  select(-SUBJID) %>%
  group_by(AGEGR01, AGEGR01C, SEXC) %>%
  miss_case_table()

step3 %>% filter(n_miss_in_case == 0)


### 2 - structural variables -- not sure this makes sense 

step2 <- ADSL %>%
  select(SUBJID, AGE, SEX, AGEGR01, AGEGR01C, SEXC)

step3 <- step2 %>%
  select(-SUBJID) %>%
  group_by(AGEGR01, AGEGR01C, SEXC) %>%
  miss_case_table()

step3 %>% filter(n_miss_in_case == 0)


### 3 - key predictors 

step1 <- ADLB %>%
  filter(PARAMCD %in% c("WBC", "BUN", "CREA", "PLT", "NEU")) %>%
  select(SUBJID, PARAMCD, AVAL) |>
  pivot_wider(names_from = PARAMCD, values_from = AVAL, values_fill = NA)

step2 <- ADSL %>%
  select(SUBJID, -BACTEREMIA, AGEGR01, AGEGR01C, SEXC) %>%
  left_join(step1, by = "SUBJID") 

step3 <- step2 %>%
  select(-SUBJID) %>%
  group_by(AGEGR01, AGEGR01C, SEXC) %>%
  miss_case_table()

step3 %>% filter(n_miss_in_case == 0)


### 3 - key predictors+ medium importance 

step1 <- ADLB %>%
  filter(PARAMCD %in% c("WBC", "BUN", "CREA", "PLT", "NEU", "POTASS", "FIB", "CRP", 
                        "ASAT", "ALAT", "GGT")) %>%
  select(SUBJID, PARAMCD, AVAL) |>
  pivot_wider(names_from = PARAMCD, values_from = AVAL, values_fill = NA)

step2 <- ADSL %>%
  select(SUBJID, -BACTEREMIA, AGEGR01, AGEGR01C, SEXC) %>%
  left_join(step1, by = "SUBJID") 

step3 <- step2 %>%
  select(-SUBJID) %>%
  group_by(AGEGR01, AGEGR01C, SEXC) %>%
  miss_case_table()

step3 %>% filter(n_miss_in_case == 0)






#####################


ard |>
  dplyr::select(name, value, freq, perc, prop) |>
  dplyr::mutate(
    name = case_when(
      name == "AGEGR01C" ~ "Age group",
      name == "SEXC" ~ "Sex",
      name == "BACTEREMIA" ~ "Presence of bactermia"
    )
  ) |>
  group_by(name)|>
  gt() 


