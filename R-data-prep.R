library(readr)
library(dplyr)
library(janitor)
library(here)
library(DT)
library(tidyr)
library(ggplot2)
library(tidyselect)

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



### variable clustering 
library(naniar)
library(Hmisc)


## plot sets 
ADLB |>
  select(SUBJID, PARAMCD, AVAL) |>
  pivot_wider(names_from = PARAMCD, values_from = AVAL, values_fill = NA) |>
  naniar::gg_miss_upset(nsets = 7,
                        nintersects = NA)


## select predictors and identify the missing values and set to 1

data_missing <- 
  ADLB |>
  select(SUBJID, PARAMCD, AVAL) |>
  pivot_wider(names_from = PARAMCD, values_from = AVAL, values_fill = NA) |>
  select(-SUBJID) |>
  is.na()*1
  

data_missing


## calculate percentage missing 
perc_miss <- round(apply(data_missing, 2, mean)*100,0)

## add to column names 
colnames(data_missing)<- paste(colnames(data_missing),"(",perc_miss,")",sep="")



d <- dist(t(data_missing), method = "binary")
hc1 <- hclust(d)
plot(hc1)

d <- dist(t(data_missing)^2 / nrow(data_missing) * 100, method = "euclidean")
hc2 <- hclust(d)
plot(hc2)


library(dendextend)
dend1 <- as.dendrogram(hc1)

# order it the closest we can to the order of the observations:
##dend <- rotate(dend, 1:15)

# We hang the dendrogram a bit:
dend1 <- hang.dendrogram(dend,hang_height=0.01)


# reduce the size of the labels:
dend1 <- assign_values_to_leaves_nodePar(dend1, 0.5, "lab.cex")
dend1 <- set(dend1, "labels_cex", 0.7)

# And plot:
par(mar = c(3,3,3,7))

plot(dend1,
     main = "Clustered variables by percentage observations discordantly missing
     (by variable percentage missing)",
     xlab="Percent discordantly missing",
     horiz =  TRUE,  nodePar = list(cex = .005))


dend2 <- as.dendrogram(hc2)

# order it the closest we can to the order of the observations:
##dend <- rotate(dend, 1:15)

# We hang the dendrogram a bit:
dend2 <- hang.dendrogram(dend2,hang_height=0.01)


# reduce the size of the labels:
dend2 <- assign_values_to_leaves_nodePar(dend2, 0.5, "lab.cex")
dend2 <- set(dend2, "labels_cex", 0.7)

# And plot:
par(mar = c(3,3,3,7))

plot(dend2,
     main = "Clustered variables by percentage observations discordantly missing
     (by variable percentage missing)",
     xlab="Percent discordantly missing",
     horiz =  TRUE,  nodePar = list(cex = .005))



## U1: Univariate descriptions: categorical variables

ADSL <- read_rds("adsl.rds")

library(tidyext)
library(ggplot2)
library(forcats)
library(vctrs)
library(dplyr)
library(tidyverse)
library(gt)
library(gtExtras)
library(tidyverse)
## TODO:: how to solve the ordering 
ARD <- ADSL %>%
  mutate(AGEGR01C = fct_reorder(AGEGR01C, AGEGR01)) %>%   
  tidyext::describe_all_cat(include_NAcat = TRUE, digits = 2) %>%
  mutate(N = nrow(ADSL)) |>
  rename(pct = `%`,
         freq = Frequency) |>
  mutate(prop = freq / N)



ARD |> glimpse()

#ARD %>%
#  gt() %>%
#  gt_plt_bar_pct(column = pct, scaled = TRUE, fill = "blue", background = "lightblue") %>%
#  gt_plt_bar_pct(column = pct, scaled = FALSE,
#                 fill = "blue", background = "lightblue") %>%
#  cols_align("center", contains("scale")) %>%
#  cols_width(4 ~ px(125),
#             5 ~ px(125)) %>%
#  gt_plt_bar(column = prop, keep_column = TRUE, width = 35,color = "lightblue")


#gtExtras::gt_plt_summary(ARD)
#library(tidyverse)
#library(gt)
#gt_plt_summary(ADSL)

ARD %>%
  filter(!(Variable == "USUBJID")) |>
  ggplot(aes(
    x = Group,
    y = pct,
    label = pct
  )) +
#  geom_text(nudge_y = 7) +
#  geom_pointrange(aes(ymin = 0, ymax = pct), alpha = 0.6, size = 1, color = "grey") +
  geom_point(color = "firebrick2",
             alpha = 0.6,
             size = 3) +
#  geom_col(alpha = 0.6) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  ylab("Percentage (%)") +
  scale_y_continuous(limits = c(0, 100)) +
  labs(caption = "Number of subjects with a non-missing value reported in brackets.") +
  facet_wrap(~ Variable, ncol = 2, scales = "free_y") +
  coord_flip() +
  theme_light(base_size = 12) +
  theme(axis.title.y = element_blank(),
        panel.grid.minor = element_blank())



## 4.2.1 U2: Univariate distributions of continuous variables

library(patchwork)
## look up summary stats from ARD - need a look up 

## look up summary stats from ARD - need a look up 
source(here::here("R", "ida_plot_describe.R"))
source(here::here("describe_long_dataset.R"))
source(here::here("TMP-plot-function.R"))

ADLB <- read_rds("adlb.rds")
ADLB |> glimpse()
ADLB |> mutate(USUBJID = as.character(SUBJID)) -> ADLB

ARD <- describe_long_dataset(ADLB)

gg <- ADLB |> 
  filter(PARAMCD == "EOS") |>
  describe_plot()

gg

ADLB %>% 
  dplyr::filter(PARAMCD == "EOS") 



## loop around all groups
plots <- 
  ADLB |> 
  group_by(PARAMCD) |> 
  group_map(~ describe_plot(.x))

plots[4]

  

                                             