library(readr)
library(dplyr)
library(janitor)
library(here)
library(DT)
library(tidyr)
library(ggplot2)
library(tidyselect)
library(naniar)


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

  

                                             