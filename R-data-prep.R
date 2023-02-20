library(readr)
library(dplyr)
library(janitor)
library(here)
library(DT)
library(tidyr)
library(ggplot2)
library(tidyselect)
library(naniar)



### variable clustering 
library(naniar)
library(Hmisc)

## U1: Univariate descriptions: categorical variables

ADSL <- read_rds("data/ADSL_01.rds")

library(tidyext)
library(ggplot2)
library(forcats)
library(vctrs)
library(dplyr)
library(tidyverse)
library(gt)
library(gtExtras)
library(tidyverse)


## 4.2.1 U2: Univariate distributions of continuous variables

library(patchwork)
## look up summary stats from ARD - need a look up 

## look up summary stats from ARD - need a look up 
source(here::here("ida_plot_describe.R"))
source(here::here("TMP-plot-function.R"))

ADLB <- read_rds("data/ADLB_01.rds")
ADLB |> glimpse()
ADLB |> mutate(USUBJID = as.character(SUBJID)) -> ADLB

source(here::here("describe_long_dataset.R"))
# summary stats 
ARD <- describe_long_dataset(ADLB)
ARD  |> glimpse()

ARD |> filter(PARAMCD == "APTT") 

gg <- ADLB |> 
  filter(PARAMCD == "APTT") |>
  describe_plot()

gg

ADLB %>% 
  dplyr::filter(PARAMCD == "EOS") 

ida_plot_univar <- function(data, var, n_dodge = 1, 
                            bin_width = diff(range(data[[var]],na.rm=T))/min(length(unique(data[[var]])),100), 
                            sigma = NA, n_bars=100, transform = TRUE)  
  
source("ida_plot_univar.R")
print(ida_plot_univar(a_bact_02_md, unique.variables[j], sigma=NA, n_bars=100, transform = FALSE))

ADLB |> 
  filter(PARAMCD == "APTT") |>
  ida_plot_univar( var = "APTT", sigma=NA, n_bars=100, transform = FALSE)  

## loop around all groups
#plots <- 
source("TMP-plot-function.R")
  ADLB |> 
  filter(KEY_PRED_FL == "Y") |> 
  group_by(PARAMCD) |> 
  group_map(~ describe_plot(.x))


  

                                             