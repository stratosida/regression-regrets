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

  

                                             