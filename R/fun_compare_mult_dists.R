library(tidyverse)
library(here)
source(here("R", "fun_compare_dist_plot.R"))
## Load the datasets
ADSL <- readRDS(here::here("data", "ADSL_01.rds"))
ADLB <- readRDS(here::here("data", "ADLB_01.rds"))
ADLB |> glimpse()
ADSL |> glimpse()

dat <-
  ADSL |> 
  select(USUBJID, AGEGR01C, AGE, SEXC)

attr(dat$AGE, "label")

dat02 <- 
  ADLB |> 
  left_join(dat) 

plot_assoc_by <- function(dat){
  
  gg <- 
    dat |>
    tidyr::drop_na(AVAL) |>
    ggplot(aes(x = AGE, y = AVAL2)) +
    geom_point() +
    facet_wrap(~ SEXC) +
    labs(x = attr(dat02$AGE, "label"))  

  return(gg)  
  }



res <- dat02 |> 
  mutate(AVAL2 = pseudo_log(AVAL)) |>
  group_by(PARAMCD) |>
  group_map(~ plot_assoc_by(.x), .keep = TRUE)


res[[3]]

#  filter(PARAMCD == "MONO") |> 

