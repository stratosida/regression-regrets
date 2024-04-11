## Load in updated data set with transformations 
library(ggplot2)
library(patchwork)
library(here)
library(dplyr)

ADLB_02 <- readRDS(here::here("data", "IDA", "ADLB_02.rds"))
source(here("R", "fun_compare_dist_plot.R")) ## plot side by side

a <- ADLB_02 |> 
  filter(PARCAT02 == "Y" & PARAMCD == "CK_T") |>
  compare_dist_plot()

b <- ADLB_02 |> 
  filter(PARCAT02 == "Y" & PARAMCD == "GGT_T") |>
  compare_dist_plot()

c <- ADLB_02 |> 
  filter(PARCAT02 == "Y" & PARAMCD == "WBC_T") |>
  compare_dist_plot()

d <- ADLB_02 |> 
  filter(PARCAT02 == "Y" & PARAMCD == "AP_T") |>
  compare_dist_plot()



fig3_plot <- a / b / c / d + 
  plot_annotation(
    caption = 'Displayed on the original [left] vs. pseudo-log transformed scale [right]') 

tiff(filename = "fig3.tiff", units="px", width=2007, height=2657, compression = 'lzw', res=300)

fig3_plot

dev.off()
