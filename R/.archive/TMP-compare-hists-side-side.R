library(tidyverse)
library(patchwork)
ADLB_02 <- readRDS(here::here("data", "ADLB_02.rds"))


## store title
title <- dat |> 
  filter(row_number()==1) |> 
  select(PARAM) |> 
  as.character() |>
  stringr::str_remove_all(": pseudo log transformed")



dat <- ADLB_02 |> 
  filter(PARCAT02 == "Y") |>
  filter(PARAMCD == "ALAT_T") |>
  select(AVAL, AVAL02, PARAM, PARAMCD, SIGMA) |>
  drop_na()


rm(a, ADLB, ADLB_02, ADSL, param, suggested_transforms, new_vars)


trans_plot <- 
  dat |>
  ggplot(aes(x = AVAL)) + 
  
  scale_x_continuous(
    breaks = fivenum(dat$AVAL, na.rm = TRUE),
    labels = round(fivenum(dat$AVAL02, na.rm = TRUE), 1),
    guide = guide_axis(check.overlap = TRUE)
  ) +

  ## long running on local machine
  ## geom_rug(sides = "b", alpha = 0.2, color = "black", outside = FALSE, length = unit(2.5, "mm")) +
  
  geom_histogram(#color = "firebrick2",
    fill = "firebrick2",
    bins = 200,
    alpha = 0.8) +
  
  geom_hline(yintercept = 0, alpha = 0.5) +
  
  theme_minimal(base_size = 13) +
  theme(
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    panel.grid.major.x = element_line(color = "grey", linewidth = 0.75),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )


original_plot <- 
  dat |>
  ggplot(aes(x = AVAL02)) + 
  
  scale_x_continuous(
    breaks = fivenum(dat$AVAL02),
    labels = round(fivenum(dat$AVAL02), 1),
    guide = guide_axis(check.overlap = TRUE)
  ) +

  ## long running on local machine
##    geom_rug(sides = "b", alpha = 0.2, color = "black", outside = FALSE, length = unit(2.5, "mm")) +

  geom_histogram(#color = "firebrick2",
    fill = "firebrick2",
    bins = 200,
    alpha = 0.8) +
  
  geom_hline(yintercept = 0, alpha = 0.5) +
  
  theme_minimal(base_size = 13) +
  theme(
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    panel.grid.major.x = element_line(color = "grey", linewidth = 0.75),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )


patchwork <- original_plot + trans_plot
patchwork + plot_annotation(
  title = title,
  subtitle = 'Displayed on the original [left] vs. pseudo-log transformed scale [right]',
  caption = 'All observed values, and the distribution min, max and interquartile range as reference lines, are displayed.'
)
