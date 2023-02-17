
u1_describe <- function(ADSL){

  final_ard <- 
    ADSL |>
    select(where(is.character)) |>
    pivot_longer(cols = -USUBJID) |>
    group_by(name, value) |>
    summarise(freq = n(), 
              prop = freq / nrow(ADSL),
              perc = prop * 100) |>
    ungroup()
    
  return (final_ard)
}



u1_display_table <- function(ARD){

  require(gtExtras)
  
  ARD |>
    group_by(name)|>
    gt() |>
    cols_align("center", contains("scale")) |>
    cols_width(4 ~ px(125),
               5 ~ px(125)) |>
    gt_plt_bar(column = prop, keep_column = TRUE, width = 20,color = "lightblue")
  
}



u1_display_plot <- function(ARD){

#gtExtras::gt_plt_summary(ARD)
#library(tidyverse)
#library(gt)
#gt_plt_summary(ADSL)

#  Columns: 5
#  $ name  <chr> "AGEGR01C", "AGEGR01C", "AGEGR01C", "BACTEREMIA", "BACTEREMIA", "SEXC", "SEXC"
#  $ value <chr> "(16, 50]", "(50, 65]", "(65, 101]", "no", "yes", "female", "male"
#  $ freq  <int> 5365, 4250, 5076, 13511, 1180, 6155, 8536
#  $ prop  <dbl> 0.36518957, 0.28929276, 0.34551766, 0.91967871, 0.08032129, 0.41896399, 0.58103601
#  $ perc  <dbl> 36.518957, 28.929276, 34.551766, 91.967871, 8.032129, 41.896399, 58.103601
  
ARD %>%
  filter(!(name == "USUBJID")) |>
  ggplot(aes(
    x = value, 
    y = prop,
    label = prop
  )) +
  #  geom_text(nudge_y = 7) +
  #  geom_pointrange(aes(ymin = 0, ymax = pct), alpha = 0.6, size = 1, color = "grey") +
  geom_point(color = "firebrick2",
             alpha = 0.6,
             size = 3) +
  #  geom_col(alpha = 0.6) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  ylab("proportion") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(caption = "Number of subjects with a non-missing value reported in brackets.") +
  facet_wrap(~ name, ncol = 2, scales = "free_y") +
  coord_flip() +
  theme_bw(base_size = 12) +
  theme(axis.title.y = element_blank(),
        panel.grid.minor = element_blank())

}
