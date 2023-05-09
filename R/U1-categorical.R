
#' Title
#'
#' @param ADSL 
#'
#' @return
#' @export
#'
#' @examples
u1_describe <- function(ADSL){

  final_ard <- 
    ADSL |>
    select(where(is.character)) |>
    pivot_longer(cols = -USUBJID) |>
    group_by(name, value) |>
    summarise(freq = n(), 
              prop = freq / nrow(ADSL),
              perc = prop * 100, 
              .groups = 'drop') |>
    ungroup()
    
  return (final_ard)
}



#' Title
#'
#' @param ARD 
#'
#' @return
#' @export
#'
#' @examples
u1_display_table_html <- function(ARD){

  u1_gt <-
    ARD |>
    dplyr::select(name, value, freq, perc, prop) |>
    dplyr::mutate(
      name = case_when(
        name == "AGEGR01C" ~ "Age group",
        name == "SEXC" ~ "Sex",
        name == "BACTEREMIA" ~ "Presence of bacteremia"
      )
    ) |>
    group_by(name) |>
    gt() |>
    cols_align("left", name) |>
    cols_align("left", value) |>
    cols_align("right", freq) |>
    cols_align("right", perc) |>
    cols_align("left", prop) |>
    #    gt_plt_bar(column = perc, keep_column = TRUE, width = 40,color = "lightblue", scale_type = "percent", text_color = "black") |>
    gt_plt_bar(
      column = prop,
      keep_column = FALSE,
      width = 40,
      color = "lightblue",
      labels = "percentage"
    ) |>
    gt::cols_label(
      value = "Category",
      freq = "Count",
      prop = "",
      perc = "%"
    ) |>
    cols_width(1 ~ px(100),
               2 ~ px(75),
               3 ~ px(75),
               4 ~ px(125),
               5 ~ px(125)) |>
    gt::fmt_number(columns = c(perc),
                   decimals = 1)  |>
    row_group_order(groups = c("Age group", "Sex", "Presence of bacteremia")) |>
    gt_theme_538() 
  
  
  return(u1_gt)
  
}



#' Title
#'
#' @param ARD 
#'
#' @return
#' @export
#'
#' @examples
u1_display_table_word <- function(ARD){
  
  
  u1_gt <- 
    ARD |>
    dplyr::select(name, value, freq, prop) |>
    dplyr::mutate(
      name = case_when(
        name == "AGEGR01C" ~ "Age group",
        name == "SEXC" ~ "Sex",
        name == "BACTEREMIA" ~ "Presence of bacteremia"
      )
    ) |>
    group_by(name)|>
    arrange(name, -prop) |>
    gt() |>
    cols_align("left", name) |>
    cols_align("left", value) |>
    cols_align("right", freq) |>
    cols_align("right", prop) |>
    gt::cols_label(
      value = "Category",
      freq = "Count",
      prop = "Proportion"
    ) |>
    cols_width(1 ~ px(100),
               2 ~ px(75),
               3 ~ px(75),
               4 ~ px(75)
    ) |>
    gt::fmt_number(
      columns = c(prop),
      decimals = 2
    )  |>
    row_group_order(groups = c("Age group", "Sex", "Presence of bacteremia")) |>
    gt_theme_538() 
  
  
  return(u1_gt)
  
}



#' Title
#'
#' @param ARD 
#'
#' @return
#' @export
#'
#' @examples
u1_display_plot <- function(ARD){
  
  gg <-
    ARD %>%
    filter(!(name == "USUBJID")) |>
    mutate(
      labs = case_when(
        name == "AGEGR01C" ~ "Age group",
        name == "SEXC" ~ "Sex",
        name == "BACTEREMIA" ~ "Presence of bacteremia")
    ) |>
    ggplot(aes(x = value,
               y = prop,
               label = prop)) +
    geom_point(color = "firebrick2",
               alpha = 0.6,
               size = 3) +
    geom_bar(
      stat = "identity",
      fill = "#f68060",
      alpha = .6,
      width = .4
    ) +
    labs(caption = "Proportions are displayed") +
    #ylab("proportion") +
    scale_y_continuous(limits = c(0, 1)) +
    facet_wrap( ~ labs, ncol = 3, scales = "free_y") +
    coord_flip() +
    theme_bw(base_size = 14) +
    theme(axis.title = element_blank(),
          panel.grid.minor = element_blank())
  
  
  return (gg)
  
}
