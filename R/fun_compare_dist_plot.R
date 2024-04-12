#' Title
#'
#' @param dat
#' @param n_bars
#' @param bin_width
#' @param sigma
#'
#' @return
#' @export
#'
#' @examples
compare_dist_plot <- function(dat, n_bars = 200, bin_width = NULL, sigma = NULL) {
    
  dat <- dat |>
    tidyr::drop_na(AVAL, AVAL02) |>
    select(AVAL, AVAL02, PARAM, PARAMCD)

    ## assumes the original transformation is stored in AVAL02
  
  ## store title
  title <- dat |> 
    filter(row_number()==1) |> 
    select(PARAM) |> 
    as.character() |>
    stringr::str_remove_all(": pseudo log transformed")

  
  ## transformed plot first   
  trans_plot <- 
    dat |>
    ggplot(aes(x = AVAL)) + 
    
    scale_x_continuous(
      breaks = fivenum(dat$AVAL, na.rm = TRUE),
      labels = round(fivenum(dat$AVAL02, na.rm = TRUE), 0),
      guide = guide_axis(check.overlap = FALSE) # gh with TRUE: missing 1.quartile label of alkaline phosphartase 
    ) +
    
    ## long running on local machine
    geom_rug(sides = "b", alpha = 0.2, color = "black", outside = FALSE, length = unit(2.5, "mm")) +
    
    geom_histogram(#color = "firebrick2",
      fill = "firebrick2",
      bins = 200,
      alpha = 0.8) +
    
    geom_hline(yintercept = 0, alpha = 0.5) +
    
    theme_minimal(base_size = 12) +
    theme(
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 8),
      axis.title = element_blank(),
      panel.grid.major.x = element_line(color = "grey", linewidth = 0.85),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    )
  
  
  original_plot <- 
    dat |>
    ggplot(aes(x = AVAL02)) + 
    
    scale_x_continuous(
      breaks = fivenum(dat$AVAL02),
      labels = round(fivenum(dat$AVAL02), 0),
      guide = guide_axis(check.overlap = TRUE)   
    ) +
    
    ## long running on local machine
        geom_rug(sides = "b", alpha = 0.4, color = "black", outside = FALSE, length = unit(2.5, "mm")) +
    
    geom_histogram(#color = "firebrick2",
      fill = "firebrick2",
      bins = 200,
      alpha = 0.8) +
    
    geom_hline(yintercept = 0, alpha = 0.5) +
    
    theme_minimal(base_size = 12) +
    theme(
      axis.text.y = element_blank(),
      axis.title = element_blank(),
      axis.text.x = element_text(size = 8),
      panel.grid.major.x = element_line(color = "grey", linewidth = 0.85),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    )
  
  ## NOTE: This line required for manuscript plots 
  # original_plot <- original_plot + ggtitle(title)
  
  plt <- original_plot + trans_plot + 
    plot_annotation(
    title = title,
    subtitle = 'Displayed on the original [left] vs. pseudo-log transformed scale [right]'
#    caption = 'All observed values, and the distribution min, max and interquartile range as reference lines, are displayed.'
  ) &
    theme(plot.tag = element_text(face = 'bold', size = 8))
    
    return(plt)
  }
