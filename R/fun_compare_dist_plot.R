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
    
    trans_plot <- 
      dat |>
      ggplot(aes(x = AVAL)) +
      geom_histogram(
        bins = 200,
        center = 0,
        alpha = 0.6,
        fill = "firebrick2"
      ) +
      scale_x_continuous(
        breaks = fivenum(dat$AVAL, na.rm = TRUE),
        labels = round(fivenum(dat$AVAL02, na.rm = TRUE), 1),
        guide = guide_axis(check.overlap = TRUE)
      ) +
      scale_y_continuous(expand = c(0, 0)) +
      theme_void(base_size = 15) +
      theme(
        axis.text.x = element_text(
          color = "black",
          size = 13
        ),
        axis.line.x = element_line(color = "black"),
        axis.ticks.x = element_line(color = "black"),
        axis.ticks.length.x = unit(1, "mm"),
        axis.title = element_blank(),
        panel.grid.major.x = element_line(color = "grey", linewidth = 0.5),
        plot.margin = margin(1, 1, 3, 1),
        text = element_text(
          size = 13)
      )
    
    
    original_plot <- 
      dat |>
      ggplot(aes(x = AVAL02)) +
      
      scale_x_continuous(
        breaks = fivenum(dat$AVAL02),
        labels = round(fivenum(dat$AVAL02), 1),
        guide = guide_axis(check.overlap = TRUE)
      ) +
      
      scale_y_continuous(expand = c(0, 0)) +
      
      geom_histogram(#color = "firebrick2",
        fill = "firebrick2",
        bins = 200,
        alpha = 0.8) +
      
      theme_void(base_size = 15) +
      theme(
        axis.text.x = element_text(
          color = "black",
          #      vjust = -2,
          size = 13
        ),
        axis.line.x = element_line(color = "black"),
        axis.ticks.x = element_line(color = "black"),
        axis.ticks.length.x = unit(1, "mm"),
        axis.title = element_blank(),
        panel.grid.major.x = element_line(color = "grey", linewidth = 0.5),
        plot.margin = margin(1, 1, 3, 1),
        text = element_text(
          size = 13)
      )
    

    
    plt <- original_plot + trans_plot
    
    return(plt)
  }
