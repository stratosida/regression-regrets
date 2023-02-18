#' Function to generate a combined plot summary of univariate distributions
#' including 
#'  * jittered strip plot to show observed values
#'  * spike histogram with reference lines on 5 -number summary
#'  * boxplot 
#'  
#'
#' @param data input dataset
#' @param var variable to summarise
#' @param n_dodge if the 5 number summary clashes, split over n lines.
#'               Default is display over 1 line
#' @param bin_width Width of the histogram bin
#' @param sigma transformation parameter for pseudo_log (NA for no transformation)
#' @param transform should the transformation be used? defaults to TRUE, suppress transformation with FALSE
#'                  idea: incorporate option 'both' to show original and transformation side by side
#' @return gg ggplot object
#' @export
#'
#' @examples
ida_plot_univar <- function(data, var, n_dodge = 1, 
                            bin_width = diff(range(data[[var]],na.rm=T))/min(length(unique(data[[var]])),100), 
                            sigma = NA, n_bars=100, transform = TRUE)                             {
  
  ## evaluate if sigma is na
  
  trans<-FALSE
  if(!is.na(sigma)) trans<-TRUE
  if(!transform) trans<-FALSE
  
  ## number of missing observations
  nmiss <-
    data %>% dplyr::filter(is.na(.data[[var]])) %>% dplyr::tally()
  
  ## number of non missing observations
  bign <-
    data %>% dplyr::filter(!is.na(.data[[var]])) %>% dplyr::tally()
  
  # vector of variable for calculating five number summary
  x <- as.numeric(data[[var]])
  x <- x[!is.na(x)]
  x_orig<-x
  if(trans) x <- pseudo_log(x, sigma=sigma)
  
  
  # breaks for histogram
  ## compute n_bars+1 quantiles and divide the range into nbars 
  ## take a weighted average of the two sequences to compromise between small and wide bins
  
  if(length(unique(x))<n_bars*2) {
    brks <- unique(x) 
  }  else {
    brks1 <- seq(min(x), max(x), length.out=n_bars+1)
    brks2 <- quantile(x, seq(0,1,1/n_bars))
    brks <- (brks2 + 3*brks1)/4
  }
  
  
  # title for plot
  title <-
    paste0("Univariate summary of ", label(data[[var]]), " [", units(data[[var]]), "]")
  
  # labels for histogram
  y_axis <- "Density"
  x_axis <-
    paste0(label(data[[var]]), " [", units(data[[var]]), "]")
  
  # caption to summarise missing data
  caption <-
    paste0(
      "All observed values, the distribution and the, min, max and interquartile range are reported\n",
      "n = ",
      bign,
      " subjects displayed. ",
      nmiss,
      " subjects with missing values are not presented."
    )
  if(trans) caption<-paste0(caption, "Using pseudo-log scale.")
  if(!is.na(sigma) & !trans) caption<-paste0(caption, " Pseudo-log transformation is suggested.")
  
  ## strip plot
  p1 <-
    ggplot(data=NULL,aes(x = x, y = 0)) +
    geom_jitter(
      width = 0.0,
      height = 0.1,
      alpha = 0.2,
      color = "firebrick2"
    ) +
    geom_rug(sides = "b") +
    scale_x_continuous(limit = c(min(x), max(x)),
                       breaks = round(fivenum(x), 1)) +
    ggplot2::theme_minimal() +
    ylab(y_axis) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank()
    )
  
  ## plot histogram
  p2 <-
    data %>%
    filter(!is.na(.data[[var]])) %>%
    ggplot2::ggplot(aes(x)) +
    geom_histogram(aes(x=x, y=..density..),
                   breaks=brks,
                   center = 0,
                   alpha = 0.6,
                   fill = "firebrick2"
    ) +
    geom_rug() +
    scale_x_continuous(
      limit = c(min(x), max(x)),
      breaks = fivenum(x),
      labels = round(fivenum(x_orig),1),
      guide = guide_axis(n.dodge = n_dodge)
    ) +
    ylab(y_axis) +
    ggplot2::theme_minimal() +
    theme(
      panel.grid.major.x = element_line(colour = "grey", size = 0.5),
      panel.grid.minor = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    )
  
  ## boxplot
  p3 <-
    data %>%
    filter(!is.na(.data[[var]])) %>%
    ggplot(aes(x = x, y = 0)) +
    geom_boxplot(outlier.shape = NA, width = 0.1) +
    scale_x_continuous(limit = c(min(x), max(x)),
                       breaks = c(fivenum(x))) +
    xlab(x_axis) +
    ggplot2::theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.y = element_blank(),
      axis.text = element_blank()
    )
  
  # layout for combined plot
  # histogram has more area
  layout <- c(patchwork::area(1, 1, 1, 6),
              patchwork::area(2, 1, 5, 6),
              patchwork::area(6, 1, 6, 6))
  
  ## combine plots
  gg <- p1 / p2 / p3 +
    patchwork::plot_layout(design = layout) +
    patchwork::plot_annotation(title = title,
                               caption = caption)
  
  return(gg)
  
}