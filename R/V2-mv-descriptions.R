


#' Scatter plot annotated with spearman and pearson cc
#'
#' @param dat 
#' https://stackoverflow.com/questions/47916307/specify-position-of-geom-text-by-keywords-like-top-bottom-left-right
#'
#' @return
#' @export
#'
#' @examples
plot_scatter_corr <- function(dat) {
  
  x <- dat$x
  y <- dat$y
  
  pearson <- dat$pearson
  spearman <- dat$spearman
  
  title <- paste0("Association between ", x, " and ", y)
  caption <- paste0("Spearman = ", round(spearman, 3), " ; Pearson = ", round(pearson, 3))
  
  
  gg <-
    dat_wide |>
    tidyr::drop_na() |>
    ggplot(aes_string(x = x, y = y)) +
    geom_point() +
    theme_light() +
 #   annotate("text", x = -Inf, y = Inf, hjust = 0, vjust = 1, label = paste0("PEARSON ", pearson)) +
 #   annotate("text", x = -Inf, y = Inf, hjust = 0, vjust = 2, label = paste0("SPEARMAN ", spearman)) +
    labs(title = title, caption = caption)
  
  
#  x = -Inf, y = Inf, hjust = 0, vjust = 1
#  annotate(paste0("PEARSON ", pearson), x = 10, y = 25, label = "Some text") +
#    annotate(paste0("SPEARMAN ", spearman), x = 12, y = 25, label = "Some text")
  
  
  return(gg)
}
