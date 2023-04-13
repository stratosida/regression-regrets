#' Scatter plot annotated with spearman and pearson cc
#'
#' @param dat 
#' https://stackoverflow.com/questions/47916307/specify-position-of-geom-text-by-keywords-like-top-bottom-left-right
#'
#' @return
#' @export
#'
#' @examples
plot_scatter_corr <- function(dat, pearson_flag = TRUE) {
  
  x <- dat$x
  y <- dat$y
  
  
  spearman <- dat$spearman
  
  if(pearson_flag){
    pearson <- dat$pearson
    caption <- paste0("Spearman = ", round(spearman, 3), " ; Pearson = ", round(pearson, 3))
  }
  else{
    caption <- paste0("Spearman = ", round(spearman, 3))
  }
  
  title <- paste0("Association between ", x, " and ", y)
  
  
  
  ## TOOD: Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
  gg <-
    dat_wide |>
    tidyr::drop_na() |>
#    ggplot(aes_string(x = x, y = y)) +
    ggplot(aes(x = !! sym(x), y =  !! sym(y))) +
    geom_point(alpha = 0.6) +
    theme_light() +
    labs(title = title, caption = caption)
  
  
  return(gg)
}
