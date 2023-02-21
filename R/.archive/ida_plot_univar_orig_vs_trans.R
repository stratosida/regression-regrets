#' Function to show univariate IDA plot of transformed and original variable side by side
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

ida_plot_univar_orig_vs_trans <- function(data, var, n_dodge = 1, 
                                       bin_width = diff(range(data[[var]],na.rm=T))/min(length(unique(data[[var]])),100), 
                                       sigma = NA, n_bars=100, transform = TRUE){
  if(is.na(sigma)){
    p_out <- ida_plot_univar(data = data, var = var, n_dodge = n_dodge, 
                             bin_width = bin_width, 
                             sigma = sigma, n_bars=n_bars, transform = transform) 
   return(p_out)
    
  } else {
    
    p_original <- ida_plot_univar(data = data, var = var, n_dodge = n_dodge, 
                                  bin_width = bin_width, 
                                  sigma = sigma, n_bars=n_bars, transform = FALSE)
    
    p_transformed <- ida_plot_univar(data = data, var = var, n_dodge = n_dodge, 
                                     bin_width = bin_width, 
                                     sigma = sigma, n_bars=n_bars, transform = TRUE)
    
    p_out <- p_original | p_transformed 
    
    return(p_out +
             patchwork::plot_annotation(
               title = p_original$patches$annotation$title,
               subtitle = 'original [left] vs. pseudo-log transformed scale [right]',
               caption = p_original$patches$annotation$caption
             )
    )
  }
  
}
