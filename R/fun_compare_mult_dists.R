
# COMMENT @Mark: is there some way to print the spearman correlation coefficients (spear_sex) into the graphs or into the column labels (in a standardized way)??
# COMMENT @Mark: can we display inverse pseudolog (back-transformed) y-axis labels for better interpretation? see function inv_pseudo_log() in ida_trans.R


# spear_sex<-by(c_bact[,c("AGE",predictor)], c_bact$GENDER, FUN=function(X) data = cor(X,use="pairwise.complete.obs",method="spearman")[1,2])
# cat("\n\nSpearman correlation coefficients of AGE with ", predictor, ":\n", paste(c("male", "female"), round(spear_sex,3), sep=":" ))


## for psudeo log scale plotting see https://scales.r-lib.org/reference/pseudo_log_trans.html
## and https://github.com/r-lib/scales/issues/219
## and https://stackoverflow.com/questions/66886197/pseudo-log-transform-still-removes-0s-from-plot-how-to-avoid

plot_assoc_by <- function(dat){
  
  spear_sex <- by(dat[,c("AGE", "AVAL")], dat$SEXC, 
                  FUN=function(X) data = cor(X, use="pairwise.complete.obs", method="spearman")[1,2])
  
  
  
  correlate <- dat |>
    group_by(SEXC) |>
    tidyr::drop_na(AGE, AVAL) |>
    summarise(r = cor(AGE, AVAL, use="pairwise.complete.obs", method="spearman"))
  
  #cat(spear_sex, " ", correlate$SEXC, correlate$r, "\n")
  
  
  xaxis <- dat |>
    filter(row_number()==1) |>
    select(PARAM) |>
    as.character()

  gg <- 
    dat |>
    tidyr::drop_na(AVAL) |>
    tidyr::drop_na(AGE) |>
    tidyr::drop_na(SEXC) |>
    ggplot(aes(x = AGE, y = AVAL)) +
    geom_point(alpha = 0.4, colour = "firebrick2", size = 0.4) +
    #stat_cor(method = "spearman", p.accuracy = 0.001, r.accuracy = 0.001) +

    
  # working code for psuedo log transform scale plotting  
  #  coord_trans(x = 'log10', ylim = c(10^-12, 10^0)) +
  #  scale_x_continuous(breaks=10^seq(-12,0,2)) +
    
    
    facet_wrap(~ SEXC) +
    labs(x = attr(dat$AGE, "label"),
         y = xaxis)  + 
    theme_bw(base_size = 10) +
    stat_cor(aes(label = after_stat(r.label)), method = "spearman", r.accuracy = 0.001, position = "jitter") 

  return(gg)  
  }


