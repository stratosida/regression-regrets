
# COMMENT @Mark: is there some way to print the spearman correlation coefficients (spear_sex) into the graphs or into the column labels (in a standardized way)??
# COMMENT @Mark: can we display inverse pseudolog (back-transformed) y-axis labels for better interpretation? see function inv_pseudo_log() in ida_trans.R


# spear_sex<-by(c_bact[,c("AGE",predictor)], c_bact$GENDER, FUN=function(X) data = cor(X,use="pairwise.complete.obs",method="spearman")[1,2])
# cat("\n\nSpearman correlation coefficients of AGE with ", predictor, ":\n", paste(c("male", "female"), round(spear_sex,3), sep=":" ))


plot_assoc_by <- function(dat){
  
  spear_sex <- by(dat[,c("AGE", "AVAL")], dat$SEXC, 
                  FUN=function(X) data = cor(X, use="pairwise.complete.obs", method="spearman")[1,2])
  
  
  
  correlate <- dat |>
    group_by(SEXC) |>
    tidyr::drop_na(AGE, AVAL) |>
    summarise(r = cor(AGE, AVAL, use="pairwise.complete.obs", method="spearman"))
  

  cat(spear_sex, " ", correlate[[1]], "\n")
  
  
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
    geom_point(alpha = 0.4, colour = "firebrick2", size = 0.3) +
    stat_cor(method = "spearman") +
    facet_wrap(~ SEXC) +
    labs(x = attr(dat$AGE, "label"),
         y = xaxis)  + 
    theme_light()

  return(gg)  
  }


