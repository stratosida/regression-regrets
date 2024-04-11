#' Generate a dendogram of variables clustered by missingness. 
#'
#' @param ADLB - Lab parameter data set
#'
#' @return plot object 
#' @export
#'
#' @examples
m3_dendogram <- function(ADLB){

  ## select predictors and identify the missing values and set to 1
  data_missing <- 
    ADLB |>
    select(SUBJID, PARAMCD, AVAL) |>
    pivot_wider(names_from = PARAMCD, values_from = AVAL, values_fill = NA) |>
    select(-SUBJID) |>
    is.na()*1
  
  
  ## calculate percentage missing 
  perc_miss <- round(apply(data_missing, 2, mean)*100,0)
  
  ## add to column names 
  colnames(data_missing)<- paste(colnames(data_missing)," [",perc_miss,"]",sep="")
  
  ## calculate the distance matrix 
  d <- dist(t(data_missing)^2 / nrow(data_missing) * 100, method = "euclidean")
  
  ## perform hierarchial clustering 
  hc <- hclust(d)


  ## create a dendongram object for reporting  
  dend <- as.dendrogram(hc)
  
  # We hang the dendrogram a bit:
  dend <- hang.dendrogram(dend,hang_height=0.01)
  
  # reduce the size of the labels:
  dend <- assign_values_to_leaves_nodePar(dend, 0.5, "lab.cex")
  dend <- set(dend, "labels_cex", 0.7)
  
  # And plot:
  par(mar = c(3,3,3,6))
  

  plot(dend,
       main = "Clustered variables by percentage observations\ndiscordantly missing [percentage missing]",
       xlab="Percent discordantly missing",
       horiz =  TRUE,  nodePar = list(cex = .004))
  

}