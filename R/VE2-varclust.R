

#' Cluster variables based on an input correlation matrix
#'
#' @param corrr A corrr correlation object  
#' uses the pattern here https://stat.ethz.ch/R-manual/R-devel/library/stats/html/dist.html
#' @return base plot object
#' @export
#'
#' @examples
ve2_varclust <- function(corrr){
  
corrr_mat <- corrr |> as_matrix()

## calculate the distance matrix 

## Use correlations between variables "as distance"
dd <- as.dist((1 - corrr_mat)/2)

#plot(hclust(dd)) # to see a dendrogram of clustered variables

## perform hierarchical clustering 
hc <- hclust(dd)

## create a dendongram object for reporting  
dend <- as.dendrogram(hc)

# We hang the dendrogram a bit:
dend <- hang.dendrogram(dend,hang_height=0.01)

# reduce the size of the labels:
dend <- assign_values_to_leaves_nodePar(dend, 0.5, "lab.cex")
dend <- set(dend, "labels_cex", 0.7)

# And plot:
par(mar = c(3,3,3,7))

plot(dend,
     main = "Variable clustering",
     xlab="Spearman rank coefficient",
     horiz =  TRUE,  nodePar = list(cex = .005))

}
