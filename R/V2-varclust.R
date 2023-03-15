library(dendextend)

dat <- dat_wide |> select(-USUBJID)
vc_bact <- Hmisc::varclus(as.matrix(dat))
plot(vc_bact, cex=0.7, hang=0.01)


dat <- dat_wide |> 
  select(-USUBJID) |>
  correlate() 

options(ggrepel.max.overlaps = 100)

dat |> network_plot()

dat |> network_plot(min_cor = .5)



dat <- dat |> as_matrix()


dat

#vc_bact <- Hmisc::varclus(dat)
#plot(vc_bact)

## calculate the distance matrix 
d <- dist(dat, method = "euclidean")
d <- dist((1 - dat)/2, method = "euclidean")
d <- dist(abs(dat), method = "euclidean")

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
par(mar = c(3,3,3,7))


plot(dend,
     main = "Clustered variables by percentage observations discordantly missing [percentage missing]",
     xlab="Percent discordantly missing",
     horiz =  TRUE,  nodePar = list(cex = .005))

