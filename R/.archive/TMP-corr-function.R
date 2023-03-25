
library(correlation)

corrd_sp |> glimpse()

str(corrd_sp)

corrd_sp[,2]


library(corrr)

d <- 
  wide |> 
  select(-USUBJID) |>
  correlate(quiet = TRUE)

d |> stretch()




for (i in 1:49) {
  for (j in (i+1):50) {
    if (abs(corrd[i , j]) > 0.1) {
      
      print(paste0(i, " ", j, " ", corrd[i , j]))
      print(paste0(attr(corrd, "dimnames")[[1]][i], " vs ", attr(corrd, "dimnames")[[2]][j]))
      
      x_lab <- attr(corrd, "dimnames")[[1]][i]
      y_lab <- attr(corrd, "dimnames")[[2]][j]
      
      dat <- 
        wide |>
        select(all_of(c(x_lab, y_lab))) |>
        tidyr::drop_na()
      
      print(dat |>
        ggplot(mapping = aes(x = dat[[x_lab]], y = dat[[y_lab]])) +
          geom_point())
      
      
      
    }
  }
}




count <- 0

for (i in 1:49) {
  
  for (j in (i+1):50) {
    if (abs(corrd[i , j]) > 0.25) {
      
      count <- count + 1 
        
      print(paste0(i, " ", j, " ", corrd[i , j]))
      print(paste0(attr(corrd, "dimnames")[[1]][i], " vs ", attr(corrd, "dimnames")[[2]][j]))
      
      x_lab <- attr(corrd, "dimnames")[[1]][i]
      y_lab <- attr(corrd, "dimnames")[[2]][j]
      
      dat <- 
        wide |>
        select(all_of(c(x_lab, y_lab))) |>
        tidyr::drop_na()
      
      #print(dat |>
      #  ggplot(mapping = aes(x = dat[[x_lab]], y = dat[[y_lab]])) +
      #    geom_point())
      
      
      
    }
  }
}


count


plot(wide[, 2:10] , pch=20 , cex=1.5 , col="#69b3a2")

ggplot(data = wide, mapping = aes_string(x = wide[[x_lab]], y = wide[[y_lab]])) +
  geom_point()


print(ggplot(data=c_bact, mapping=aes(x=.data[[variables[j]]],y=.data[[variables[jj]]]))+ geom_point(alpha = alpha_value) +
        theme_minimal())


if(abs(corrd[j, jj])>0.1) print(ggplot(data=c_bact, mapping=aes(x=.data[[variables[j]]],y=.data[[variables[jj]]]))+ geom_point(alpha = alpha_value) +
                                theme_minimal())




########################################################################
##
########################################################################


names(dat)

formula <- as.formula(paste(c("~",paste(names(dat), collapse="+")), collapse=""))

formula

red<-Hmisc::redun(formula, data = dat, nk=0, pr=FALSE)
vif<-1/(1-red$rsq1)

cat("\nAvailable sample size:\n", red$n, " (", round(100*red$n/nrow(dat),2), "%)\n")

cat("\nVariance inflation factors:\n")
print(round(vif,2))

cat("\nMultiple R-squared:\n")
print(round(red$rsq1,4))

