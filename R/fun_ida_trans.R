pseudo_log <- function(x, sigma = 1, base = 10){
    return(asinh(x / (2 * sigma)) / log(base))
  }

cornorm <- function(x, qn){
  return(cor(x,qn))
}

# equ.marg: how much better a correlation with normal deviates has to be than the original variable
#           to avoid only slight improvements
ida_trans <- function(dat, equ.marg=0.05){

  ## store PARAMCD
  PARAMCD <- dat |> 
    filter(row_number()==1) |> 
    select(PARAMCD) |> 
    as.character()  
  
  ## prepare data - remove missing values
  x <- dat |> 
    select(AVAL) |> 
    drop_na()
  
  ## stire daa vector
  x <- x$AVAL
  
  ## subsample long data frame
  if(length(x) > 5000) 
    x <- sample(x, size=5000, replace=FALSE)
  
  xr <- rank(x)/(length(x)+1)
  qn <-qnorm(xr,0,1)
  
  cn.orig <- cornorm(x, qn)
  
  if (cn.orig > 1 - equ.marg) {
    res <- list(dat = NULL, code = PARAMCD, const=NA, fun=function(x) x)
  } 
  else {
  
#    if(min(x, na.rm=T)>0) interval=c(0,max(x))
#    else interval=c(min(x[x>0]), max(x))
    interval <- c(-10,10)
    
#   topt<-optimize(f=function(const) 1-cornorm(X=pseudo_log(x, sigma=const), qn=qn), interval=interval, tol=0.05)
    topt <- optimize(f = function(const) 1-cornorm(x = pseudo_log(x, sigma = 2**const), qn = qn), interval=interval, tol=0.5)
    
    if(topt$objective>(1- cn.orig - equ.marg)) {
      #cat("Original distribution better than any log.\n")
      res <- list(dat = NULL,  code = PARAMCD, const=NA, fun=function(x) x)
    } else {
      #res<-list(const=topt$minimum, fun=function(x) log(x))
      
      x_t <- pseudo_log(dat$AVAL, sigma = 2**topt$minimum, base=10)
      
      dat$AVAL <- x_t
      dat <- dat |>
        mutate(
          PARAM_DERIVED = PARAMCD, 
          PARAM_TYPE = "DERIVED",
          PARAMCD = paste0(PARAMCD, "_T"),
          PARAM = paste0(PARAM, " : pseudo log transformed"),
          KEY_PRED_FL = "",
          MED_PRED_FL = "",
          REM_PRED_FL = ""
          )
      
      cat(PARAMCD, " transformed : ")
      
      res <- list(dat = dat, code = PARAMCD, const=2**topt$minimum, fun=function(x) log(x))
    } 
  }
  
  return(res)
}

