## TODO align x-axis 
describe_plot <- function(data) {
  

## Parameter of interest
#data |> filter(PARAMCD == param) -> dat
dat <- data 

title <- dat |> 
  filter(row_number()==1) |> 
  select(PARAM) |> 
  as.character()

## filter on data
## remove missing 
dat <- data |> 
  tidyr::drop_na(AVAL)

## get bin width 
bw <- .bin_width(dat$AVAL)

## expand range 
rng_vals <- scales::expand_range(range(dat$AVAL, na.rm = TRUE), mul = 0.01)

## range of the data 
rng <- range(dat$AVAL)


# breaks for histogram
## compute n_bars+1 quantiles and divide the range into nbars 
## take a weighted average of the two sequences to compromise between small and wide bins
n_bars <- 100

if(length(unique(dat$AVAL))<n_bars*2) {
  brks <- unique(dat$AVAL) 
}  else {
  brks1 <- seq(min(dat$AVAL), max(dat$AVAL), length.out=n_bars+1)
  brks2 <- quantile(dat$AVAL, seq(0,1,1/n_bars))
  brks <- (brks2 + 3*brks1)/4
}



dat_median <- median(dat$AVAL, na.rm = TRUE)

dat_5 <- fivenum(dat$AVAL, na.rm = TRUE)


gg <- dat |>
  ggplot(aes(AVAL)) +
  geom_histogram(#color = "firebrick2",
    fill = "firebrick2",
    binwidth = bw, alpha = 0.4) +
  scale_x_continuous(
    breaks = rng,
    labels = scales::label_number(big.mark = ",", scale_cut = scales::cut_long_scale())(rng) ## understand this code
  ) +
  geom_point(data = NULL, aes(x = rng_vals[1], y = 1), color = "transparent", size = 0.1) +
  geom_point(data = NULL, aes(x = rng_vals[2], y = 1), color = "transparent", size = 0.1) +
  scale_y_continuous(expand = c(0, 0)) +
  {
    ## do we need the check in here - do it earlier 
    if (length(unique(dat$AVAL)) > 2) 
      geom_vline(xintercept = dat_median)
  } +

  geom_vline(xintercept = dat_5[1], colour = "red", alpha = 0.2) +
  geom_vline(xintercept = dat_5[2], colour = "red", alpha = 0.2) +
#  geom_vline(xintercept = dat_5[3], colour = "red", alpha = 0.2) +
  geom_vline(xintercept = dat_5[4], colour = "red", alpha = 0.2) +
  geom_vline(xintercept = dat_5[5], colour = "red", alpha = 0.2) +
  
#  geom_rug(sides = "b", alpha = 0.2, color = "black", outside = FALSE) +
  
  ggplot2::ggtitle(title) +
  theme_void() +
  theme(
    axis.text.x = element_text(
      color = "black",
      vjust = -2,
      size = 10
    ),
    axis.line.x = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.length.x = unit(1, "mm"),
    plot.margin = margin(1, 1, 3, 1)
    #text = element_text(family = "mono", size = 10)
    )


  return(gg)
}


# auto binwidth per Rob Hyndman
# https://stats.stackexchange.com/questions/798/calculating-optimal-number-of-bins-in-a-histogram
.bin_width <- function(x){
  bw <- 2 * IQR(x) / length(x)^(1/3)
  return(bw)
}
