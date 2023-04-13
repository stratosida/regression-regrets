## TODO align x-axis 
describe_plot <- function(data, num_bins = 200) {
  

dat <- data 

title <- dat |> 
  filter(row_number()==1) |> 
  select(PARAMCD) |> 
  as.character()


n_distinct <- length(unique(dat$AVAL))
n_bins <- min(num_bins, n_distinct)

#cat(title, " ", n_bins, " ", n_distinct, "\n")

## filter on data
## remove missing 
dat <- dat |> 
  tidyr::drop_na(AVAL)

## get bin width 
bw <- .bin_width(dat$AVAL)

## sanity check width is not less than zero
if (bw <= 0) {
  #cat ("bin width is negative, setting to default for ", title)
  bw = 0.5
}

## expand range 
rng_vals <- scales::expand_range(range(dat$AVAL, na.rm = TRUE), mul = 0.01)

## range of the data 
rng <- range(dat$AVAL)

## sanity check lower range not less than zero
if (rng_vals[1] < 0) {
  #cat ("min range is negative, setting to default for ", title)
  rng_vals[1] = 0
}



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

#rng  <- bind_cols(rng, dat_median) 

rng[3] <- median(dat$AVAL)


gg <- dat |>
  ggplot(aes(AVAL)) +
  
  scale_x_continuous(
    breaks = dat_5,
#    breaks = dat_5#,
    labels = scales::label_number(big.mark = ",", scale_cut = scales::cut_long_scale())(dat_5),
    guide = guide_axis(check.overlap = TRUE)
  ) +
  
  geom_point(data = NULL, aes(x = rng_vals[1], y = 1), color = "transparent", size = 0.1) +
  geom_point(data = NULL, aes(x = rng_vals[2], y = 1), color = "transparent", size = 0.1) +
  
  scale_y_continuous(expand = c(0, 0)) +
  {
    if (length(unique(dat$AVAL)) > 2){
      geom_vline(xintercept = dat_median, colour = "black", alpha = 0.6, linewidth = 0.8)
    } 

  } +

  geom_vline(xintercept = dat_5[1], colour = "black", alpha = 0.4, linewidth = 0.6) +
  geom_vline(xintercept = dat_5[2], colour = "black", alpha = 0.4, linewidth = 0.6) +
  geom_vline(xintercept = dat_5[4], colour = "black", alpha = 0.4, linewidth = 0.6) +
  geom_vline(xintercept = dat_5[5], colour = "black", alpha = 0.4, linewidth = 0.6) +
  
  
  
  ## long running 
  ## geom_rug(sides = "b", alpha = 0.2, color = "black", outside = FALSE) +

  geom_histogram(#color = "firebrick2",
    fill = "firebrick2",
    bins = n_bins,
    alpha = 0.8) +
  
  
  labs(xlab = "", ylab = "") +
  ggplot2::ggtitle(title) +

  theme_void(base_size = 16) +
  theme(
    axis.text.x = element_text(
      color = "black",
#      vjust = -2,
      size = 13
    ),
  axis.line.x = element_line(color = "black"),
  axis.ticks.x = element_line(color = "black"),
  axis.ticks.length.x = unit(1, "mm"),
  axis.title = element_blank(),
  plot.margin = margin(1, 1, 3, 1),
  text = element_text(
    #family = "mono", 
    size = 13)
  )


  return(gg)
}


# auto binwidth per Rob Hyndman
# https://stats.stackexchange.com/questions/798/calculating-optimal-number-of-bins-in-a-histogram
.bin_width <- function(x){
  bw <- 2 * IQR(x) / length(x)^(1/3)
  return(bw)
}
