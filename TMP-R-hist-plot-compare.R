pseudo_log <- function(x, sigma = 1, base = 10){
  return(asinh(x / (2 * sigma)) / log(base))
}



compare_dist_plot <- function(dat, n_bars = 100, bin_width = NULL, sigma = NULL){
  


bin_width <- diff(range(dat$AVAL,na.rm=T))/min(length(unique(dat$AVAL)),100)
sigma = 0.041068411 
#sigma = 2.041040980 
n_bars <- 100


dat <- dat |>
  tidyr::drop_na(AVAL) |>
  mutate(AVAL2 = pseudo_log(AVAL, sigma))

dat_median <- median(dat$AVAL, na.rm = TRUE)
dat_5 <- fivenum(dat$AVAL, na.rm = TRUE)


## expand range 
rng_vals <- scales::expand_range(range(dat$AVAL, na.rm = TRUE), mul = 0.01)

## range of the data 
rng <- range(dat$AVAL)

a <- 
  dat |> 
  ggplot(aes(x = AVAL, y=..density..)) +
  geom_histogram(
    binwidth = bin_width, 
    bins = n_bars, 
    #breaks = brks,
    center = 0,
    alpha = 0.4,
    fill = "firebrick2") +
  scale_x_continuous(
    limit = c(min(dat$AVAL), max(dat$AVAL)),
    breaks = fivenum(dat$AVAL),
    labels = round(fivenum(dat$AVAL),1)) +
  geom_point(data = NULL, aes(x = rng_vals[1], y = 1), color = "transparent", size = 0.1) +
  geom_point(data = NULL, aes(x = rng_vals[2], y = 1), color = "transparent", size = 0.1) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_vline(xintercept = dat_5[1], colour = "red", alpha = 0.2) +
  geom_vline(xintercept = dat_5[2], colour = "red", alpha = 0.2) +
  geom_vline(xintercept = dat_5[3], colour = "red", alpha = 0.2) +
  geom_vline(xintercept = dat_5[4], colour = "red", alpha = 0.2) +
  geom_vline(xintercept = dat_5[5], colour = "red", alpha = 0.2) +
  #theme_void() +
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

x <- dat$AVAL2

if(length(unique(x))<n_bars*2) {
  brks <- unique(x) 
}  else {
  brks1 <- seq(min(x), max(x), length.out=n_bars+1)
  brks2 <- quantile(x, seq(0,1,1/n_bars))
  brks <- (brks2 + 3*brks1)/4
}

b <- 
  dat |> 
  ggplot(aes(x = AVAL2, y=..density..)) +
  geom_histogram(
    #bin_width = bin_width, 
    #n_bars = n_bars, 
    breaks = brks,
                 center = 0,
                 alpha = 0.6,
                 fill = "firebrick2") +
  scale_x_continuous(
    limit = c(min(dat$AVAL2), max(dat$AVAL2)),
    breaks = fivenum(dat$AVAL2),
    labels = round(fivenum(dat$AVAL),1)) +
  geom_point(data = NULL, aes(x = rng_vals[1], y = 1), color = "transparent", size = 0.1) +
  geom_point(data = NULL, aes(x = rng_vals[2], y = 1), color = "transparent", size = 0.1) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_vline(xintercept = dat_5[1], colour = "red", alpha = 0.2) +
  geom_vline(xintercept = dat_5[2], colour = "red", alpha = 0.2) +
  geom_vline(xintercept = dat_5[3], colour = "red", alpha = 0.2) +
  geom_vline(xintercept = dat_5[4], colour = "red", alpha = 0.2) +
  geom_vline(xintercept = dat_5[5], colour = "red", alpha = 0.2) +
  #theme_void() +
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


plt <- a + b

return(plt)
}

