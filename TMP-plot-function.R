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

dat_median <- median(dat$AVAL, na.rm = TRUE)

dat_5 <- fivenum(dat$AVAL, na.rm = TRUE)


#print(dat |>
#  ggplot(aes(AVAL)) +
#  geom_histogram(binwidth = bw) )

p2 <- dat |>
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
    if (length(unique(dat$AVAL)) > 2) geom_vline(xintercept = dat_median)
  } +

  geom_vline(xintercept = dat_5[1], colour = "red", alpha = 0.2) +
  geom_vline(xintercept = dat_5[2], colour = "red", alpha = 0.2) +
  geom_vline(xintercept = dat_5[3], colour = "red", alpha = 0.2) +
  geom_vline(xintercept = dat_5[4], colour = "red", alpha = 0.2) +
  geom_vline(xintercept = dat_5[5], colour = "red", alpha = 0.2) +
  
  geom_rug(sides = "b", alpha = 0.4, color = "firebrick") +

  theme_void() +
  theme(
    axis.text.x = element_text(
      color = "black",
      vjust = -2,
      size = 6
    ),
    axis.line.x = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.length.x = unit(1, "mm"),
    plot.margin = margin(1, 1, 3, 1),
    text = element_text(family = "mono", size = 6))



## strip plot
p1 <-
  ggplot(data=dat,aes(x = AVAL, y = 0)) +
  geom_jitter(
    width = 0.0,
    height = 0.4,
    alpha = 0.2,
    color = "#f8bb87"
  ) +
  geom_rug(sides = "b") +
  scale_x_continuous(
    breaks = rng,
    labels = scales::label_number(big.mark = ",", scale_cut = scales::cut_long_scale())(rng) ## understand this code
  ) +
  theme_void() +
  #  ylab(y_axis) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank()
  )

#p1



## boxplot
p3 <-dat |>
  ggplot(aes(x = AVAL, y = 0)) +
  geom_boxplot(width = 0.5, alpha = 0.4, outlier.colour = "firebrick2", outlier.alpha = 0.3) + #outlier.shape = NA, 
  scale_x_continuous(
    breaks = rng,
    labels = scales::label_number(big.mark = ",", scale_cut = scales::cut_long_scale())(rng)) + 
  geom_point(data = NULL, aes(x = rng_vals[1], y = 1), color = "transparent", size = 0.1) +
  geom_point(data = NULL, aes(x = rng_vals[2], y = 1), color = "transparent", size = 0.1) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_void() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank()
  )



# layout for combined plot
# histogram has more area
layout <- c(patchwork::area(1, 1, 2, 6),
            patchwork::area(2, 1, 6, 6) #, patchwork::area(18, 1, 18, 6)
)

# layout for combined plot
# histogram has more area
#layout <- c(patchwork::area(1, 1, 1, 6),
          #  patchwork::area(2, 1, 5, 6),
#            patchwork::area(6, 1, 6, 6))



## combine plots
gg <- p3 / p2 + # / p1 +
  patchwork::plot_layout(design = layout) +
  patchwork::plot_annotation(title = title)

  return(p2)
  #return(gg)
  #print(gg)
  #gg
}


# auto binwidth per Rob Hyndman
# https://stats.stackexchange.com/questions/798/calculating-optimal-number-of-bins-in-a-histogram
.bin_width <- function(x){
  bw <- 2 * IQR(x) / length(x)^(1/3)
  return(bw)
}



# this function takes each individual plot and
# returns a list of ggplots that have been modified so they can be stacked
align_plots <- function(pltlist) {
  # set all x axis ranges to be the same
  x_range <-
    ggplot2::ggplot_build(pltlist[[1]])$layout$panel_params[[1]]$x.range
  for (i in setdiff(seq_along(pltlist), 1L)) {
    y_range <-
      ggplot2::ggplot_build(pltlist[[i]])$layout$panel_params[[1]]$y.range
    
    pltlist[[i]] <-
      pltlist[[i]] +
      ggplot2::coord_cartesian(
        xlim = x_range,
        ylim = y_range,
        expand = FALSE
      )
  }
  
  # turn plots into grobs and determine number of columns
  plots_grobs <- lapply(pltlist, ggplot2::ggplotGrob)
  ncols <- lapply(plots_grobs, function(x) dim(x)[[2]])
  maxcols <- max(unlist(ncols))
  
  # Function to add more columns to compensate for eg missing legend
  .addcols <- function(x) {
    diffcols <- maxcols - dim(x)[[2]]
    
    if (diffcols > 0) {
      for (i in seq(1:diffcols)) {
        x <- gtable::gtable_add_cols(x, widths = grid::unit(1, "null"), pos = 8)
      }
    }
    
    x
  }
  
  ### TableGrob 1 has 11 columns while the others have only 9 because lacking legend+spacer
  ## => add two columns and then resize
  plots_grobs_xcols <- lapply(plots_grobs, .addcols)
  
  ### assign max length to ensure alignment
  max_width <- do.call(grid::unit.pmax, lapply(plots_grobs_xcols, "[[", "widths"))
  for (i in seq(1, length(plots_grobs_xcols))) {
    plots_grobs_xcols[[i]]$widths <- max_width
  }
  
  xcol_widths <- grid::convertWidth(
    plots_grobs_xcols[[1]]$widths,
    unitTo = "cm",
    valueOnly = FALSE
  )
  grob_widths <- grid::convertWidth(
    plots_grobs[[1]]$widths,
    unitTo = "cm",
    valueOnly = FALSE
  )
  x <- xcol_widths[[4]] - grob_widths[[4]]
  
  plots_grobs_xcols[[1]]$grobs[[13]]$children[[1]]$x <- grid::unit(x, "cm")
  
  plots_grobs_xcols
}
