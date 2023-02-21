


main_descriptives <-
  function(x,
           quantiles = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95),
           moments = 4,
           distinct = TRUE,
           most_frequent = 5) {

    ## mb - check issue is related to tibble
    x <- as.data.frame(x)
    
    ran <- c(min(x, na.rm = T), max(x, na.rm = T))

    ran <- c(ran, diff(ran))

    names(ran) <- c("minimum", "maximum", "range")
    if (!is.null(quantiles))
      quan <- quantile(x, quantiles, na.rm = TRUE)
    else
      quan <- NULL
    if (!is.null(moments)) {
      meanx <- mean(x)
      mom <-
        c(
          mean = mean(x, na.rm = T),
          sd = sd(x, na.rm = T),
          skewness = skewness(x, na.rm = T),
          kurtosis = kurtosis(x, na.rm = T)
        )
    } else
      mom <- NULL
    if (distinct)
      dis <- length(unique(x))
    else
      dis <- NULL
    if (!is.null(most_frequent)) {
      tabx <- table(x)
      mostfreq <- tail(sort(tabx), most_frequent)[most_frequent:1]
      concrat <- mostfreq[1] / mean(table(x))
    } else {
      mostfreq <- NULL
      concrat <- NULL
    }
    
    
    
    return (list(
      quantiles = quan,
      range = ran,
      moments = mom,
      distinct_values = dis,
      most_frequent = mostfreq,
      concentration_ratio = concrat
    ))
  }


