#' Describe the numerical contents of a long format data set following CDISC BDS data model
#'
#' Inspect distributions with high-resolution histogram, summary of main quantiles
#' (e.g. 1st, 5th, 25th, 50th, 75th, 90th, 99th)  and extremes (e.g. 5 highest and 5   lowest values),
#' mean, first 4 moments (mean, variance/standard deviation, skewness, kurtosis),
#' number of distinct values.
#' Describe the mode of the data and its frequency. Similarly, inspect distributions of transformed variables, if applicable.
#'
#' @param x input data set
#'
#' @return summary statistics per parameter
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#' library(idatools)
#' data(adlbh_baseline)
#' describe_long_dataset(adlbh_baseline)
#'
#' ## plays with tidyverse syntax
#' adlbh_baseline |>
#'   dplyr::filter(PARAMCD == "ANISO") |>
#'   describe_long_dataset()
#'
#' ## by statistic
#' adlbh_baseline |> describe_long_dataset() |> dplyr::filter(STATISTIC  == "n_distinct")
#'
#' ## by parameter
#' adlbh_baseline |> describe_long_dataset() |> dplyr::filter(PARAMCD  == "ANISO")
#'
#' ## by condition
#' adlbh_baseline |>
#'   describe_long_dataset() |>
#'   dplyr::filter(STATISTIC  == "kurtosis" & RESULT > 2)
#'
describe_long_dataset <- function(x){
  
  if (missing(x)) {
    cli::cli_abort("{.arg x} is absent, but must be supplied.")
  }
  
  ## crude way to determine how many rows is missing and fill
  all <- x |>
    tidyr::expand(PARAMCD, USUBJID)
  
  x <- x|>
    dplyr::right_join(all, by = c("USUBJID", "PARAMCD")) |>
    dplyr::ungroup()
  
  
  summary <- x |>
    dplyr::group_by(PARAM, PARAMCD) |>  ## this assumes CDISC data structure
    dplyr::summarise(
      mean = mean(AVAL, na.rm = TRUE),
      mode = .mode_estimate(AVAL),
      sd = sd(AVAL, na.rm = TRUE),
      skewness = e1071::skewness(AVAL, na.rm = TRUE),
      kurtosis = e1071::kurtosis(AVAL, na.rm = TRUE),
      min = min(AVAL, na.rm = TRUE),
      median = median(AVAL, na.rm = TRUE),
      max = max(AVAL, na.rm = TRUE),
      range = max - min,
      n_distinct = dplyr::n_distinct(AVAL, na.rm = TRUE),
      qt_05 = quantile(AVAL, 0.05, na.rm = TRUE),
      qt_10 = quantile(AVAL, 0.1, na.rm = TRUE),
      qt_25 = quantile(AVAL, 0.25, na.rm = TRUE),
      qt_50 = quantile(AVAL, 0.5, na.rm = TRUE),
      qt_75 = quantile(AVAL, 0.75, na.rm = TRUE),
      qt_90 = quantile(AVAL, 0.9, na.rm = TRUE),
      qt_95 = quantile(AVAL, 0.95, na.rm = TRUE),
      .groups = 'drop'
    ) |>
    tidyr::pivot_longer(cols = c(-PARAM, -PARAMCD), names_to = "STATISTIC", values_to = "RESULT")
  
  max_5 <- 
    x |> 
    dplyr::group_by(PARAM, PARAMCD) |>  ## this assumes CDISC data structure
    slice_min(n = 5, order_by = -AVAL) |>
    filter(row_number() <= 5) |> # condition for ties
    select(PARAM, PARAMCD, AVAL) |>
    pivot_longer(cols = c(-PARAM, -PARAMCD), names_to = "STATISTIC", values_to = "RESULT") |>
    mutate(STATISTIC = paste0("max_", row_number())) |>
    ungroup()
  
  min_5 <- 
    x |> 
    dplyr::group_by(PARAM, PARAMCD) |>  ## this assumes CDISC data structure
    slice_min(n = 5, order_by = AVAL) |>
    filter(row_number() <= 5) |> # condition for ties
    select(PARAM, PARAMCD, AVAL) |>
    pivot_longer(cols = c(-PARAM, -PARAMCD), names_to = "STATISTIC", values_to = "RESULT") |>
    mutate(STATISTIC = paste0("min_", row_number())) |>
    ungroup()
  
  summary <- bind_rows(summary, max_5, min_5) 
  
  return(summary)
}



.mode <- function(x) {
  x <- na.omit(x)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

.mode_estimate <- function(x) {
  x <- x[complete.cases(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


.n_extreme_values <- function(data, n = 5){
  data |>
    dplyr::select(SUBJID, PARAM, PARAMCD, AVAL) |>
    dplyr::group_by(PARAM, PARAMCD) |>
    dplyr::mutate(
      bottom_n = dplyr::min_rank(dplyr::desc(AVAL)),
      top_n = dplyr::min_rank(AVAL)) |>
    dplyr::filter(bottom_n <= n |
                    top_n <= n ) |>
    tidyr::pivot_longer(cols = c(-SUBJID, -PARAM, -PARAMCD, -AVAL), names_to = 'statistics', values_to = 'values') |>
    dplyr::filter(values <= n) -> n_extreme_df
  
  return(n_extreme_df)
}


