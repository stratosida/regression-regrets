# tidy function for mfp output

tidy.mfp <- function(x, conf.int = FALSE, conf.level = 0.95, ...) {
  
  #warn_on_subclass(x, "tidy")
  
  ret <- as_tibble(summary(x)$coefficients, rownames = "term")
  colnames(ret) <- c("term", "estimate", "std.error", "statistic", "p.value")
  
  # summary(x)$coefficients misses rank deficient rows (i.e. coefs that
  # summary.lm() sets to NA), catch them here and add them back
  coefs <- tibble::enframe(stats::coef(x), name = "term", value = "estimate")
  coefs$term <- ret$term
  ret <- left_join(coefs, ret, by = c("term", "estimate"))
  
  #if (conf.int) {
  if (FALSE) {
    ci <- broom_confint_terms(x, level = conf.level)
    ret <- dplyr::left_join(ret, ci, by = "term")
  }
  
  ret
}