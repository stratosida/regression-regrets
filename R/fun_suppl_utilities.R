logreg_summary_plot <- function(model, title = NULL){
  
  
  gg <-
    tibble(BR = model$y,
           p_hat = predict(model, type = 'response')) |>
    mutate(BR = if_else(BR == 1, "Yes", "No")) |>
    ggplot(aes(x = as.factor(BR), y = p_hat, group = BR)) +
    #geom_jitter(alpha = 0.2, color = "red") +
    geom_boxplot(width = .4) +
    theme_minimal(base_size = 10) +
    scale_y_continuous(limits = c(0,1)) +
    coord_flip() +
    labs(
      title = title,
      subtitle = 'Presence of bacteremia',
      y = 'prediction',
      caption = paste0(
        'scaled Brier score = ',
        scaled_brier(model) |> round(3) ,
        '\nAUC = ',
        auc(model$y, predict(model, type = 'response')) |> round(2)
      )
    ) +
    theme(
      axis.title.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    )

  
  if(is.null(title))
    gg <- gg + theme(plot.title = element_blank())
    
  return(gg)
}

gt_model_table <- function(data, title = NA){
  data |>
    gt () |>
    fmt_number(
      2:4,
      decimals = 2
    ) |>
    fmt_scientific(5) |>
    tab_header(title = title) |>
    gt_theme_538()
}

scaled_brier <- function(model){
  cor(model$y, predict(model, type = 'response'))^2
}




get_model_ready_data <- function (flag, complete_cases = TRUE, outcome = TRUE) {
  data <- NULL
  
  data <-
    ADLB |> filter(!!sym(flag) == "Y") |>
    select(USUBJID, PARAMCD, AVAL) |>
    pivot_wider(names_from = "PARAMCD", values_from = "AVAL")
  
  if (outcome) {
    data <-
      data |>
      left_join(ADSL |> select(USUBJID, BACTEREMIAN), by = "USUBJID")
  }
  
  if (complete_cases) {
    data <- data |>
      drop_na()
  }
  
  return(data)
}





plot_model_covariates <- function(data, fit_model, predictor){
  gg <- NULL
  
  # calculate the median vakue for all key predictors, selected variables will be adjusted to these medians in effect plots
  medians <- calc_medians(data)
  
  
  ## bind model data with median of current predictor
  new_data <- bind_cols(
    data |> select(predictor), 
    medians |> select(-predictor)) |>
    as_tibble() 
  
  pred_complete <- predict(fit_model, newdata = new_data, type = 'link', se.fit = TRUE)
  
  plot_df <- cbind(
    new_data,
    yhat = pred_complete$fit,
    yhat.lwr = pred_complete$fit - 1.96*pred_complete$se.fit,
    yhat.upr = pred_complete$fit + 1.96*pred_complete$se.fit
  ) |>
    as_tibble() 
  
  gg <- plot_df |> 
    ggplot(aes(x = !! sym(predictor), y = yhat, ymin = yhat.lwr, ymax = yhat.upr)) +
    geom_ribbon(alpha = .2, color = NA) +
    geom_line() +
    geom_rug(sides="b") +
    labs(
      y = 'log odds',
      x = predictor
    ) +
    theme_minimal(base_size = 10) +
    scale_color_ptol() +
    scale_fill_ptol()
  
  return(gg)
}




# medians of all key predictors, selected variables will be adjusted to these medians in effect plots
calc_medians <- function(data){
  medians <- NULL
  
  medians <- data |>
    select(-USUBJID, -BACTEREMIAN) |>
    summarise_all(median)
  
  return(medians)
  
}

plot_compare_model_predictions <- function(predictor = NULL, 
                                           data_model_trans = NULL, 
                                           data_model_orig = NULL, 
                                           fit_mfp_orig = NULL, 
                                           fit_mfp_trans = NULL ){
  
  #print(predictor)
  
  medians_model_orig <- calc_medians(data_model_orig)
  medians_model_trans <- calc_medians(data_model_trans)
  
  predictor_orig <- str_remove(predictor, "_T")
  
  ## make a function 
  
  ## bind model data with median of current predictor
  new_data <- bind_cols(
    data_model_trans |> select(USUBJID, all_of(predictor)), 
    medians_model_trans |> select(-predictor)) 
  
  ## bind model data with median of current predictor
  new_data02 <- bind_cols(
    data_model_orig |> select(USUBJID, all_of(predictor_orig)), 
    medians_model_orig |> select(-predictor_orig)) 
  
  new_data1 <- new_data |>
    left_join(new_data02 |> select(USUBJID, all_of(predictor_orig)), by = "USUBJID") |>
    select(-USUBJID) |>
    distinct()
  
  new_data02_1 <- new_data02 |>     
    left_join(new_data |> select(USUBJID, all_of(predictor)), by = "USUBJID") |>
    select(-USUBJID) |>
    distinct()
  
  # predict on transformed data
  pred_trans <- predict(fit_mfp_trans, newdata = new_data1 , type = 'link', se.fit = TRUE)    
  
  pred_original <- predict(fit_mfp_orig, newdata = new_data02_1 , type = 'link', se.fit = TRUE)
  
  plot_df_a <- bind_cols(
    new_data1,
    yhat = pred_original$fit,
    yhat.lwr = pred_original$fit - 1.96*pred_original$se.fit,      
    yhat.upr = pred_original$fit + 1.96*pred_original$se.fit) |>
    mutate(model = "pseudo-log transformed")
  
  plot_df_b <- bind_cols(
    new_data02_1,
    yhat = pred_trans$fit,
    yhat.lwr = pred_trans$fit - 1.96*pred_trans$se.fit,
    yhat.upr = pred_trans$fit + 1.96*pred_trans$se.fit) |>
    mutate(model = "original scale")
  
  plot_df <- bind_rows(plot_df_a, plot_df_b)
  
  p_trans <- 
    plot_df |>
    ggplot(aes(x = !!sym(predictor), y = yhat, ymin = yhat.lwr, ymax = yhat.upr, color = model, fill = model)) +
    geom_ribbon(alpha = .2, color = NA) +
    geom_line() +
    geom_rug(sides="b") +
    labs(
      y = 'log odds',
      title = 'on pseudo-log scale',
      x = predictor,
      color = 'model with data on',
      fill = 'model with data on'
    ) +
    theme_minimal() +
    scale_color_ptol() +
    scale_fill_ptol()
  
  
  p_original <- 
    plot_df |>
    ggplot(aes(x = !!sym(predictor_orig), y = yhat, ymin = yhat.lwr, ymax = yhat.upr, color = model, fill = model)) +
    geom_ribbon(alpha = .2, color = NA) +
    geom_line() +
    geom_rug(sides="b") +
    labs(
      y = 'log odds',
      title = 'on original scale',
      x = predictor_orig,
      color = 'model with data on',
      fill = 'model with data on'
    ) +
    theme_minimal() +
    scale_color_ptol() +
    scale_fill_ptol()
  
  p <- p_original + (p_trans +
                       theme(
                         axis.title.y = element_text(color = NA)
                       )) +
    plot_layout(guides = 'collect') +
    plot_annotation(caption = 'adjusted to medians of all other covariates')  & 
    theme(legend.position = 'bottom')
  
  return(p)
  
}


r_squared_efron <- function(y, prediction){
  n <- length(y)
  1-(((1/n)*sum((y-prediction)^2))/((1/n)*sum((y-mean(y))^2)))
}
