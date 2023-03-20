
## assumes the same number of variables but with transformation
key_predictors <- key_predictors |> stringr::str_sort()
key_predictors_02 <- key_predictors_02 |> stringr::str_sort()

# medians of all key predictors, selected variables will be adjusted to these medians in effect plots
model_df_medians_02 <- model_data_02 |>
  select(-USUBJID, -BACTEREMIAN) |>
  summarise_all(median)

lookup <- c(WBC_noNEU_T = "WBC_noNEU", CREA_T = "CREA")
lookup02 <- c(WBC_noNEU = "WBC_noNEU_T", CREA = "CREA_T")

for(predictor in key_predictors){
  
  if(!(predictor %in% key_predictors_02))
  {
    print(predictor)
    
    predictor_02 <- str_remove(predictor, "_T")
    
    ## make a function 
    
    ## bind model data with median of current predictor
    new_data <- bind_cols(
      model_data |> select(USUBJID, all_of(predictor)), 
      model_df_medians |> select(-predictor)) 
    
    
    ## bind model data with median of current predictor
    new_data02 <- bind_cols(
      model_data_02 |> select(USUBJID, all_of(predictor_02)), 
      model_df_medians_02 |> select(-predictor_02)) 
  
    new_data1 <- new_data |>
      left_join(new_data02 |> select(USUBJID, all_of(predictor_02)), by = "USUBJID") |>
      select(-USUBJID) |>
      distinct()
  
    new_data02_1 <- new_data02 |>     
      left_join(new_data |> select(USUBJID, all_of(predictor)), by = "USUBJID") |>
      select(-USUBJID) |>
      distinct()
    
      
    
    # predict on transformed data
    pred_trans <- predict(fit_mfp_complete, newdata = new_data1 , type = 'link', se.fit = TRUE)    
    
    pred_original <- predict(fit_mfp_complete_02, newdata = new_data02_1 , type = 'link', se.fit = TRUE)
    
    
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
      ggplot(aes(x = !!sym(predictor_02), y = yhat, ymin = yhat.lwr, ymax = yhat.upr, color = model, fill = model)) +
      geom_ribbon(alpha = .2, color = NA) +
      geom_line() +
      geom_rug(sides="b") +
      labs(
        y = 'log odds',
        title = 'on original scale',
        x = predictor_02,
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
    
    print(p)
    
    
    
    
  }
}

    
    
      