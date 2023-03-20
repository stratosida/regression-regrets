
## assumes the same number of variables but with transformation
key_predictors <- key_predictors |> stringr::str_sort()
key_predictors_02 <- key_predictors_02 |> stringr::str_sort()

# medians of all key predictors, selected variables will be adjusted to these medians in effect plots
model_df_medians_02 <- model_data_02 |>
  select(-USUBJID, -BACTEREMIAN) |>
  summarise_all(median)



for(predictor in key_predictors){
  
  if(!(predictor %in% key_predictors_02))
  {
    print(predictor)
    
    predictor_02 <- str_remove(predictor, "_T")
    
    ## make a function 
    
    ## bind model data with median of current predictor
    new_data <- bind_cols(
      model_data |> select(predictor), 
      model_df_medians |> select(-predictor)) |>
      as_tibble() 
    
    ## bind model data with median of current predictor
    new_data02 <- bind_cols(
      model_data_02 |> select(predictor_02), 
      model_df_medians_02 |> select(-predictor_02)) |>
      as_tibble() 
    
        
  }
  
  
  
  
}



    
    pred_trans <- predict(fit_mfp_complete, 
                          newdata = new_data %>%
                            rename(  # is needed so predict finds the variables
                              !!key_predictors_orig[i] := x_orig,
                              !!key_predictors_trans[i] := x_trans), 
                          type = 'link', se.fit = TRUE)
    pred_original <- predict(fit_mfp_complete_notrans, 
                             newdata = new_data %>%
                               rename(
                                 !!key_predictors_orig[i] := x_orig,
                                 !!key_predictors_trans[i] := x_trans), 
                             type = 'link', se.fit = TRUE)
    
    plot_df <- cbind(
      new_data,
      yhat_original = pred_original$fit,
      yhat.lwr_original = pred_original$fit - 1.96*pred_original$se.fit,
      yhat.upr_original = pred_original$fit + 1.96*pred_original$se.fit,
      yhat_trans = pred_trans$fit,
      yhat.lwr_trans = pred_trans$fit - 1.96*pred_trans$se.fit,
      yhat.upr_trans = pred_trans$fit + 1.96*pred_trans$se.fit
    ) %>%
      as_tibble() %>%
      pivot_longer(
        cols = contains('yhat')
      ) %>%
      separate(name, c('var', 'model'), sep = '_') %>%
      pivot_wider(
        names_from = 'var', values_from = 'value'
      ) %>%
      mutate(
        model = case_when(
          model == 'trans' ~ 'pseudo-log transformed',
          model == 'original' ~ 'original scale'
        )
      )
    
    
    p_original <- plot_df %>% 
      ggplot(aes(x = x_orig, y = yhat, ymin = yhat.lwr, ymax = yhat.upr, color = model, fill = model)) +
      geom_ribbon(alpha = .2, color = NA) +
      geom_line() +
      geom_rug(data = fit_mfp_complete_notrans$X %>% as.data.frame, 
               aes_string(x = key_predictors_orig[i]), 
               inherit.aes = FALSE
      ) +
      labs(
        y = 'log odds',
        title = 'on original scale',
        x = key_predictors_orig[i],
        color = 'model with data on',
        fill = 'model with data on'
      ) +
      theme_minimal() +
      scale_color_ptol() +
      scale_fill_ptol()
    
    p_trans <- plot_df %>%
      ggplot(aes(x = x_trans, y = yhat, ymin = yhat.lwr, ymax = yhat.upr, color = model, fill = model)) +
      geom_ribbon(alpha = .2, color = NA) +
      geom_line() +
      geom_rug(data = fit_mfp_complete$X %>% as.data.frame, 
               aes_string(x = key_predictors_trans[i]), 
               inherit.aes = FALSE
      ) +
      labs(
        y = 'log odds',
        title = 'on pseudo-log scale',
        x = key_predictors_trans[i],
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