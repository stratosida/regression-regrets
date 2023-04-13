set.seed(1972)
m_pct <- .5


sel_central <- (model_df_complete$AGE > quantile(model_df_complete$AGE, 0.5-m_pct/2)) & 
  (model_df_complete$AGE < quantile(model_df_complete$AGE, 0.5+m_pct/2)) #needed later


sel_sample <- as.logical(round(runif(dim(model_df_complete)[1]))) # 50% random selection, needed later

pred_complete <- predict(fit_mfp_complete, 
                         newdata = model_df_trans_complete,
                         type = 'response')

y_complete <- fit_mfp_complete$y

pred_central <- predict(fit_mfp_complete, 
                        newdata = model_df_trans_complete[sel_central,],
                        type = 'response')

y_central <- fit_mfp_complete$y[sel_central]

pred_sample <- predict(fit_mfp_complete, 
                       newdata = model_df_trans_complete[sel_sample,],
                       type = 'response')

y_sample <- fit_mfp_complete$y[sel_sample]


r_squared_efron <- function(y, prediction){
  n <- length(y)
  1-(((1/n)*sum((y-prediction)^2))/((1/n)*sum((y-mean(y))^2)))
}

tribble(
  ~data, ~AUC, ~`scaled Brier score`,
  'complete', auc(y_complete, pred_complete) |> as.numeric(), cor(y_complete, pred_complete)^2,
  'central 50%', auc(y_central, pred_central) |> as.numeric(), cor(y_central, pred_central)^2,
  '50% sample', auc(y_sample, pred_sample) |> as.numeric(), cor(y_sample, pred_sample)^2,
) |>
  gt() %>%
  fmt_number(2, decimals = 3) |>
  fmt_number(3, decimals = 5) |>
  gt_theme_538()

p_ex2 <- rbind(
  tibble(
    BC = y_complete,
    prediction = pred_complete,
    model = 'complete data'
  ),
  tibble(
    BC = y_central,
    prediction = pred_central,
    model = 'within IQR (age)'
  ),
  tibble(
    BC = y_sample,
    prediction = pred_sample,
    model = 'random 50% subsample'
  )) |>
  mutate(model = factor(model, levels = c('complete data', 'within IQR (age)', 'random 50% subsample'))) %>%
  ggplot(aes(x = factor(BC), y = prediction, group = BC)) +
  geom_boxplot() + 
  facet_grid(~model) +
  theme_minimal() +
  labs(x = 'BC')

p_ex2
