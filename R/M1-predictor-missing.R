# M1 - lab parameters by type


table_pred_missing_by_type <- function(data = ABLB, flag = NULL, tab_title = NA){

  describe_predictor_cont_vars <-
    ADLB |>
    filter(!!sym(flag) == "Y") |>
    select(PARAM, PARAMCD, AVAL) |>
    group_by(PARAM, PARAMCD) |>
    miss_var_summary() |>
    select(-variable) |>
    ungroup()
  
  describe_predictor_cont_vars |>
    select(PARAMCD, PARAM, n_miss, pct_miss) |>
    arrange(-n_miss) |>
    gt() |>
    cols_label(PARAMCD = "Predictor", 
               PARAM  = "Description",
               n_miss = "Missing (count)",
               pct_miss = "Missing (%)") |>
    fmt_number(columns = vars(pct_miss),
               decimals = 1) |>
    tab_header(title = tab_title) |>
    gt_theme_538()  
}

