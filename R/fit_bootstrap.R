fit_bootstrap <- function(data_list, fun_list) {

}

if (FALSE) {
  data_list <- modelr::crossv_mc(data_structure, 50) %>%
    dplyr::mutate_at(c("train", "test"), list(df = ~map(., as_tibble)))
  fun_list <- list(ann = ann_fit, rf1 = rf_ssc, rf2 = rf_sscs)
  fun_df <- tibble::enframe(fun_list, "model", "fun")
  data_funs <- tidyr::crossing(data_list, fun_df)

  i <- 3
  y <- data_funs$train[[3]]
  z <- data_funs$fun[[i]](data_funs$train_df[[i]])

  pb <- progress::progress_bar$new(total = nrow(data_funs))
  results <- data_funs %>%
    dplyr::mutate(fit = purrr::map2(train_df, fun, ~.y(.x, .pb = pb)))

  pred_funs <- tibble::tribble(
    ~model, ~pred_fun,
    "ann", ann_predict,
    "rf1", predict,
    "rf2", predict
  )

  pred <- results %>%
    dplyr::left_join(pred_funs, by = "model") %>%
    dplyr::mutate(pred = purrr::pmap(list(pred_fun, fit, test_df), purrr::exec))

  pred$test_df[[1]]

  pred %>%
    dplyr::mutate(compare = purrr::map(test_df, "Unsaturated_K2cm_cmhr")) %>%
    tidyr::unnest(pred, compare) %>%
    group_by(.id, model) %>%
    summarize(corr = cor(pred, compare, method = "spearman")) %>%
    ggplot() +
    aes(x = corr) +
    geom_density() +
    facet_grid(vars(model))

}
