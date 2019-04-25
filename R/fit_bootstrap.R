fit_bootstrap <- function(data_list, fun_list) {

}

if (FALSE) {

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
