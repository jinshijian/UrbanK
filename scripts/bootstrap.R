library(tidyverse)
pkgload::load_all(".", attach_testthat = FALSE)

data_orig <- read.csv("AllCities_Victoria_RDS.csv") %>% as_tibble()

type_df <- as_tibble(soil_types())

data_structure <- data_orig %>%
  filter_at(c("Unsaturated_K2cm_cmhr", "Percent_Sand", "Type"),
            negate(is.na)) %>%
  select(-Top_Type) %>%
  mutate(Type = as.character(Type),) %>%
  left_join(type_df, by = "Type") %>%
  mutate(
    Top_Type = factor(Top_Type, soil_type_levels()),
    sum = Percent_Sand + Percent_Silt + Percent_Clay,
    ratio = 100 / sum
  ) %>%
  mutate_at(paste0("Percent_", c("Clay", "Silt", "Sand")), ~.x * ratio)

data_list <- modelr::crossv_mc(data_structure, 50) %>%
  dplyr::mutate_at(c("train", "test"), list(df = ~map(., as_tibble)))
fun_list <- list(
  ann = fit_jian_ann,
  rf1 = purrr::partial(fit_jian_rf, top_type = FALSE),
  rf2 = purrr::partial(fit_jian_rf, top_type = TRUE)
)
fun_df <- tibble::enframe(fun_list, "model", "fun")
data_funs <- tidyr::crossing(data_list, fun_df)

pb <- progress::progress_bar$new(total = nrow(data_funs))
results <- data_funs %>%
  dplyr::mutate(fit = purrr::map2(train_df, fun, ~with_pb(.y, pb)(.x)))

fitted_models_big <- results %>%
  select(sample = .id, model_type = model, model_fit = fit)
fitted_models <- fitted_models_big %>%
  mutate(model_fit = modify_if(model_fit, ~inherits(., "randomForest"), shrink_randomforest))

# Store models
usethis::use_data(fitted_models, compress = "xz")

##################################################
# Statistical analysis
##################################################

pred <- results %>%
  select(.id, train_df, test_df, model, fit) %>%
  gather(data_type, data, train_df, test_df) %>%
  mutate(
    predicted = purrr::map2(fit, data, predict),
    model = fct_recode(
      model,
      "Neural network" = "ann",
      "RandomForest (no type)" = "rf1",
      "RandomForest (with type)" = "rf2"
    )
  )

pred_fits <- pred %>%
  filter(data_type == "test_df") %>%
  unnest(data, predicted) %>%
  select(.id, model, observed = Unsaturated_K2cm_cmhr, predicted) %>%
  group_by(.id, model) %>%
  summarize(
    fit = list(lm(observed ~ predicted)),
    coefs = list(coefficients(fit)),
    slope = coefs[[2]],
    intercept = coefs[[1]],
    r2 = summary(fit)[["adj.r.squared"]]
  )

obs <- tibble(predicted = modelr::seq_range(data_structure[["Unsaturated_K2cm_cmhr"]], 10))
pred_lm <- pred_fits %>%
  mutate(xpred = list(obs),
         lmpred = map2(fit, xpred, predict)) %>%
  unnest(xpred, lmpred) %>%
  group_by(model, predicted) %>%
  summarize_at(vars(lmpred), list(
    mean = mean,
    sd = sd,
    lo = ~quantile(., 0.1),
    hi = ~quantile(., 0.9)
  ))

pred_summary <- pred %>%
  filter(data_type == "test_df") %>%
  unnest(data, predicted) %>%
  group_by(model, observed = Unsaturated_K2cm_cmhr) %>%
  summarize_at(vars(predicted), list(
    mean = mean,
    sd = sd,
    lo = ~quantile(., 0.1),
    hi = ~quantile(., 0.9),
    n = length
  ))

# Predicted-observed regressions
ggplot() +
  aes(x = predicted, y = mean) +
  geom_point(aes(y = observed, x = mean),
             data = pred_summary,
             size = 0.5) +
  geom_errorbarh(aes(y = observed, xmin = lo, xmax = hi, x = NULL),
                 data = pred_summary,
                 color = "gray40",
                 size = 0.5) +
  geom_ribbon(
    data = pred_lm,
    aes(ymin = lo, ymax = hi, y = NULL),
    alpha = 0.5,
    fill = "blue"
  ) +
  geom_line(aes(y = mean), data = pred_lm) +
  facet_grid(vars(model)) +
  labs(x = expression('Predicted K2' ~ (cm ~ hr^{-1})),
       y = expression('Observed K2' ~ (cm ~ hr^{-1}))) +
  cowplot::theme_cowplot()

# Correlations
pred %>%
  unnest(data, predicted) %>%
  mutate(data_type = fct_inorder(data_type) %>% fct_recode(
    "Training" = "train_df",
    "Testing" = "test_df"
  )) %>%
  group_by(model, .id, data_type) %>%
  summarize(corr = cor(predicted, Unsaturated_K2cm_cmhr, method = "spearman")) %>%
  ggplot() +
  aes(x = corr) +
  geom_density() +
  facet_grid(vars(model), vars(data_type)) +
  labs(x = "Correlation between prediction and data") +
  cowplot::theme_cowplot()

##################################################
# Other plots
##################################################

ggplot(pred_fits) +
  aes(slope = slope, intercept = intercept, group = .id) +
  geom_abline() +
  facet_grid(vars(model))

  select(observed = Unsaturated_K2cm_cmhr, predicted) %>%
  nest() %>%
  mutate(
    fit = map(data, lm, formula = observed ~ predicted),
    coefs = map(fit, coefficients),
    slope = map_dbl(coefs, 2),
    intercept = map_dbl(coefs, 1),
    r2 = map(fit, summary) %>% map_dbl("adj.r.squared"),
    xpred = map(data, "predicted") %>%
      map(modelr::seq_range, n = 10) %>%
      map(~tibble(predicted = .)),
    pred = map2(fit, xpred, predict)
  ) %>%
  unnest(xpred, pred)

pred_summary <- pred %>%
  filter(data_type == "test_df") %>%
  unnest(data, predicted) %>%
  group_by(model, observed = Unsaturated_K2cm_cmhr) %>%
  summarize_at(vars(predicted), list(
    mean = mean,
    sd = sd,
    lo = ~quantile(., 0.1),
    hi = ~quantile(., 0.9),
    n = length
  ))

pred_summary_lm <- pred_summary %>%
  group_by(model) %>%
  nest() %>%
  mutate(
    ## fit = map(data, ~lm(observed ~ mean, data = ., weights = .[["sd"]] ^ -2)),
    ## fit = map(data, ~lm(observed ~ mean, data = .)),
    ## fit = map(data, ~brms::brm(observed ~ brms::me(mean, sd), data = .)),
    xpred = map(data, ~data.frame(mean = modelr::seq_range(.[["mean"]], 20))),
    pred = map2(fit, xpred, predict, se.fit = TRUE),
    pred_mean = map(pred, "fit"),
    pred_se = map(pred, "se.fit"),
    coefs = map(fit, coefficients),
    slope = map_dbl(coefs, 2),
    intercept = map_dbl(coefs, 1),
    positive_intercept = intercept > 0,
    r2 = map(fit, summary) %>% map_dbl("adj.r.squared"),
    p = map_dbl(fit, pvalue),
    label = sprintf("atop(y == %.2f * x %s %.2f, R^2 == %.2f)",
                    slope,
                    if_else(positive_intercept, "+", "-"),
                    abs(intercept), r2)
  )

pred_lm <- pred_summary_lm %>%
  unnest(xpred, pred_mean, pred_se) %>%
  mutate(lo = pred_mean - pred_se,
         hi = pred_mean + pred_se)

pred_lab <- pred_summary_lm %>%
  select(model, label)

ggplot(pred_summary) +
  aes(x = mean, xmin = lo, xmax = hi, y = observed) +
  geom_errorbarh(color = "grey50") +
  geom_point(size = 0.5) +
  geom_abline(linetype = "dashed") +
  geom_ribbon(aes(x = mean, ymin = lo, ymax = hi, y = NULL),
              data = pred_lm,
              fill = "blue", alpha = 0.3, inherit.aes = FALSE) +
  geom_line(aes(x = mean, y = pred_mean), data = pred_lm,
            color = "blue",
            inherit.aes = FALSE) +
  geom_text(aes(x = Inf, y = Inf, label = label),
            data = pred_lab, inherit.aes = FALSE, 
            hjust = 1.2, vjust = 1.2, parse = TRUE) +
  facet_grid(vars(model)) +
  labs(y = "Observed", x = "Predicted") +
  cowplot::theme_cowplot() +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 20))

