#!/usr/bin/env Rscript

options(conflicts.policy = NULL)

library(tibble)
library(dplyr)
library(purrr)
library(tidyr)
library(modelr)
library(progress)
library(usethis)
library(urbankfs)

# Number of samples to bootstrap
n_boot <- 500
set.seed(20190426)

data_orig <- file.path("extdata", "AllCities_Victoria_RDS.csv") %>%
  read.csv() %>%
  as_tibble()

type_df <- as_tibble(soil_types())

soil_vars <- paste0("Percent_", c("Sand", "Silt", "Clay"))

data_structure <- data_orig %>%
  filter_at(c("Unsaturated_K2cm_cmhr", "Percent_Sand", "Type"),
            negate(is.na)) %>%
  select(-Top_Type) %>%
  mutate(Type = as.character(Type)) %>%
  left_join(type_df, by = "Type") %>%
  mutate(Top_Type = factor(Top_Type, soil_type_levels())) %>%
  normalize_soil_pct_data()
  
data_list <- data_structure %>%
  crossv_mc(n_boot) %>%
  mutate_at(c("train", "test"), list(df = ~map(., as_tibble)))

fun_list <- list(
  ann = fit_jian_ann,
  rf1 = partial(fit_jian_rf, top_type = FALSE),
  rf2 = partial(fit_jian_rf, top_type = TRUE)
)

fun_df <- enframe(fun_list, "model", "fun")
data_funs <- crossing(data_list, fun_df) %>%
  select(sample = .id, train_data = train_df, test_data = test_df,
         model_type = model, fit_fun = fun)

pb <- progress_bar$new(total = nrow(data_funs))

# Save these in extdata for use in downstream analyses
fitted_models <- data_funs %>%
  mutate(model_fit = map2(train_data, fit_fun, ~with_pb(.y, pb)(.x)))
save(fitted_models, file = "extdata/fitted_models.rda", compress = "xz")
if (requireNamespace("fs", quietly = TRUE)) fs::file_size("extdata/fitted_models.rda")

# Store the first 100 runs locally inside the package
fitted_models_full <- fitted_models
fitted_models <- fitted_models_full %>%
  filter(as.numeric(sample) <= 100) %>%
  mutate(model_fit = modify_if(model_fit, ~inherits(., "randomForest"), shrink_randomforest))
use_data(fitted_models, compress = "xz")
if (requireNamespace("fs", quietly = TRUE)) fs::file_size("data/fitted_models.rda")
