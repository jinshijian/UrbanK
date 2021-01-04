#!/usr/bin/env Rscript

# For compatibility with R >=3.6
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

data_orig <- file.path("extdata", "AllCities_Victoria_RDS_rock_bd.csv") %>%
  read.csv() %>%
  as_tibble()

type_df <- as_tibble(soil_types())

data_structure <- data_orig %>%
  mutate(ssc = Percent_Sand + Percent_Silt + Percent_Clay) %>%
  filter(ssc > 99 & ssc < 101) %>% 
  select(Percent_Sand, Percent_Silt, Percent_Clay, Percent_Rock_Fragment,
         Unsaturated_K2cm_cmhr, Type) %>%
  filter_at(vars(-Percent_Rock_Fragment), negate(is.na)) %>%
  mutate(Type = as.character(Type)) %>%
  left_join(type_df, by = "Type") %>%
  mutate(Top_Type = factor(Top_Type, soil_type_levels()))

data_list <- data_structure %>%
  crossv_mc(n_boot) %>%
  mutate_at(c("train", "test"), list(df = ~map(., as_tibble)))

fun_list <- list(
  ann = fit_jian_ann,
  annr = partial(fit_jian_ann, use_rock = TRUE),
  rf1 = partial(fit_jian_rf, top_type = FALSE),
  rf1r = partial(fit_jian_rf, use_rock = TRUE, top_type = FALSE),
  rf2 = partial(fit_jian_rf, top_type = TRUE),
  rf2r = partial(fit_jian_rf, use_rock = TRUE, top_type = TRUE)
)

fun_df <- enframe(fun_list, "model", "fun")
data_funs <- data_list %>%
  select(-train, -test) %>%
  crossing(fun_df) %>%
  select(sample = .id, train_data = train_df, test_data = test_df,
         model_type = model, fit_fun = fun)

message("Beginning model fit...")
if (requireNamespace("furrr", quietly = TRUE)) {
  # Fit in parallel
  message("Detected furrr package. Running in parallel.")
  future::plan("multisession")
  fitted_models <- data_funs %>%
    mutate(model_fit = furrr::future_map2(train_data, fit_fun, ~.y(.x),
                                          .progress = TRUE,
                                          .options = furrr::furrr_options(seed = TRUE)))
} else {
  pb <- progress_bar$new(total = nrow(data_funs))
  fitted_models <- data_funs %>%
    mutate(model_fit = map2(train_data, fit_fun, ~with_pb(.y, pb)(.x)))
}

# Save these in extdata for use in downstream analyses
# only save this data if want to update the old runs
message("Saving fitted models.")
save(fitted_models, file = "extdata/fitted_models.rda")
if (requireNamespace("fs", quietly = TRUE)) fs::file_size("extdata/fitted_models.rda")

# Store the first 100 runs locally inside the package
# fitted_models_full <- fitted_models
message("Saving model subset for package")
fitted_models_100 <- fitted_models %>%
  filter(as.numeric(sample) <= 100) %>%
  mutate(model_fit = modify_if(model_fit, ~inherits(., "randomForest"), shrink_randomforest))
use_data(fitted_models_100, compress = "xz")
if (requireNamespace("fs", quietly = TRUE)) fs::file_size("data/fitted_models_100.rda")
