library(tidyverse)

data_orig <- read.csv("AllCities_Victoria_RDS.csv") %>% as_tibble()

type_df <- tibble(
  Type = c(
    "fine granular structure", "single grain", "medium granular structure",
    "thin and medium plate-like structure", "massive", "medium subangular blocky",
    "medium and fine granular", "coarse granular blocky", "fine subangular blocky",
    "fine and medium granular structure", "medium platy structure",
    "fine and medium subangular blocky", "fine and medium prismatic structure",
    "medium granular and strong", "medium angular blocky", "fine angular structure",
    "medium prismatic parting to moderate medium subangular blocky",
    "medium prismatic structure parting to moderate medium subangular blocky",
    "coarse prismatic", "medium prismatic", "angular blocky",
    "very coarse prismatic structure", "very fine granular structure",
    "coarse subangular blocky" , "fine subangular and angular blocky",
    "very fine and fine subangular blocky", "medium and coarse subangular blocky",
    "subangular blocky", "fine granular structure and weak very fine subangular blocky"
  ),
  Top_Type = c(
    "granular", "single grain", "granular", "platy", "massive",
    "blocky", "granular", "blocky", "blocky", "granular", "platy",
    "blocky", "prismatic", "granular", "blocky", "blocky", "blocky",
    "blocky", "prismatic", "prismatic", "blocky", "prismatic", "granular",
    "blocky", "blocky", "blocky", "blocky", "blocky", "granular"      
  )
)

ann_fit <- function(data, .pb = NULL) {
  if (!is.null(.pb)) .pb$tick()
  neuralnet::neuralnet(
    Unsaturated_K2cm_cmhr_scaled ~ Percent_Sand_scaled + Percent_Silt_scaled + Percent_Clay_scaled,
    data = data,
    hidden = c(5, 3),
    linear.output = TRUE,
    stepmax = 1e6
  )
}

ann_predict <- function(model, data) {
  sdata <- data[, c("Percent_Sand_scaled", "Percent_Silt_scaled", "Percent_Clay_scaled",
                    "Unsaturated_K2cm_cmhr_scaled")]
  neuralnet::compute(model, sdata)[["net.result"]]
}

rf_ssc <- function(data, .pb = NULL) {
  if (!is.null(.pb)) .pb$tick()
  sdata <- data[, c("Percent_Sand", "Percent_Silt", "Percent_Clay", "Unsaturated_K2cm_cmhr")]
  randomForest::randomForest(
    Unsaturated_K2cm_cmhr ~ Percent_Sand + Percent_Silt + Percent_Clay,
    data = sdata,
    ntree = 100,
    mtry = 2,
    importance = TRUE,
    proximity = TRUE
  )
} 

rf_sscs <- function(data, .pb = NULL) {
  if (!is.null(.pb)) .pb$tick()
  sdata <- data[, c("Percent_Sand", "Percent_Silt", "Percent_Clay",
                    "Top_Type", "Unsaturated_K2cm_cmhr")]
  randomForest::randomForest(
    Unsaturated_K2cm_cmhr ~ Percent_Sand + Percent_Silt + Percent_Clay + Top_Type,
    data = sdata,
    ntree = 100,
    mtry = 2,
    importance = TRUE,
    proximity = TRUE
  )
}

myscale <- function(x) {
  hi <- max(x)
  lo <- min(x)
  (x - lo) / (hi - lo)
}

unscale <- function(x, y = x) {
  hi <- max(y)
  lo <- min(y)
  x * (hi - lo) + lo
}

cor_kfs <- function(x, y) cor(x, y[["Unsaturated_K2cm_cmhr"]], method = "spearman")

data_structure <- data_orig %>%
  filter_at(c("Unsaturated_K2cm_cmhr", "Percent_Sand", "Type"),
            negate(is.na)) %>%
  select(-Top_Type) %>%
  mutate(Type = as.character(Type),) %>%
  left_join(type_df, by = "Type") %>%
  mutate(
    Top_Type = factor(Top_Type),
    sum = Percent_Sand + Percent_Silt + Percent_Clay,
    ratio = 100 / sum
  ) %>%
  mutate_at(paste0("Percent_", c("Clay", "Silt", "Sand")), ~.x * ratio) %>%
  mutate_at(
    c(paste0("Percent_", c("Clay", "Silt", "Sand")), "Unsaturated_K2cm_cmhr"),
    list(scaled = myscale)
  )

nrep <- 50
pb <- function() progress::progress_bar$new(total = nrep)

input <- tibble(rep = seq_len(nrep)) %>%
  mutate(
    index = map(rep, ~sample.int(nrow(data_structure), round(0.85 * nrow(data_structure)))),
    train = map(index, ~data_structure[.x,]),
    test = map(index, ~data_structure[-.x,])
  )

# Fit each model. Split these into separate statements so we can
# recover from errors.
fits <- mutate(input, ann_ksat = map(train, ann_fit, .pb = pb()))
fits <- mutate(fits, rf1_ksat = map(train, rf_ssc, .pb = pb()))
fits <- mutate(fits, rf2_ksat = map(train, rf_sscs, .pb = pb()))

preds <- fits %>%
  mutate(
    # Neural network
    pred_ann_train = map2(ann_ksat, train, ann_predict),
    pred_ann_test = map2(ann_ksat, test, ann_predict),
    cor_ann_train = map2_dbl(pred_ann_train, train, cor_kfs),
    cor_ann_test = map2_dbl(pred_ann_test, test, cor_kfs),
    # RF1
    pred_rf1_train = map2(rf1_ksat, train, predict),
    pred_rf1_test = map2(rf1_ksat, test, predict),
    cor_rf1_train = map2_dbl(pred_rf1_train, train, cor_kfs),
    cor_rf1_test = map2_dbl(pred_rf1_test, test, cor_kfs),
    # RF2
    pred_rf2_train = map2(rf2_ksat, train, predict),
    pred_rf2_test = map2(rf2_ksat, test, predict),
    cor_rf2_train = map2_dbl(pred_rf2_train, train, cor_kfs),
    cor_rf2_test = map2_dbl(pred_rf2_test, test, cor_kfs)
  )

preds %>%
  select(starts_with("cor")) %>%
  gather(variable, value) %>%
  separate(variable, c("stat", "model", "dataset")) %>%
  ggplot() +
  aes(x = value) +
  geom_density() +
  facet_grid(model ~ dataset, scales = "free_y") +
  labs(x = "Correlation between prediction and data")

ggsave("~/Desktop/urbankfs_core_fig.png")
