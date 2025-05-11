require(data.table)
require(here)
require(tidyverse)
require(cmdstanr)

source(here::here("src", "standardise.R"))

# import data ==================================================================

data.dir <- here("data", "derived")
out.dir <- here("outputs")

model_compiled <- cmdstan_model(
  here("analyses", "200-model-fit", "temp_precipitation_model.stan")
)

file <- file.path(data.dir, 
                  'crop_temp_precipitation.csv')
dt <- as.data.table(read.csv(file)) %>%
  mutate(Crop = as.factor(Crop))
  
# data handling ================================================================

stan_data <- list(
  N = nrow(dt),
  K = nlevels(dt$Crop),
  y = dt$Yield,
  crop = as.integer(dt$Crop),
  temp = dt$Avg.Temp,
  precipitation = dt$Annual.precipitation
)

# model fit ====================================================================

model_fit <- model_compiled$sample(
  data = stan_data,
  seed = 42,
  chains = 2,
  parallel_chains = 2,
  iter_warmup = 500,
  iter_sampling = 1000,
  refresh = 500, # print update every 500 iters,
  save_warmup = TRUE,
  adapt_delta = 0.99
)

model_fit$save_object(
  file = file.path(out.dir, "200-model-fit", "temp_precipitation_model.rds")
)
