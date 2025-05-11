require(data.table)
require(here)
require(ggplot2)

# import data ==================================================================

data.dir <- here("data", "derived")
out.dir <- here("outputs")

file <- file.path(data.dir, 
                  'crop_temp_precipitation.csv')
dt <- as.data.table(read.csv(file)) %>%
  mutate(Crop = as.factor(Crop)) %>%
  rename(OBS_ID = X)

model_fit <- readRDS(
  file.path(out.dir, "200-model-fit", "temp_precipitation_model.rds")
)

# data handling ================================================================

# get posterior predictions for yield
model_posterior <- model_fit$draws(
  variables = "y_pred",
  inc_warmup = FALSE,
  format = "draws_df"
) %>%
  as.data.table() %>%
  melt(id.vars = c('.chain','.iteration','.draw')) %>%
  mutate(
    OBS_ID = as.integer(
      gsub('y_pred\\[([0-9]+)\\]', '\\1', as.character(variable))
    )
  )

# construct 95% posterior predictive interval
model_predictive_interval <-
  model_posterior[,
                  list( summary_value = quantile(value, prob = c(0.025, 0.25, 0.5, 0.75, 0.975)),
                        summary_name = c('q_lower','iqr_lower','median','iqr_upper','q_upper') 
                  ),
                  by = 'OBS_ID'
  ] %>%
  dcast(OBS_ID ~ summary_name, value.var = 'summary_value')

model_pp_summary <- merge(model_predictive_interval, dt, by='OBS_ID')
model_pp_summary[, IN_PPI := Yield >= q_lower & Yield <= q_upper]

# plot predictive interval =====================================================

p <- ggplot(model_pp_summary, aes(x = Year)) +
  geom_boxplot( aes( group = Year,
                     ymin = q_lower,
                     lower = iqr_lower,
                     middle = median,
                     upper = iqr_upper,
                     ymax = q_upper),
                stat = 'identity') +
  geom_point(aes(y = Yield, colour = IN_PPI)) +
  scale_y_log10() + 
  labs(title = "",
       x = "Year",
       y = "Yield",
       colour = 'within\n95% posterior\nprediction\ninterval') +
  facet_wrap(~Crop, ncol = 1) +
  theme_minimal()

ggsave(file = file.path(out.dir, '200-model-fit', 'posterior_pred.png'), 
       p,
       width = 12,
       height = 8,
       dpi = 300)