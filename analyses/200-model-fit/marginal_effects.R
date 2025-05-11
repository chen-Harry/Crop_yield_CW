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

# temp plot ====================================================================
po <- model_fit$draws(
  variables = c('beta_std'),
  inc_warmup = FALSE,
  format = "draws_df"
)

beta_colnames <- colnames(po)[which(grepl("^beta", colnames(po)))]
setnames(po, beta_colnames, levels(dt$Crop))

# exponentiate to find multiplicative increase in median crop yield
po <- po %>%
  mutate(across(-c(.chain, .iteration, .draw), ~exp(.x)))

po <- data.table::melt(as.data.table(po), 
                       id.vars = c('.chain','.iteration','.draw')
)
pos <- 
  po[,
     list( summary_value = quantile(value, prob = c(0.025, 0.5, 0.975)),
           summary_name = c('q_lower','median','q_upper') 
     ),
     by = 'variable'
  ]

pos <- data.table::dcast(pos,
                         variable ~ summary_name,
                         value.var = "summary_value")

p <- ggplot(pos, aes(x =fct_rev(variable), y = median)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = q_lower, ymax = q_upper), width = 0.2) +
  scale_y_log10() + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  labs(
    x = "",
    y = "Effect (multiplicative)",
    title = "Effect of temperature on median yield."
  ) +
  theme_minimal()

ggsave(file = file.path(out.dir, '200-model-fit', 'temp_effects.png'), 
       p,
       width = 6,
       height = 4,
       dpi = 300)


# precipitation plot ===========================================================
po <- model_fit$draws(
  variables = c('gamma_std'),
  inc_warmup = FALSE,
  format = "draws_df"
)

gamma_colnames <- colnames(po)[which(grepl("^gamma", colnames(po)))]
setnames(po, gamma_colnames, levels(dt$Crop))

# exponentiate to find multiplicative increase in median crop yield
po <- po %>%
  mutate(across(-c(.chain, .iteration, .draw), ~exp(.x)))

po <- data.table::melt(as.data.table(po), 
                       id.vars = c('.chain','.iteration','.draw')
)
pos <- 
  po[,
     list( summary_value = quantile(value, prob = c(0.025, 0.5, 0.975)),
           summary_name = c('q_lower','median','q_upper') 
     ),
     by = 'variable'
  ]

pos <- data.table::dcast(pos,
                         variable ~ summary_name,
                         value.var = "summary_value")

p <- ggplot(pos, aes(x =fct_rev(variable), y = median)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = q_lower, ymax = q_upper), width = 0.2) +
  scale_y_log10() + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  labs(
    x = "",
    y = "Effect (multiplicative)",
    title = "Effect of precipitation on median yield."
  ) +
  theme_minimal()

ggsave(file = file.path(out.dir, '200-model-fit', 'precipitation_effects.png'), 
       p,
       width = 6,
       height = 4,
       dpi = 300)

