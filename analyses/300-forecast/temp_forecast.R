require(data.table)
require(here)
require(tidyverse)
require(cmdstanr)

source(here::here("src", "standardise.R"))

# import data ==================================================================

data.dir <- here("data", "raw")
out.dir <- here("outputs")

model_compiled <- cmdstan_model(
  here("analyses", "300-forecast", "temp_forecast.stan")
)

file <- file.path(data.dir,
                  'temp',
                  'observed-annual-average.csv')

# extract only the annual temperature data
dt <- as.data.table(read.csv(file)) %>%
  rename(Year = Category,
         Avg.Temp = Annual.Mean) %>%
  select(-X5.yr.smooth)

# data handling ================================================================

n_years_forecast <- 10
all_years <- min(dt$Year):(max(dt$Year) + n_years_forecast)

standardised_years <- standardise(all_years)

stan_data <- list(
  `T` = nrow(dt),
  y = dt$Avg.Temp,
  standardised_t = standardised_years[1:nrow(dt)],
  T_star = n_years_forecast,
  standardised_t_star = tail(standardised_years, n_years_forecast),
  hsgp_c = 1.3,
  hsgp_M = 30
)

# model fit ====================================================================

model_fit <- model_compiled$sample(
  data = stan_data,
  seed = 42,
  chains = 2,
  parallel_chains = 2,
  iter_warmup = 500,
  iter_sampling = 2000,
  refresh = 500, # print update every 500 iters,
  save_warmup = TRUE,
  adapt_delta = 0.99
)

model_fit$save_object(
  file = file.path(out.dir, "300-forecast", "temp_forecast.rds")
)

# model forecast ===============================================================

po <- model_fit$draws(
  variables = c("y_all_pred"),
  inc_warmup = FALSE,
  format = "draws_df"
) 
po <- as.data.table(po)

y_colnames <- colnames(po)[which(grepl("^y_all_pred", colnames(po)))]
setnames(po, y_colnames, as.character(all_years))

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
                         value.var = "summary_value") %>%
  mutate(Year = as.integer(as.character(variable))) %>%
  merge(dt, by="Year", all.x = TRUE)



p <- ggplot(pos, aes(x =Year)) +
  geom_ribbon(aes(ymin = q_lower, ymax = q_upper), fill="lightblue", alpha=0.5) +
  geom_line(aes(y = median)) +
  geom_point(aes(y = Avg.Temp), size = 2, colour="red") +
  scale_x_continuous() +
  labs(
    x = "Year",
    y = "Average Temp (Celsius)",
    title = "Average annual temperature forecast"
  ) +
  theme_minimal()

ggsave(file = file.path(out.dir, '300-forecast', 'temp_forecast.png'), 
       p,
       width = 6,
       height = 4,
       dpi = 300)
