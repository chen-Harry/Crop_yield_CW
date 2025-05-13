require(data.table)
require(here)
require(ggplot2)

# import data ==================================================================
data.dir <- here("data", "derived")
out.dir <- here("outputs")

file <- file.path(data.dir, 
                  'crop_temp_precipitation.csv')
dt <- as.data.table(read.csv(file))%>%
      mutate(Crop = as.factor(Crop))

forecast_model <- readRDS(
  file.path(out.dir, "300-forecast", "temp_forecast.rds")
)

effects_model <- readRDS(
  file.path(out.dir, "200-model-fit", "temp_precipitation_model.rds")
)

# data handling ================================================================

n_years_forecast <- 10
all_years <- min(dt$Year):(max(dt$Year) + n_years_forecast)

po1 <- forecast_model$draws(
  variables = c("y_all_pred"),
  inc_warmup = FALSE,
  format = "draws_df"
) %>%
  as.data.table()

y_colnames <- colnames(po1)[which(grepl("^y", colnames(po1)))]
forecast_y_colnames <- tail(y_colnames, n_years_forecast)
po1 <- select(po1, forecast_y_colnames)
setnames(po1, forecast_y_colnames, as.character(tail(all_years, n_years_forecast)))


po2 <- effects_model$draws(
  variables = c("alpha", "beta", "gamma", "sigma"),
  inc_warmup = FALSE,
  format = "draws_df"
) %>%
  as.data.table()


# function =====================================================================

get_post_pred <- function(crop, 
                          forecast_po, 
                          effects_model_po, 
                          confidence=0.68,
                          precipitation_mean_window=10) {
  
  # return confidence interval of yield for crop at forecast years
  
  # select parameters relevant to his crop
  crop_index <- which(levels(dt$Crop) == crop)
  po <- cbind(forecast_po, effects_model_po)
  alpha_col <- colnames(po)[which(grepl("^alpha", colnames(po)))][crop_index]
  beta_col <- colnames(po)[which(grepl("^beta", colnames(po)))][crop_index]
  gamma_col <- colnames(po)[which(grepl("^gamma", colnames(po)))][crop_index]
  
  year_colnames <- colnames(po)[which(grepl("^\\d+", colnames(po)))]
  
  # get mean precipitation of the last n years
  mean_precipitation <- mean(tail(dt$Annual.precipitation, precipitation_mean_window))
  
  po[, (year_colnames) := lapply(.SD,
                                 function(x) 
                                   exp(
                                     get(alpha_col) +
                                     get(beta_col) * x+
                                     get(gamma_col) * mean_precipitation
                                   )
  ),
  .SDcols = year_colnames]
  
  
  cols_to_keep <- c('.iteration', '.chain', '.draw', year_colnames)
  po <- po[, ..cols_to_keep] %>%
    melt(id.vars = c('.chain','.iteration','.draw'))
  
  po_summary <- 
    po[,
       list( summary_value = quantile(value, 
                                      prob = c((1-confidence)/2, 0.5, 0.5 + confidence/2)),
             summary_name = c('q_lower','median','q_upper') 
       ),
       by = 'variable'
    ]
  
  po_summary <- dcast(po_summary,
                      variable ~ summary_name,
                      value.var = "summary_value") %>%
    rename(Year = variable) %>%
    mutate(Year = as.integer(as.character(Year))) %>%
    mutate(Type = "Forecast")
  return(po_summary)
}

get_forecast_plot <- function(crop, po_summary) {
  # return forecast graph for yield given a crop
  
  tmp <- dt[Crop == crop] %>%
    select(Year, Yield) %>%
    rename(median = Yield) %>%
    mutate(Type = "Data", q_lower=NA_real_, q_upper=NA_real_)
  
  tmp <- rbind(tmp, po_summary)
  
  p <- ggplot(tmp, aes(x = Year, y = median, color = Type)) +
    geom_line() +
    geom_ribbon(
      data = tmp[Type == "Forecast"],
      aes(x = Year, ymin = q_lower, ymax = q_upper),
      fill="lightblue", alpha=0.5, inherit.aes = FALSE
    ) +
    geom_point() +
    scale_color_manual(values = c("Data" = "black", "Forecast" = "blue")) +
    labs(
      title = paste0("Yield forecast for ", crop, " with 68% Confidence Interval"),
      x = "Year",
      y = "Yield"
    ) +
    theme_minimal()
  
  return(p)
}

# graph making =================================================================

crop = "Barley"
p <- get_forecast_plot(crop, get_post_pred(crop, po1, po2))
ggsave(file = file.path(out.dir, '300-forecast', 'barley_forecast.png'), 
       p,
       width = 6,
       height = 4,
       dpi = 300)

crop = "Rape or colza seed"
p <- get_forecast_plot(crop, get_post_pred(crop, po1, po2))
ggsave(file = file.path(out.dir, '300-forecast', 'rapeseed_forecast.png'), 
       p,
       width = 6,
       height = 4,
       dpi = 300)

crop = "Wheat"
p <- get_forecast_plot(crop, get_post_pred(crop, po1, po2))
ggsave(file = file.path(out.dir, '300-forecast', 'wheat_forecast.png'), 
       p,
       width = 6,
       height = 4,
       dpi = 300)
