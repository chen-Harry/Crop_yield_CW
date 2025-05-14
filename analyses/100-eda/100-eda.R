# create plots for 100_eda.qmd

require(ggplot2)
require(data.table)
require(here)
require(tidyverse)

data.dir <- here("data", "derived")
out.dir <- here("outputs")


file <- file.path(data.dir, 
                  'crop_temp_precipitation.csv')
dt <- as.data.table(read.csv(file))


p <- ggplot(dt, 
            aes(x = as.integer(Year), 
                y=Yield, 
                group=Crop, 
                colour=Crop)) +
  geom_line() +
  labs(title = "",
       x = "Year",
       y = "Yield") +
  theme_minimal()

ggsave(file = file.path(out.dir, '100-eda', 'annual_yield.png'), 
       p,
       width = 6,
       height = 4,
       dpi = 300)


p <- ggplot(dt, 
            aes(x=Annual.precipitation, 
                y=Yield, 
                group=Crop, 
                colour=Crop)) +
  geom_point() +
  labs(x = "Annual Precipitation (mm)") +
  theme_minimal()

ggsave(file = file.path(out.dir, '100-eda', 'precipitation_yield.png'), 
       p,
       width = 6,
       height = 4,
       dpi = 300)


p <- ggplot(dt, 
            aes(x=Avg.Temp, 
                y=Yield, 
                group=Crop, 
                colour=Crop)) +
  geom_point() +
  labs(x = "Average temperature (Celsius)") +
  theme_minimal()

ggsave(file = file.path(out.dir, '100-eda', 'temp_yield.png'), 
       p,
       width = 6,
       height = 4,
       dpi = 300)


