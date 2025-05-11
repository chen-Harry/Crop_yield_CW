# create a data table of crop yield, precipitation, temperature annual data

require(ggplot2)
require(data.table)
require(here)
require(tidyverse)

data.dir <- here("data", "raw")
out.dir <- here("data", "derived")

file <- file.path(data.dir, 
                  'crop', 
                  'Production_Crops_Livestock_E_Europe_NOFLAG.csv')
dt <- as.data.table(read.csv(file))

crops <- c("Wheat", "Barley", "Rape or colza seed")

crop_data <- dt %>%
  filter(Area == "United Kingdom of Great Britain and Northern Ireland",
         Item %in% crops,
         Element == "Yield") %>%
  select(matches("^Y\\d+"), Item) %>% 
  melt(id.vars = "Item", variable.name="Year", value.name="Yield") %>%
  mutate(Year = as.numeric(sub("^Y", "", as.character(Year)))) %>%
  rename(Crop = Item)

file <- file.path(data.dir, 
                  'precipitation', 
                  'average-precipitation-per-year.csv')
dt <- as.data.table(read.csv(file))

precipitation_data <- dt %>%
  filter(Entity == "United Kingdom") 

crop_and_precipitation_data <- merge(crop_data, precipitation_data, by="Year")

file <- file.path(data.dir, 'temp', 'observed-annual-average.csv')
dt <- as.data.table(read.csv(file))

temp_data <- dt %>%
  rename(Year = Category,
         Avg.Temp = Annual.Mean) %>%
  select(-X5.yr.smooth)

crop_temp_precipitation_data <- merge(temp_data, crop_and_precipitation_data, by="Year")

write.csv(crop_temp_precipitation_data, 
          file.path(out.dir, "crop_temp_precipitation.csv"))
