library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

# latest and first year for which carbon emissions are reported
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  pull(year) %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  pull(year) %>%
  min()
  
# how much larger were carbon emissions in the last year relative to first
carbon1 <- temp_carbon %>%
  filter(year == 1751) %>%
  .$carbon_emissions

carbon2 <- temp_carbon %>%
  filter(year == 2014) %>%
  .$carbon_emissions

carbon2/carbon1

# time series plot of temperature anomaly
p <- temp_carbon %>% filter(year >= 1880) %>%
  ggplot(aes(year, temp_anomaly, color = year)) +
  geom_line()

p

# adding y-axis label
p + ylab("Temperature anomaly (degrees C)") +
  title("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")


# temperature anomaly relative to 20th century mean
temp_carbon %>%
  ggplot(aes(year, temp_anomaly)) +
  geom_line() +
  geom_hline(aes(yintercept = 0), col = "blue") +
  ylab("Temperature anomaly (degrees C)") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue") +
  xlim(c(1880, 2018)) +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018")

# adding layers to include line graphs of ocean and land anomalies
temp_carbon %>%
  ggplot(aes(year, temp_anomaly)) +
  geom_line() +
  geom_line(aes(year, land_anomaly), col = "red") +
  geom_line(aes(year, ocean_anomaly), col = "blue") +
  ylab("Temperature anomaly (degrees C)") +
  xlim(c(1880, 2018)) +
  ggtitle("Temperature anomaly on land and ocean")

# creating a line plot of concentration by year, comparing different gases
greenhouse_gases %>%
  ggplot(aes(year, concentration)) +
  geom_line() +
  facet_grid(gas~., scales = "free") +
  geom_vline(xintercept = 1850) +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

# annual  global carbon emission, 1751-2014
temp_carbon %>%
  ggplot(aes(year, carbon_emissions)) +
  geom_line() +
  ylab("Carbon emissions (metric tons)") +
  ggtitle("Annual global carbon emissions, 1751-2014")

# line plot of carbon emissions against year
temp_carbon %>% 
  ggplot(aes(year, carbon_emissions)) +
  geom_line()

# line plot of historic co2 emissions by year
historic_co2 %>%
  ggplot(aes(year, co2)) +
  geom_line() +
  xlim(-800000, -775000)

# showing how co2 concentration has changed over time
co2_time <- historic_co2 %>%
  ggplot(aes(year, co2, col = source)) +
  geom_line() +
  ggtitle("Atmospheric CO2 concentration, -800,000 BC to today") +
  ylab("co2 (ppmv)")
co2_time

























