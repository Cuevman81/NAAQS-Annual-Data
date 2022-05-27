library(expss)
library(VIF)
library(forecast)
library(Metrics)
library(DMwR2)
library(purrr)
library(dplyr)
library(plyr)
library(readr)
library(maps)
library(ggplot2)
library(openair)
library(lubridate)
library(worldmet)
library(tidyverse)

#Gathering Hourly Ozone Data for Sites Across the State.
download.file("https://aqs.epa.gov/aqsweb/airdata/hourly_44201_2021.zip",
              destfile="data/hourly_44201_2021.zip")
unzip("data/hourly_44201_2021.zip")
h = read.csv("hourly_44201_2021.csv")
h21 = h[h$County.Name == "DeSoto",]
write.csv(h21, "data/hourlyO3/h21.csv")

h12 <- read_csv("Data/HourlyO3/h12.csv")
h13 <- read_csv("Data/HourlyO3/h13.csv")
h14 <- read_csv("Data/HourlyO3/h14.csv")
h15 <- read_csv("Data/HourlyO3/h15.csv")
h16 <- read_csv("Data/HourlyO3/h16.csv")
h17 <- read_csv("Data/HourlyO3/h17.csv")
h18 <- read_csv("Data/HourlyO3/h18.csv")
h19 <- read_csv("Data/HourlyO3/h19.csv")
h20 <- read_csv("Data/HourlyO3/h20.csv")
h21 <- read_csv("Data/HourlyO3/h21.csv")

#Gathering Meteorological Data from ASOS.
getMeta(lat = 35.5, lon = -90.5, returnMap = TRUE)

kmem_met <- importNOAA(code = "723340-13893", year = 2021)
write.csv(kmem_met, "Data/HourlyMet/kmem_met21.csv")

kmem_met12 <- read_csv("Data/HourlyMet/kmem_met12.csv")
kmem_met13 <- read_csv("Data/HourlyMet/kmem_met13.csv")
kmem_met14 <- read_csv("Data/HourlyMet/kmem_met14.csv")
kmem_met15 <- read_csv("Data/HourlyMet/kmem_met15.csv")
kmem_met16 <- read_csv("Data/HourlyMet/kmem_met16.csv")
kmem_met17 <- read_csv("Data/HourlyMet/kmem_met17.csv")
kmem_met18 <- read_csv("Data/HourlyMet/kmem_met18.csv")
kmem_met19 <- read_csv("Data/HourlyMet/kmem_met19.csv")
kmem_met20 <- read_csv("Data/HourlyMet/kmem_met20.csv")
kmem_met21 <- read_csv("Data/HourlyMet/kmem_met21.csv")

#Binding Files.
O312_21 <- list.files(path = "Data/HourlyO3", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 
write.csv(O312_21, "Data/HourlyO3/O312_21.csv")

names(O312_21)[names(O312_21) == "date"] <- "date"
O312_21$date <- as.POSIXct(O312_21$date, format="%Y-%m-%d %H:%M:%S", tz="UTC")

h = read.csv("12_21.csv")

windRose(kglh_met18_20)

#Merging both Air Quality and Met Data.
aq_met_Hernando <- left_join((O312_21),
                          Met12_21,
                          by = "date"
)

write.csv(aq_met_Hernando, "aq_met_Hernando.csv")

names(aq_met_Hernando)

#Analyzing Stats, Trends, ETC.
aq_met_Hernando$date <- as.POSIXct(aq_met_Hernando$date, format="%Y-%m-%d %H:%M:%S", tz="UTC")
write.csv(aq_met_Hernando, "aq_met_Hernando.csv")

names(aq_met_Hernando)

head(aq_met_Hernando)

plot( factor(aq_met_Hernando$County.Name), aq_met_Hernando$Sample.Measurement)

percentileRose(aq_met_Hernando, pollutant = "Sample.Measurement")


calendarPlot(aq_met_Hernando, pollutant = "Sample.Measurement", year = 2020, month = 5)
calendarPlot(aq_met_Hernando, pollutant = "Sample.Measurement", year = 2017, annotate = "wd", main = "monthly mean o3")


scatterPlot(aq_met_Hernando, x = "Sample.Measurement", y = "air_temp", avg.time = "day", key = FALSE)

corPlot(aq_met_Hernando, type = "season")

# Variation in the median, 25/75th and 5/95th quantile values for PM10. The shading shows the extent to the 25/75th and 5/95th quantiles.
timeVariation(aq_met_Hernando, pollutant = "Sample.Measurement",
              statistic = "median",
              col = "firebrick")

timeVariation(aq_met_Hernando, 
              pollutant = c("Sample.Measurement"), 
              normalise = TRUE, main = "10 Year 2012-2021 Time Variation for Hourly Ozone, Hernando MS")


# Percentiles
{smoothTrend(aq_met_Hernando, pollutant = "Sample.Measurement", statistic = "percentile", percentile = c(5, 50, 95), lwd = c(1, 2, 1), lty = c(5, 1, 5))}

#Plots
ggplot( aq_met_Hernando[aq_met_Hernando$County.Name =="DeSoto",], aes(x = date, y = Sample.Measurement)) +
  geom_point(aes(color = Sample.Measurement), size = 2) +
  geom_smooth() +
  labs(title="DeSoto 10 Year Hourly Ozone Trends") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (aq_met_Hernando[aq_met_Hernando$County.Name == "DeSoto",])

smoothTrend(aq_met_Hernando, pollutant = "Sample.Measurement", ylab = "concentration (ppm)",
            main = "10 Year Hourly Ozone Trends")
