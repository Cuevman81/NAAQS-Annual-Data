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
library(openairmaps)

#Gathering Hourly Sample.Measurement Data for Sites Across the State.
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
getMeta(lat = 34.5, lon = -90.5, returnMap = TRUE)

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

windRose(kmem_met18_20)

#Merging both Air Quality and Met Data.
aq_met_Hernando <- join_all(list(O312_21, Met12_21), by='date', type='left')
aq_met_Hernando <- aq_met_Hernando[!duplicated(aq_met_Hernando$date), ]
colnames(aq_met_Hernando)[which(names(aq_met_Hernando) == "avg_wind_speed_kts")] <- "ws"
colnames(aq_met_Hernando)[which(names(aq_met_Hernando) == "avg_wind_drct")] <- "wd"
colnames(aq_met_Hernando)[which(names(aq_met_Hernando) == "Latitude")] <- "latitude"
colnames(aq_met_Hernando)[which(names(aq_met_Hernando) == "Longitude")] <- "longitude"
write.csv(aq_met_Hernando, "aq_met_Hernando.csv")



aq_met_Hernando <- left_join((O312_21),
                          Met12_21,
                          by = "date"
)


names(aq_met_Hernando)

#Analyzing Stats, Trends, ETC.
aq_met_Hernando$date <- as.POSIXct(aq_met_Hernando$date, format="%Y-%m-%d %H:%M:%S", tz="UTC")
write.csv(aq_met_Hernando, "aq_met_Hernando.csv")

names(aq_met_Hernando)

head(aq_met_Hernando)

#Calendar Plots
calendarPlot(aq_met_Hernando, pollutant = "Sample.Measurement", year = 2020, month = 5)
calendarPlot(aq_met_Hernando, pollutant = "Sample.Measurement", year = 2018, annotate = "ws", main = "Hourly Sample.Measurement")


calendarPlot(aq_met_Hernando,
             pollutant = "Sample.Measurement", year = 2012,
             annotate = "ws",
             breaks = c(0.000, 0.054, 0.071, 0.084, 0.096),
             labels = c("Good", "Moderate", "USG", "Unhealthy"),
             cols = c("green", "yellow", "orange", "red"),
             statistic = "max",
             main = "Hernando Hourly Max Ozone with Wind Direction/Speed Yearly Calendar"
)


# Percentiles
{smoothTrend(aq_met_Hernando, pollutant = "Sample.Measurement", statistic = "percentile", percentile = c(5, 50, 95), lwd = c(1, 2, 1), lty = c(5, 1, 5))}

percentileRose(aq_met_Hernando, pollutant = "Sample.Measurement")

percentileRose(aq_met_Hernando, 
               pollutant = c("Sample.Measurement", "Sample.Measurement"),
               percentile = 95, method = "cpf", col = "darkorange",
               layout = c(1, 1))

#Plots

timePlot(selectByDate(aq_met_Hernando, year = 2018, month = "aug"),
         pollutant = c("NONOxNOy", "VOC", "Sample.Measurement"),
         y.relation = "free")

timePlot(aq_met_Hernando, 
         pollutant = c("NONOxNOy", "Sample.Measurement"),
         avg.time = "year", normalise = "1/1/2018", 
         lwd = 4, lty = 1,
         group = TRUE, ylim = c(0, 120))

timePlot(selectByDate(aq_met_Hernando, year = 2018, month = 4),
         pollutant = c("Sample.Measurement", "NONOxNOy"),
         y.relation = "free")

timePlot(head(aq_met_Hernando, 48), pollutant = c("Sample.Measurement"), 
         windflow = list(scale = 0.1, lwd = 2, 
                         col = "turquoise4"), 
         lwd = 3, group = FALSE, 
         ylab = "concentration (ppm)")

ggplot(aq_met_Hernando[aq_met_Hernando$Parameter.Name =="Ozone",], aes(x = date, y = Sample.Measurement)) +
  geom_point(aes(color = Sample.Measurement), size = 2, col = "blue") +
  geom_smooth() +
  geom_point(aes(color = ifelse(Sample.Measurement>0.054, "forestgreen", "darkgoldenrod1"))) +
  scale_colour_manual(labels = c("Good", "Moderate"), values=c("forestgreen","darkgoldenrod1")) +
  labs(title="DeSoto County 10 Year Hourly Ozone Trends") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (aq_met_Hernando[aq_met_Hernando$County.Code == "33",])

#Polar Plot Frequencies
glimpse(aq_met_Hernando)

polarMap(aq_met_Hernando,
         pollutant = "Sample.Measurement",
         statistic = "max",
         latitude = "latitude",
         longitude = "longitude",
         type = "Parameter.Name",
         main = "Max Hourly Ozone Polar Map"
)

polarPlot(aq_met_Hernando, pollutant = "Sample.Measurement", statistic = "max", col = "jet", 
          key.position = "bottom",
          key.header = "Hourly Max O3 (ppm)", 
          key.footer = NULL)

polarPlot(aq_met_Hernando,
          pollutant = "Sample.Measurement",
          breaks = c(0.000, 0.054, 0.070, 0.085, 0.105, 0.200),
          labels = c("Good", "Moderate", "USG", "Unhealthy", "Very Unhealthy"),
          cols = c("green", "yellow", "orange", "red", "purple"),
          statistic = "max",
          min.bin = 12,
          main = "Daily 8HR Ozone Max Yearly Calendar DeSoto County"
)


polarFreq(aq_met_Hernando, pollutant = "Sample.Measurement", 
          type = "year",
          statistic = "max",
          main = "Daily 8HR Ozone Max Directional Plots by Year DeSoto County",
          min.bin = 9)

polarFreq(aq_met_Hernando, pollutant = "Sample.Measurement", ws.int = 30, 
          statistic = "max",
          offset = 80, trans = FALSE, 
          col = "heat")

polarPlot(aq_met_Hernando,
          poll = c("Max_VOC", "Sample.Measurement"),
          statistic = "robust_slope",
          col = "jet",
          limits = c(0, 1),
          ws_spread = 1.5,
          wd_spread = 10
)


polarPlot(aq_met_Hernando,
          pollutant = "Sample.Measurement",
          statistic = "cpf",
          percentile = 90
)

#Pollution Roses — pollution episodes

pollutionRose(aq_met_Hernando, pollutant = "Sample.Measurement")


pollutionRose(aq_met_Hernando,
              pollutant = "Sample.Measurement",
              type = "date", "wd",
              layout = c(4, 1),
              key.position = "bottom"
)


aq_met_Hernando <- selectRunning(aq_met_Hernando, pollutant = "Sample.Measurement", 
                                     threshold = 0.071,
                                     run.len = 1)
table(aq_met_Hernando$criterion)

pollutionRose(aq_met_Hernando, pollutant = "Sample.Measurement", 
              type = "criterion")



#Scatter Plots
scatterPlot(aq_met_Hernando, x = "NONOxNOy", y = "Sample.Measurement", avg.time = "day", key = FALSE)

scatterPlot(aq_met_Hernando, x = "Sample.Measurement", y = "wd", 
            type = "NONOxNOy", smooth = FALSE,
            linear = TRUE, layout = c(2, 2))

scatterPlot(selectByDate(aq_met_Hernando, year = 2018, month = 7), 
            x = "date", y = "Sample.Measurement",
            z = "wd")

scatterPlot(aq_met_Hernando, x = "wd", y = "Sample.Measurement", method = "hexbin", col= "jet")

plot( factor(aq_met_Hernando$County.Name), aq_met_Hernando$Sample.Measurement)

corPlot(aq_met_Hernando.csv, type = "season")

scatterPlot(selectByDate(aq_met_Hernando, start = "18/7/2018", 
                         end = "20/07/2018"), 
            x = "date", y = "NONOxNOy", z = "Sample.Measurement", 
            col = "increment", 
            windflow = list(scale = 0.15), 
            key.footer = "Sample.Measurement\n (ppm)", 
            main = NULL, ylab = "NONOxNOy (ppm)")


#Trends
trendLevel(aq_met_Hernando, pollutant = "Sample.Measurement", 
           layout = c(5, 2),
           cols = "increment")

trendLevel(aq_met_Hernando, pollutant = "Sample.Measurement", y = "wd", 
           border = "white",
           statistic = "max",
           cols = "jet")

trendLevel(aq_met_Hernando, pollutant = "Sample.Measurement",
           x = "week",
           border = "white",  statistic = "max",
           breaks = c(0, 0.054, 0.070, 0.085, 0.105, 0.200),
           labels = c("good", "moderate", "USG", "Unhealthy", "Very Unhealthy"),
           cols = c("forestgreen", "yellow", "orange", "red", "purple"),
           key.position = "top")

# Time Variation in the median, 25/75th and 5/95th quantile values for PM10. The shading shows the extent to the 25/75th and 5/95th quantiles.
timeVariation(aq_met_Hernando, pollutant = "Sample.Measurement",
              statistic = "median",
              col = "firebrick")

timeVariation(aq_met_Hernando, 
              pollutant = c("Sample.Measurement"), 
              normalise = TRUE, main = "10 Year 2012-2021 Time Variation for Hourly Sample.Measurement, DeSoto County")


#Thresholds

aq_met_Hernando <- selectRunning(aq_met_Hernando, pollutant = "Sample.Measurement", 
                                     threshold = 71, 
                                     run.len = 8)




smoothTrend(hNOx, pollutant = "Sample.Measurement", ylab = "concentration (ppm)",
            main = "10 Year Hourly Sample.Measurement Trends")
