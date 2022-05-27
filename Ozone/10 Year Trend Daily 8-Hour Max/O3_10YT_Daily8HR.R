library(expss)
library(VIF)
library(forecast)
library(Metrics)
library(DMwR2)
library(dplyr)
library(readr)
library(maps)
library(ggplot2)
library(openair)
library(lubridate)

#Daily Ozone Data for Sites Across the State

download.file("https://aqs.epa.gov/aqsweb/airdata/daily_44201_2012.zip",
              destfile="daily_44201_2012.zip")
unzip("daily_44201_2012.zip")
d12 = read.csv("daily_44201_2012.csv")

download.file("https://aqs.epa.gov/aqsweb/airdata/daily_44201_2013.zip",
              destfile="daily_44201_2013.zip")
unzip("daily_44201_2013.zip")
d13 = read.csv("daily_44201_2013.csv")

download.file("https://aqs.epa.gov/aqsweb/airdata/daily_44201_2014.zip",
              destfile="daily_44201_2014.zip")
unzip("daily_44201_2014.zip")
d14 = read.csv("daily_44201_2014.csv")

download.file("https://aqs.epa.gov/aqsweb/airdata/daily_44201_2015.zip",
              destfile="daily_44201_2015.zip")
unzip("daily_44201_2015.zip")
d15 = read.csv("daily_44201_2015.csv")

download.file("https://aqs.epa.gov/aqsweb/airdata/daily_44201_2016.zip",
              destfile="daily_44201_2016.zip")
unzip("daily_44201_2016.zip")
d16 = read.csv("daily_44201_2016.csv")

download.file("https://aqs.epa.gov/aqsweb/airdata/daily_44201_2017.zip",
              destfile="daily_44201_2017.zip")
unzip("daily_44201_2017.zip")
d17 = read.csv("daily_44201_2017.csv")

download.file("https://aqs.epa.gov/aqsweb/airdata/daily_44201_2018.zip",
              destfile="daily_44201_2018.zip")
unzip("daily_44201_2018.zip")
d18 = read.csv("daily_44201_2018.csv")

download.file("https://aqs.epa.gov/aqsweb/airdata/daily_44201_2019.zip",
              destfile="daily_44201_2019.zip")
unzip("daily_44201_2019.zip")
d19 = read.csv("daily_44201_2019.csv")

download.file("https://aqs.epa.gov/aqsweb/airdata/daily_44201_2020.zip",
              destfile="daily_44201_2020.zip")
unzip("daily_44201_2020.zip")
d20 = read.csv("daily_44201_2020.csv")

download.file("https://aqs.epa.gov/aqsweb/airdata/daily_44201_2021.zip",
              destfile="daily_44201_2021.zip")
unzip("daily_44201_2021.zip")
d21 = read.csv("daily_44201_2021.csv")

#Bind CSV files into one file.
d <- list.files(path = "Daily", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 
write.csv(d, "12_21.csv")

d = read.csv("12_21.csv")

names(d)

d = d[d$State.Name == "Mississippi",]
write.csv(d, "d.csv")

head(d)


plot( factor(d$Local.Site.Name), d$X1st.Max.Value )

unique(d$Local.Site.Name)
colnames(d)
names(d)[names(d) == "Date.Local"] <- "date"
d$date = as.Date(d$date)

#Calendar Plots of 8 Hour Color Coded with respect to AQI Scale
calendarPlot(d[d$Local.Site.Name == "Hernando",],
             pollutant = "X1st.Max.Value", year = 2012,
             breaks = c(0.000, 0.054, 0.071, 0.084, 0.096),
             labels = c("Good", "Moderate", "USG", "Unhealthy"),
             cols = c("green", "yellow", "orange", "red"),
             statistic = "max",
)

scatterPlot(d, x = "date", y = "X1st.Max.Value", avg.time = "day", key = FALSE)
scatterPlot(d[d$Local.Site.Name =="Hernando",], x = "date", y = "X1st.Max.Value", avg.time = "day", key = FALSE)


# percentiles
{smoothTrend(d, pollutant = "X1st.Max.Value", statistic = "percentile", percentile = c(5, 50, 95), lwd = c(1, 2, 1), lty = c(5, 1, 5))}
{smoothTrend(d[d$Local.Site.Name =="Hernando",], pollutant = "X1st.Max.Value", statistic = "percentile", percentile = c(5, 50, 95), lwd = c(1, 2, 1), lty = c(5, 1, 5))}


ggplot( d[d$Local.Site.Name =="Cleveland Delta State",], aes(x = date, y = X1st.Max.Value)) +
  geom_point(aes(color = X1st.Max.Value), size = 2) +
  geom_smooth() +
  labs(title="Cleveland Daily 8 Hour Max Ozone 10 Year Trends") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (d[d$Local.Site.Name == "Cleveland Delta State",])

ggplot( d[d$Local.Site.Name =="Gulfport Youth Court",], aes(x = date, y = X1st.Max.Value)) +
  geom_point(aes(color = X1st.Max.Value), size = 2) +
  geom_smooth() +
  labs(title="Gulfport Daily 8 Hour Max Ozone 10 Year Trends") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (d[d$Local.Site.Name == "Gulfport Youth Court",])

ggplot( d[d$Local.Site.Name =="Hernando",], aes(x = date, y = X1st.Max.Value)) +
  geom_point(aes(color = X1st.Max.Value), size = 2) +
  geom_smooth() +
  labs(title="Hernando Daily 8 Hour Max Ozone 10 Year Trends") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (d[d$Local.Site.Name == "Hernando",])

ggplot( d[d$Local.Site.Name =="Hinds CC",], aes(x = date, y = X1st.Max.Value)) +
  geom_point(aes(color = X1st.Max.Value), size = 2) +
  geom_smooth() +
  labs(title="Hinds CC Daily 8 Hour Max Ozone 10 Year Trends") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (d[d$Local.Site.Name == "Hinds CC",])

ggplot( d[d$Local.Site.Name =="Meridian",], aes(x = date, y = X1st.Max.Value)) +
  geom_point(aes(color = X1st.Max.Value), size = 2) +
  geom_smooth() +
  labs(title="Meridian Daily 8 Hour Max Ozone 10 Year Trends") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (d[d$Local.Site.Name == "Meridian",])

ggplot( d[d$Local.Site.Name =="Jackson NCORE",], aes(x = date, y = X1st.Max.Value)) +
  geom_point(aes(color = X1st.Max.Value), size = 2) +
  geom_smooth() +
  labs(title="Jackson NCORE Daily 8 Hour Max Ozone 10 Year Trends") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (d[d$Local.Site.Name == "Jackson NCORE",])

ggplot( d[d$Local.Site.Name =="Pascagoula",], aes(x = date, y = X1st.Max.Value)) +
  geom_point(aes(color = X1st.Max.Value), size = 2) +
  geom_smooth() +
  labs(title="Pascagoula Daily 8 Hour Max Ozone 10 Year Trends") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (d[d$Local.Site.Name == "Pascagoula",])

ggplot( d[d$Local.Site.Name =="TUPELO AIRPORT NEAR OLD NWS OFFICE",], aes(x = date, y = X1st.Max.Value)) +
  geom_point(aes(color = X1st.Max.Value), size = 2) +
  geom_smooth() +
  labs(title="Tupelo Daily 8 Hour Max Ozone 10 Year Trends") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (d[d$Local.Site.Name == "TUPELO AIRPORT NEAR OLD NWS OFFICE",])

ggplot( d[d$Local.Site.Name =="Waveland",], aes(x = date, y = X1st.Max.Value)) +
  geom_point(aes(color = X1st.Max.Value), size = 2) +
  geom_smooth() +
  labs(title="Waveland Daily 8 Hour Max Ozone 10 Year Trends") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (d[d$Local.Site.Name == "Waveland",])
