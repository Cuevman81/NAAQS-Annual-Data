library(expss)
library(VIF)
library(forecast)
library(Metrics)
library(DMwR2)
library(dplyr)
library(readr)
library(maps)
library(ggplot2)

#Daily PM2.5 3 Year Data Trends for Sites Across the State of Mississippi
download.file("https://aqs.epa.gov/aqsweb/airdata/daily_88101_2019.zip",
              destfile="Data/daily_88101_2019.zip")
unzip("Data/daily_88101_2019.zip")
p = read.csv("daily_88101_2019.csv")

download.file("https://aqs.epa.gov/aqsweb/airdata/daily_88101_2020.zip",
              destfile="Data/daily_88101_2020.zip")
unzip("Data/daily_88101_2020.zip")
p = read.csv("daily_88101_2020.csv")

download.file("https://aqs.epa.gov/aqsweb/airdata/daily_88101_2021.zip",
              destfile="Data/daily_88101_2021.zip")
unzip("Data/daily_88101_2021.zip")
p = read.csv("daily_88101_2021.csv")

p2 <- list.files(path = "Data", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 
write.csv(p2, "Data/19_21.csv")

p2 = read.csv("Data/19_21.csv")

p2 <- select(filter(p2, Pollutant.Standard == "PM25 24-hour 2012", Sample.Duration == "24-HR BLK AVG"),c(Sample.Duration,Date.Local,Arithmetic.Mean,Local.Site.Name,Latitude,Longitude,State.Name))
write.csv(p2, file="PM25Daily.csv")

names(p2)

p2 = p2[p2$State.Name == "Mississippi",]

head(p2)

p2$Date.Local = as.Date(p2$Date.Local)

plot( factor(p2$Local.Site.Name), p2$Arithmetic.Mean )

unique(p2$Local.Site.Name)


# Linear trends an summaries
ggplot( p2[p2$Local.Site.Name =="Cleveland Delta State",], aes(x = Date.Local, y = Arithmetic.Mean)) +
  geom_point(aes(color = Arithmetic.Mean), size = 2) +
  geom_smooth() +
  labs(title="Cleveland Daily 24 Hour Average PM2.5, 3 Year Trends") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (p2[p2$Local.Site.Name == "Cleveland Delta State",])

ggplot( p2[p2$Local.Site.Name =="Gulfport Youth Court",], aes(x = Date.Local, y = Arithmetic.Mean)) +
  geom_point(aes(color = Arithmetic.Mean), size = 2) +
  geom_smooth() +
  labs(title="Gulfport Daily 24 Hour Average PM2.5, 3 Year Trends") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (p2[p2$Local.Site.Name == "Gulfport Youth Court",])

ggplot( p2[p2$Local.Site.Name =="Hernando",], aes(x = Date.Local, y = Arithmetic.Mean)) +
  geom_point(aes(color = Arithmetic.Mean), size = 2) +
  geom_smooth() +
  labs(title="Hernando Daily 24 Hour Average PM2.5, 3 Year Trends") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (p2[p2$Local.Site.Name == "Hernando",])

ggplot( p2[p2$Local.Site.Name =="Hinds CC",], aes(x = Date.Local, y = Arithmetic.Mean)) +
  geom_point(aes(color = Arithmetic.Mean), size = 2) +
  geom_smooth() +
  labs(title="Hinds CC Daily 24 Hour Average PM2.5, 3 Year Trends") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (p2[p2$Local.Site.Name == "Hinds CC",])

ggplot( p2[p2$Local.Site.Name =="Jackson NCORE",], aes(x = Date.Local, y = Arithmetic.Mean)) +
  geom_point(aes(color = Arithmetic.Mean), size = 2) +
  geom_smooth() +
  labs(title="Jackson NCORE Daily 24 Hour Average PM2.5, 3 Year Trends") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (p2[p2$Local.Site.Name == "Jackson NCORE",])

ggplot( p2[p2$Local.Site.Name =="Pascagoula",], aes(x = Date.Local, y = Arithmetic.Mean)) +
  geom_point(aes(color = Arithmetic.Mean), size = 2) +
  geom_smooth() +
  labs(title="Pascagoula Daily 24 Hour Average PM2.5, 3 Year Trends") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (p2[p2$Local.Site.Name == "Pascagoula",])

ggplot( p2[p2$Local.Site.Name =="Waveland",], aes(x = Date.Local, y = Arithmetic.Mean)) +
  geom_point(aes(color = Arithmetic.Mean), size = 2) +
  geom_smooth() +
  labs(title="Waveland Daily 24 Hour Average PM2.5, 3 Year Trends") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (p2[p2$Local.Site.Name == "Waveland",])
