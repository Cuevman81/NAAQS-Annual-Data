library(expss)
library(VIF)
library(forecast)
library(Metrics)
library(DMwR2)
library(dplyr)
library(readr)
library(maps)
library(ggplot2)

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


d <- list.files(path = "Daily", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 
write.csv(d, "12_21.csv")

d = read.csv("12_21.csv")

names(d)

d = d[d$State.Name == "Mississippi",]

head(d)

d$Date.Local = as.Date(d$Date.Local)

plot( factor(d$Local.Site.Name), d$X1st.Max.Value )

unique(d$Local.Site.Name)

ggplot( d[d$Local.Site.Name =="Cleveland Delta State",], aes(x = Date.Local, y = X1st.Max.Value)) +
  geom_point(aes(color = X1st.Max.Value), size = 2) +
  geom_smooth() +
  labs(title="Cleveland Daily 8 Hour Max Ozone 10 Year Trends") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (d[d$Local.Site.Name == "Cleveland Delta State",])

ggplot( d[d$Local.Site.Name =="Gulfport Youth Court",], aes(x = Date.Local, y = X1st.Max.Value)) +
  geom_point(aes(color = X1st.Max.Value), size = 2) +
  geom_smooth() +
  labs(title="Gulfport Daily 8 Hour Max Ozone 10 Year Trends") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (d[d$Local.Site.Name == "Gulfport Youth Court",])

ggplot( d[d$Local.Site.Name =="Hernando",], aes(x = Date.Local, y = X1st.Max.Value)) +
  geom_point(aes(color = X1st.Max.Value), size = 2) +
  geom_smooth() +
  labs(title="Hernando Daily 8 Hour Max Ozone 10 Year Trends") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (d[d$Local.Site.Name == "Hernando",])

ggplot( d[d$Local.Site.Name =="Hinds CC",], aes(x = Date.Local, y = X1st.Max.Value)) +
  geom_point(aes(color = X1st.Max.Value), size = 2) +
  geom_smooth() +
  labs(title="Hinds CC Daily 8 Hour Max Ozone 10 Year Trends") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (d[d$Local.Site.Name == "Hinds CC",])

ggplot( d[d$Local.Site.Name =="Meridian",], aes(x = Date.Local, y = X1st.Max.Value)) +
  geom_point(aes(color = X1st.Max.Value), size = 2) +
  geom_smooth() +
  labs(title="Meridian Daily 8 Hour Max Ozone 10 Year Trends") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (d[d$Local.Site.Name == "Meridian",])

ggplot( d[d$Local.Site.Name =="Jackson NCORE",], aes(x = Date.Local, y = X1st.Max.Value)) +
  geom_point(aes(color = X1st.Max.Value), size = 2) +
  geom_smooth() +
  labs(title="Jackson NCORE Daily 8 Hour Max Ozone 10 Year Trends") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (d[d$Local.Site.Name == "Jackson NCORE",])

ggplot( d[d$Local.Site.Name =="Pascagoula",], aes(x = Date.Local, y = X1st.Max.Value)) +
  geom_point(aes(color = X1st.Max.Value), size = 2) +
  geom_smooth() +
  labs(title="Pascagoula Daily 8 Hour Max Ozone 10 Year Trends") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (d[d$Local.Site.Name == "Pascagoula",])

ggplot( d[d$Local.Site.Name =="TUPELO AIRPORT NEAR OLD NWS OFFICE",], aes(x = Date.Local, y = X1st.Max.Value)) +
  geom_point(aes(color = X1st.Max.Value), size = 2) +
  geom_smooth() +
  labs(title="Tupelo Daily 8 Hour Max Ozone 10 Year Trends") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (d[d$Local.Site.Name == "TUPELO AIRPORT NEAR OLD NWS OFFICE",])

ggplot( d[d$Local.Site.Name =="Waveland",], aes(x = Date.Local, y = X1st.Max.Value)) +
  geom_point(aes(color = X1st.Max.Value), size = 2) +
  geom_smooth() +
  labs(title="Waveland Daily 8 Hour Max Ozone 10 Year Trends") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (d[d$Local.Site.Name == "Waveland",])
