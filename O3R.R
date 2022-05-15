#Installing or importing all packages

library(expss)
library(VIF)
library(forecast)
library(Metrics)
library(DMwR2)
library(dplyr)
library(maps)
library(ggplot2)
library(aqsr)

myuser <- create_user(email="",
                      key="")


#Ozone Summary

Ozone <- aqs_annualData(aqs_user=myuser,
                        endpoint="byState",
                        state="28",
                        bdate="20200101",
                        edate="20201231",
                        param="44201")

dim(Ozone)

write.csv(Ozone, file='Ozone.csv')

newdataO3 <- select(filter(Ozone, pollutant_standard == "Ozone 8-hour 2015"),c(sample_duration,fourth_max_value,fourth_max_datetime,local_site_name,latitude,longitude))
write.csv(newdataO3, file="Ozone4H.csv")

#Summary Statistics
summary(file = 'Ozone4H.csv', newdataO3$fourth_max_value)

#Date for 4th Highest Ozone Value
ggplot(newdataO3, aes(x = fourth_max_datetime , y = fourth_max_value)) +
  geom_point(aes(color = local_site_name), size = 3) +
  xlab("Date")+ylab("4th Highest Ozone Value") +
  labs(title="Date for 4th Highest Ozone Value") +
  theme(axis.text.x=element_text(angle=70, hjust=1))

#Statewide Map of 4th Highest Ozone Value(PPB)
map(database = 'county','mississippi')
Ozone4H <- read.csv("Ozone4H.csv")
points(x = Ozone4H$longitude, y = Ozone4H$latitude, pch = 19, col = 'blue')
title(main = "Annual 4th Highest Ozone Value (ppm)")
text(Ozone4H$longitude, y = Ozone4H$latitude, labels = Ozone4H$fourth_max_value, pos = 1, col = 'darkgreen', font = 2)

#Daily Ozone Data for Sites Across the State
download.file("https://aqs.epa.gov/aqsweb/airdata/daily_44201_2021.zip",
              destfile="daily_44201_2021.zip")

unzip("daily_44201_2021.zip")
d = read.csv("daily_44201_2021.csv")

names(d)

d = d[d$State.Name == "Mississippi",]

head(d)

d$Date.Local = as.Date(d$Date.Local)

plot( factor(d$Local.Site.Name), d$X1st.Max.Value )

unique(d$Local.Site.Name)

      #Linear trends and Summaries
ggplot( d[d$Local.Site.Name =="Cleveland Delta State",], aes(x = Date.Local, y = X1st.Max.Value)) +
geom_point(aes(color = X1st.Max.Value), size = 2) +
geom_smooth() +
labs(title="Cleveland Daily 8 Hour Max Ozone") +
theme(axis.text.x=element_text(angle=70, hjust=1))
summary (d[d$Local.Site.Name == "Cleveland Delta State",])

ggplot( d[d$Local.Site.Name =="Gulfport Youth Court",], aes(x = Date.Local, y = X1st.Max.Value)) +
  geom_point(aes(color = X1st.Max.Value), size = 2) +
  geom_smooth() +
  labs(title="Gulfport Daily 8 Hour Max Ozone") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (d[d$Local.Site.Name == "Gulfport Youth Court",])

ggplot( d[d$Local.Site.Name =="Hernando",], aes(x = Date.Local, y = X1st.Max.Value)) +
  geom_point(aes(color = X1st.Max.Value), size = 2) +
  geom_smooth() +
  labs(title="Hernando Daily 8 Hour Max Ozone") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (d[d$Local.Site.Name == "Hernando",])

ggplot( d[d$Local.Site.Name =="Hinds CC",], aes(x = Date.Local, y = X1st.Max.Value)) +
  geom_point(aes(color = X1st.Max.Value), size = 2) +
  geom_smooth() +
  labs(title="Hinds CC Daily 8 Hour Max Ozone") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (d[d$Local.Site.Name == "Hinds CC",])

ggplot( d[d$Local.Site.Name =="Meridian",], aes(x = Date.Local, y = X1st.Max.Value)) +
  geom_point(aes(color = X1st.Max.Value), size = 2) +
  geom_smooth() +
  labs(title="Meridian Daily 8 Hour Max Ozone") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (d[d$Local.Site.Name == "Meridian",])

ggplot( d[d$Local.Site.Name =="Jackson NCORE",], aes(x = Date.Local, y = X1st.Max.Value)) +
  geom_point(aes(color = X1st.Max.Value), size = 2) +
  geom_smooth() +
  labs(title="Jackson NCORE Daily 8 Hour Max Ozone") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (d[d$Local.Site.Name == "Jackson NCORE",])

ggplot( d[d$Local.Site.Name =="Pascagoula",], aes(x = Date.Local, y = X1st.Max.Value)) +
  geom_point(aes(color = X1st.Max.Value), size = 2) +
  geom_smooth() +
  labs(title="Pascagoula Daily 8 Hour Max Ozone") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (d[d$Local.Site.Name == "Pascagoula",])

ggplot( d[d$Local.Site.Name =="TUPELO AIRPORT NEAR OLD NWS OFFICE",], aes(x = Date.Local, y = X1st.Max.Value)) +
  geom_point(aes(color = X1st.Max.Value), size = 2) +
  geom_smooth() +
  labs(title="Tupelo Daily 8 Hour Max Ozone") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (d[d$Local.Site.Name == "TUPELO AIRPORT NEAR OLD NWS OFFICE",])

ggplot( d[d$Local.Site.Name =="Waveland",], aes(x = Date.Local, y = X1st.Max.Value)) +
  geom_point(aes(color = X1st.Max.Value), size = 2) +
  geom_smooth() +
  labs(title="Waveland Daily 8 Hour Max Ozone") +
  theme(axis.text.x=element_text(angle=70, hjust=1))
summary (d[d$Local.Site.Name == "Waveland",])
