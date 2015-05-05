#39.2847064,-76.6204859,12z --> google maps of baltimore
setwd("/Users/kylesuess/Box Sync/baltimore")
crime <- read.csv("alldata.csv", header=T) #all crime data - can subset
head(crime)
str(crime)
##########
guns <- read.csv("Gun_Offenders.csv", header=T) #choose gun offenders
str(guns)
##########
cctv <- read.csv("CCTV_Locations.csv", header=T) #choose cctv_locations
str(cctv)
##########
beer <- read.csv("Liquor_Licenses.csv", header=T)
str(beer)
##########
vacant <- read.csv("Vacant_Buildings.csv", header=T)
str(vacant)

library(dplyr)
##########
#CRIME
########## 
arrange(crime, CrimeCode, Description)
murder <- filter(crime, CrimeCode == '1F')
#1F = homicide
nrow(murder)

larcenyauto <- filter(crime, CrimeCode == '6D')
#6D = auto lareceny
nrow(larcenyauto)

larceny <- filter(crime, CrimeCode == '6A')
#6A = larceny (not from auto)
nrow(larceny)

gta <- filter(crime, CrimeCode == '3AJF')
#3AJF = robbery by carjacking
nrow(gta)

assault <- filter(crime, CrimeCode == '4A' | CrimeCode == '4E')
#4A = aggravated assault; 4E = common assault
nrow(assault)

autotheft <- filter(crime, CrimeCode == '7A')
#auto theft cases
nrow(autotheft)

arson <- filter(crime, CrimeCode == '8AO')
#arson cases
nrow(arson)

burglary <- filter(crime, CrimeCode == '5A')
#burglary cases
nrow(burglary)

mugging <- filter(crime, CrimeCode == '3AF')
#street robbery
nrow(mugging)

threat <- filter(crime, CrimeCode == '4F')
#4F = assault by threatening
nrow(threat)

commrob <- filter(crime, CrimeCode == '3CF')
#commercial robbery
nrow(commrob)

library(ggmap)
library(ggplot2)
library(rgdal)
#A static approach:
myMap <- get_map(location="Baltimore", zoom=12)

#murders and cctv cameras
ggmap(myMap, extent="device", legend="bottomleft")+
  geom_point(aes(x=Lon, y=Lat, size=.25), data=cctv, alpha=.8, color="blue")+
  geom_point(aes(x=Lon, y=Lat, size=.25), data=murder, alpha=.8, color="darkred")

#murders and vacant homes
ggmap(myMap, extent="device")+
  geom_point(aes(x=Lon, y=Lat, size=.25), data=vacant, alpha=.8, color="black")+
  geom_point(aes(x=Lon, y=Lat, size=.25), data=murder, alpha=.8, color="darkred")

#murders and gun offenders
ggmap(myMap, extent="device")+
  geom_point(aes(x=Lon, y=Lat, size=.25), data=guns, alpha=.8, color="#774F38")+
  geom_point(aes(x=Lon, y=Lat, size=.25), data=murder, alpha=.8, color="darkred")

#assault and muggings
ggmap(myMap, extent="device")+
  geom_point(aes(x=Lon, y=Lat, size=.25), data=assault, alpha=.8, color="blue")+
  geom_point(aes(x=Lon, y=Lat, size=.25), data=mugging, alpha=.8, color="purple")

#auto theft, auto larceny, car jacking
ggmap(myMap, extent="device")+
  geom_point(aes(x=Lon, y=Lat, size=.25), data=gta, alpha=.8, color="red")+
  geom_point(aes(x=Lon, y=Lat, size=.25), data=autotheft, alpha=.8, color="blue")

#arson
ggmap(myMap, extent="device")+
  geom_point(aes(x=Lon, y=Lat, size=.25), data=arson, alpha=.8, color="red")

#burglary
ggmap(myMap, extent="device")+
  geom_point(aes(x=Lon, y=Lat, size=.25), data=assault, alpha=.8, color="orange")

#commercial robbery
ggmap(myMap, extent="device")+
  geom_point(aes(x=Lon, y=Lat, size=.25), data=commrob, alpha=.8, color="#3A2A3C")

#liquor licenses and assault
ggmap(myMap, extent="device")+
  geom_point(aes(x=Lon, y=Lat, size=.25), data=beer, alpha=.8, color="yellow")+
  geom_point(aes(x=Lon, y=Lat, size=.25), data=murder, alpha=.8, color="darkred")

#A leaflet approach:
#install.packages('devtools')
#devtools::install_github("rstudio/leaflet")
library(leaflet)
#set the basic view for the map
m = leaflet() %>% addTiles() %>% setView(-76.6155, 39.307798, zoom = 15) %>% addCircles(data = murder, lat = ~ Lat, lng = ~ Lon, color = "darkred") %>% addCircles(data = gta, lat = ~ Lat, lng = ~ Lon, color = "#purple") %>% addCircles(data = mugging, lat = ~ Lat, lng = ~ Lon, color = "#black") %>% addCircles(data = cctv, lat = ~ Lat, lng = ~ Lon, color = "#blue") 
m