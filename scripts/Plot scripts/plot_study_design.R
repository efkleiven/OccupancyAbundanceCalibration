
# load libraries
library(dplyr)

# setwd
setwd("H:/UiT/Phd/Felt/Koordinater/Porsanger")

dir()

# read coordinate file
cor <- read.delim("Porsanger.txt", skip=22)

str(cor)

# subset only columns of interest
cor2 <- select(cor, c(lat,lon,name))

unique(cor2$name)
 sitelist <- unique(cor2$name)[c(1,3,7,11,15,20,28,29,36,38,40,43,44,46,49)]       
 
cor3 <- filter(cor2, name %in% sitelist) 
cor3$lat <- as.numeric(cor3$lat)
cor3$lon <- as.numeric(cor3$lon)


library("leaflet")
library("sp")
library("htmlwidgets")
library("webshot")


map <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  setView(lat = 70.35, lng = 24.6, zoom = 8) %>%
  addCircleMarkers(data=cor3, radius=2.5, fillOpacity = 0.5, col="red") %>%
  addMiniMap(zoomLevelFixed = 1)%>%
  addScaleBar(options = scaleBarOptions(maxWidth = 200,imperial=F))

map
