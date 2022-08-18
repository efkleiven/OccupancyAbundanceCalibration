##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PLOT BACKGROUND MAP
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

# load coordinates
library(readr)
library(rgdal)
library(sf)
library(sp)
library(dplyr)

# Koordinaten <- read_table2("C:\\Users\\jonas\\Documents\\Dokumente\\Masterarbeit\\R\\Mapdata\\Koordinaten.txt")## the script can be used to plot a background map of the COAT study areas Komagdalen and Vestre Jakobselv (or other prats of Finnmark and Tromsø)
## maps for 'Troms og Finnmark' need to be downloaded from www.geonorgen.no
## - N100 Map data (https://kartkatalog.geonorge.no/metadata/n100-map-data/11a70876-5b21-4cc6-8229-902266c4968f)
## - N250 Map data (https://kartkatalog.geonorge.no/metadata/n250-map-data/442cae64-b447-478d-b384-545bc1d9ab48)
## - N500 Map data (https://kartkatalog.geonorge.no/metadata/n500-map-data/58e0dbf8-0d47-47c8-8086-107a3fa2dfa4)

## the map of the selected area will be plottet in a new window and can then be used to plot coordinates

## projection -> UTM33 wgs84
## all coordinates have to be transformed to UTM 33 wgs 84

###############

# setwd
setwd("C:/Users/ekl013/OneDrive - UiT Office 365/Phd/Felt/Koordinater/Porsanger")

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

# Setting existing coordinate as lat-long system
cord.dec = SpatialPoints(cbind(cor3$lon, cor3$lat), proj4string=CRS("+proj=longlat"))


# Transforming coordinate to UTM using EPSG=32748 for WGS=84, UTM Zone=48M,
# Southern Hemisphere)
#cord.UTM <- spTransform(cord.dec, CRS("+init=epsg:32633"))
#cord.UTM  ## data used for plotting

#ID<-paste("4"," "," "," "," "," "," "," "," "," "," ","2"," "," "," "," "," "," "," "," "," "," ","1"," "," "," "," "," "," "," "," "," "," ","3"," "," "," "," "," "," "," "," "," "," ",sep = ",")
#ID <- paste("", 1:4, sep = "")

plot(cord.dec, axes = TRUE, main = "UTM Coordinates", col = "red", cex.axis = 0.95)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## Start script
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## load libraries


## path to file geodatabase with maps
fgdb100 = "C:/Eivind/maps/Basisdata_54_Troms_og_Finnmark_25833_N100Kartdata_FGDB.gdb"
fgdb250 = "C:/Eivind/maps/Basisdata_54_Troms_og_Finnmark_25833_N250Kartdata_FGDB.gdb"
fgdb500 = "C:/Eivind/maps/Basisdata_54_Troms_og_Finnmark_25833_N500Kartdata_FGDB.gdb"

## List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list = ogrListLayers(fgdb250)
print(fc_list)

## load map layers
#contour.lines <- st_read(fgdb100, layer = "N100_Høyde_senterlinje")  # contour lines
arealdekke.omrade <- st_read(fgdb250, layer = "N250_Arealdekke_omrade")  # contour lines
#rivers <- st_read(fgdb500, layer = "N500_Arealdekke_senterlinje")  # contour lines
#path <- st_read(fgdb100, layer = "N100_Samferdsel_senterlinje")  # contour lines
#names <- st_read(fgdb250, layer = "N250_Stedsnavn_tekstplassering")  # contour lines
#buildings <- st_read(fgdb250, layer = "N250_BygningerOgAnlegg_posisjon")  # contour lines
#xx <- st_read(fgdb100, layer = "N100_Arealdekke_grense")  # contour lines

areal <- st_transform(arealdekke.omrade, crs=4326)

## set coordinates for the map area
coord.po<-c(xmin = 24.25, ymin = 69.3, xmax = 25.8, ymax = 70.9)  # covers study design Komag -> coordinates can be adjusted
box<-coord.po  # specify which area should be plotted

#chosen_names <- names %>% filter(TextString %in% c("571", "Kjøltindan","403","Gárgas","Ryggfjellet","365","Komagelva"))
sf::sf_use_s2(FALSE)
arealdekke.omrade.2<-st_crop(areal, box)

library(ggplot2)
library(raster) #for scalebar
library(scales) # for transparency
library(ggspatial)
## plot map
# ggplot

ggplot(data = arealdekke.omrade.2$SHAPE[arealdekke.omrade.2$objtype=="Skog"], color="black")+
  geom_sf(color="palegreen3", fill="palegreen3")+
  geom_sf(data = arealdekke.omrade.2$SHAPE[arealdekke.omrade.2$objtype=="Havflate"], color="deepskyblue3", fill="deepskyblue3")+
  geom_point(data = cor3, aes(x = lon, y = lat), color="black", fill=gray(.5), size=6, alpha=0.7 )+
  coord_sf(xlim = c(24.25, 25.8), ylim = c(69.3, 70.9), expand=F, label_graticule = "SE")+
  theme(panel.grid.major = element_blank(), 
        panel.background = element_rect(fill ="papayawhip"),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size=20),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_text(size=20),
        axis.ticks.y=element_blank())+
  scale_y_continuous(position="right", breaks = c(69.5,70,70.5))+
  scale_x_continuous(breaks = c(24.5,25,25.5))+
  annotation_scale(
    location = "bl",
    height = 	unit(0.75, "cm"),
    bar_cols = c("grey60", "white"),
    text_cex =1.5,
    text_face = "bold",
    pad_x = unit(1, "cm"),
    pad_y = unit(1, "cm"),
  ) +
  annotation_north_arrow(
    location = "bl", which_north = "true",
    height=unit(1.5,"cm"), width=unit(1.5,"cm"),
    pad_x = unit(0.5, "in"), pad_y = unit(0.9, "in"),
    style = ggspatial::north_arrow_orienteering(
      line_width = 0.2,
      line_col = "black",
      fill = c("white", "black"),
      text_col = "black",
      text_family = "",
      text_face = "bold",
      text_size = 15,
      text_angle = 0
    )
  )

ggsave("ggmap_porsanger.png", width = 15.2, height=40, units = "cm")
  
setwd("C:/Users/ekl013/OneDrive - UiT Office 365/GitProjects/OccupancyAbundanceCalibration/plots")
png(filename="map_porsanger.png", width = 480*2, height = 480*4, units="px")
par(mar=c(4.5,4,2.5,2))
plot(arealdekke.omrade.2$SHAPE[arealdekke.omrade.2$objtype=="Skog"], bgc = "papayawhip", col = alpha("palegreen3",0.8), setParUsrBB=TRUE)
axis(1, at=c(24.4,24.8,25.2, 25.6), cex.axis=4, padj = 0.7)
axis(2, at=c(69.5,70,70.5), cex.axis=4)
plot(arealdekke.omrade.2$SHAPE[arealdekke.omrade.2$objtype=="Havflate"], add = TRUE, col = "deepskyblue3", setParUsrBB=TRUE)
plot(cord.dec, axes = TRUE, add=TRUE, col = alpha("black", 0.85), cex.axis = 0.95, pch = 16, cex=7, )
scalebar(10, xy = c(24.55, 69.555), type = "line", divs = 4, below = "10 km", 
lonlat = NULL, label="10 km", adj=c(0.4,1.5), lwd=20, cex=3.5)
box()
dev.off()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## plot Håkøya
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#69g 39m 33 s
#18g 49m 26s 

#H1:   34W 0415978     7730120
#H2:   34W 0416011     7730287
#H3:   34W 0415952     7730368

# H1: 18.83305585, 69.66602593
# H2: 18.83375353, 69.66753294
# H3: 18.83215908, 69.66824003
# H4: 18.8239514 , 69.659206

corHA <- data.frame(lat=c(69.66602593,69.66753294,69.66824003,69.659206), lon=c(18.83305585,18.83375353, 18.83215908, 18.8239514)) 
corHA$lat <- as.numeric(corHA$lat)
corHA$lon <- as.numeric(corHA$lon)

# Setting existing coordinate as lat-long system
cord.dec = SpatialPoints(cbind(corHA$lon, corHA$lat), proj4string=CRS("+proj=longlat"))

# Transforming coordinate to UTM using EPSG=32748 for WGS=84, UTM Zone=48M,
# Southern Hemisphere)
cord.UTM <- spTransform(cord.dec, CRS("+init=epsg:32633"))
cord.UTM  ## data used for plotting

#ID<-paste("4"," "," "," "," "," "," "," "," "," "," ","2"," "," "," "," "," "," "," "," "," "," ","1"," "," "," "," "," "," "," "," "," "," ","3"," "," "," "," "," "," "," "," "," "," ",sep = ",")
#ID <- paste("", 1:4, sep = "")

plot(cord.dec, axes = TRUE, main = "UTM Coordinates", col = "red", cex.axis = 0.95)


coord.ha <- c(xmin = 18.75, ymin = 69.63, xmax = 18.85, ymax = 69.69)
box<-coord.ha  # specify which area should be plotted

## crop layers (= reduce to observed area only)
#contour.lines.2<-st_crop(contour.lines, box)
arealdekke.omrade.2<-st_crop(areal, box)


## plot map
#library(ggpattern)

# ggplot
ggplot(data = arealdekke.omrade.2$SHAPE[arealdekke.omrade.2$objtype=="Skog"], color="green")+
  geom_sf(color="black", fill="palegreen3")+
  geom_sf(data = arealdekke.omrade.2$SHAPE[arealdekke.omrade.2$objtype=="Havflate"], color="black", fill="deepskyblue3")+
  geom_sf(data = arealdekke.omrade.2$SHAPE[arealdekke.omrade.2$objtype=="Myr"], color="black", fill="lightgray")+
  geom_point(data = corHA, aes(x = lon, y = lat), color="black", fill="black", size=8, alpha=0.8 )+
  coord_sf(xlim = c(18.76, 18.84), ylim = c(69.63, 69.69), expand=F)+
  theme(panel.grid.major = element_blank(), 
        panel.background = element_rect(fill ="papayawhip"),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size=20),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_text(hjust = -7, size=20),
        axis.ticks.y=element_blank())+
  scale_y_continuous(breaks = c(69.64,69.66,69.68))+
  scale_x_continuous(breaks = c(18.77,18.80,18.83))+
  annotation_scale(
    location = "br",
    height = 	unit(0.75, "cm"),
    bar_cols = c("grey60", "white"),
    text_cex =1.5,
    text_face = "bold",
    pad_x = unit(0.6, "cm"),
    pad_y = unit(1, "cm"),
  ) +
  annotation_north_arrow(
    location = "br", which_north = "true",
    height=unit(1.5,"cm"), width=unit(1.5,"cm"),
    pad_x = unit(0.5, "in"), pad_y = unit(0.9, "in"),
    style = ggspatial::north_arrow_orienteering(
      line_width = 0.2,
      line_col = "black",
      fill = c("white", "black"),
      text_col = "black",
      text_family = "",
      text_face = "bold",
      text_size = 15,
      text_angle = 0
    )
  )

ggsave("ggmap_hakoya.png", width = 20.5, height=40, units ="cm")


# basic r
setwd("C:/Users/ekl013/OneDrive - UiT Office 365/GitProjects/OccupancyAbundanceCalibration/plots")
png(filename="map_hakoya.png", width = 480*3, height = 480*3, units="px")
par(mar=c(4.5,4,2.5,2))
plot(arealdekke.omrade.2$SHAPE[arealdekke.omrade.2$objtype=="Skog"], bgc = "papayawhip", col = alpha("palegreen3",0.8), setParUsrBB=TRUE, axes=T, cex.axis=3)
plot(arealdekke.omrade.2$SHAPE[arealdekke.omrade.2$objtype=="Havflate"], add = TRUE, col = "deepskyblue3", setParUsrBB=TRUE)
plot(arealdekke.omrade.2$SHAPE[arealdekke.omrade.2$objtype=="Myr"], add = TRUE, col = "white", setParUsrBB=TRUE)

plot(cord.dec, axes = TRUE, add=TRUE, col = alpha("black", 0.8), cex.axis = 0.95, pch = 16, cex=7 )
scalebar(1, xy = c(18.8, 69.645), type = "line", divs = 4, below = "1 km", 
         lonlat = NULL, label="1 km", adj=c(0.4,1.3), lwd=20, cex=3.5)
box()
dev.off()

unique(arealdekke.omrade.2$objtype)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## plot norway map
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
library("ggplot2")
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
nz <- map_data("nz")

png("norwaymap.jpg", width = 800*4, height=1200*4, unit="px")

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(2, 30.5), ylim = c(57.5, 72))+
  theme(axis.text=element_text(size=35, face="bold"),
        axis.title=element_text(size=35,face="bold"),
        plot.margin = margin(0,0,0,0, "cm"))+ 
  scale_y_continuous(breaks = c(58,62,66,70))
  scale_x_continuous(breaks = c(5,15,25))

ggsave("norwaymap.png", width = 800*4, height=1200*4, unit="px")



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## End script
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
?scale_y_continuous
