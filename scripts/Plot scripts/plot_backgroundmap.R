setwd("~/Dokumente/Masterarbeit/R")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## PLOT BACKGROUND MAP
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

# load coordinates
library(readr)
library(rgdal)
library(sf)
library(sp)

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
cord.UTM <- spTransform(cord.dec, CRS("+init=epsg:32633"))
cord.UTM  ## data used for plotting

#ID<-paste("4"," "," "," "," "," "," "," "," "," "," ","2"," "," "," "," "," "," "," "," "," "," ","1"," "," "," "," "," "," "," "," "," "," ","3"," "," "," "," "," "," "," "," "," "," ",sep = ",")
#ID <- paste("", 1:4, sep = "")

plot(cord.UTM, axes = TRUE, main = "UTM Coordinates", col = "red", cex.axis = 0.95)

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
contour.lines <- st_read(fgdb100, layer = "N100_Høyde_senterlinje")  # contour lines
arealdekke.omrade <- st_read(fgdb250, layer = "N250_Arealdekke_omrade")  # contour lines
rivers <- st_read(fgdb500, layer = "N500_Arealdekke_senterlinje")  # contour lines
path <- st_read(fgdb100, layer = "N100_Samferdsel_senterlinje")  # contour lines
names <- st_read(fgdb250, layer = "N250_Stedsnavn_tekstplassering")  # contour lines
#buildings <- st_read(fgdb250, layer = "N250_BygningerOgAnlegg_posisjon")  # contour lines
xx <- st_read(fgdb100, layer = "N100_Arealdekke_grense")  # contour lines


coord.ko<-c(xmin = 1051683, ymin = 7865725, xmax = 1067108, ymax = 7881869)  # covers study design Komag -> coordinates can be adjusted
## set coordinates for the map area
coord.ko<-c(xmin = 1048683, ymin = 7868725, xmax = 1068608, ymax = 7877869)  # covers study design Komag -> coordinates can be adjusted
coord.vj<-c(xmin = 1017153, ymin = 7855491, xmax = 1029747, ymax = 7866000)  # covers study design VJ -> coordinates can be adjusted

coord.po <- c(xmin = 850000, ymin = 7740000, xmax = 910000, ymax = 7875000)
box<-coord.po  # specify which area should be plotted


library(dplyr)
chosen_names <- names %>% filter(TextString %in% c("571", "Kjøltindan","403","Gárgas","Ryggfjellet","365","Komagelva"))

## crop layers (= reduce to observed area only)
contour.lines.2<-st_crop(contour.lines, box)
arealdekke.omrade.2<-st_crop(arealdekke.omrade, box)
rivers.2<-st_crop(rivers, box)
path.2<-st_crop(path, box)
names.2<-st_crop(chosen_names, box)     # chosen names 
#names.2<-st_crop(names, box)            # all names 
#buildings.2<-st_crop(buildings, box)
xx<-st_crop(xx, box)


library(raster) #for scalebar
library(scales) # for transparency

## plot map
#x11(height = 10, width = 10)
setwd("C:/Users/ekl013/OneDrive - UiT Office 365/GitProjects/OccupancyAbundanceCalibration/plots")

x11()
par(mar=c(1,1,1,1))
#plot(contour.lines.2$SHAPE, bgc = "papayawhip", col = "white", setParUsrBB=TRUE)
#plot(arealdekke.omrade.2$SHAPE[arealdekke.omrade.2$objtype=="Innsjø"],  col = "deepskyblue3", setParUsrBB=TRUE)
#plot(arealdekke.omrade.2$SHAPE[arealdekke.omrade.2$objtype=="Myr"], add = TRUE, col = "white", setParUsrBB=TRUE)
plot(arealdekke.omrade.2$SHAPE[arealdekke.omrade.2$objtype=="Skog"], bgc = "papayawhip", col = alpha("palegreen3",0.8), setParUsrBB=TRUE)
#plot(rivers.2$SHAPE, add = TRUE, col = "lightblue", lwd = 1.9, setParUsrBB=TRUE)
#plot(path.2$SHAPE, add = TRUE, col = "darkred", lty = 2, lwd = 1.8, setParUsrBB=TRUE)
plot(arealdekke.omrade.2$SHAPE[arealdekke.omrade.2$objtype=="Havflate"], add = TRUE, col = "deepskyblue3", setParUsrBB=TRUE)
#plot(buildings.2$SHAPE, add = TRUE, col = "grey30", pch = 15, cex = 1.5, setParUsrBB=TRUE)
plot(cord.UTM, axes = TRUE, add=TRUE, col = alpha("black", 0.85), cex.axis = 0.95, pch = 16, cex=2.5, )
scalebar(10000, xy = c(873000, 7745000), type = "line", divs = 4, below = "10 km", 
lonlat = NULL, label="10 km", adj=c(0.4,1.3), lwd=10)
#text(st_coordinates(st_centroid(names.2)), labels = names.2$TextString, cex=0.8)
#text(st_coordinates(st_centroid(names.2)), labels = names.2$TextString, cex=0.8, setParUsrBB=TRUE)
#text(coordinates(cord.UTM), labels = ID, cex=0.8, setParUsrBB=TRUE)
#          pos = at_proj$pos, offset = at_proj$offset, 
#labels = parse(text = as.character(at_proj$labels)), cex = 0.6)
box()






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

plot(cord.UTM, axes = TRUE, main = "UTM Coordinates", col = "red", cex.axis = 0.95)


coord.ha <- c(xmin = 643000, ymin = 7730000, xmax = 651000, ymax = 7736000)
box<-coord.ha  # specify which area should be plotted

library(dplyr)
chosen_names <- names %>% filter(TextString %in% c("571", "Kjøltindan","403","Gárgas","Ryggfjellet","365","Komagelva"))

## crop layers (= reduce to observed area only)
contour.lines.2<-st_crop(contour.lines, box)
arealdekke.omrade.2<-st_crop(arealdekke.omrade, box)
rivers.2<-st_crop(rivers, box)
path.2<-st_crop(path, box)
#names.2<-st_crop(chosen_names, box)     # chosen names 
#names.2<-st_crop(names, box)            # all names 
#buildings.2<-st_crop(buildings, box)
xx<-st_crop(xx, box)


#library(raster) #for scalebar
#library(scales) # for transparency

## plot map
#x11(height = 10, width = 10)
#setwd("C:/Users/ekl013/OneDrive - UiT Office 365/GitProjects/OccupancyAbundanceCalibration/plots")

x11()
par(mar=c(1,1,1,1))
plot(contour.lines.2$SHAPE, bgc = "papayawhip", col = "white", setParUsrBB=TRUE)
#plot(arealdekke.omrade.2$SHAPE[arealdekke.omrade.2$objtype=="Innsjø"],  col = "deepskyblue3", setParUsrBB=TRUE)
#plot(arealdekke.omrade.2$SHAPE[arealdekke.omrade.2$objtype=="Myr"], add = TRUE, col = "white", setParUsrBB=TRUE)
plot(arealdekke.omrade.2$SHAPE[arealdekke.omrade.2$objtype=="Skog"], add=T, col = alpha("palegreen3",0.8), setParUsrBB=TRUE)
#plot(rivers.2$SHAPE, add = TRUE, col = "lightblue", lwd = 1.9, setParUsrBB=TRUE)
#plot(path.2$SHAPE, add = TRUE, col = "darkred", lty = 2, lwd = 1.8, setParUsrBB=TRUE)
plot(arealdekke.omrade.2$SHAPE[arealdekke.omrade.2$objtype=="Havflate"], add = TRUE, col = "deepskyblue3", setParUsrBB=TRUE)
#plot(buildings.2$SHAPE, add = TRUE, col = "grey30", pch = 15, cex = 1.5, setParUsrBB=TRUE)
plot(cord.UTM, axes = TRUE, add=TRUE, col = alpha("black", 0.8), cex.axis = 0.95, pch = 16, cex=2.5, )
scalebar(1000, xy = c(647000, 7730700), type = "line", divs = 4, below = "1 km", 
         lonlat = NULL, label="1 km", adj=c(0.4,1.3), lwd=8)
#text(st_coordinates(st_centroid(names.2)), labels = names.2$TextString, cex=0.8)
#text(st_coordinates(st_centroid(names.2)), labels = names.2$TextString, cex=0.8, setParUsrBB=TRUE)
#text(coordinates(cord.UTM), labels = ID, cex=0.8, setParUsrBB=TRUE)
#          pos = at_proj$pos, offset = at_proj$offset, 
#labels = parse(text = as.character(at_proj$labels)), cex = 0.6)
box()



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## plot norway map
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
library("ggplot2")
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(2, 30.5), ylim = c(57.5, 72))+
  theme(axis.text=element_text(size=12, face="bold"),
        axis.title=element_text(size=14,face="bold"))



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## End script
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

