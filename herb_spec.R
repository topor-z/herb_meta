install.packages("rfishbase")
install.packages("reshape")
install.packages("rgl")
install.packages("gridExtra")
install.packages("ade4")
install.packages("dismo")
install.packages("XML")
install.packages("jsonlite")
install.packages("graphics")
install.packages("maps")
install.packages("mapdata")
install.packages("maptools")
install.packages("rgeos")
install.packages("rgdal")
install.packages("spThin")
install.packages("biogeo")
install.packages("letsR")
library(rfishbase)
library(reshape)
library(ggplot2)
library(rgl)
library(gridExtra)
library(ade4)
library(dplyr)
library(plyr)
library(dismo)
library(XML)
library(jsonlite)
library(graphics)
library(maps)
library(mapdata)
library(maptools)
library(rgeos)
library(rgdal)
library(spThin)
library(biogeo)
library(letsR)

##code written by Simon Brandl, 2016###
sig.list <- species_list(Family = "Siganidae")
sig.species <- species(sig.list, fields=c("sciname", "Genus", "Species", "FamCode"))
sig.species

sca.list <- species_list(Family = "Scaridae")
sca.species <- species(sca.list, fields=c("sciname", "Genus", "Species", "FamCode"))
sca.species

aca.list <- species_list(Family = "Acanthuridae")
aca.species <- species(aca.list, fields=c("sciname", "Genus", "Species", "FamCode"))
aca.species

kyp.list <- species_list(Genus = "Kyphosus")
kyp.species <- species(kyp.list, fields=c("sciname", "Genus", "Species", "FamCode"))
kyp.species

herbivore.spp <- rbind(sig.species, sca.species, aca.species, kyp.species)
herbivore.spp

##write loop to run through herbivore list
###create empty dataframes

gps.sp <- data.frame(long = numeric(0), lat = numeric(0), genus = character(0), species = character(0))
gps.sp

for (i in 1:nrow(herbivore.spp))
{
  ##define genus & species
  genus <- herbivore.spp[i,2]
  species <- herbivore.spp[i,3]
  print(i)
  
  ####### Get records from FishBase 
  ###define URL for access
  url <- paste("http://www.fishbase.org/Map/OccurrenceMapList.php?showAll=yes&genus=",
               genus,"&species=",species,sep="")
  
  fishbase.data <- try(readHTMLTable(url),silent=TRUE)
  if(class(fishbase.data)=="try-error"){
    fishbase.coordinates<-data.frame(matrix(ncol=2, nrow=0))
    warning("FishBase data download failed, check internet connection")
  }else{
    n.rows <- unlist(lapply(fishbase.data, function(t) dim(t)[1]))
    fishbase.data <- fishbase.data[[which.max(n.rows)]]
    fishbase.coordinates <- data.frame(fishbase.data[,4],fishbase.data[,3])
    fishbase.coordinates[,1] <- as.numeric(as.character(fishbase.coordinates[,1]))
    fishbase.coordinates[,2] <- as.numeric(as.character(fishbase.coordinates[,2]))
    fishbase.coordinates <- na.omit(fishbase.coordinates)
  }
  
  coordinates <- fishbase.coordinates
  coord_unique <- na.omit(unique(coordinates))
  coord_unique$genus <- rep(genus, nrow(coord_unique))
  coord_unique$species <- rep(species, nrow(coord_unique))
  
  gps.sp <- rbind(gps.sp, coord_unique)
  
}
#####Generate first data set#####
colnames(gps.sp) <- c("lon", "lat", "genus", "species")
str(gps.sp1)
gps.sp$id <- interaction(gps.sp$genus, gps.sp$species)
gps.sp1 <- as.data.frame(gps.sp)
gps.sp2 <- gps.sp1[,-c(3:5)]
write.table(gps.sp1, row.names = FALSE, file = "species_coord.txt", sep = " ")


genspe.list <- split(gps.sp2, gps.sp2$id)
genspe.list
genspe.list <- lapply(genspe.list, function(x) { x["id"] <- NULL; x })
genspe.list
ps <- sapply(genspe.list, Polygon)

#####Polygon####

xy <- gps.sp1[,c(1,2)]
spdf <- SpatialPointsDataFrame(coords = xy, data = gps.sp1,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
spdf
data <- data.frame(genspe=unique(gps.sp1$genspe),row.names=unique(gps.sp1$genspe))

P1 = Polygon(xy)
P1
Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
Ps1
plot(Ps1, axes = TRUE)


data <- data.frame(genspe=unique(gps.sp$genspe), row.names=unique(gps.sp$ID))
data
prab.poly <- points2polygons(gps.sp1, data)


##############################################################################################
# Set geographical area ######################################################################

x <- coord_unique[,1]
y <- coord_unique[,2]

xmin=min(x)-5
xmax=max(x)+5
ymin=min(y)-5
ymax=max(y)+5


map("world", col="gray60",  border="gray60", mar=c(0,0,2,0), fill=T)
title(main="Fishbase", cex.main=0.9)
mtext(paste('n = ', nrow(fishbase.coordinates)), side=1, cex=0.6)
box(which = "plot", lty = "solid", lwd=0.25)
points(coord_unique[-c(1:2),], pch=21, col='black', bg='red', cex=1, lwd=0.2) 

##############################################################################################
# Print summary table ########################################################################

r.summary <- data.frame(c(nrow(fishbase.coordinates),
                          length(dups), length(x.ocean), length(x.land), length(x.thinned)))
rownames(r.summary) <- c("GBIF","OBIS","FishBase","Duplicated","Ocean","Land", "Thinned")
colnames(r.summary) <- c("RECORDS")
r.summary



##########
##########
##########
genus <- "Eviota"
species <- "tetha"
print(i)

####### Get records from FishBase 
###define URL for access
url <- paste("http://www.fishbase.org/Map/OccurrenceMapList.php?showAll=yes&genus=",
             genus,"&species=",species,sep="")

fishbase.data <- try(readHTMLTable(url),silent=TRUE)
if(class(fishbase.data)=="try-error"){
  fishbase.coordinates<-data.frame(matrix(ncol=2, nrow=0))
  warning("FishBase data download failed, check internet connection")
}else{
  n.rows <- unlist(lapply(fishbase.data, function(t) dim(t)[1]))
  fishbase.data <- fishbase.data[[which.max(n.rows)]]
  fishbase.coordinates <- data.frame(fishbase.data[,4],fishbase.data[,3])
  fishbase.coordinates[,1] <- as.numeric(as.character(fishbase.coordinates[,1]))
  fishbase.coordinates[,2] <- as.numeric(as.character(fishbase.coordinates[,2]))
  fishbase.coordinates <- na.omit(fishbase.coordinates)
}

coordinates <- fishbase.coordinates
coord_unique <- na.omit(unique(coordinates))
coord_unique$genus <- rep(genus, nrow(coord_unique))
coord_unique$species <- rep(species, nrow(coord_unique))
colnames(coord_unique) <- c("long", "lat", "genus", "species")
coord_unique
data <- data.frame(species=unique(coord_unique$species),row.names=unique(coord_unique$species))

x <- coord_unique[,1]
y <- coord_unique[,2]

xmin=min(x)-5
xmax=max(x)+5
ymin=min(y)-5
ymax=max(y)+5


map("world", col="gray60",  border="gray60", mar=c(0,0,2,0), fill=T)
title(main="Fishbase", cex.main=0.9)
mtext(paste('n = ', nrow(fishbase.coordinates)), side=1, cex=0.6)
box(which = "plot", lty = "solid", lwd=0.25)
points(coord_unique[-c(1:2),], pch=21, col='black', bg='red', cex=1, lwd=0.2) 

library(sp)
coordinates(coord_unique) <- ~long+lat
coord_unique
prab <- lets.presab(coord_unique, xmn = -180, xmx = 180,ymn = -180, ymx = 180)
prab.poly <- points2polygons(coord_unique, data)
XY
P1 = Polygon(coord_unique[,c(1:2)])
P1
Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
Ps1
s_poly <- SpatialPolygonsDataFrame(Ps1, coord_unique)


#########
#########
