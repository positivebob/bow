##### ____ Robert McGuinn startdate_20151017 enddate... #####

##### install packages #####
# install.packages("sp")
# install.packages("rgdal")
# install.packages("maptools") 
# install.packages("ggplot2")
# install.packages("rgeos") 
# install.packages("gcclib") 
# install.packages("ggmap") 
# install.packages("dplyr")
# install.packages("lazyeval")

library(sp)
library(rgdal)
library(maptools)
library(ggplot2)
library(rgeos)
library(ggmap)
library(dplyr)
library(lazyeval)

##### ____ Mapping the McGuinn land in Polk County, NC #####

##### various working directories #####
setwd("C:/rworking")
setwd("C:/rworking/spatial_killa/code")
setwd("C:/rworking/spatial_killa/i/polkcounty")
C:\rworking\spatial_killa\i\polkcounty
setwd("C:/rworking/spatial_killa/outputs")
setwd("C:/rworking/spatial_killa/plots")

##### read in input files #####
setwd("C:/rworking/spatial_killa/i/polkcounty")

# read in the shapefile using R's interface to the Geospatial Data Abstraction Library (rGDAL)
# load the shapefile and turn it into a SpatialPolygonDataFrame
library(rgdal)
land <- readOGR(dsn = ".", "mcguinnland")

# print the contents to see the structure of the file
print(land)

# look at the variables in the SpatialPolygonDataFrame
names(land)

# look at the attribute table in the slot called data
land@data

# look at the projection, it is Lambert Conic Conformal (1SP)
proj4string(land)

# do a summary of land
summary(land)

# checking ownership
unique(land$OWNAM1)
unique(land$OWNAM3)

# selecting parts of the dataframe for plotting
p <- ggplot(land@data, aes(OWNAM1,as.numeric(as.character(LAND_VALUE))))
p <- ggplot(land@data, aes(OWNAM1, area))
p <- ggplot(land@data, aes(OWNAM1, TMS))
p <- ggplot(land@data, aes(OWNAM1, DEED_YEAR))
p <- ggplot(land@data, aes(OWNAM1, DEEDED_ACR))

# adding a geometry to the plot
p + geom_point(colour= 'black', size=6)
p + geom_point(colour='black', size=3) + geom_text(colour = 'black', size = 6, aes(label = land@data$LAND_VALUE), 
                                                   vjust = -1.1, nudge_y = .5)



# make the polygons plottable with ggplot2 using fortify
land.f <- fortify(land, region = "TMS")

# add back the attribute data to the polygons
land.f <- merge(land.f, land@data, by.x = "id", by.y = "TMS")

# check the object
print(land.f)

# make the map
map <- ggplot(land.f, aes(long, lat, group = group, fill = as.numeric(as.character(LAND_VALUE)))) + 
  geom_polygon() + 
  coord_equal() + 
  labs(x = "Easting (feet)", y = "Northing (feet)", fill = "Land Value") + 
  ggtitle("McGuinn Land")

# plot the map 
map

# save the map
ggsave("my_large_plot.png", scale = 1, dpi = 400)

# get a wgs84 version of the original SpatialPolygonDataFrame
land.wgs84 <- spTransform(land, CRS("+init=epsg:4326"))

# fortify the wgs version of the land polys for plotting purposes
land.wgs84.f <- fortify(land.wgs84, region = "TMS")
land.wgs84.f <- merge(land.wgs84.f, land.wgs84@data, 
                      by.x = "id", by.y = "TMS")

##### Extracting coordinates from KML file #####
#Extracting Coordinates and ID from KML  
setwd("C:/rworking/spatial_killa/i")
kml.text <- readLines("point.kml")  

re <- "<coordinates> *([^<]+?) *<\\/coordinates>"  
coords <- grep(re,kml.text)  

re2 <- "src_id:"  
SCR.ID <- grep(re2,kml.text)  

re3 <- "<tr><td><b>Name:</b><td>"  
Name <- grep(re3,kml.text)  

kml.coordinates <- matrix(0,length(coords),4,dimnames=list(c(),c("ID","LAT","LON","ELEV")))  
kml.names <- matrix(0,length(coords),1)  

for(i in 1:length(coords)){  
  sub.coords <- coords[i]  
  temp1 <- gsub("<coordinates>"," ",kml.text[sub.coords])  
  temp2 <- gsub("</coordinates>"," ",temp1)  
  coordinates <- as.numeric(unlist(strsplit(temp2,",")))  
  
  sub.ID <- SCR.ID[i]  
  ID <- as.numeric(gsub("<tr><td><b>src_id:</b><td>"," ",kml.text[sub.ID]))  
  
  sub.Name <- Name[i]  
  NAME <- gsub(paste("<tr><td><b>Name:</b><td>"),"",kml.text[sub.Name])  
  
  kml.coordinates[i,] <- matrix(c(ID,coordinates),ncol=4)  
  kml.names[i,] <- matrix(c(NAME),ncol=1)  
}  


write.table(kml.coordinates,"KML_coordinates.csv",sep=";",row.names=F)

##### Getting a google map from KML coordinates(see above on how to get coordinates) #####
google <- get_googlemap(c(kml.coordinates[1,2],kml.coordinates[1,3]), 
                        zoom = 14,
                        maptype = "satellite")


ggmap(google) +
  geom_point(aes(x = c(kml.coordinates[1,2]), y = c(kml.coordinates[1,3]), colour = "red", size = 10))
##### making the map #####
# create a bounding box we will use to get base map
b <- bbox(land.wgs84) 


# get a basemap using bbox and ggmap 
## hybrid map
lnd.b1 <- ggmap(get_map(zoom = 15, location = b, maptype = "hybrid", crop = FALSE))

# draw the map
lnd.b1 + 
  geom_polygon(data = land.wgs84.f, 
               aes(x = long, y = lat, group = group, fill =  as.numeric(as.character(LAND_VALUE))), 
               alpha = 0.5)

## terrain map

lnd.b1 <- ggmap(get_map(zoom = 15, location = b, maptype = "terrain", crop = FALSE))

# draw the map
lnd.b1 + 
  geom_polygon(data = land.wgs84.f, 
               aes(x = long, y = lat, group = group, fill =  as.numeric(as.character(LAND_VALUE)), 
                   alpha = 0.7))

## Sattellite Map

lnd.b1 <- ggmap(get_map(zoom = 15, location = b, maptype = "satellite", crop = FALSE))

# draw the map
lnd.b1 + 
  geom_polygon(data = land.wgs84.f, 
               aes(x = long, y = lat, group = group, fill =  as.numeric(as.character(LAND_VALUE))), 
               alpha = 0.7)

                        

##### ____ Using ggmap #####

##### Getting a map #####
# set location variable
location <- "Asheville, NC"
gc <- geocode(location)

# customizing gc

# gc
# gc$lon <- kml.coordinates[1,3]
# gc$lat <- kml.coordinates[1,2]
  

google <- get_googlemap(c(gc$lon,gc$lat), 
                        zoom = 8,
                        maptype = "hybrid")


ggmap(google) +
  geom_point(aes(x = lon, y = lat), data=gc, colour = "red", size = 4)

#### bounding box creation ####
# bbox creation
z <- .45
#bbox <- c(left = gc$lon-.01, bottom = gc$lat-.01, right = gc$lon+.01, top = gc$lat+.01)
bbox <- c(left = gc$lon-z, bottom = gc$lat-z, right = gc$lon+z, top = gc$lat+z)

##### _____ Working with rerddap #####

##### install.packages for working with rerddap ##### 
install.packages("devtools", lib = "C:/Users/robert.mcguinn/Documents/R/win-library/3.4")
install.packages("digest", lib = "C:/Users/robert.mcguinn/Documents/R/win-library/3.4")
install.packages("data.table", lib = "C:/Users/robert.mcguinn/Documents/R/win-library/3.4")
install.packages("DBI", lib = "C:/Users/robert.mcguinn/Documents/R/win-library/3.4"))
install.packages("assertthat", lib = "C:/Users/robert.mcguinn/Documents/R/win-library/3.4")
install.packages("Rcpp", lib = "C:/Users/robert.mcguinn/Documents/R/win-library/3.4")
install.packages("rerddap", lib = "C:/Users/robert.mcguinn/Documents/R/win-library/3.4")
install.packages("magrittr", lib = "C:/Users/robert.mcguinn/Documents/R/win-library/3.4")


library(magrittr)
library(Rcpp)
library(assertthat)
library(DBI)
library(devtools)
library(data.table, lib = "C:/Users/robert.mcguinn/Documents/R/win-library/3.4")
library(digest)
library(rerddap)

##### using erddap #####
library(rerddap)

# search for key word 
x <- ed_search(query='deep', url = "https://ecowatch.ncddc.noaa.gov/erddap/")
x$alldata[[1]]
x$info

# list all datasets on server
x <- head(ed_datasets('table', url = "https://ecowatch.ncddc.noaa.gov/erddap/"))
View(x)

x <- head(ed_datasets('grid', url = "https://ecowatch.ncddc.noaa.gov/erddap/"))
fix(x)

# Get info on a datasetid, then get data given information learned
info('deep_sea_corals', url = "https://ecowatch.ncddc.noaa.gov/erddap/")$variables

# Limiting by depth.
x <- tabledap('deep_sea_corals', 
         fields=c('latitude','longitude', "DepthInMeters", "Temperature", 'ScientificName', "ImageURL"),
         url = "https://ecowatch.ncddc.noaa.gov/erddap/", "DepthInMeters>4000")

View(x$DepthInMeters)

# Spatial delimitation
x <- tabledap('deep_sea_corals', 
              fields=c('latitude','longitude', "DepthInMeters", "Temperature", 'ScientificName', "ImageURL"),
              url = "https://ecowatch.ncddc.noaa.gov/erddap/", 
              'latitude>=34.8', 'latitude<=35', 'longitude>=-145', 'longitude<=-100')


# tabledap('hawaii_b55f_a8f2_ad70',
#   fields = c('longitude', 'latitude', 'chlorophyll', 'salinity'),
#   'time>=2010-06-24', 'time<=2010-07-01'
# )

x <- x[is.na(x$ImageURL) == F,]
        
View(x)

x <- x %>% 
  filter(ScientificName == "Lophelia pertusa")

setwd("C:/rworking/spatial_killa/o")
write.csv(x, "x.csv")



# # Integrate with taxize
# out <- tabledap('erdCalCOFIlrvcntHBtoHI',
#    fields = c('latitude','longitude','scientific_name','itis_tsn'),
#    'time>=2007-06-10', 'time<=2007-09-21'
# )
# tsns <- unique(out$itis_tsn[1:100])
# library("taxize")
# classif <- classification(tsns, db = "itis")
# head(rbind(classif)); tail(rbind(classif))

##### Exporting to KML ##### 

## Load required packages
#install.packages("maptools")
library(maptools)
library(rgdal)

##Set your working directory

setwd("C:/rworking/spatial_killa/inputs")

# load the live deep sea coral and sponge database occurrence locations via ERDDAP.

x <- tabledap('deep_sea_corals', 
              fields=c('latitude','longitude','ScientificName', "CatalogNumber", "FishCouncilRegion", "Locality",
                       "DataProvider", "Vessel", "VehicleName", "ObservationYear", "DepthInMeters", "ImageURL"),
              url = "https://ecowatch.ncddc.noaa.gov/erddap/")

x <- x[is.na(x$ImageURL) == F,]
x <- data.frame(x)
x$latitude <- as.numeric(x$latitude)
x$longitude <- as.numeric(x$longitude)

# do some geographic subsetting

x <- filter(x, latitude > 25 , latitude < 30, longitude > -100)
#fix(x)

x <- filter(x, x$FishCouncilRegion == "North Pacific")

# # plot the xY coordinates
# 
# plot(x$longitude, x$latitude)

## create a SpatialPointsDataframe object and add the appropriate CRS

coordinates(x)<- c("longitude", "latitude")
proj4string(x)<- "+proj=longlat +ellps=WGS84 +datum=WGS84"

# Using the OGR KML driver we can then export the data to KML. 
# dsn should equal the name of the exported file and the dataset_options 
# argument allows us to specify the labels displayed by each of the points

setwd("C:/rworking/spatial_killa/outputs")
writeOGR(x, dsn="x.kml", layer= "Subset of NOAA's Deep Sea Coral and Sponge Database", driver="KML", dataset_options=c("NameField=ScientificName"), overwrite_layer = T)

## if you have Google Earth installed double click on the kml file you just created to open it. 
#The points should be loaded as labelled pins on the map.If you click on the pin you will be able to see its full name and capacity. 

##### _____ Using stringdist and clustering for large factors #####
# idea from: https://github.com/amunategui/string-distance-on-large-factors/blob/master/string-distance-on-large-factors.R

##### getting deep sea coral data using rerrdap::tabledap function #####
library(rerddap)
x <- tabledap('deep_sea_corals', 
              fields=c('latitude','longitude','ScientificName', "CatalogNumber", "FishCouncilRegion", "Locality",
                       "DataProvider", "Vessel", "VehicleName", "ObservationYear", 
                       "DepthInMeters", "ImageURL","SurveyID", "SamplingEquipment"),
              url = "https://ecowatch.ncddc.noaa.gov/erddap/")

# # just looking at the data that has images #
x1 <- x[is.na(x$ImageURL) == F,]

##### using the stringdist function to analyze short text clusters #####
# call the stringdistmatrix function and request some number of groups
#install.packages("stringdist")
library(stringdist)

# create a combined string using paste function 
x1$combined <- paste0(as.character(x1$Vessel),
                     "::", as.character(x1$VehicleName)#,
                     # " ", as.character(x$ObservationYear)#,
                     # " ", as.character(x$DataProvider)
)

#check the number of unique
print(length(unique(x1$combined)))

# create a vector of unique combined strings
unique <- unique(as.character(x1$combined))

# creating the distance matrix 
distancemodels <- stringdistmatrix(unique,unique,method = "jw")

# get names
rownames(distancemodels) <- unique

# make clusters
hc <- hclust(as.dist(distancemodels))

# visualize the dendrogram
plot(hc)
rect.hclust(hc,k=5)

# get tabular data and view
dfClust <- data.frame(unique, cutree(hc, k=5))
names(dfClust) <- c('modelname','cluster')
View(dfClust)

# visualize the groupings
plot(table(dfClust$cluster))
print(paste('Average number of models per cluster:', mean(table(dfClust$cluster))))

# look at the top clusters
t <- table(dfClust$cluster)
t <- cbind(t,t/length(dfClust$cluster))
t <- t[order(t[,2], decreasing=TRUE),]
p <- data.frame(factorName=rownames(t), binCount=t[,1], percentFound=t[,2])
dfClust <- merge(x=dfClust, y=p, by.x = 'cluster', by.y='factorName', all.x=T)
dfClust <- dfClust[rev(order(dfClust$binCount)),]
names(dfClust) <-  c('cluster','modelname')
head (dfClust[c('cluster','modelname')],50)
View(dfClust)

##### _____ Looking at a bunch of images at once #####
install.packages("rvest")
install.packages("imager")
library(rvest)
library(imager)
#Run a search query (returning html content)
search <- read_html("https://www.google.com/search?site=&tbm=isch&q=parrot")

#Grab all <img> tags, get their "src" attribute, a URL to an image
urls <- search %>% html_nodes("img") %>% html_attr("src") #Get urls of parrot pictures

class(urls)


#Load the first four, return as image list, display
map_il(urls[1:4],load.image) %>% plot



