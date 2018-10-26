library(maptools)
library(RColorBrewer)
library(classInt)
library(OpenStreetMap)
library(sp)
library(rgeos)
library(tmap)
library(tmaptools)
library(sf)
library(rgdal)
library(geojsonio)
library(tidyverse)

setwd("D:/Xin/MSc Smart Cities and Urban Analytics/Modules/CASA0005 GISS/Week 3")

#Making some choropleth maps
LondonDataOSK <- read_csv("https://files.datapress.com/london/dataset/ward-profiles-and-atlas/2015-09-24T14:21:24/ward-profiles-excel-version.csv", na = "n/a")

library(tidyverse)
#wang the data in straight from the web using read_csv, skipping over the 'n/a' entries as you go...
LondonData <- read_csv("https://files.datapress.com/london/dataset/ward-profiles-and-atlas/2015-09-24T14:21:24/ward-profiles-excel-version.csv", na = "n/a")
class(LondonData)
class(LondonDataOSK)
datatypelist <- data.frame(cbind(lapply(LondonData,class)))
LondonData <- edit(LondonData)
summary(df)
names(LondonData)
LondonBoroughs<-LondonData[626:658,]
LondonData <- data.frame(LondonData)
LondonBoroughs <- LondonData[grep("^E09",LondonData[,3]),]
head(LondonBoroughs)
LondonBoroughs <- LondonBoroughs[2:34,]
LondonBoroughs<-LondonBoroughs[,c(1,19,20,21)]
names(LondonBoroughs)[1] <- c("Borough Name")
plot(LondonBoroughs$Male.life.expectancy..2009.13, LondonBoroughs$X..children.in.reception.year.who.are.obese...2011.12.to.2013.14)


#Part 2

EW <- geojson_read("http://geoportal.statistics.gov.uk/datasets/8edafbe3276d4b56aec60991cbddda50_2.geojson", what = "sp")
#pull out london using grep and the regex wildcard for'start of the string' (^) to to look for the bit of the district code that relates to London (E09) from the 'lad15cd' column in the data slot of our spatial polygons dataframe
LondonMap <- EW[grep("^E09",EW@data$lad15cd),]
#plot it using the base plot function
qtm(LondonMap)
#read the shapefile into a simple features object
BoroughMapSF <- read_shape("BoundaryData/england_lad_2011Polygon.shp", as.sf = TRUE)
BoroughMapSP <- LondonMap
#plot it very quickly usking qtm (quick thematic map) to check it has been read in correctly
qtm(BoroughMapSF)
qtm(BoroughMapSP)
library(methods)
#check the class of BoroughMapSF
class(BoroughMapSF)
#And check the class of BoroughMapSP
class(BoroughMapSP)
#now convert the SP object into an SF object...
newSF <- st_as_sf(BoroughMapSP)
#and try the other way around SF to SP...
newSP <- as(newSF, "Spatial")
#simples!
BoroughMapSP <- as(BoroughMapSF, "Spatial")

#Joining Attribute Data
#join the data to the @data slot in the SP data frame
BoroughMapSP@data <- data.frame(BoroughMapSP@data,LondonData[match(BoroughMapSP@data[,"code"],LondonData[,"New.code"]),])
#check it's joined.
#head(BoroughMapSP@data)

BoroughDataMap <- append_data(BoroughMapSF,LondonData, key.shp = "code", key.data = "New.code", ignore.duplicates = TRUE)
BoroughDataMap2 <- BoroughMapSF %>% left_join(LondonData, by = c("code" = "New.code"))

library(tmap)
library(tmaptools)
# tmap mode set to plotting
tmap_mode("plot")
qtm(BoroughDataMap, fill = "Rate.of.JobSeekers.Allowance..JSA..Claimants...2015")
london_osm <- read_osm(BoroughDataMap, type = "esri", zoom = NULL)
qtm(london_osm) + 
  tm_shape(BoroughDataMap) + 
  tm_polygons("Rate.of.JobSeekers.Allowance..JSA..Claimants...2015", 
              style="jenks",
              palette="YlOrBr",
              midpoint=NA,
              title="Rate per 1,000 people",
              alpha = 0.5) + 
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Job seekers' Allowance Claimants", legend.position = c("right", "bottom"))


tm_shape(BoroughDataMap) +
  tm_polygons(c("Average.Public.Transport.Accessibility.score...2014", "Violence.against.the.person.rate...2014.15"), 
              style=c("jenks","pretty"),
              palette=list("YlOrBr", "Blues"),
              auto.palette.mapping=FALSE,
              title=c("Average Public Transport Accessibility", "Violence Against the Person Rate"))


#You might need to install the shinyjs paclage for this to work
#install.packages("shinyjs")
library(shinyjs)
#it's possible to explicitly tell R which package to get the function from with the :: operator...
#tmaptools::palette_explorer()
tmap_mode("view")

tm_shape(BoroughDataMap) +
  tm_polygons("X..children.in.year.6.who.are.obese..2011.12.to.2013.14",
              style="jenks",
              palette="PuRd",
              midpoint=NA,
              title="Truffle Shuffle Intensity")

#Mapping with 
ggplot(data.frame, aes(x=x, y=y))
geom_polygon(aes(x=x, y=y), data.frame)
ggplot()+geom_sf(mapping = aes(geometry=geometry),data = BoroughDataMap)+theme_minimal()
ggplot()+geom_sf(mapping = aes(geometry=geometry, fill=Median.House.Price...U.00A3.....2014),data = BoroughDataMap)+theme_minimal()
palette1<-scale_fill_continuous(low="white", high="orange", "Price(£)")
labels<-labs(list(title="Average House Price 2014",x="Longitude", y="Latitude"))
ggplot()+geom_sf(mapping = aes(geometry=geometry, fill=Median.House.Price...U.00A3.....2014),data = BoroughDataMap)+theme_minimal()+palette1+labels

#Changing Projections
unloadNamespace('shinyjs')
print(BoroughMapSP)
print(BoroughMapSF)
+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy

#EPSG
#If the boundary data does not have a spatial reference system, ca read it in and set the projection either with the full proj4 string, or more easily with the EPSG code.
#Read borough map in and explicitly set projection to British National Grid using the EPSG string code 27700
BoroughMapSP <- read_shape("BoundaryData/england_lad_2011Polygon.shp", current.projection = 27700)
#or, for SF
BoroughMapSF <- st_read("BoundaryData/england_lad_2011Polygon.shp") %>% st_set_crs(27700)

#create a variable for the EPSG code to reference the proj4string (EPSG codes are shorter and easier to remember than the full strings!) and store it in a variable...
UKBNG<-"+init=epsg:27700"
#now set the proj4string for your BoroughMap object - note, this will probably throw an error if your dataset already has a CRS, this is just for demonstration...
proj4string(BoroughMapSP)<-CRS(UKBNG)

print(BoroughMapSP) # check for new CRS
BoroughMapSF <- BoroughMapSF %>% st_set_crs(27700)
print(BoroughMapSF)

#Reprojecting the coordinate system
BoroughMapSPWGS84 <-spTransform(BoroughMapSP, CRS("+proj=longlat +datum=WGS84"))
print(BoroughMapSPWGS84)

#transform it back again:
BoroughMapSPBNG <-spTransform(BoroughMapSP, CRS(UKBNG))
print(BoroughMapSPBNG)

#You may want to create a similar variable for WGS84
latlong <- "+init=epsg:4326"
BoroughMapSFWGS84 <- st_transform(BoroughMapSF, 4326)
print(BoroughMapSFWGS84)

#Maps with extra features

#install.packages("ggmap")
library(ggmap)

BoroughDataMap <- append_data(BoroughMapSFWGS84,LondonData, key.shp = "code", key.data = "New.code", ignore.duplicates = TRUE)

londonbbox1 <- c(left = -0.5103766, bottom = 51.28676, right = 0.3340146, top = 51.69187)

londonbbox2 <- as.vector(st_bbox(BoroughMapSFWGS84))
#this bit of code gets 
map <- get_stamenmap(londonbbox2, zoom = 10, maptype = "toner-lite")

ggmap(map) + geom_sf(mapping = aes(geometry=geometry, fill=Median.House.Price...U.00A3.....2014),data = BoroughDataMap, inherit.aes = FALSE,alpha=0.7)+theme_minimal()+palette1+labels


#Extension 1
#warning, this is messy, but it sort-of works...
library(reshape2)
library(dplyr)

#use melt to bung all of the variables into a single column on top of each other, but leave out the geometry column or it will throw an error
borough_melt <- melt(BoroughDataMap,id.vars = 1:6, measure.vars = 7:32)
#now join the geometry column back on - you will have to join it along with all of the data again.
borough_melt <- left_join(borough_melt,BoroughDataMap,by = c("code" = "code"))
#drop all data apart from the geometry column again
borough_melt <- borough_melt[,c(1:8,78)]

library(tmap)
library(sf)

borough_melt <- st_as_sf(borough_melt)

tmap_mode("plot")
library(tmap)
library(sf)

borough_melt <- st_as_sf(borough_melt)

tmap_mode("plot")
qtm(borough_melt, fill = "value", by = "variable")
ggplot()+geom_sf(mapping = aes(geometry=geometry, fill=value),data = borough_melt)+facet_wrap(~variable)


#Extension 2: Leaflet

library(leaflet)
library(sf)
library(sp)
library(magrittr)
library(classInt)

colours<- brewer.pal(5, "Blues")
breaks<-classIntervals(BoroughDataMap$Claimant.Rate.of.Housing.Benefit..2015., n=5, style="jenks")
graphics::plot(breaks, pal=colours)

summary(breaks)
breaks<-breaks$brks
BoroughDataMapSP<-BoroughDataMap %>%
  st_transform(crs=4326) %>%
  as("Spatial")

#Create a colour palette using colorBin colour mapping
leaflet(BoroughDataMapSP) %>%
  addPolygons(stroke = FALSE, 
              fillOpacity = 0.5, 
              smoothFactor = 0.5,
              color = ~pal(Claimant.Rate.of.Housing.Benefit..2015.),
              popup = ~name
  ) %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addLegend("bottomright", 
            pal= pal, 
            values = ~Claimant.Rate.of.Housing.Benefit..2015., 
            title = "Housing Benefit Claimant Rate", 
            labFormat = labelFormat(prefix = "Per 1,000 people "),
            opacity = 1
  )
