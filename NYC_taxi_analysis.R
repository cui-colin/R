library(dplyr)
library(chron)
library(ggmap)
library(ggplot2)
library(rgdal)
library(tigris)
taxi <- read.table("data/NYC_yellow_2015_7_clean_small.csv", header = TRUE)
hist(taxi$pickup_latitude, breaks = 10)
plot(density(taxi$pickup_latitude))
hist(taxi$pickup_longitude)
plot(density(taxi$pickup_longitude))

taxi$tpep_pickup_datetime <- as.character(taxi$tpep_pickup_datetime)
taxi$tpep_dropoff_datetime <- as.character(taxi$tpep_dropoff_datetime)
dtparts <- t(as.data.frame(strsplit(taxi$tpep_pickup_datetime, ' ')))
row.names(dtparts) = NULL
thetimes = chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))
head(as.POSIXct(thetimes))
head(as.POSIXct(taxi$tpep_dropoff_datetime))

# Exploratory Data Analysis
# 1) where is the most popular pick up?
# pickup latitude: 40.75279
# pickup longitude: -73.98218
median(taxi$pickup_latitude) # pickup latitude: 40.75279
length(which(taxi$pickup_latitude == 40.75279)) # 458

median(taxi$pickup_longitude) # pickup longitude: -73.98218
length(which(taxi$pickup_longitude == -73.98218)) # 1253

# 2) most frequent dropoff location (pair of latitutde and longitue)
median(taxi$dropoff_latitude) # pickup latitude: 40.75351
length(which(taxi$dropoff_latitude == 40.75351)) # 540

median(taxi$dropoff_longitude) # pickup longitude: -73.98049
length(which(taxi$dropoff_longitude == -73.98049)) # 653

# maps
all_states <- map_data('state') 
ny <- subset(all_states, subregion %in% c( "manhattan") )

# NYC longtitue and latitude bounds
# latitude: 40 < x < 42 
# longtitue: -75< x < -72
# Ma
xRange <- c(-74.03, -73.92)
yRange <- c(40.7, 40.82)

lookup_code("New York", "Brox")

counties <- c(5, 47, 61, 81, 85)
tracts <- tracts(state = 'NY', county = c(5, 47, 61, 81, 85), cb=TRUE)
geo<-geo.make(state=c("NY"),
              county=c(5, 47, 61, 81, 85), tract="*")



tract <- readOGR(dsn=".", layer = "cb_2014_36_tract_500k")

ggtract<-fortify(tract, region = "GEOID") 
# join tabular data
ggtract<-left_join(ggtract, data, by=c("id")) 

# here we limit to the NYC counties
ggtract <- ggtract[grep("Kings|Bronx|New York County|Queens", ggtract$geography),]

ggplot() +
  geom_polygon(data = ggtract , aes(x=long, y=lat, group = group, fill=percent), color="grey50") +
  scale_fill_gradientn(colours = c("red", "white", "cadetblue"),
                       values = c(1,0.5, .3, .2, .1, 0))+
  coord_map(xlim = c(-74.26, -73.71), ylim = c(40.49,40.92))



zones <-readOGR("/Users/Colin/Desktop/taxi_zones", layer="taxi_zones")
ggplot() +  geom_polygon(data=zones, aes(x=long, y=lat, group=group))



mymap <- get_map(location = "New York", maptype = "watercolor")
mymap <- get_map(location = "New York", source = "stamen", zoom = 10)

ggmap(mymap)
# get_openstreetmap(bbox = c(left = -74.2065, bottom = 40.6327, right = -73.4903, top = 40.9633), format = "png")


# median
# 40.75279
# -73.98049
# Method 1: centering
mymap <- get_map(c(-73.98, 40.79), maptype = "roadmap", zoom = 12, color = "bw")
ggmap(mymap)

# Method 2: define boundary
mymap <- get_map(c(left = -74.35, bottom = 40.6327, right = -73.4903, top = 40.9633), maptype = "roadmap", zoom = 12)
ggmap(mymap)

# Method 3: another center defined using source
mymap <- get_map(c(-73.95, 40.79), source = "google", zoom = 12)
ggmap(mymap)


#############
# Upper East Side and Astoria 
# 40.764198, -73.973001
