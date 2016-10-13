# loading the required packages
library(ggplot2)
library(ggmap)
library(dplyr)

dataset <- read.csv(file="C:/Users/somaye/Documents/Big Data/Project/output.csv",head=TRUE,sep=",")
head(dataset)

# creating a sample data.frame with your lat/lon points
lon <- select(dataset, longitude)
head(lon)
lat <- select(dataset, latitude)
head(lat)
df <- as.data.frame(cbind(lon,lat))
head(df)

# getting the map
mapgilbert <- get_map(location = c(lon = mean(df$lon), lat = mean(df$lat)), zoom = 4,
                      maptype = "satellite", scale = 2)

# plotting the map with some points on it
ggmap(mapgilbert) +
  geom_point(data = df, aes(x = lon, y = lat, fill = "red", alpha = 0.8), size = 5, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

