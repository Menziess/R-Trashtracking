
##
# Distance matrix

lat <- 52.755
lng <- 5.221
rad <- 1000
type <- 'food'

trash <- read.csv('../Data/output.csv') 
trash <- filter(trash, latitude != 0 & latitude != 1 & longitude != 0 & longitude != 1
                & latitude > lat - 0.1 & latitude < lat + 0.1
                & longitude > lng - 0.1 & longitude < lng + 0.1)

places <- radarSearch(lat, lng, rad, type)

# API response analysis function
analyse <- function(trash, places) {
  places <- do.call(rbind, lapply(places$results, data.frame, stringsAsFactors=FALSE))
   
  # cl<-makeCluster(8)
  # registerDoParallel(cl)
  
  # create distance matrix
  matrix <- distm(trash[,c('longitude','latitude')], 
                  places[,c('geometry.location.lng','geometry.location.lat')], 
                  fun=distVincentyEllipsoid)
  
  # assign the name to the point in placecs based on shortest distance in the matrix
  trash$place_id <- places$place_id[apply(matrix, 1, which.min)]
  
  # stopCluster(cl)
  
  trash <- trash %>% count(place_id, sort = TRUE)
  total <- merge(trash, places, by=c("place_id", "place_id"))
  
  return (total)
}

total <- analyse(trash, places)
