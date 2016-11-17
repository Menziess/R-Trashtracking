
##
# Distance matrix

lat <- 52.745
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
  
  cl<-makeCluster(8)
  registerDoParallel(cl)
  
  # create distance matrix
  matrix <- distm(places[,c('geometry.location.lng','geometry.location.lat')], 
               trash[,c('longitude','latitude')], fun=distVincentyEllipsoid)
  
  # assign the name to the point in placecs based on shortest distance in the matrix
  places$locality <- trash$id[apply(matrix, 1, which.min)]
  
  stopCluster(cl)
  
  return (places)
}

test <- analyse(trash, places)
