
# API Keys
#
googleKey = "AIzaSyBWD7q2E2YsJWmYbHjdr41jmMZntrUUtxs"

# Google Places API
# 
# 150k requests can be made with 3 different methods
# https://developers.google.com/places/web-service/
#
# A Nearby Search lets you search for places within a specified area. You can refine your search-
# request by supplying keywords or specifying the type of place you are searching for.
nearbySearch <- function(latitude = 0, longitude = 0, radius = 100, type = NULL, name = NULL, keyword = NULL) {
  r <- GET("https://maps.googleapis.com/maps/api/place/nearbysearch/json?", query = list(location=paste(latitude, longitude), radius=radius, type=type, name=name, keyword=keyword, key=googleKey))
  return (content(r, "parsed"))
}

# The Google Places API Text Search Service is a web service that returns information about a set of 
# places based on a string — for example "pizza in New York" or "shoe stores near Ottawa". 
textSearch <- function(search = NULL, latitude = 0, longitude = 0, radius = 100, type = NULL, name = NULL, keyword = NULL) {
  r <- GET("https://maps.googleapis.com/maps/api/place/textsearch/json?", query = list(query=search, location=paste(latitude, longitude), radius=radius, type=type, name=name, keyword=keyword, key=googleKey))
  return (content(r, "parsed"))
}

# The Google Places API Radar Search Service allows you to search for up to 200 places at once, 
# but with less detail than is typically returned from a Text Search or Nearby Search request.
radarSearch <- function(latitude = 0, longitude = 0, radius = 100, type = NULL, name = NULL, keyword = NULL) {
  if (is.null(type))
    stop('Missing required parameter "type"')
  tryCatch(r <- GET("https://maps.googleapis.com/maps/api/place/radarsearch/json?", query = list(location=paste(latitude, longitude), radius=radius, type=type, name=name, keyword=keyword, key=googleKey)))
  return (content(r, "parsed"))
}

# API response analysis function
analyse <- function(trash, places) {
  # places <- do.call(rbind, lapply(places$results, data.frame, stringsAsFactors=FALSE))
  matrix <- distm(trash[,c('longitude','latitude')], 
                  places[,c('geometry.location.lng','geometry.location.lat')], 
                  fun=distVincentyEllipsoid)
  trash$place_id <- places$place_id[apply(matrix, 1, which.min)]  
  trash <- trash %>% count(place_id, sort = TRUE)
  total <- merge(trash, places, by=c("place_id", "place_id"))
  
  return (total)
}

# Convert meters to bearing
distanceInLatLng <- function(meters) {
  return(meters / 100)
}