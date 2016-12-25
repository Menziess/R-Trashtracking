
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
# nearbySearch <- function(latitude = 0, longitude = 0, radius = 100, type = NULL, name = NULL, keyword = NULL) {
#   r <- GET("https://maps.googleapis.com/maps/api/place/nearbysearch/json?", query = list(location=paste(latitude, longitude), radius=radius, type=type, name=name, keyword=keyword, key=googleKey))
#   return (content(r, "parsed"))
# }

# The Google Places API Text Search Service is a web service that returns information about a set of 
# places based on a string — for example "pizza in New York" or "shoe stores near Ottawa". 
# textSearch <- function(search = NULL, latitude = 0, longitude = 0, radius = 100, type = NULL, name = NULL, keyword = NULL) {
#   r <- GET("https://maps.googleapis.com/maps/api/place/textsearch/json?", query = list(query=search, location=paste(latitude, longitude), radius=radius, type=type, name=name, keyword=keyword, key=googleKey))
#   return (content(r, "parsed"))
# }

# The Google Places API Radar Search Service allows you to search for up to 200 places at once, 
# but with less detail than is typically returned from a Text Search or Nearby Search request.
radarSearch <- function(latitude = 0, longitude = 0, radius = 100, type = NULL, name = NULL, keyword = NULL) {
  if (is.null(type))
    stop('Missing required parameter "type"')
  tryCatch({r <- GET("https://maps.googleapis.com/maps/api/place/radarsearch/json?", query = list(location=paste(latitude, longitude), radius=radius, type=type, name=name, keyword=keyword, key=googleKey))},
           warning = function(W) {
             print(w)
           },
           error = function(e) {
             print(e)
           })
  return (content(r, "parsed"))
}

#The Google Places API Location Search Service allows you to search for a location and get all the details from Google.
locationSearch <- function(placeid) {
  if (is.null(placeid))
    stop('Missing required parameter "placeid"')
  tryCatch(r <- GET("https://maps.googleapis.com/maps/api/place/details/json?", query = list(placeid=placeid, key=googleKey)))
  return (content(r, "parsed"))
}

# API response analysis function to associate trash with places
analyse <- function(trash, places) {
  if(nrow(trash) < 1 || is.null(places) || nrow(places) < 1) return (NULL)
  matrix <- distm(
    trash[,c('longitude','latitude')], 
    places[,c('geometry.location.lng','geometry.location.lat')], 
    fun=distVincentyEllipsoid
  )

  trash$place_id <- places$place_id[apply(matrix, 1, which.min)]  
  trash <- trash %>% count(place_id, sort = T)
  total <- merge(trash, places, by="place_id") %>% arrange(desc(n))
  total <- total[,1:4]
  colnames(total) <- c("Place", "Amount", "Latitude", "Longitude")

  return (head(total, 10))
}

# Returns dataframe with detailed places information
retrievePlacesDetails <- function(analyzation = NULL) {
  if (is.null(analyzation) || !nrow(analyzation) > 1)
    return()
  googleData <- data.frame(matrix(nrow = 0, ncol = 8))
  colnames(googleData) <- c('Name', 'Lng', 'Lat', 'Icon', 'Address', 'Phone', 'Website', 'Place')
  for(i in analyzation$Place) {
    response <- locationSearch(i)
    if (is.null(response)) stop("Response was NULL")
    googleData[i,1] <- ifelse(is.null(response$result$name), "Unknown", response$result$name)
    googleData[i,2] <- ifelse(is.null(response$result$geometry$location$lng), 0, response$result$geometry$location$lng)
    googleData[i,3] <- ifelse(is.null(response$result$geometry$location$lat), 0, response$result$geometry$location$lat)
    googleData[i,4] <- ifelse(is.null(response$result$icon), "https://cdn4.iconfinder.com/data/icons/online-store/300/404-512.png", response$result$icon)
    googleData[i,5] <- ifelse(is.null(response$result$formatted_address), "Unknown", response$result$formatted_address)
    googleData[i,6] <- ifelse(is.null(response$result$formatted_phone_number), "Unknown", response$result$formatted_phone_number)
    googleData[i,7] <- ifelse(is.null(response$result$website), "Unknown", response$result$website)
    googleData[i,8] <- i
  }
  googleData <- merge(googleData, analyzation, by="Place")
  rownames(googleData) <- googleData$Place
  googleData <- googleData[c(2,3,4,5,6,7,8,9,10,1)]
  return (googleData)
}

# Convert meters to distance in latitude and distance in longitude
metersToLatLng <- function(lat, lng, meters) {
  R=111111                    # Rough amount of meters per degree
  dLat = abs(meters/R)        # Difference in latitude
  dLng = abs(dLat * cos(lat)) # Difference in longitude

  return (list(dLat, dLng))
}