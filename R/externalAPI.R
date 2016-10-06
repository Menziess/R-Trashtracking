
# API Keys
#
googleKey = "AIzaSyBWD7q2E2YsJWmYbHjdr41jmMZntrUUtxs"

# Google Places API
# 
# https://developers.google.com/places/web-service/
#

googlePlaces <- function(latitude = 0, longitude = 0, radius = 500, types = NULL, name = NULL) {
  r <- GET("https://maps.googleapis.com/maps/api/place/nearbysearch/json?", query = list(location=paste(latitude, longitude), radius=radius, types=types, name=name, key=googleKey))
  return (content(r, "parsed"))
}

# Other API
#