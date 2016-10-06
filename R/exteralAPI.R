
key = "AIzaSyBWD7q2E2YsJWmYbHjdr41jmMZntrUUtxs"
googlePlaces <- function() {
  return <- content(GET("https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=-33.8670,151.1957&radius=500&types=food&name=cruise&key=" + key))
}