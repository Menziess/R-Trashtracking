

trash <- read.csv('../Data/output.csv') 
trash <- filter(trash, latitude != 0 & latitude != 1 & longitude != 0 & longitude != 1)

places <- radarSearch(52.745, 5.221, 1000, 'food')

# API response analysis function
analyse <- function(trash, places) {
  places <- do.call(rbind, lapply(places$results, data.frame, stringsAsFactors=FALSE))
  
  #input$map_bounds select trash within bounds
  
  cl<-makeCluster(8)
  registerDoParallel(cl)
  
  

  stopCluster(cl)
  
  return (result)
}

test <- analyse(trash, places)