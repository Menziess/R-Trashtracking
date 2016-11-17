
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

##
# Cluster example
clusters <- function(x, centers) {
  # compute squared euclidean distance from each sample to each cluster center
  tmp <- sapply(seq_len(nrow(x)),
                function(i) apply(centers, 1,
                                  function(v) sum((x[i, ]-v)^2)))
  max.col(-t(tmp))  # find index of min distance
}

# create a simple data set with two clusters
set.seed(1)
x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
colnames(x) <- c("x", "y")
x_new <- rbind(matrix(rnorm(10, sd = 0.3), ncol = 2),
               matrix(rnorm(10, mean = 1, sd = 0.3), ncol = 2))
colnames(x_new) <- c("x", "y")

cl <- kmeans(x, centers=2)

all.equal(cl[["cluster"]], clusters(x, cl[["centers"]]))
# [1] TRUE
clusters(x_new, cl[["centers"]])
# [1] 2 2 2 2 2 1 1 1 1 1

plot(x, col=cl$cluster, pch=3)
points(x_new, col= clusters(x_new, cl[["centers"]]), pch=19)
points(cl[["centers"]], pch=4, cex=2, col="blue")