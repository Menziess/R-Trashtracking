
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

places2 <- radarSearch(lat, lng, rad, 'cafe')

analyzation <- analyse(trash, places)
analyzation2 <- analyse(trash, places2)
analyzation$Place <- paste('food' ,seq.int(nrow(analyzation)))
analyzation2$Place <- paste('cafe' ,seq.int(nrow(analyzation2)))

x<-merge(analyzation, analyzation2, all = T)


trash["dates"] <- NA
trash$dates <- as.POSIXct(trash$taken, format="%Y-%m-%d")
trash$dates <- as.Date(trash$dates, "%d-%m-%y")


test <- subset(trash, as.Date(trash$dates) >= as.Date("2012-07-22"))

# Overview

tabletype = table(trash$brand, trash$type)
types = as.data.frame(tabletype)
names(types)[1] = 'brand'
names(types)[2] = 'type'
names(types)[3] = 'amount'
types <- types[order(-types$amount),]
types <- filter(types, grepl('Red Bull|Heineken|Coca Cola|AH|AA|Spa|Amstel|Slammers',brand))
df = ddply(types, .(brand), transform, percent = round((amount/sum(amount) * 100),1))


ggplot(Mdata(), aes(x=reorder(brand,amount,function(x)+sum(x)), y=percent, fill=type))+
  geom_bar(position = "fill", stat='identity',  width = .7)+
  geom_text(aes(label=percent, ymax=100, ymin=0), vjust=0, hjust=2, color = "white",  position=position_fill())+
  coord_flip() +
  scale_y_continuous(labels = percent_format())+
  ylab("")+
  xlab("")
