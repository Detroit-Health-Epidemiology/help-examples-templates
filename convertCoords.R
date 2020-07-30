library(rgdal)
# Get ready to transform the coordinates from MDSS to usable in Tableau!
add2_coords <- add2[!is.na(add2$Investigation_Addr_Geo_X)&!is.na(add2$Investigation_Addr_Geo_Y),]
x <- add2_coords$Investigation_Addr_Geo_X
y <- add2_coords$Investigation_Addr_Geo_Y

# Define the coordinate systems.
d <- data.frame(lon=x, lat=y, add2$InvestigationID)
coordinates(d) <- c("lon", "lat")
proj4string(d) <- CRS("+init=epsg:3078") 
CRS.new <- CRS("+init=epsg:4326")
transformed <- spTransform(d, CRS.new)

# Plot the results.
par(mfrow=c(1,3))
plot.default(x,y, main="Raw data", cex.axis=.95)
plot(d, axes=TRUE, main="Original lat-lon", cex.axis=.95)
plot(transformed, axes=TRUE, main="Projected", cex.axis=.95)

unclass(transformed)
newCoordinates <- data.frame(coordinates(transformed)[,1], coordinates(transformed)[,2], as.character(transformed[1]$add2.InvestigationID))
colnames(newCoordinates) <- c("Transformed_Longitude", "Transformed_Lattitude", "InvestigationID")
covidDataNew<- left_join(covidDataNew, newCoordinates, by = "InvestigationID")