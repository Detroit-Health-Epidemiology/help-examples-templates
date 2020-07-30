library(censusxy)


# Add coordinates to the street addresses, Takes a long time!
1- Takes list of positive addresses which are complete, and not PO box or a lab
2- Compares to census data and creates lattitude and longitude
3- Joins lattitude and longitude back into the complete dataframe, matching to appropriate row
```{r findCoords}
# Make a subset with investigation ID and address for positive cases
posOnly <- covidData[covidData$Case_Status == "Confirmed",]
add <- as.data.frame(cbind(posOnly$Street_Address, posOnly$City, posOnly$State, posOnly$Simple_Investigation_Zip, posOnly$InvestigationID))

# Take out rows with missing pieces, or not Detroit, MI
add <- add[!is.na(add[,1])&add[,1]!="",]
add <- add[add[,2]=="Detroit"|add[,2] =="detroit"|add[,2] =="DETROIT"|add[,2] =="Detroit City",]
add <- add[add[,3]=="Michigan"|add[,3]=="MI",]
add <- add[!is.na(add[,4])&add[,4]!="",]

# Remove PO Boxes and 400 Mack (WSU) and 4201 Antoine (DMC) from addresses to be given coords
# Remove Jefferson Ave E Nursing home testing site
add <- add[!grepl("(400 Mack|PO Box|P O BOX|P.O. BOX|4201 ST ANTOINE ST|4201 ST ANTOINE ST STE A|Unknown|homeless)", add$V1, ignore.case = TRUE),]

# THE ACTION #
# Get coords for street addresses by pinging the USA census data
addresses <- cxy_geocode(add, address = V1, city = V2, state = V3, zip = V4,
                         style = "minimal", output = "sf")

# For each address, separate out lat and long coords
Lattitude <- c()
Longitude <- c()
for(i in 1:dim(addresses)[1]){
  Longitude[i] <- addresses$geometry[[i]][1]
  Lattitude[i] <- addresses$geometry[[i]][2]
}

# bind latittude, longitude, and investigation ID into a dataframe, then join back into covidData
coords <- as.data.frame(cbind(as.character(addresses$V5), Lattitude, Longitude))
covidData$InvestigationID <- as.character(covidData$InvestigationID)
coords$V1 <- as.character(coords$V1)
covidData <- left_join(covidData, coords, by = c("InvestigationID"="V1"))
