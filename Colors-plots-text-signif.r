library(ggplot2)
library(ggsignif)
library(TeachingDemos)

# Offical City of Detroit color pallette
detroitColors <- c("#4e79a7", "#c4ced3", "#74a9cf", "#004445",	"#279989",	"#9fd5b3","#feb70d",	"#18252a",
                   "#f2f2f2","#2e3761",	"#5f355a",	"#94456c", "#cb4d4f",	"#b6683a",	"#629547",	"#a3c76f", 
                   "#b9ddf1", "#f28e2b", "#ffbe7d", "#607c97", "#899eb3","#e5ba64", "#e7cea2")


# prepare a colorful background
#1
randcolors <- sprintf( "#%02X%02X%02X99", sample(1:255, 1000, replace=T), sample(1:255, 1000,replace=T), sample(1:255, 1000, replace=T))
plot( NULL, xlim=c(0,1), ylim=c(0,1), xaxt="n", bty="n", yaxt="n")
points( runif(1000, 0, 1 ), runif( 1000, 0, 1 ), cex=runif(1000, 0.5, 5), col= randcolors, pch= 19)
shadowtext( 0.5, 0.5, "test text", cex= 4.5, col="white" , r=0.3)
#2 (Detroit Colors) 
plot( NULL, xlim=c(0,1), ylim=c(0,1), xaxt="n", bty="n", yaxt="n")
points( runif(1000, 0, 1 ), runif( 1000, 0, 1 ), cex=runif(1000, 0.5, 5), col= detroitColors, pch= 19)
shadowtext( 0.5, 0.5, "These are the City Colors", cex= 2.5, col="white" , r=0.3)

# Make a color more transparent:
transparent1 <- adjustcolor( "red", alpha.f = 0.2)
transparent2 <- adjustcolor( "red", alpha.f = 0.5)
plot(x = as.factor(c("a", "b", "c", "a", "b", "c")), y = c(2,4,6,12,24,36), col = c(transparent1, transparent2, "red"))

# add significance bar/stars
d <- data.frame(x = as.factor(c("a", "b", "c", "a", "b", "c")), y = c(2,4,6,12,24,36))
ggplot(d, aes(x=x, y=y, col = x)) + 
  geom_boxplot() +
  # to fill box plots with color, use aes(fill=x)
  geom_signif(comparisons = list(c("a", "b")), map_signif_level=TRUE)+
  geom_signif(comparisons = list(c("c", "b")), map_signif_level=TRUE)

# Flip a barplot
barplot(table(Assumed_Owner_Occupied$Building_Style), horiz = TRUE, col = detroitColors, cex.names = 0.5, las = 2) # las=2 makes 90 degree labels

# Add to ggplot to tilt labels on bars
+ theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Add a count column:
parcel_tib <- parcel_tib%>% add_count(Owner, name = "Total_Number_Owned") # Column with total number properties owned by each owner
#Test number per child(UCI) is counted up by each sorted UCI, and added to a new column 
dedup_dat <- dedup_dat[order(dedup_dat$UCI, dedup_dat$Specimen_Date),]
dedup_dat <-dedup_dat %>% group_by(UCI) %>% dplyr::mutate(id = dplyr::row_number())