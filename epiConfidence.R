
#Example with 2018 Infant Death Data risk("rate")
# Town names
locale <- c("Ann Arbor", "Battle Creek", "Dearborn","Dearborn Heights", "Detroit", "Farmington Hills","Flint", "Grand Rapids", "Kalamazoo",
            "Kentwood", "Lansing", "Livonia", "Midland", "Muskegon", "Novi", "Pontiac", "Portage", "Rochester Hills", "Roseville", "Royal Oak",
            "Saginaw", "Southfield", "St Clair Shores", "Sterling Heights", "Taylor", "Troy", "Warren", "Westland", "Wyoming")
# Number of infant deaths
dead <- c(6,4,8,4,158,3,14,14,6,5,7,3,2,3,9,9,1,3,5,2,11,8,2,12,5,6,11,9,5)
# Number of live births
born <- c(1019,696,1663,840,9476,936,1239,2996,1030,817,1632,911,467,526,656,1014,537,756,561,739,771,739,650,1381,837,768,1736,1006,1129)

# Combine into dataframe with locale as row names
dat <- data.frame(dead, born, row.names = locale)
dat

# Calculate incidence and confidence intervals by locale ( two ways[same results in this case], also can choose exact, wilson, or fleiss method )
epi.conf(as.matrix(dat), ctype = "inc.risk", method = "wilson")
deathrate <- epi.conf(as.matrix(dat), ctype = "prevalence", method = "wilson") # save to variable
deathrate <- cbind(deathrate[1]*1000, deathrate[,2:4]) # Turn into rate
deathrate <- cbind(dead, born, deathrate) # Add other columns in
colnames(deathrate) <- c("Infant Deaths", "Live Births", "Est", "SE", "Lower", "Upper") #Update column names

#Remove rates and intervals for locales without enough death data (<6 events)
for(i in 1:length(deathrate$`Infant Deaths`)){ 
  d <- as.numeric(deathrate$`Infant Deaths`[i])
  if(d < 6){ # If fewer than 6 events, set estimate and intervals to NA
    deathrate$Est[i] <- NA
    deathrate$Lower[i] <- NA
    deathrate$Upper[i] <- NA
  }
}
deathrate

# If we have data for more years, we can compare proportions;
dead2 <- round(abs(rnorm(n = 29, mean = 25, sd = 40)), digits = 0)  # Making fake data for year2
born2<- round(abs(rnorm(n = 29, mean = 1700, sd = 1000)), digits =0)
dat <- cbind(dat, dead2, born2)
dat

# See if the two proportions are significantly different (Null Hypothesis is proportion1 = proportion2)
all <- prop.test(x = c(dat$dead, dat$dead2), n = c(dat$born, dat$born2)) # For all at once, gives p-value for comparison of all locales by year
# consider one locale:
detroit <- which(rownames(dat)=="Detroit")
prop.test(x=c(dat$dead[detroit], dat$dead2[detroit]), 
          n = c(dat$born[detroit], dat$born2[detroit]))

# Get p-value of year comparison for each locale and append to dataframe
pval <- c()
prop1 <- c()
prop2 <- c()
for(i in 1:length(rownames(dat))){
  loc <- rownames(dat)[i]
  propt <- prop.test(x=c(dat$dead[i], dat$dead2[i]), 
                     n = c(dat$born[i], dat$born2[i]))
  pval[i] <- round(propt$p.value, digits = 5)
  prop1[i] <- propt$estimate[1]
  prop2[i] <- propt$estimate[2]
}
finaldat <- cbind(dat, pval, prop1, prop2)
colnames(finaldat) <- c("Infant Deaths Year1", "Live Births Year1", "Infant Deaths Year2", "Live Births Year2", "p-value", "Year1 Proportion", "Year2 Proportion") #Update column names
finaldat