#install.packages("epiR")
#install.packages("EpiCurve")
library(epiR)
library(EpiCurve)

## EXAMPLE 1: mean, standard error, and confidence from one column of data
dat <- rnorm(n = 100, mean = 0, sd = 1)
mean(dat)
sd(dat)
epi.conf(dat, ctype = "mean.single")

# EXAMPLE: incidence risk of occurrence
dat <- cbind(158, 9476)
epi.conf(dat, ctype = "inc.risk", method = "exact")

## EXAMPLE 2: Mean difference between groups, unpaired
group <- c(rep("A", times = 5), rep("B", times = 5))
val = round(c(rnorm(n = 5, mean = 10, sd = 5), 
              rnorm(n = 5, mean = 7, sd = 5)), digits = 0)
dat <- data.frame(group = group, val = val)
epi.conf(dat, ctype = "mean.unpaired")

## EXAMPLE 3:
## Two paired samples (Altman et al. 2000, page 31):
## Systolic blood pressure levels were measured in 16 middle-aged men
## before and after a standard exercise test. The mean rise in systolic 
## blood pressure was 6.6 mmHg. The standard deviation of the difference
## was 6.0 mm Hg. The standard error of the mean difference was 1.49 mm Hg.

before <- c(148,142,136,134,138,140,132,144,128,170,162,150,138,154,126,116)
after <- c(152,152,134,148,144,136,144,150,146,174,162,162,146,156,132,126)
dat <- data.frame(before, after)
dat <- data.frame(cbind(before, after))
epi.conf(dat, ctype = "mean.paired", conf.level = 0.95)
## The 95% confidence interval for the population value of the mean 
## systolic blood pressure increase after standard exercise was 3.4 to 9.8mm Hg.



## EXAMPLE 4:
## Single sample (Altman et al. 2000, page 47):
## Out of 263 giving their views on the use of personal computers in 
## general practice, 81 thought that the privacy of their medical file
## had been reduced.

pos <- 81
neg <- (263 - 81)
dat <- as.matrix(cbind(pos, neg))
round(epi.conf(dat, ctype = "prop.single"), digits = 3)
## The 95% confidence interval for the population value of the proportion
## of patients thinking their privacy was reduced was from 0.255 to 0.366. 



## EXAMPLE 5:
## Two samples, unpaired (Altman et al. 2000, page 49):
## Goodfield et al. report adverse effects in 85 patients receiving either
## terbinafine or placebo treatment for dermatophyte onchomychois.
## Out of 56 patients receiving terbinafine, 5 patients experienced
## adverse effects. Out of 29 patients receiving a placebo, none experienced
## adverse effects.

grp1 <- matrix(cbind(5, 51), ncol = 2)
grp2 <- matrix(cbind(0, 29), ncol = 2)
dat <- as.matrix(cbind(grp1, grp2))
round(epi.conf(dat, ctype = "prop.unpaired"), digits = 3)
## The 95% confidence interval for the difference between the two groups is
## from -0.038 to +0.193.


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

## EXAMPLE 6:
## Two samples, paired (Altman et al. 2000, page 53):
## In a reliability exercise, 41 patients were randomly selected from those
## who had undergone a thalium-201 stress test. The 41 sets of images were
## classified as normal or not by the core thalium laboratory and, 
## independently, by clinical investigators from different centres.
## Of the 19 samples identified as ischaemic by clinical investigators 
## 5 were identified as ischaemic by the laboratory. Of the 22 samples 
## identified as normal by clinical investigators 0 were identified as 
## ischaemic by the laboratory. 

## Clinic       | Laboratory    |             |   
##              | Ischaemic     | Normal      | Total
## ---------------------------------------------------------
##  Ischaemic   | 14            | 5           | 19
##  Normal      | 0             | 22          | 22
## ---------------------------------------------------------
##  Total       | 14            | 27          | 41
## ---------------------------------------------------------

dat <- as.matrix(cbind(14, 5, 0, 22))
round(epi.conf(dat, ctype = "prop.paired", conf.level = 0.95), digits = 3)
## The 95% confidence interval for the population difference in 
## proportions is 0.011 to 0.226 or approximately +1% to +23%.

## EXAMPLE 7:
## A herd of 1000 cattle were tested for brucellosis. Four samples out of 200
## test returned a positive result. Assuming 100% test sensitivity and 
## specificity, what is the estimated prevalence of brucellosis in this 
## group of animals?

pos <- 4; pop <- 200
dat <- as.matrix(cbind(pos, pop))
epi.conf(dat, ctype = "prevalence", method = "exact", N = 1000, 
         design = 1, conf.level = 0.95) * 100

## The estimated prevalence of brucellosis in this herd is 2 cases
## per 100 cattle (95% CI 0.54 -- 5.0 cases per 100 cattle).

## EXAMPLE 8:
## The observed disease counts and population size in four areas are provided
## below. What are the the standardised morbidity ratios of disease for each
## area and their 95% confidence intervals?

obs <- c(5, 10, 12, 18); pop <- c(234, 189, 432, 812)
dat <- as.matrix(cbind(obs, pop))
round(epi.conf(dat, ctype = "smr"), digits = 2)

## EXAMPLE 9:
## A survey has been conducted to determine the proportion of broilers
## protected from a given disease following vaccination. We assume that 
## the intra-cluster correlation coefficient for protection (also known as the 
## rate of homogeneity, rho) is 0.4 and the average number of birds per
## flock is 30. A total of 5898 birds from a total of 10363 were identified
## as protected. What proportion of birds are protected and what is the 95%
## confidence interval for this estimate?

## Calculate the design effect, given rho = (design - 1) / (nbar - 1), where
## nbar equals the average number of individuals sampled per cluster:

D <- 0.4 * (30 - 1) + 1

## The design effect is 12.6. Now calculate the proportion protected:

dat <- as.matrix(cbind(5898, 10363))
epi.conf(dat, ctype = "prevalence", method = "fleiss", N = 1000000, 
         design = D, conf.level = 0.95)

## The estimated proportion of the population protected is 0.57 (95% CI
## 0.53 -- 0.60). If we had mistakenly assumed that data were a simple random 
## sample the confidence interval would have been 0.56 -- 0.58.


EpiCurve()