location <- "S:/Health/Epidemiology/Data/MDSS/original/COVD-19 Coronavirus/2020_03_Coronavirus"

# load the 2 halves
covidData1 <- as_tibble(readWorkbook(paste0(location, "/", "BaseCaseInvestigation1sthalf.xlsx"), sheet = 1, na.strings = c('NA', '#N/A', ''), detectDates = TRUE)) 
covidData2 <- as_tibble(readWorkbook(paste0(location, "/", "BaseCaseInvestigation2ndhalf.xlsx"), sheet = 1, na.strings = c('NA', '#N/A', ''), detectDates = TRUE))

# Make sure columns are in same order
table((colnames(covidData1) == colnames(covidData2)))

covidData1 <- covidData1 %>% mutate_all(as.character)
covidData2 <- covidData2 %>% mutate_all(as.character)

covidData_All <- bind_rows(covidData1, covidData2)
