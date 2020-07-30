library(readxl)

# Load NH data
Full_NH <- read_excel("S:/Health/Epidemiology/Data/MDSS/Nursing Homes/Full NH Line List Export 5-24-2020.xlsx", col_names = TRUE)

# Make combination id as unique id for each patient, using first last and DOB
Full_NH <- Full_NH %>% mutate(
  comboID = paste0(`Patient First Name`, `Patient Last Name`, `Date of Birth`),
  rowNum = row_number()
) 
# Give them row numbers
Full_NH$rowNum <- as.character(Full_NH$rowNum)

# Start with empty new id field 
newID <- rep(0, dim(Full_NH)[1]) 
rowNum <- ""

for(i in 1:dim(Full_NH)[1]){ # For each record
  matched <- agrep(Full_NH$comboID[i], Full_NH$comboID, ignore.case = TRUE, value = FALSE, max.distance = 0.1) # Fuzzy match the combo ids
  # Each fuzzy matched id gets the same new id as its matches
  newID[i] <- paste0(i, "_Lauren")
  newID[matched] <- paste0(i, "_Lauren")
  rowNum[i] <- i
}
# Bind the new ids and row numbsers, then join back to dataframe
test_tib <- as_tibble(cbind(rowNum, newID)) %>% mutate_all(as.character())

together <- left_join(Full_NH, test_tib, by = "rowNum")
