#Make tibble
ex <- as.tibble(rbind(c("Banana", "Apple", "Confirmed", "Deceased", "x"), c("Banana", "Orange", "Confirmed", "Deceased", "x"), c("Banana", "Apple", "Probable", "Alive", "x"), 
                      c("Grapefruit", "Orange", "Confirmed", "Deceased", "x"), c("Grapefruit", "Orange", "Confirmed", "Alive", "x"), c("Blueberry","Raspberry", "Probable", "Deceased", "x")))
colnames(ex) <- c("N1", "N2", "Status", "Deceased_Status", "x")

#Group to get count by unique combo of the two names and status
ex2 <- ex %>% group_by(N1, N2, Status) %>% summarise(count=n()) %>% mutate(
  #Take the sum of counts by status and make into separate columns
  confirmed_count = sum(count[Status=="Confirmed"]),
  probable_count = sum(count[Status=="Probable"])
  )

ex3 <- ex %>% group_by(N1, N2, Deceased_Status) %>% summarise(count=n()) %>% mutate(
  #Take the sum of death counts accross all statuses
  death_count = sum(count[Deceased_Status=="Deceased"])
)

#Group by the names and new count columns only
ex4 <- ex2 %>% group_by(N1, N2, confirmed_count, probable_count) %>% summarise()

#Group by the names and death count column only
ex5 <- ex3 %>% group_by(N1, N2, death_count) %>% summarise()

#Attach the status counts and death counts to unique name combos (for each N1&N2 combo, one count for each status and total death count)
final <- left_join(ex4, ex5, by = c("N1", "N2"))