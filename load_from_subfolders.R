library(svDialogs)

# Where is the main folder located?
location <- dlgDir()$res

# Find the file paths that match your pattern
my_paths <- list.files(location, pattern = "daily-test", recursive = TRUE, include.dirs = TRUE)

# Load the files
loaded <-  lapply(paste0(location, "/", my_paths), function(x)read.csv(x, header=TRUE))

# Combine all the data
combined.df <- do.call(rbind , loaded)
