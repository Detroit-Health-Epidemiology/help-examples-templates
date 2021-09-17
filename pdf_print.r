library(gridExtra)
library(svDialogs)

locale <- dlgDir()$res
myTable <- data.frame(1:20, rnorm(n=20, mean=5, sd=1), rnorm(n=20, mean=5, sd=1))
colnames(myTable) <- c("Name", "A", "B")
rownames(myTable) <- 1:20

myTable2 <- rbind(myTable, myTable)

for(i in unique(myTable2$Name)){
  pdf(file=paste0(locale,"/", i, "_test.pdf"),width  = 16, height = 32, onefile = TRUE) 
  tib <- myTable2[myTable2$Name==i,]
  grid.table(tib)
  dev.off()
}