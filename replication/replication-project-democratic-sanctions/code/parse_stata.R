setwd("/Users/jakoblutkemeier/Documents/projects/replication-project-democratic-sanctions")

install.packages("haven")
library(haven)

df <- read_dta("src/vS&W_replicationJPR.dta")
View(df)
str(df)
names(df)
summary(df)

write.csv(df, "data/vSW_replicationJPR.csv", row.names = FALSE)


do_text <- readLines("src/vS&W_replicationJPR.do")
cat(do_text, sep = "\n")


