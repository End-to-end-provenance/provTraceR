# provTraceR script 2

df2 <- read.table("file-2.csv", sep=",")

df3 <- read.table("file-3.csv", sep=",")

df5 <- df2 + df3

write.table(df5, "file-5.csv", sep=",", row.names=FALSE, col.names=FALSE)

