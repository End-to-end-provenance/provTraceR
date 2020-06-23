# provTraceR script 1

df1 <- read.table("file-1.csv", sep=",")

df3 <- df1 + 2

write.table(df3, "file-3.csv", sep=",", row.names=FALSE, col.names=FALSE)

df4 <- df1 + df3

write.table(df4, "file-4.csv", sep=",", row.names=FALSE, col.names=FALSE)

