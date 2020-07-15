# provTraceR script 3

df6 <- read.table("file-6.csv", sep=",")

write.table(df6, "file-6.csv", sep=",", row.names=FALSE, col.names=FALSE)

x <- rep(7, 10)

df7 <- data.frame(x,x,x,x,x,x,x,x,x,x)

write.table(df7, "file-7.csv", sep=",", row.names=FALSE, col.names=FALSE)

df7 <- read.table("file-7.csv", sep=",")

