voice <- read.table("E:/DC_WorkLoad/VOICE.csv", sep=";", quote="\"")

names(voice) <- c("DATE","Q")
voice.ts     <- ts(voice[, -1], start = c(2013,1,2), frequency = 365)

plot(voice.ts)