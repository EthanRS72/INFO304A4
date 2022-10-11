citya <- read.csv('cityA.csv')
hist(citya$LOS)
citya$GENDER <- as.factor(citya$GENDER)
boxplot(citya$GENDER, citya$LOS)

males <- citya[which(citya$GENDER == 'M'),]
females <- citya[which(citya$GENDER == 'F'),]
par(mfrow= c(2,1))
hist(males$LOS)
hist(females$LOS)

par(mfrow= c(1,1))
citya.bedcount <- bedcount(citya)
plot.bc(citya.bedcount)
plot.bc(citya.bedcount,r=1:7)

citya.bedcount$weekday <- "weekday"
for (i in 1:nrow(citya.bedcount)) {
  if (i %% 7 == 1 || i %% 7 == 2) {
    citya.bedcount[i,]$weekday <- "weekend"
  } 
}

citya.bedcount$weekday <- as.factor(citya.bedcount$weekday)
boxplot(citya.bedcount$Beds ~ citya.bedcount$weekday  )


citya.mat <- citya[which(citya$HLTHSPEC == 'P60' | citya$HLTHSPEC == "P70"),]
citya.matbed <- bedcount(citya.mat)
par(mfrow = c(2,1))
plot.bc(citya.matbed)
plot.bc(citya.matbed,r=1:100)

head(sort(table(citya$diag01), decreasing = TRUE), 10)
citya.I48 <- citya[which(citya$diag01 == 'I48'),]
citya.I48bed <- bedcount(citya.I48)
citya.I48bed$weekday <- "weekday"
for (i in 1:nrow(citya.I48bed)) {
  if (i %% 7 == 1 || i %% 7 == 2) {
    citya.I48bed[i,]$weekday <- "weekend"
  } 
}
citya.I48 <- citya.I48[,-9]

citya.I48$EVSTDATE <- as.Date(citya.I48$EVSTDATE, "%Y-%m-%d")
citya.I48$EVSTDATE <- format(citya.I48$EVSTDATE, "%m")

citya.I48$EVENDATE <- as.Date(citya.I48$EVENDATE, "%Y-%m-%d")
citya.I48$EVENDATE <- format(citya.I48$EVENDATE, "%m")

colnames(citya.I48) <- c("AGE_ADM", "GENDER", "EVENT_TYPE", "ADM_MONTH", 
                         "REL_DATE", "LOS", "HLTH_SPEC", "DEP06")

citya.I48[,c(2:5, 7)] <- lapply(citya.I48[,c(2:5,7)], factor)


mdl <- test.lm(citya.I48, LOS ~., perc.train = 0.9)