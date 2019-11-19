#read the dataset

analytic <- read.csv("/Users/mindcontrol/Documents/R BRFSS Proj/analytic.csv", header = TRUE, sep = ",")

#looking at the distribution

AsthmaFreq <- table(analytic$ASTHMA4)
AsthmaFreq

write.csv(AsthmaFreq, file = "AsthmaFreq.csv")

#what proportion has ashtma in our data

PropAsthma <- 5343/52788
PropAsthma

#looking at categorical outcome ashtma by exposure, ALCGRP

AsthmaAlcFreq <- table(analytic$ASTHMA4, analytic$ALCGRP)
AsthmaAlcFreq

write.csv(AsthmaAlcFreq, file= "AsthmaAlcFrq.csv")


#looking at distribution of slepp duration

summary(analytic$SLEPTIM2)

#create a histogram and boxplot 

hist(analytic$SLEPTIM2, 
     main = "Histogram of SLEPTIM2",
     xlab = "Class SLEPTIM2",
     ylab = "Frequency",
     xlim=c(0,15), 
     ylim=c(0,20000),
     border = "red",
     col= "yellow",
     las = 1,
     breaks = 24)

boxplot(analytic$SLEPTIM2, main="Box Plot of SLEPTIM2", 
        xlab="Total File", ylab="SLEPTIM2")

#See box plots of groups next to each other

boxplot(SLEPTIM2~ALCGRP, data=analytic, main="Box Plot of SLEPTIM2 by ALCGRP", 
        xlab="ALCGRP", ylab="SLEPTIM2")




