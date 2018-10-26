#install packages
install.packages("MASS")
library(MASS)

##Importing files
baseballHOF <- read.csv("~/Downloads/baseballHOF.csv")
baseballHOF2019 <- read.csv("~/Downloads/baseballHOF2019.csv")


### Descriptive Statistics by HoF.

library(dplyr)
describe.by(baseballHOF,baseballHOF$HoF)



## Classification analysis on previous year Hall of Fame data
attach(baseballHOF)
lda.baseballHOF <- lda(baseballHOF$HoF~ Yrs + WAR + WAR7 + JAWS + Jpos + G+ AB+ R+H+HR+RBI
                       +SB+BB+BA+OBP+SLG+OPS+OPSadj)
lda.baseballHOF

## Cut off point from the past years Hall of Fame.

zbar.no <- sum(lda.baseballHOF$scaling*lda.baseballHOF$means[1,])
zbar.yes <- sum(lda.baseballHOF$scaling*lda.baseballHOF$means[2,])
cutoof <- (zbar.no+zbar.yes)/2
cutoff_point <- (zbar.no+zbar.yes)/2
cutoff_point


## 
attach(baseballHOF2019)
Todd_Helton <- baseballHOF2019[1,]
Todd_Helton$Name<- NULL
sum(lda.baseballHOF$scaling*Todd_Helton)
Helton <- sum(lda.baseballHOF$scaling*Todd_Helton)
baseballHOF2019$Name <- NULL
Tejada <- sum(lda.baseballHOF$scaling*baseballHOF2019[3,])
Youkilis <- sum(lda.baseballHOF$scaling*baseballHOF2019[4,])
Wells <- sum(lda.baseballHOF$scaling*baseballHOF2019[5,])
Hafner <- sum(lda.baseballHOF$scaling*baseballHOF2019[6,])
Bay <- sum(lda.baseballHOF$scaling*baseballHOF2019[7,])
Young <- sum(lda.baseballHOF$scaling*baseballHOF2019[8,])
results <- c(Helton,Tejada,Youkilis,Wells,Hafner,Bay,Young)
names(results) <- c("Helton","Tejada","Youkilis","Wells","Hafner","Bay","Young")
results
