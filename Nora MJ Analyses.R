#Nora MJ Analayses
#RSA 2018

setwd("C:/Users/sbuja/Documents/Conferences/RSA 2018/")

#required libraries
library(SpPack) #Spencer's suite of functions
library(plyr) #data wrangling
library(dplyr) #data wrangling 2
library(tidyr) #data wrangling 3
library(xlsx) #package to import xls files directly
library(psych) #for alpha (cronbach's alpha function)
library(stats) #supplemental stats functions
library(gmodels) #CrossTable Function

#Age, Female, edu, maritalst, ATLFB, CTLFB, MTLFB, BDItot, BAItot, ftnd_1, FTND, AUDIT 
#Three predictors cudituse (binary marijuana self-report), CUDIT (CUDIT total score, MJ consequences), Drug_THC_Beh (Drug test results)

#Import Data----
MJData <- read.csv("MJ merged dataset - Full - Fat Update.csv")

#Computing variables

#Age
SpDesc(MJData$Age)
SpHist(MJData, variable="Age")
MJData$Age.sq <- sqrt(MJData$Age)
SpHist(MJData, variable="Age.sq")
MJData$Age.log <- log(MJData$Age)
SpHist(MJData, variable="Age.log")

#Female
MJData$Female <- MJData$Sex - 1
SpDesc(as.factor(MJData$Female))

#ethnicity
table(MJData$ethnicity)
MJData$ethnicity <- as.factor(MJData$ethnicity)
SpDesc(MJData$ethnicity)

MJData$ethnicity <- ifelse(MJData$ethnicity==8,5,MJData$ethnicity) #putting middle eastern(8) into white(5) 
table(MJData$ethnicity)

MJData$ethnicity <- ifelse(MJData$ethnicity==7,6,MJData$ethnicity) #putting other(7) into multiracial/other(6) 
table(MJData$ethnicity)

#educat
table(MJData$educat)
MJData$edu <- ifelse(MJData$educat<=1,0,
                     ifelse(MJData$educat==2,1,
                            ifelse(MJData$educat==3,2,
                                   ifelse(MJData$educat>=4,3,NA))))
table(MJData$edu)

#maritalst

#ATLFB
MJData$Drink.Tot <- rowSums(subset(MJData, select=c("ATLFB_01", "ATLFB_02", "ATLFB_03", "ATLFB_04", "ATLFB_05", "ATLFB_06", "ATLFB_07", "ATLFB_08", "ATLFB_09", "ATLFB_10",
                                                    "ATLFB_11", "ATLFB_12", "ATLFB_13", "ATLFB_14", "ATLFB_15", "ATLFB_16", "ATLFB_17", "ATLFB_18", "ATLFB_19", "ATLFB_20",
                                                    "ATLFB_21", "ATLFB_22", "ATLFB_23", "ATLFB_24", "ATLFB_25", "ATLFB_26", "ATLFB_27", "ATLFB_28", "ATLFB_29", "ATLFB_30")))
MJData$Drink.Days <- rowSums(subset(MJData, select=c("ATLFB_01", "ATLFB_02", "ATLFB_03", "ATLFB_04", "ATLFB_05", "ATLFB_06", "ATLFB_07", "ATLFB_08", "ATLFB_09", "ATLFB_10",
                                                     "ATLFB_11", "ATLFB_12", "ATLFB_13", "ATLFB_14", "ATLFB_15", "ATLFB_16", "ATLFB_17", "ATLFB_18", "ATLFB_19", "ATLFB_20",
                                                     "ATLFB_21", "ATLFB_22", "ATLFB_23", "ATLFB_24", "ATLFB_25", "ATLFB_26", "ATLFB_27", "ATLFB_28", "ATLFB_29", "ATLFB_30"))>0)
MJData$DPD <- MJData$Drink.Tot/30
MJData$DPW <- MJData$DPD * 7
MJData$DPDD <- MJData$Drink.Tot/MJData$Drink.Days

SpDesc(MJData[c("Drink.Tot", "Drink.Days", "DPD", "DPW", "DPDD")])

#Compute Binge Variables
MJData$ATLFB_01F <- MJData$ATLFB_01 + MJData$Female
MJData$ATLFB_02F <- MJData$ATLFB_02 + MJData$Female
MJData$ATLFB_03F <- MJData$ATLFB_03 + MJData$Female
MJData$ATLFB_04F <- MJData$ATLFB_04 + MJData$Female
MJData$ATLFB_05F <- MJData$ATLFB_05 + MJData$Female
MJData$ATLFB_06F <- MJData$ATLFB_06 + MJData$Female
MJData$ATLFB_07F <- MJData$ATLFB_07 + MJData$Female
MJData$ATLFB_08F <- MJData$ATLFB_08 + MJData$Female
MJData$ATLFB_09F <- MJData$ATLFB_09 + MJData$Female
MJData$ATLFB_10F <- MJData$ATLFB_10 + MJData$Female
MJData$ATLFB_11F <- MJData$ATLFB_11 + MJData$Female
MJData$ATLFB_12F <- MJData$ATLFB_12 + MJData$Female
MJData$ATLFB_13F <- MJData$ATLFB_13 + MJData$Female
MJData$ATLFB_14F <- MJData$ATLFB_14 + MJData$Female
MJData$ATLFB_15F <- MJData$ATLFB_15 + MJData$Female
MJData$ATLFB_16F <- MJData$ATLFB_16 + MJData$Female
MJData$ATLFB_17F <- MJData$ATLFB_17 + MJData$Female
MJData$ATLFB_18F <- MJData$ATLFB_18 + MJData$Female
MJData$ATLFB_19F <- MJData$ATLFB_19 + MJData$Female
MJData$ATLFB_20F <- MJData$ATLFB_20 + MJData$Female
MJData$ATLFB_21F <- MJData$ATLFB_21 + MJData$Female
MJData$ATLFB_22F <- MJData$ATLFB_22 + MJData$Female
MJData$ATLFB_23F <- MJData$ATLFB_23 + MJData$Female
MJData$ATLFB_24F <- MJData$ATLFB_24 + MJData$Female
MJData$ATLFB_25F <- MJData$ATLFB_25 + MJData$Female
MJData$ATLFB_26F <- MJData$ATLFB_26 + MJData$Female
MJData$ATLFB_27F <- MJData$ATLFB_27 + MJData$Female
MJData$ATLFB_28F <- MJData$ATLFB_28 + MJData$Female
MJData$ATLFB_29F <- MJData$ATLFB_29 + MJData$Female
MJData$ATLFB_30F <- MJData$ATLFB_30 + MJData$Female

MJData$Binge.Days <- rowSums(subset(MJData, select=c("ATLFB_01F", "ATLFB_02F", "ATLFB_03F", "ATLFB_04F", "ATLFB_05F", "ATLFB_06F", "ATLFB_07F", "ATLFB_08F", "ATLFB_09F", "ATLFB_10F",
                                                     "ATLFB_11F", "ATLFB_12F", "ATLFB_13F", "ATLFB_14F", "ATLFB_15F", "ATLFB_16F", "ATLFB_17F", "ATLFB_18F", "ATLFB_19F", "ATLFB_20F",
                                                     "ATLFB_21F", "ATLFB_22F", "ATLFB_23F", "ATLFB_24F", "ATLFB_25F", "ATLFB_26F", "ATLFB_27F", "ATLFB_28F", "ATLFB_29F", "ATLFB_30F"))>=5)
SpDesc(MJData$Binge.Days)
SpHist(MJData, variable="Binge.Days")

MJData$Binge.Per <- MJData$Binge.Days / MJData$Drink.Days
SpDesc(MJData$Binge.Per)
SpHist(MJData, var="Binge.Per")

MJData$Cig.Tot <- rowSums(subset(MJData, select=c("CTLFB_01", "CTLFB_02", "CTLFB_03", "CTLFB_04", "CTLFB_05", "CTLFB_06", "CTLFB_07", "CTLFB_08", "CTLFB_09", "CTLFB_10",
                                                    "CTLFB_11", "CTLFB_12", "CTLFB_13", "CTLFB_14", "CTLFB_15", "CTLFB_16", "CTLFB_17", "CTLFB_18", "CTLFB_19", "CTLFB_20",
                                                    "CTLFB_21", "CTLFB_22", "CTLFB_23", "CTLFB_24", "CTLFB_25", "CTLFB_26", "CTLFB_27", "CTLFB_28", "CTLFB_29", "CTLFB_30")))
MJData$Cig.Days <- rowSums(subset(MJData, select=c("CTLFB_01", "CTLFB_02", "CTLFB_03", "CTLFB_04", "CTLFB_05", "CTLFB_06", "CTLFB_07", "CTLFB_08", "CTLFB_09", "CTLFB_10",
                                                     "CTLFB_11", "CTLFB_12", "CTLFB_13", "CTLFB_14", "CTLFB_15", "CTLFB_16", "CTLFB_17", "CTLFB_18", "CTLFB_19", "CTLFB_20",
                                                     "CTLFB_21", "CTLFB_22", "CTLFB_23", "CTLFB_24", "CTLFB_25", "CTLFB_26", "CTLFB_27", "CTLFB_28", "CTLFB_29", "CTLFB_30"))>0)
MJData$CPD <- MJData$Cig.Tot/30
MJData$CPSD <- MJData$Cig.Tot/MJData$Cig.Days

SpDesc(MJData[c("Cig.Tot", "CPD", "Cig.Days", "CPSD")])
SpHist(MJData, var="Cig.Tot")
SpHist(MJData, var="CPD")
SpHist(MJData, var="Cig.Days")

table(MJData$ftnd_1)




#Descriptives----
SpDesc(subset(MJData, select=c("cudituse", "CUDIT", "Drug_THC_Beh")))
SpHist(MJData, variable="CUDIT")
table(MJData$CUDIT)
table(MJData$CUDIT, MJData$cudituse)

SpHist(subset(MJData, cudituse==1), variable="CUDIT")

SpHist(sqrt(subset(MJData, cudituse==1)$CUDIT))




#Analysis----


#Age
t.test(Age~cudituse, data=MJData)
MJData %>% group_by(cudituse) %>% summarise (n=n(), mean=mean(Age), sd=sd(Age))
t.test(Age~cudituse, data=subset(MJData, Source==1))
t.test(Age~cudituse, data=subset(MJData, Source==2))
t.test(Age~cudituse, data=subset(MJData, Source==3))
t.test(Age~cudituse, data=subset(MJData, Source==4))

cor.test(MJData$Age, MJData$CUDIT)
cor.test(subset(MJData, Source==1)$Age, subset(MJData, Source==1)$CUDIT)
cor.test(subset(MJData, Source==2)$Age, subset(MJData, Source==2)$CUDIT)
cor.test(subset(MJData, Source==3)$Age, subset(MJData, Source==3)$CUDIT)
cor.test(subset(MJData, Source==4)$Age, subset(MJData, Source==4)$CUDIT)

t.test(Age~Drug_THC_Beh, data=MJData)
MJData %>% group_by(Drug_THC_Beh) %>% summarise (n=n(), mean=mean(Age), sd=sd(Age))
t.test(Age~Drug_THC_Beh, data=subset(MJData, Source==1))
t.test(Age~Drug_THC_Beh, data=subset(MJData, Source==2))
t.test(Age~Drug_THC_Beh, data=subset(MJData, Source==3))
t.test(Age~Drug_THC_Beh, data=subset(MJData, Source==4))

#Female
CrossTable(MJData$Female, MJData$cudituse)
chisq.test(table(MJData$Female, MJData$cudituse))

t.test(CUDIT~Female, data=MJData)
t.test(CUDIT~Female, data=subset(MJData, Source==1))
t.test(CUDIT~Female, data=subset(MJData, Source==2))
t.test(CUDIT~Female, data=subset(MJData, Source==3))
t.test(CUDIT~Female, data=subset(MJData, Source==4))

CrossTable(MJData$Female, MJData$Drug_THC_Beh)
chisq.test(table(MJData$Female, MJData$Drug_THC_Beh))

#edu
CrossTable(MJData$edu, MJData$cudituse)
table(MJData$edu, MJData$cudituse)
chisq.test(table(MJData$edu, MJData$cudituse))

MJData %>% group_by(edu) %>% summarise(meanCUDIT = mean(CUDIT))
summary(aov(CUDIT~edu, data=MJData))

CrossTable(MJData$edu, MJData$Drug_THC_Beh)
table(MJData$edu, MJData$Drug_THC_Beh)
chisq.test(table(MJData$edu, MJData$Drug_THC_Beh))


#Drink.Days
t.test(Drink.Days~cudituse, data=MJData)
MJData %>% group_by(cudituse) %>% summarise (n=n(), mean=mean(Drink.Days, na.rm=T), sd=sd(Drink.Days, na.rm=T))
t.test(Drink.Days~cudituse, data=subset(MJData, Source==1))
t.test(Drink.Days~cudituse, data=subset(MJData, Source==2))
t.test(Drink.Days~cudituse, data=subset(MJData, Source==3))
t.test(Drink.Days~cudituse, data=subset(MJData, Source==4))

cor.test(MJData$Drink.Days, MJData$CUDIT)
cor.test(subset(MJData, Source==1)$Drink.Days, subset(MJData, Source==1)$CUDIT)
cor.test(subset(MJData, Source==2)$Drink.Days, subset(MJData, Source==2)$CUDIT)
cor.test(subset(MJData, Source==3)$Drink.Days, subset(MJData, Source==3)$CUDIT)
cor.test(subset(MJData, Source==4)$Drink.Days, subset(MJData, Source==4)$CUDIT)

t.test(Drink.Days~Drug_THC_Beh, data=MJData)
MJData %>% group_by(Drug_THC_Beh) %>% summarise (n=n(), mean=mean(Drink.Days, na.rm=T), sd=sd(Drink.Days, na.rm=T))
t.test(Drink.Days~Drug_THC_Beh, data=subset(MJData, Source==1))
t.test(Drink.Days~Drug_THC_Beh, data=subset(MJData, Source==2))
t.test(Drink.Days~Drug_THC_Beh, data=subset(MJData, Source==3))
t.test(Drink.Days~Drug_THC_Beh, data=subset(MJData, Source==4))


#DPDD
t.test(DPDD~cudituse, data=MJData)
MJData %>% group_by(cudituse) %>% summarise (n=n(), mean=mean(DPDD, na.rm=T), sd=sd(DPDD, na.rm=T))
t.test(DPDD~cudituse, data=subset(MJData, Source==1))
t.test(DPDD~cudituse, data=subset(MJData, Source==2))
t.test(DPDD~cudituse, data=subset(MJData, Source==3))
t.test(DPDD~cudituse, data=subset(MJData, Source==4))

cor.test(MJData$DPDD, MJData$CUDIT)
cor.test(subset(MJData, Source==1)$DPDD, subset(MJData, Source==1)$CUDIT)
cor.test(subset(MJData, Source==2)$DPDD, subset(MJData, Source==2)$CUDIT)
cor.test(subset(MJData, Source==3)$DPDD, subset(MJData, Source==3)$CUDIT)
cor.test(subset(MJData, Source==4)$DPDD, subset(MJData, Source==4)$CUDIT)

t.test(DPDD~Drug_THC_Beh, data=MJData)
MJData %>% group_by(Drug_THC_Beh) %>% summarise (n=n(), mean=mean(DPDD, na.rm=T), sd=sd(DPDD, na.rm=T))
t.test(DPDD~Drug_THC_Beh, data=subset(MJData, Source==1))
t.test(DPDD~Drug_THC_Beh, data=subset(MJData, Source==2))
t.test(DPDD~Drug_THC_Beh, data=subset(MJData, Source==3))
t.test(DPDD~Drug_THC_Beh, data=subset(MJData, Source==4))


#Binge.Days
t.test(Binge.Days~cudituse, data=MJData)
MJData %>% group_by(cudituse) %>% summarise (n=n(), mean=mean(Binge.Days, na.rm=T), sd=sd(Binge.Days, na.rm=T))
t.test(Binge.Days~cudituse, data=subset(MJData, Source==1))
t.test(Binge.Days~cudituse, data=subset(MJData, Source==2))
t.test(Binge.Days~cudituse, data=subset(MJData, Source==3))
t.test(Binge.Days~cudituse, data=subset(MJData, Source==4))

cor.test(MJData$Binge.Days, MJData$CUDIT)
cor.test(subset(MJData, Source==1)$Binge.Days, subset(MJData, Source==1)$CUDIT)
cor.test(subset(MJData, Source==2)$Binge.Days, subset(MJData, Source==2)$CUDIT)
cor.test(subset(MJData, Source==3)$Binge.Days, subset(MJData, Source==3)$CUDIT)
cor.test(subset(MJData, Source==4)$Binge.Days, subset(MJData, Source==4)$CUDIT)

t.test(Binge.Days~Drug_THC_Beh, data=MJData)
MJData %>% group_by(Drug_THC_Beh) %>% summarise (n=n(), mean=mean(Binge.Days, na.rm=T), sd=sd(Binge.Days, na.rm=T))
t.test(Binge.Days~Drug_THC_Beh, data=subset(MJData, Source==1))
t.test(Binge.Days~Drug_THC_Beh, data=subset(MJData, Source==2))
t.test(Binge.Days~Drug_THC_Beh, data=subset(MJData, Source==3))
t.test(Binge.Days~Drug_THC_Beh, data=subset(MJData, Source==4))


#AUDIT
t.test(AUDIT~cudituse, data=MJData)
MJData %>% group_by(cudituse) %>% summarise (n=n(), mean=mean(AUDIT, na.rm=T), sd=sd(AUDIT, na.rm=T))
t.test(AUDIT~cudituse, data=subset(MJData, Source==1))
t.test(AUDIT~cudituse, data=subset(MJData, Source==2))
t.test(AUDIT~cudituse, data=subset(MJData, Source==3))
t.test(AUDIT~cudituse, data=subset(MJData, Source==4))

cor.test(MJData$AUDIT, MJData$CUDIT)
cor.test(subset(MJData, Source==1)$AUDIT, subset(MJData, Source==1)$CUDIT)
cor.test(subset(MJData, Source==2)$AUDIT, subset(MJData, Source==2)$CUDIT)
cor.test(subset(MJData, Source==3)$AUDIT, subset(MJData, Source==3)$CUDIT)
cor.test(subset(MJData, Source==4)$AUDIT, subset(MJData, Source==4)$CUDIT)

t.test(AUDIT~Drug_THC_Beh, data=MJData)
MJData %>% group_by(Drug_THC_Beh) %>% summarise (n=n(), mean=mean(AUDIT, na.rm=T), sd=sd(AUDIT, na.rm=T))
t.test(AUDIT~Drug_THC_Beh, data=subset(MJData, Source==1))
t.test(AUDIT~Drug_THC_Beh, data=subset(MJData, Source==2))
t.test(AUDIT~Drug_THC_Beh, data=subset(MJData, Source==3))
t.test(AUDIT~Drug_THC_Beh, data=subset(MJData, Source==4))


#ftnd_1
CrossTable(MJData$ftnd_1, MJData$cudituse)
chisq.test(table(MJData$ftnd_1, MJData$cudituse))

MJData %>% group_by(ftnd_1) %>% summarise(meanCUDIT = mean(CUDIT))
summary(aov(CUDIT~ftnd_1, data=MJData))

CrossTable(MJData$ftnd_1, MJData$Drug_THC_Beh)
chisq.test(table(MJData$ftnd_1, MJData$Drug_THC_Beh))


#BDItot
t.test(BDItot~cudituse, data=MJData)
MJData %>% group_by(cudituse) %>% summarise (n=n(), mean=mean(BDItot, na.rm=T), sd=sd(BDItot, na.rm=T))
t.test(BDItot~cudituse, data=subset(MJData, Source==1))
t.test(BDItot~cudituse, data=subset(MJData, Source==2))
t.test(BDItot~cudituse, data=subset(MJData, Source==3))
t.test(BDItot~cudituse, data=subset(MJData, Source==4))

cor.test(MJData$BDItot, MJData$CUDIT)
cor.test(subset(MJData, Source==1)$BDItot, subset(MJData, Source==1)$CUDIT)
cor.test(subset(MJData, Source==2)$BDItot, subset(MJData, Source==2)$CUDIT)
cor.test(subset(MJData, Source==3)$BDItot, subset(MJData, Source==3)$CUDIT)
cor.test(subset(MJData, Source==4)$BDItot, subset(MJData, Source==4)$CUDIT)

t.test(BDItot~Drug_THC_Beh, data=MJData)
MJData %>% group_by(Drug_THC_Beh) %>% summarise (n=n(), mean=mean(BDItot, na.rm=T), sd=sd(BDItot, na.rm=T))
t.test(BDItot~Drug_THC_Beh, data=subset(MJData, Source==1))
t.test(BDItot~Drug_THC_Beh, data=subset(MJData, Source==2))
t.test(BDItot~Drug_THC_Beh, data=subset(MJData, Source==3))
t.test(BDItot~Drug_THC_Beh, data=subset(MJData, Source==4))


#BAItot
t.test(BAItot~cudituse, data=MJData)
MJData %>% group_by(cudituse) %>% summarise (n=n(), mean=mean(BAItot, na.rm=T), sd=sd(BAItot, na.rm=T))
t.test(BAItot~cudituse, data=subset(MJData, Source==1))
t.test(BAItot~cudituse, data=subset(MJData, Source==2))
t.test(BAItot~cudituse, data=subset(MJData, Source==3))
t.test(BAItot~cudituse, data=subset(MJData, Source==4))

cor.test(MJData$BAItot, MJData$CUDIT)
cor.test(subset(MJData, Source==1)$BAItot, subset(MJData, Source==1)$CUDIT)
cor.test(subset(MJData, Source==2)$BAItot, subset(MJData, Source==2)$CUDIT)
cor.test(subset(MJData, Source==3)$BAItot, subset(MJData, Source==3)$CUDIT)
cor.test(subset(MJData, Source==4)$BAItot, subset(MJData, Source==4)$CUDIT)

t.test(BAItot~Drug_THC_Beh, data=MJData)
MJData %>% group_by(Drug_THC_Beh) %>% summarise (n=n(), mean=mean(BAItot, na.rm=T), sd=sd(BAItot, na.rm=T))
t.test(BAItot~Drug_THC_Beh, data=subset(MJData, Source==1))
t.test(BAItot~Drug_THC_Beh, data=subset(MJData, Source==2))
t.test(BAItot~Drug_THC_Beh, data=subset(MJData, Source==3))
t.test(BAItot~Drug_THC_Beh, data=subset(MJData, Source==4))

CrossTable(MJData$ethnicity, MJData$cudituse)
chisq.test(table(MJData$ethnicity, MJData$cudituse))

#CUDIT predictor
summary(aov(CUDIT~ethnicity, data=MJData))

summary(lm(Age~CUDIT, data=MJData))
summary(lm(Age~CUDIT, data=subset(MJData, Source==1)))
summary(lm(Age~CUDIT, data=subset(MJData, Source==2)))
summary(lm(Age~CUDIT, data=subset(MJData, Source==3)))
summary(lm(Age~CUDIT, data=subset(MJData, Source==4)))

t.test(CUDIT~Female, data=MJData)
t.test(CUDIT~Female, data=subset(MJData, Source==1))
t.test(CUDIT~Female, data=subset(MJData, Source==2))
t.test(CUDIT~Female, data=subset(MJData, Source==3))
t.test(CUDIT~Female, data=subset(MJData, Source==4))



CrossTable(MJData$ethnicity, MJData$Drug_THC_Beh)
chisq.test(table(MJData$ethnicity, MJData$Drug_THC_Beh))
fisher.test(table(MJData$ethnicity, MJData$Drug_THC_Beh))

MJData$MJ.Days <- rowSums(subset(MJData, select=c("MTLFB_01", "MTLFB_02", "MTLFB_03", "MTLFB_04", "MTLFB_05", "MTLFB_06", "MTLFB_07", "MTLFB_08", "MTLFB_09", "MTLFB_10",
                                                  "MTLFB_11", "MTLFB_12", "MTLFB_13", "MTLFB_14", "MTLFB_15", "MTLFB_16", "MTLFB_17", "MTLFB_18", "MTLFB_19", "MTLFB_20",
                                                  "MTLFB_21", "MTLFB_22", "MTLFB_23", "MTLFB_24", "MTLFB_25", "MTLFB_26", "MTLFB_27", "MTLFB_28", "MTLFB_29", "MTLFB_30")))


table(MJData$MJ.Days>0)
SpDesc(subset(MJData, MJ.Days>0)$MJ.Days)

MJData$ftnd_1jit <- MJData$ftnd_1+rnorm(length(MJData$ftnd_1), 0,0.05)
ggplot(MJData, aes(x=ftnd_1jit, y=CUDIT)) +
  geom_point(alpha=0.2) +
  SpTheme()

