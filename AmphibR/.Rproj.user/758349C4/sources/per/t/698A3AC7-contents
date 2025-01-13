#Load packages
library(randomForest)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(evaluate)
library(pROC)

#Load Dataset
both <- read_excel("MasterAmphibWithTemps.xlsx")
frog = subset(both, both$Common_Nam=="Rocky Mountain Tailed Frog")
frog = subset(frog, frog$Latitude<=49)#only modelling in United States-->remove canadian samples

#You will need to unzip this file first
#Load 1 km points across all streams in RMTF range
frog.all = read.csv("Frog_allpoints.csv")

#Clean points
frog.all=subset(frog.all,frog.all$S1_93_11>=0 & frog.all$BFI>=0)
frog.all=frog.all%>%rename(slope=SLOPE)

#Randomly select pseudo absences and merge dataframes of observations and pseudo-absences
frog.all2=subset(frog.all,frog.all$WATERBODY==0)
rand_df <- frog.all2[sample(nrow(frog.all2), size=3855), ]
rand_df$Latitude=NA
rand_df$Longitude=NA
frog.A = frog[,c(8,9,12,20:26)]
rand_df$OBSPRED_ID=NULL
rand_df$WATERBODY=NULL
frog.A$present = NA
frog.A$present = 1
rand_df$present = NA
rand_df$present = 0
frog.B = rbind(frog.A, rand_df)


###########################Split into training and validation sets
fset <- sample(seq(1, 2), size = nrow(frog.B), replace = T, prob = c(.8, .2))
train <- frog.B[fset == 1,]
test <- frog.B[fset == 2,]
train=as.data.frame(train)
test=as.data.frame(test)



#RandomForest - classification
###create variables
occ = as.factor(train$present)
slope = train$slope
TEMP = train$S1_93_11
ELEV = train$ELEV
CANOPY = train$CANOPY
PRECIP = train$PRECIP
BFI = train$BFI

model = occ~slope+TEMP+ELEV+CANOPY+PRECIP+BFI
frog.rf = randomForest(model)
eval.rf = eval(frog.rf)
eval.rf
varImpPlot(frog.rf)
#frog.all=frog.all%>%rename(TEMP=S1_93_11)
predictors = frog.all[,c(3:8,1,2,9,10)]
predictors$occ = NA
predictors$occ=predict(frog.rf, predictors[,c(1:6)])


##########LOOK AT PREDICTION ACCURACY on VALIDATION DATASET (TEST)
test=test%>%rename(TEMP=S1_93_11)
test$rf.predicted=NA
test$rf.predicted=predict(frog.rf, test[,c(3:8)])
test$rf.predicted=as.numeric(as.character(test$rf.predicted))
test_roc <- roc(test$present, test$rf.predicted)
auc(test_roc)#0.8966 nice
test%>%
  group_by(present, rf.predicted)%>%
  summarise(n=length(slope))

#limit entire SDM network with super steep slopes (>16% as suggested by Isaak et al. 2025 NAJFM)
sdm=predictors%>%filter(slope<0.16 & WATERBODY==0)
write.csv(sdm, file = "frog_sdm.csv") #This file can then be merged by the "OBSPRED_ID field to NORWEST)

################################Look at loss and refugia in modelled distribution
sdm$TooWarm2080=NA
sdm$TooWarm2040=NA
sdm$TooWarmHIST=NA
sdm$TooWarm2080[which(sdm$S32_2080D>=13)]="TOO WARM"
sdm$TooWarm2080[which(sdm$S32_2080D>12 & sdm$S32_2080D<13)]="margCWH"
sdm$TooWarm2080[which(sdm$S32_2080D<=12)]="CWH"
sdm$TooWarmHIST[which(sdm$TEMP>=12)]="TOO WARM"
sdm$TooWarmHIST[which(sdm$TEMP>12 & sdm$TEMP<13)]="margCWH"
sdm$TooWarmHIST[which(sdm$TEMP<=12)]="CWH"
sdm$TooWarm2040[which(sdm$S30_2040D>=13)]="TOO WARM"
sdm$TooWarm2040[which(sdm$S30_2040D>12 & sdm$S30_2040D<13)]="margCWH"
sdm$TooWarm2040[which(sdm$S30_2040D<=12)]="CWH"

#Historical
sdm%>%filter(occ==1)%>%
  group_by(TooWarmHIST)%>%
  summarise(n=length(slope), p=length(slope)/14324) #

#2040s
sdm%>%filter(occ==1)%>%
  group_by(TooWarm2040)%>%
  summarise(n=length(slope), p=length(slope)/14324) 

#2080s
sdm%>%filter(occ==1)%>%
  group_by(TooWarm2080)%>%
  summarise(n=length(slope), p=length(slope)/14324) 

#Loss Calculation (Low End)
sdm%>%filter(occ==1 & TooWarmHIST!="TOO WARM")%>%
  group_by(TooWarm2080)%>%
  summarise(n=length(slope), p=length(slope)/13876) 
#Loss Calculation (High End)
sdm%>%filter(occ==1 & TooWarmHIST=="CWH")%>%
  group_by(TooWarm2080)%>%
  summarise(n=length(slope), p=length(slope)/12522) 

################################Look at loss and refugia at observation sites
#USA Actual sites too warm end of century
frog$TooWarm2080=NA
frog$TooWarm2080[which(frog$S32_2080D>=13)]="TOO WARM"
frog$TooWarm2080[which(frog$S32_2080D>12 & frog$S32_2080D<13)]="margCWH"
frog$TooWarm2080[which(frog$S32_2080D<=12)]="CWH"
frog%>%
  group_by(TooWarm2080)%>%
  summarise (n = length(slope),p = length(slope)/3855)

#USA Historical
frog$TooWarmHIST=NA
frog$TooWarmHIST[which(frog$S1_93_11>=12)]="TOO WARM"
frog$TooWarmHIST[which(frog$S1_93_11>12 & frog$S1_93_11<13)]="margCWH"
frog$TooWarmHIST[which(frog$S1_93_11<=12)]="CWH"
frog%>%
  group_by(TooWarmHIST)%>%
  summarise (n = length(slope),p = length(slope)/3855)

#Canada Actual sites too warm end of century
frogBC=read.csv("CANADA_ONLY.csv")
frogBC$TooWarm2080=NA
frogBC$TooWarm2080[which(frogBC$TEMP_2080_RCP6>=13)]="TOO WARM"
frogBC$TooWarm2080[which(frogBC$TEMP_2080_RCP6>12 & frogBC$TEMP_2080_RCP6<13)]="margCWH"
frogBC$TooWarm2080[which(frogBC$TEMP_2080_RCP6<=12)]="CWH"
frogBC%>%
  group_by(TooWarm2080)%>%
  summarise (n = length(Common_Nam), p=length(Common_Nam)/22)
