library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lars)
library(data.table)
library(modelr)
library(xgboost)
library(Metrics)
library(caret)

terr_db <- read_csv("C:/Users/Manaswini/Desktop/Project/globalterrorismdb_new.csv")

terr_db_subset <- dplyr::select(terr_db, eventid, iyear, imonth, iday, country,
                                region,latitude, longitude, specificity, vicinity,
                                crit1, crit2, crit3, doubtterr, multiple, success, suicide, attacktype1,
                                targtype1, targsubtype1, gname, natlty1, guncertain1, individual, weaptype1, nkill, nwound,
                                property, ishostkid, ransom)
#country
terr_db_subset$country[is.na(terr_db_subset$country) | terr_db_subset$country == "Unknown"] <- -99

#region
terr_db_subset$region[is.na(terr_db_subset$region) | terr_db_subset$region == "Unknown"] <- -99

#latitude
terr_db_subset$latitude[is.na(terr_db_subset$latitude) | terr_db_subset$latitude == "Unknown"] <- -99

#longitude
terr_db_subset$longitude[is.na(terr_db_subset$longitude) | terr_db_subset$longitude == "Unknown"] <- -99

#specificity
terr_db_subset$specificity[is.na(terr_db_subset$specificity) | terr_db_subset$specificity == "Unknown"] <- -99

#vicinity
terr_db_subset$vicinity[is.na(terr_db_subset$vicinity) | terr_db_subset$vicinity == -9 ] <- -99
terr_db_subset$vicinity[is.na(terr_db_subset$property) | terr_db_subset$property == -9 ] <- -99

#crit1
terr_db_subset$crit1[is.na(terr_db_subset$crit1) | terr_db_subset$crit1 == "Unknown"] <- -99

#crit2
terr_db_subset$crit2[is.na(terr_db_subset$crit2) | terr_db_subset$crit2 == "Unknown"] <- -99

#crit3
terr_db_subset$crit3[is.na(terr_db_subset$crit3) | terr_db_subset$crit3 == "Unknown"] <- -99

#doubterr
terr_db_subset$doubtterr[is.na(terr_db_subset$doubtterr) | terr_db_subset$doubtterr == -9 | terr_db_subset$doubtterr == "Unknown"] <- -99

#multiple
terr_db_subset$multiple[is.na(terr_db_subset$multiple) | terr_db_subset$multiple == "Unknown"] <- -99

#success
terr_db_subset$success[is.na(terr_db_subset$success) | terr_db_subset$success == "Unknown"] <- -99

#suicide
terr_db_subset$suicide[is.na(terr_db_subset$suicide)] <- -99

#attacktype1
terr_db_subset$attacktype1[is.na(terr_db_subset$attacktype1) | terr_db_subset$attacktype1 == 9 | terr_db_subset$attacktype1 == "Unknown" ] <- -99

#targtype1
terr_db_subset$targtype1[terr_db_subset$targtype1 == "unk"] <- "unknown"
terr_db_subset$targtype1[is.na(terr_db_subset$targtype1) | terr_db_subset$targtype1 == 20 | terr_db_subset$targtype1 == "Unknown"] <- -99

#targetsubtype1
terr_db_subset$targsubtype1[terr_db_subset$targsubtype1 == "unk"] <- "unknown"
terr_db_subset$targsubtype1[is.na(terr_db_subset$targsubtype1) | terr_db_subset$targsubtype1 == "Unknown"] <- -99

#natlty1
terr_db_subset$natlty1[is.na(terr_db_subset$natlty1) | terr_db_subset$natlty1 == "Unknown"] <- -99

#guncertain1
terr_db_subset$guncertain1[is.na(terr_db_subset$guncertain1) | terr_db_subset$guncertain1 == "Unknown"] <- -99

#individual
terr_db_subset$individual[is.na(terr_db_subset$individual) | terr_db_subset$individual == "Unknown"] <- -99

#weaptype1
terr_db_subset$weaptype1[is.na(terr_db_subset$weaptype1) | terr_db_subset$weaptype1 == 13 | terr_db_subset$weaptype1 == "Unknown"] <- -99

#nkill
terr_db_subset$nkill[is.na(terr_db_subset$nkill)] <- mean(terr_db_subset$nkill, na.rm=T)

#nwound
terr_db_subset$nwound[is.na(terr_db_subset$nwound)] <- mean(terr_db_subset$nwound, na.rm=T)

#casulaties
terr_db_subset$casualties <- terr_db_subset$nkill + terr_db_subset$nwound

#property
terr_db_subset$property[is.na(terr_db_subset$property) | terr_db_subset$property == -9 | terr_db_subset$property == "Unknown"] <- -99

#ishostkid
terr_db_subset$ishostkid[is.na(terr_db_subset$ishostkid) | terr_db_subset$ishostkid == -9 | terr_db_subset$ishostkid == "Unknown"] <- -99

#ransom
terr_db_subset$ransom[is.na(terr_db_subset$ransom) | terr_db_subset$ransom == -9 | terr_db_subset$ransom == "Unknown"] <- -99

#gname
terr_db_subset$gname[is.na(terr_db_subset$gname)] <- -99

keyword <- -99
terr_db_subset<-terr_db_subset[!rowSums(terr_db_subset==keyword),]
terr_db_subset<-na.omit(terr_db_subset)

max(terr_db_subset$casualties)

spec = c(train = .60, test = .20,valid = .2)

g = sample(cut(
  seq(nrow(df)),
  nrow(df)*cumsum(c(0,spec)),
  labels = names(spec)
))

res = split(df, g)

train<- res$train
test<-res$test

#LASSO
trainx<- train[-8]
trainy <- train[8]
testx<- test[-8]
testx<-as.numeric(as.character(unlist(test[-8])))
testy<-as.numeric(as.character(unlist(test[8])))
trainx<- data.matrix(trainx)
trainx
trainy<-data.matrix(trainy)
mdlasso<- lars(testx, testy, type= "lasso")
predictionlasso<- predict(mdlasso, data.matrix(testx))
rmse(predictionlasso,testy)

#XGBoost
mdl<-xgboost(data=trainx, label= trainy, nrounds= 75, max_depth= 45)
prediction<-predict(mdl, data.matrix(testx))
rmse(testy, prediction)
mae(testy, prediction)



