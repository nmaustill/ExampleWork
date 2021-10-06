# Bring in college data from google sheets
library(googlesheets4)
library(tidyverse)
library(rvest)

options(digits = 3)

current_class <- read_sheet("https://docs.google.com/spreadsheets/d/1gR7GjEZsMbB03JEPi3-lGj8S6_xQnidK0UQ2xvb0fAI/edit?ts=601069c5#gid=1165542844",
                            sheet = "prospect_master",
                            col_names = T, 
                            skip = 3)

current_class_clean <- current_class[, -c(1, 3:9, 11:53, 128:363)]

col_names <- colnames(current_class_clean)

new_col_names <- gsub('.{5}$','', col_names[2:47])

new_col_names1 <- gsub('.{6}$', '', col_names[48:75])

colnames(current_class_clean) <- c("Player", new_col_names, new_col_names1)

current_class_clean1 <- as.data.frame(current_class_clean[,c(1,2,29,31:42,44,45,48:57,59:74)])

colnames(current_class_clean1)[2] <- "Age"

current_class_clean1[,2] <- sapply(current_class_clean1[,2], as.numeric)

colnames(current_class_clean1) <- c("Player", "Age", "ThreePM", "ThreeP_Perc", "FTM", "FTA", "FT_Perc", "ORB", "DRB", "RPG", "APG", 
                                    "SPG", "BPG", "TOV", "PF", "FGMForty", "FGAForty", "ThreePMForty", "ThreePAForty", "FTMForty", 
                                    "FTAForty", "ORBForty", "DRBForty", "REBForty", "ASTForty", "STLForty", "BLKForty", "TOVForty", 
                                    "PFForty", "PTSForty", "PER", "TS_Perc", "eFG_Perc", "ORB_Perc", "DRB_Perc", "TRB_Perc", "AST_Perc", 
                                    "STL_Perc", "BLK_Perc", "TOV_Perc", "USG_Perc", "ORtg", "DRtg")

#Replace missing values with 0s
current_class_clean1[is.na(current_class_clean1)] <- 0

current_class_clean1 <- current_class_clean1[,-18]


#Read in preNBA data
prenba <- readRDS('prenba.RData')

#Read in NBA data
allNBAstats <- readRDS('allNBAstats.RData')

#All prenba and first 4 years nba stats
allstats <- left_join(prenba, allNBAstats, by = c("Player", "Pick"))

#Replace special characters and numbers to make it R friendly
colnames(allstats) <- str_replace_all(colnames(allstats), "%", "_Perc")

colnames(allstats) <- str_replace_all(colnames(allstats), "3", "Three")

colnames(allstats) <- str_replace_all(colnames(allstats), "/40", "Forty")

colnames(allstats) <- str_replace(colnames(allstats), "WS/48", "WSFortyEight")

allstats <- allstats[,-c(33,42,43,46,47)]

#Remove missing values
allstats.nona <- drop_na(allstats)


#Random forest model for Pick
library(randomForest)

set.seed(1584)
n.allstats <- 442
train.rows <- sample(n.allstats, 400)
allstats.train <- allstats.nona[train.rows,]
allstats.test <- allstats.nona[-train.rows,]

out.pick <- randomForest(x=allstats.train[,-c(1,2,44:49)], y=allstats.train$Pick, 
                          xtest=allstats.test[,-c(1,2,44:49)], ytest=allstats.train$Pick,
                         keep.forest = TRUE, ntree=500)

out.WS48 <- randomForest(x=allstats.train[,-c(1,2,44:49)], y=allstats.train$`WS/48`,
                       xtest=allstats.test[,-c(1,2,44:49)], ytest=allstats.train$`WS/48`,
                       keep.forest = TRUE, ntree=500)

out.WS <- randomForest(x=allstats.train[,-c(1,2,44:49)], y=allstats.train$WS,
                       xtest=allstats.test[,-c(1,2,44:49)], ytest=allstats.train$WS,
                       keep.forest = TRUE, ntree=500)

out.VORP <- randomForest(x=allstats.train[,-c(1,2,44:49)], y=allstats.train$VORP,
                         xtest=allstats.test[,-c(1,2,44:49)], ytest=allstats.train$VORP,
                         keep.forest = TRUE, ntree=500)

out.BPM <- randomForest(x=allstats.train[,-c(1,2,44:49)], y=allstats.train$BPM,
                         xtest=allstats.test[,-c(1,2,44:49)], ytest=allstats.train$BPM,
                         keep.forest = TRUE, ntree=500)

out.Raptor <- randomForest(x=allstats.train[,-c(1,2,44:49)], y=allstats.train$AVG_Raptor,
                         xtest=allstats.test[,-c(1,2,44:49)], ytest=allstats.train$AVG_Raptor,
                         keep.forest = TRUE, ntree=500)

out.WAR <- randomForest(x=allstats.train[,-c(1,2,44:49)], y=allstats.train$AVG_WAR,
                         xtest=allstats.test[,-c(1,2,44:49)], ytest=allstats.train$AVG_WAR,
                         keep.forest = TRUE, ntree=500)

predict.pick <- as.data.frame(predict(out.pick, newdata = current_class_clean1[,-1]))

predict.WS48 <- as.data.frame(predict(out.WS48, newdata = current_class_clean1[,-1]))

predict.WS <- as.data.frame(predict(out.WS, newdata = current_class_clean1[,-1]))

predict.VORP <- as.data.frame(predict(out.VORP, newdata = current_class_clean1[,-1]))

predict.BPM <- as.data.frame(predict(out.BPM, newdata = current_class_clean1[,-1]))

predict.Raptor <- as.data.frame(predict(out.Raptor, newdata = current_class_clean1[,-1]))

predict.War <- as.data.frame(predict(out.WAR, newdata = current_class_clean1[,-1]))

#Put all prediction together with player names
current_class_players <- as.data.frame(current_class_clean1[,1])
allpredictions <- cbind(current_class_players, predict.pick) %>% 
  cbind(predict.WS48) %>% 
  cbind(predict.WS) %>% 
  cbind(predict.VORP) %>% 
  cbind(predict.BPM) %>% 
  cbind(predict.Raptor) %>% 
  cbind(predict.War)

colnames(allpredictions) <- c("Player", "Pick", "WS48", "WS", "VORP", "BPM", "Raptor", "WAR")

#Variables that made the biggest impact
varImpPlot(out.pick, main = "Pick Important Variables")
varImpPlot(out.WS48, main = "WS/48 Important Variables")
varImpPlot(out.WS, main = "WS Important Variables")
varImpPlot(out.VORP, main = "VORP Important Variables")
varImpPlot(out.BPM, main = "BPM Important Variables")
varImpPlot(out.Raptor, main = "RAPTOR Important Variables")
varImpPlot(out.WAR, main = "WAR Important Variables")
