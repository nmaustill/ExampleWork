# Pull NBA data from all years and split up into groups of 4 years the compare to picks of the first year
# For example pull data 2013 - 2016 and then compare to 2013 picks, pull 2014-2017 and compare to 2014 picks

library(tidyverse)
library(rvest)


#2017-2020 draft data
#2017 class and how well they are currently doing in NBA
nba2017class <- read_html("https://www.basketball-reference.com/draft/NBA_2017.html")

nba2017class_df <- nba2017class %>% 
  html_table() %>% 
  as.data.frame() 


nba2017class_df[1,c(2,15:18)] <- c("Pick", "AVG_MP", "AVG_PTS", "AVG_TRB", "AVG_AST")

colnames(nba2017class_df) <- nba2017class_df[1,]

nba2017class_df[,c(2,7:22)] <- sapply(nba2017class_df[,c(2,7:22)], as.numeric)

nba2017class_df <- nba2017class_df[-c(1,32:33),-c(1,3,5)]


#2018 class and how well they are currently doing in NBA
nba2018class <- read_html("https://www.basketball-reference.com/draft/NBA_2018.html")

nba2018class_df <- nba2018class %>% 
  html_table()

nba2018class_df <- as.data.frame(nba2018class_df) 

nba2018class_df[1,c(2,15:18)] <- c("Pick", "AVG_MP", "AVG_PTS", "AVG_TRB", "AVG_AST")

colnames(nba2018class_df) <- nba2018class_df[1,]

nba2018class_df[,c(2,7:22)] <- sapply(nba2018class_df[,c(2,7:22)], as.numeric)

nba2018class_df <- nba2018class_df[-c(1,32:33),-c(1,3,5)]


#2019 class and how well they are currently doing in NBA
nba2019class <- read_html("https://www.basketball-reference.com/draft/NBA_2019.html")

nba2019class_df <- nba2019class %>% 
  html_table()

nba2019class_df <- as.data.frame(nba2019class_df) 

nba2019class_df[1,c(2,15:18)] <- c("Pick", "AVG_MP", "AVG_PTS", "AVG_TRB", "AVG_AST")

colnames(nba2019class_df) <- nba2019class_df[1,]

nba2019class_df[,c(2,7:22)] <- sapply(nba2019class_df[,c(2,7:22)], as.numeric)

nba2019class_df <- nba2019class_df[-c(1,32:33),-c(1,3,5)]


#2020 class and how well they are currently doing in NBA
nba2020class <- read_html("https://www.basketball-reference.com/draft/NBA_2020.html")

nba2020class_df <- nba2020class %>% 
  html_table()

nba2020class_df <- as.data.frame(nba2020class_df) 

nba2020class_df[1,c(2,15:18)] <- c("Pick", "AVG_MP", "AVG_PTS", "AVG_TRB", "AVG_AST")

colnames(nba2020class_df) <- nba2020class_df[1,]

nba2020class_df[,c(2,7:22)] <- sapply(nba2020class_df[,c(2,7:22)], as.numeric)

nba2020class_df <- nba2020class_df[-c(1,32:33),-c(1,3,5)]

#Pull NBA data and separate into groups of 4 years
#Combine like players with WS, WS/48, BPM, and VORP

columns <- c("Player", "WS", "VORP", "WS/48", "BPM")

for (i in 2013:2020) {
  nba <- read_html(paste0("https://www.basketball-reference.com/leagues/NBA_", i , "_advanced.html#advanced_stats")) %>% 
    html_table() %>% 
    as.data.frame()
  
  nba <- nba[nba$Tm != 'TOT',]
  
  nba[,c(6:29)] <- sapply(nba[,c(6:29)], as.numeric)
  
  if(i == 2013){
    nba2012  <- nba
  } 
  else if(i == 2014){
    nba2012  <- full_join(nba2012 , nba)
    nba2013  <- nba
  }
  else if(i == 2015){
    nba2012  <- full_join(nba2012 , nba)
    nba2013  <- full_join(nba2013 , nba)
    nba2014  <- nba
  }
  else if(i == 2016){
    nba2012  <- full_join(nba2012 , nba)
    
    nbaWS <- aggregate(nba2012 [, c(23,29)], list(nba2012 $Player), sum)
    nbaBPM <- aggregate(nba2012 [, c(24, 28)], list(nba2012 $Player), mean)
    nba2012  <- merge(nbaWS, nbaBPM)
    colnames(nba2012 ) <- columns
    
    nba2013  <- full_join(nba2013 , nba)
    nba2014  <- full_join(nba2014 , nba)
    nba2015  <- nba
  }
  else if (i == 2017){
    nba2013  <- full_join(nba2013 , nba)
    
    nbaWS <- aggregate(nba2013 [, c(23,29)], list(nba2013 $Player), sum)
    nbaBPM <- aggregate(nba2013 [, c(24, 28)], list(nba2013 $Player), mean)
    nba2013  <- merge(nbaWS, nbaBPM)
    colnames(nba2013 ) <- columns
    
    nba2014  <- full_join(nba2014 , nba)
    nba2015  <- full_join(nba2015 , nba)
    nba2016  <- nba
  }
  else if (i == 2018){
    nba2014  <- full_join(nba2014 , nba)
    
    nbaWS <- aggregate(nba2014 [, c(23,29)], list(nba2014 $Player), sum)
    nbaBPM <- aggregate(nba2014 [, c(24, 28)], list(nba2014 $Player), mean)
    nba2014  <- merge(nbaWS, nbaBPM)
    colnames(nba2014 ) <- columns
    
    nba2015  <- full_join(nba2015 , nba)
    nba2016  <- full_join(nba2016 , nba)
  }
  else if (i == 2019){
    nba2015  <- full_join(nba2015 , nba)
    
    nbaWS <- aggregate(nba2015 [, c(23,29)], list(nba2015 $Player), sum)
    nbaBPM <- aggregate(nba2015 [, c(24, 28)], list(nba2015 $Player), mean)
    nba2015  <- merge(nbaWS, nbaBPM)
    colnames(nba2015 ) <- columns
    
    nba2016  <- full_join(nba2016 , nba)
  }
  else if (i == 2020){
    nba2016  <- full_join(nba2016 , nba)
    
    nbaWS <- aggregate(nba2016 [, c(23,29)], list(nba2016 $Player), sum)
    nbaBPM <- aggregate(nba2016 [, c(24, 28)], list(nba2016 $Player), mean)
    nba2016  <- merge(nbaWS, nbaBPM)
    colnames(nba2016 ) <- columns
  }
  
  print(i)
}


#Pull 2012-2016 drafts to match NBA data to

nba2012class <- read_html("https://www.basketball-reference.com/draft/NBA_2012.html") %>% 
  html_table() %>% 
  as.data.frame()
nba2012class[1,2] <- "Pick"
colnames(nba2012class) <- nba2012class[1,]
nba2012class <- nba2012class[-c(1,32,33), c(2,4)]

nba2013class <- read_html("https://www.basketball-reference.com/draft/NBA_2013.html") %>% 
  html_table() %>% 
  as.data.frame()
nba2013class[1,2] <- "Pick"
colnames(nba2013class) <- nba2013class[1,]
nba2013class <- nba2013class[-c(1,32,33), c(2,4)]

nba2014class <- read_html("https://www.basketball-reference.com/draft/NBA_2014.html") %>% 
  html_table() %>% 
  as.data.frame()
nba2014class[1,2] <- "Pick"
colnames(nba2014class) <- nba2014class[1,]
nba2014class <- nba2014class[-c(1,32,33), c(2,4)]

nba2015class <- read_html("https://www.basketball-reference.com/draft/NBA_2015.html") %>% 
  html_table() %>% 
  as.data.frame()
nba2015class[1,2] <- "Pick"
colnames(nba2015class) <- nba2015class[1,]
nba2015class <- nba2015class[-c(1,32,33), c(2,4)]

nba2016class <- read_html("https://www.basketball-reference.com/draft/NBA_2016.html") %>% 
  html_table() %>% 
  as.data.frame()
nba2016class[1,2] <- "Pick"
colnames(nba2016class) <- nba2016class[1,]
nba2016class <- nba2016class[-c(1,32,33), c(2,4)]

#Combine draft names with data
nba2012draft <- merge(nba2012class, nba2012)
nba2012draft[,2] <- as.numeric(nba2012draft[,2])

nba2013draft <- merge(nba2013class, nba2013)
nba2013draft[,2] <- as.numeric(nba2013draft[,2])

nba2014draft <- merge(nba2014class, nba2014)
nba2014draft[,2] <- as.numeric(nba2014draft[,2])

nba2015draft <- merge(nba2015class, nba2015)
nba2015draft[,2] <- as.numeric(nba2015draft[,2])

nba2016draft <- merge(nba2016class, nba2016)
nba2016draft[,2] <- as.numeric(nba2016draft[,2])

#Pull RealGM draft info to match names
realgmdraft2012 <- read_html(paste0("https://basketball.realgm.com/nba/draft/past_drafts/2012")) %>% 
  html_table(fill = TRUE)
picknames2012 <- full_join(as.data.frame(realgmdraft2012[[13]]), as.data.frame(realgmdraft2012[[14]]))
picknames2012 <- picknames2012[,c(1,2)]

realgmdraft2013 <- read_html(paste0("https://basketball.realgm.com/nba/draft/past_drafts/2013")) %>% 
  html_table(fill = TRUE)
picknames2013 <- full_join(as.data.frame(realgmdraft2013[[13]]), as.data.frame(realgmdraft2013[[14]]))
picknames2013 <- picknames2013[,c(1,2)]

realgmdraft2014 <- read_html(paste0("https://basketball.realgm.com/nba/draft/past_drafts/2014")) %>% 
  html_table(fill = TRUE)
picknames2014 <- full_join(as.data.frame(realgmdraft2014[[13]]), as.data.frame(realgmdraft2014[[14]]))
picknames2014 <- picknames2014[,c(1,2)]

realgmdraft2015 <- read_html(paste0("https://basketball.realgm.com/nba/draft/past_drafts/2015")) %>% 
  html_table(fill = TRUE)
picknames2015 <- full_join(as.data.frame(realgmdraft2015[[13]]), as.data.frame(realgmdraft2015[[14]]))
picknames2015 <- picknames2015[,c(1,2)]

realgmdraft2016 <- read_html(paste0("https://basketball.realgm.com/nba/draft/past_drafts/2016")) %>% 
  html_table(fill = TRUE)
picknames2016 <- full_join(as.data.frame(realgmdraft2016[[13]]), as.data.frame(realgmdraft2016[[14]]))
picknames2016 <- picknames2016[,c(1,2)]

#Get names from RealGM
realgm2017draft <- read_html("https://basketball.realgm.com/nba/draft/past_drafts/2017") %>% 
  html_table(fill = TRUE)
pick2017names <- full_join(as.data.frame(realgm2017draft[[13]]), as.data.frame(realgm2017draft[[14]]))
pick2017names <- pick2017names[,-c(3:12)]

realgm2018draft <- read_html("https://basketball.realgm.com/nba/draft/past_drafts/2018") %>% 
  html_table(fill = TRUE)
pick2018names <- full_join(as.data.frame(realgm2018draft[[13]]), as.data.frame(realgm2018draft[[14]]))
pick2018names <- pick2018names[,-c(3:12)]

realgm2019draft <- read_html("https://basketball.realgm.com/nba/draft/past_drafts/2019") %>% 
  html_table(fill = TRUE)
pick2019names <- full_join(as.data.frame(realgm2019draft[[13]]), as.data.frame(realgm2019draft[[14]]))
pick2019names <- pick2019names[,-c(3:12)]

realgm2020draft <- read_html("https://basketball.realgm.com/nba/draft/past_drafts/2020") %>% 
  html_table(fill = TRUE)
pick2020names <- full_join(as.data.frame(realgm2020draft[[13]]), as.data.frame(realgm2020draft[[14]]))
pick2020names <- pick2020names[,-c(3:12)]

#Raptor Stats
raptor2012 <- read_csv(file = "raptor12draft.csv", col_names = T)
raptor2012_avg <- aggregate(raptor2012[, c(15,16)], list(raptor2012$Player), mean)
colnames(raptor2012_avg) <- c("Player", "AVG_Raptor", "AVG_WAR")
raptor2012_names <- left_join(picknames2012, raptor2012_avg, by = "Player")

raptor2013 <- read_csv(file = "raptor13draft.csv", col_names = T)
raptor2013_avg <- aggregate(raptor2013[, c(15,16)], list(raptor2013$Player), mean)
colnames(raptor2013_avg) <- c("Player", "AVG_Raptor", "AVG_WAR")
raptor2013_names <- left_join(picknames2013, raptor2013_avg, by = "Player")

raptor2014 <- read_csv(file = "raptor14draft.csv", col_names = T)
raptor2014_avg <- aggregate(raptor2014[, c(15,16)], list(raptor2014$Player), mean)
colnames(raptor2014_avg) <- c("Player", "AVG_Raptor", "AVG_WAR")
raptor2014_names <- left_join(picknames2014, raptor2014_avg, by = "Player")

raptor2015 <- read_csv(file = "raptor15draft.csv", col_names = T)
raptor2015_avg <- aggregate(raptor2015[, c(15,16)], list(raptor2015$Player), mean)
colnames(raptor2015_avg) <- c("Player", "AVG_Raptor", "AVG_WAR")
raptor2015_names <- left_join(picknames2015, raptor2015_avg, by = "Player")

raptor2016 <- read_csv(file = "raptor16draft.csv", col_names = T)
raptor2016_avg <- aggregate(raptor2016[, c(15,16)], list(raptor2016$Player), mean)
colnames(raptor2016_avg) <- c("Player", "AVG_Raptor", "AVG_WAR")
raptor2016_names <- left_join(picknames2016, raptor2016_avg, by = "Player")


#Make the player column RealGM names
nba2012names <- right_join(raptor2012_names, nba2012draft, by = "Pick")
nba2012names <- nba2012names[,-5]
colnames(nba2012names)[2] <- c("Player")

nba2013names <- right_join(raptor2013_names, nba2013draft, by = "Pick")
nba2013names <- nba2013names[,-5]
colnames(nba2013names)[2] <- c("Player")

nba2014names <- right_join(raptor2014_names, nba2014draft, by = "Pick")
nba2014names <- nba2014names[,-5]
colnames(nba2014names)[2] <- c("Player")

nba2015names <- right_join(raptor2015_names, nba2015draft, by = "Pick")
nba2015names <- nba2015names[,-5]
colnames(nba2015names)[2] <- c("Player")

nba2016names <- right_join(raptor2016_names, nba2016draft, by = "Pick")
nba2016names <- nba2016names[,-5]
colnames(nba2016names)[2] <- c("Player")

nba2017names <- right_join(pick2017names, nba2017class_df, by = "Pick")
nba2017names <- nba2017names[,-3]
colnames(nba2017names)[2] <- c("Player")

nba2018names <- right_join(pick2018names, nba2018class_df, by = "Pick")
nba2018names <- nba2018names[,-3]
colnames(nba2018names)[2] <- c("Player")

nba2019names <- right_join(pick2019names, nba2019class_df, by = "Pick")
nba2019names <- nba2019names[,-3]
colnames(nba2019names)[2] <- c("Player")

nba2020names <- right_join(pick2020names, nba2020class_df, by = "Pick")
nba2020names <- nba2020names[,-3]
colnames(nba2020names)[2] <- c("Player")

#All NBA Draftees
all_nba <- nba2017names %>% 
  full_join(nba2018names) %>% 
  full_join(nba2019names) %>% 
  full_join(nba2020names)

#All draft names 2017-2020
all_pick_names <- pick2017names %>% 
  full_join(pick2018names) %>% 
  full_join(pick2019names) %>% 
  full_join(pick2020names)

#Raptor stat 2017-2020 data
raptor <- read_csv(file = "raptor_years.csv", col_names = T)

raptor <- raptor[,-3]

raptor_avg <- aggregate(raptor[, c(14,15)], list(raptor$Player), mean)

colnames(raptor_avg) <- c("Player", "AVG_Raptor", "AVG_WAR")

raptor_names <- left_join(all_pick_names, raptor_avg, by = "Player")

#Add Raptor to NBA stats
all_nba_final <- left_join(all_nba, raptor_names, by = c("Pick", "Player"))

#All in one data frame with nba stats 2017-2020
nba_all_in_one <- all_nba_final[,c(1,2,16:21)]


#All 2012-2016 draftees
all1216nba <- nba2012names %>% 
  full_join(nba2013names) %>% 
  full_join(nba2014names) %>% 
  full_join(nba2015names) %>% 
  full_join(nba2016names)

all1216nba <- all1216nba[,c(1,2,5,6,7,8,3,4)]

#Join all nba stats
allNBAstats <- full_join(nba_all_in_one, all1216nba)

#Save it for future use
saveRDS(allNBAstats, file = 'allNBAstats.RData')
