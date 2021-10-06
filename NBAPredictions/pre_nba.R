library(tidyverse)
library(rvest)

prenba <- data.frame()

for (j in 2012:2020) {

  realgmdraft <- read_html(paste0("https://basketball.realgm.com/nba/draft/past_drafts/", j)) %>% html_table(fill = TRUE)

  picknames <- full_join(as.data.frame(realgmdraft[[13]]), as.data.frame(realgmdraft[[14]]))

  picknames <- picknames[,-c(3:7,9:12)]

  ncaa <- list()

  for (i in 1:45) {
    ncaa[i] <- read_html(paste0("https://basketball.realgm.com/ncaa/stats/", j, "/Averages/All/All/Season/All/points/desc/", i)) %>% 
      html_table()
    print(i)
  }
  
  ncaa_df <- do.call(rbind, ncaa)
  
  ncaa_df <- ncaa_df[,-1]

  ncaaAdvanced <- list()
  
  for (i in 1:45) {
    ncaaAdvanced[i] <- read_html(paste0("https://basketball.realgm.com/ncaa/stats/", j, "/Advanced_Stats/All/All/Season/All/points/desc/", i, "/")) %>% 
      html_table()
    print(i)
  }
  
  ncaa_adv <- do.call(rbind, ncaaAdvanced)
  
  ncaa_adv <- ncaa_adv[,-1]
  
  ncaa40 <- list()
  
  for (i in 1:45) {
    ncaa40[i] <- read_html(paste0("https://basketball.realgm.com/ncaa/stats/", j, "/Per_40/All/All/Season/All/per/desc/", i, "/")) %>% 
      html_table()
    print(i)
  }
  
  ncaa_per40 <- do.call(rbind, ncaa40)
  
  colnames(ncaa_per40)[c(5:23)] <- paste(colnames(ncaa_per40)[c(5:23)], "40", sep = "/")
  
  ncaa_per40 <- ncaa_per40[,-c(1,4,5)]

  draftncaa <- merge(ncaa_df, ncaa_per40, by = c("Player", "Team")) %>% 
    merge(ncaa_adv, by = c("Player", "Team")) %>% 
    merge(picknames, by = c("Player"))
  
  draftncaa <- draftncaa[, c(1, 59, 60, 2:58)]

# International stats
  inter <- list()
  
  for (z in 1:90) {
    inter[z] <- read_html(paste0("https://basketball.realgm.com/international/stats/", j, "/Averages/All/All/points/All/desc/", z, "/Regular_Season")) %>% 
      html_table()
    print(z)
  }
  
  inter_df <- do.call(rbind, inter)
  
  inter_df <- inter_df[,-1]
  
  interAdvanced <- list()
  
  for (z in 1:90) {
    interAdvanced[z] <- read_html(paste0("https://basketball.realgm.com/international/stats/", j, "/Advanced_Stats/All/All/points/All/desc/", z, "/Regular_Season")) %>% 
      html_table()
    print(z)
  }
  
  inter_adv <- do.call(rbind, interAdvanced)
  
  inter_adv <- inter_adv[,-1]
  
  inter40 <- list()
  
  for (z in 1:90) {
    inter40[z] <- read_html(paste0("https://basketball.realgm.com/international/stats/", j, "/Per_40/All/All/per/All/desc/", z, "/Regular_Season")) %>% 
      html_table()
    print(z)
  }
  
  inter_per40 <- do.call(rbind, inter40)
  
  colnames(inter_per40)[c(5:23)] <- paste(colnames(inter_per40)[c(5:23)], "40", sep = "/")
  
  inter_per40 <- inter_per40[,-c(1,4,5)]
  
  draftinter <- merge(inter_df, inter_per40, by = c("Player", "Team")) %>% 
    merge(inter_adv, by = c("Player", "Team")) %>% 
    merge(picknames, by = c("Player"))
  
  draftinter <- draftinter[, c(1, 59, 60, 2:58)]

#All  Pre NBA stats
    pre <- rbind(draftinter, draftncaa)
  
  prenba <- rbind(prenba, pre)
  
  print(j)
}

prenba <- prenba[-c(5,6,8,24,77,164,181,185,224,307,337,340,392,483:489,512),-c(4,20,22,25)]

saveRDS(prenba, file = "prenba.RData")


