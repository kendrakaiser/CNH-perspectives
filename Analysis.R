## Analysis of interdisciplinary proposals 
# March 20, 2020
setwd("~/Documents/GitRepos/CNH-perspectives")

library(dplyr)
library(tidyverse)
grants<- read.csv('Grants_toTrack.csv')
pubs<- read.csv('PublicationTracker.csv')

grant_sum<-grants %>% group_by(Grant.Searched) %>% summarize(count=n())

ggplot(grants)+
  geom_bar(aes(x=Grant.Searched)) # this should be re-done to show as a percentage of grants in the dimensions results (with dates 2000-2015 - I orginially had done this manually, but we should probably write another script to do this and remove duplicate titles from Collaborative proposals)

pub_sum<- pubs %>% group_by(Journal) %>% summarize(count=n()) # we need to do QAQC on journal titles
