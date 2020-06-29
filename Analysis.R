## Analysis of interdisciplinary proposals 
# March 20, 2020
#setwd("~/Documents/GitRepos/CNH-perspectives")

# setwd("C:/Users/Megan/Google Drive/CNH_perspectives")

library(dplyr)
library(tidyverse)
library(quanteda)
library(ggplot2)
#packages for word cloud
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(RCurl) 
library(XML)
library(vegan)

grants<- read.csv('Grants_toTrack.csv')
pubs<- read.csv('PublicationTracker_0422.csv')
journals <- read.csv('SubsetJournals_11Jun20.csv')

grant_sum<-grants %>% group_by(Grant.Searched) %>% summarize(count=n())


#Bar chart of all grants pulled
p <- ggplot(grants)+ geom_bar(aes(x=Grant.Searched)) 
t <- p + labs(x = "Grant Program", y ="Number of Grants Found" ) +
  
  theme(
    panel.background = element_rect(fill = 'white', colour = 'black'),
    axis.text = element_text(size = 18),
    axis.text.x = element_text(colour = "gray30"),
    axis.text.y = element_text(colour = "gray30"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size =20),
    axis.title.y = element_text(size =18))
  #scale_x_continuous(breaks = seq(0, 100, 10))+
  #scale_fill_manual(colours="navy")
#xlim(1900, 2015)+
#ylim(0, 4)+
#scale_color_manual(values=c("green3", "grey50", "yellow4", "black", "darkcyan", "darkcyan", "green4","olivedrab3")) #"green4","olivedrab3"))+  #("cyan4", "yellow3","orange3","olivedrab4", "green3")"green3", "grey50"  #"dodgerblue", "navy" #"darkcyan", "yellow4", "black" #"green4","olivedrab3"
#scale_linetype_manual(values=c("solid", "dotted", "dotdash")) +
#guides(color = guide_legend(override.aes = list(size = 3))) 
t
ggsave(filename= "Figures/Grants_searched_barchart.pdf", t, width=10, height=8)
# this should be re-done to show as a percentage of grants in the dimensions results (with dates 2000-2015 - I orginially had done this manually, but we should probably write another script to do this and remove duplicate titles from Collaborative proposals)



pubs$Journal<-as.character(pubs$Journal) 
pubs$Journal<-char_tolower(pubs$Journal)
pub_sum<- pubs %>% group_by(Journal) %>% summarize(count=n()) 

rub_sum<- pubs %>% group_by(Rubric1_OG) %>% summarize(count=n()) 
rub_sum_v2<- pubs %>% group_by(Rubric2) %>% summarize(count=n()) 

grant_ids <- unique(pubs$Grant.Number)
grant_deets <- data.frame('Grant.Number'=grant_ids, 'sdi'=-1)

for (i in 1:length(grant_ids)) {
  if (length(pubs$Publication.Year[pubs$Grant.Number == grant_ids[i]]) > 2){
    sdi <- diversity(pubs$Rubric2[pubs$Grant.Number == grant_ids[i]], index="shannon")
  } else {sdi = NA}
  grant_deets$sdi[i] <- sdi
}

hist(grant_deets$sdi, plot = T, xlab = "Shannon Diversity Index of Rubric for Each Grant", main='')


## journal bar charts - maybe we'll want to reference journals with a number and then give their names in a table?
# bar color = CHANS
ggplot(journals, aes(x = reorder(journal, -number.of.papers), y = number.of.papers, fill = Mission.includes.humans.social.)) +
  geom_bar(stat = "identity") 

# bar color = interdisciplinary
ggplot(journals, aes(x = reorder(journal, -number.of.papers), y = number.of.papers, fill = Mission.includes.interdisciplinary.)) +
  geom_bar(stat = "identity") 
  



## --- Word Cloud

source('http://www.sthda.com/upload/rquery_wordcloud.r')

quartz(10, 10)
intd<-rquery.wordcloud(pubs$Publication.Title[pubs$Rubric.2..original. ==1], type ="text", lang = "english",excludeWords = c("and"), min.freq = 3,  max.words = 100)

quartz.save("test.pdf", type="pdf")

freqTable_int <- intd$freqTable

quartz(10, 10)
dis<-rquery.wordcloud(pubs$Publication.Title[pubs$Rubric.2..original. ==3], type ="text", lang = "english",excludeWords = c("and"), min.freq = 3,  max.words = 100)

quartz.save("test2.pdf", type="pdf")

freqTable_dis <- dis$freqTable

## I can't get this to write out with this code, but I can manually save
journ.all <- rquery.wordcloud(journals$Stated.aims.scope, type = "text", lang = "english", excludeWords = c("and", "the"), min.freq = 3,  max.words = 500)

journ.inter <- rquery.wordcloud(subset(journals, Gutcheck.Interdisciplinary. == "Y" | Gutcheck.Interdisciplinary. == "kinda")$Stated.aims.scope, type = "text", lang = "english", excludeWords = c("and", "the"), min.freq = 3,  max.words = 500)

journ.CNHS <- rquery.wordcloud(subset(journals, Gutcheck.CNHS. == "Y" | Gutcheck.CNHS. == "kinda")$Stated.aims.scope, type = "text", lang = "english", excludeWords = c("and", "the"), min.freq = 3,  max.words = 500)




#GRANTS by number of papers and dollar amount
grants$Number.of.Papers <- as.numeric(grants$Number.of.Papers)

df <- grants %>%
  group_by(Grant.Searched) %>%
  summarise(count = n(), numberpaper = sum(Number.of.Papers), FundingAmount = sum(Funding.Amount))

## COLUMN plot
df<-subset(df, Grant.Searched != "")
p <- ggplot(data = df) +
  geom_col(aes(x = Grant.Searched, y= df$count, fill = numberpaper)) 
p <- p + theme(panel.background = element_rect(fill = 'white', colour = 'black'))
t <- p + labs(x = "Grant Program", y ="Number of Grants" ) +
  theme(
    panel.background = element_rect(fill = 'white', colour = 'black'),
    axis.text = element_text(size = 18),
    axis.text.x = element_text(colour = "gray30", size =10),
    axis.text.y = element_text(colour = "gray30"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size =20),
    axis.title.y = element_text(size =18))+
    scale_fill_gradientn(colours=c("navy", "yellow"))   #"cyan3", "yellow4")"green3", "grey50"  #"dodgerblue", "navy" #"darkcyan", "yellow4", "black" #"green4","olivedrab3", "darkorange1", # "red","navy", "dodgerblue" #"darkorchid4","lightskyblue4", "black"
t
ggsave(filename= "Figures/Grants_numberofpapers_columnchart.pdf", t, width=10, height=8)


df<-subset(df, Grant.Searched != "")
p <- ggplot(data = df) +
  geom_col(aes(x = Grant.Searched, y= df$count, fill = FundingAmount)) 
p <- p + theme(panel.background = element_rect(fill = 'white', colour = 'black'))
t <- p + labs(x = "Grant Program", y ="Number of Grants" ) +
  theme(
    panel.background = element_rect(fill = 'white', colour = 'black'),
    axis.text = element_text(size = 18),
    axis.text.x = element_text(colour = "gray30", size =10),
    axis.text.y = element_text(colour = "gray30"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size =20),
    axis.title.y = element_text(size =18))+
  scale_fill_gradientn(colours=c("navy", "yellow"))   #"cyan3", "yellow4")"green3", "grey50"  #"dodgerblue", "navy" #"darkcyan", "yellow4", "black" #"green4","olivedrab3", "darkorange1", # "red","navy", "dodgerblue" #"darkorchid4","lightskyblue4", "black"
t
ggsave(filename= "Figures/Grants_fundingamount_columnchart.pdf", t, width=10, height=8)


### CODE FOR MOSAIC PLOT - need different data
library(ggmosaic)

ggplot(data = df) +
  geom_mosaic(aes(x = product(Grant.Searched), weight = count, fill = numberpaper), na.rm=TRUE) +
  labs(x="Hours of sleep a night ", title='f(SleepHrsNight)') + guides(fill=guide_legend(title = "SleepHrsNight", reverse = TRUE))

df$numberpaper <- as.numeric(grants$Number.of.Papers)

ggplot(data = grants) +
  geom_mosaic(aes(weight =  Grant.Searched, x = product(Number.of.Papers), fill=factor(Number.of.Papers)), na.rm=TRUE) +
  labs(x="Hours of sleep a night ", title='f(SleepHrsNight)') + guides(fill=guide_legend(title = "SleepHrsNight", reverse = TRUE))

ggplot(data = NHANES) +
  geom_mosaic(aes(weight = Weight, x = product(SleepHrsNight), fill=factor(SleepHrsNight)), na.rm=TRUE) +
  labs(x="Hours of sleep a night ", title='f(SleepHrsNight)') + guides(fill=guide_legend(title = "SleepHrsNight", reverse = TRUE))