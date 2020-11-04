## Analysis of interdisciplinary proposals 
# March 20, 2020
setwd("~/Documents/GitRepos/CNH-perspectives")

# setwd("C:/Users/Megan/Google Drive/CNH_perspectives")

library(dplyr)
library(plyr)
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
library(cowplot)

<<<<<<< HEAD
grants<- read.csv('Grants.csv')
colnames(grants)[which(colnames(grants)=="ï..Grant.Searched")] <- "Grant.Searched"
## change "GEO-CHN" to "GEO-CNH"
grants <- transform(grants,
                    Grant.Searched=revalue(ï..Grant.Searched,c("GEO-CHN"="GEO-CNH")))
pubs<- read.csv('Publications.csv')
journals <- read.csv('Journals.csv')
=======
grants<- read.csv('Grants_toTrack_0722.csv')
pubs<- read.csv('PublicationTracker_0722.csv')
journals <- read.csv('JournalDisciplines.csv')
>>>>>>> 987696ed4a12d9eb7f93176a8dc383ebccbbd8f8

names(grants)[1] <- "Grant.Searched"
grant_sum <- grants %>% group_by(Grant.Searched) %>% summarize(count=n())


#################################################################
#Bar chart of all grants pulled
#################################################################

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


#################################################################
## Diversity Index - Shannon - Analysis 
##############################################################


pubs$Journal<-as.character(pubs$journal) 
pubs$Journal<-char_tolower(pubs$journal)
pub_sum<- pubs %>% group_by(journal) %>% summarize(count=n()) 

rub_sum<- pubs %>% group_by(CNH.Rubric.2) %>% summarize(count=n()) 
rub_sum_v2<- pubs %>% group_by(Interdis.Rubric.1) %>% summarize(count=n()) 

pubs$X <- NULL
pubs <- pubs[, -c(14:29)] 
pubs <- na.omit(pubs) 
#grant_ids <- unique(pubs$Grant.Number)
#grant_deets <- data.frame('Grant.Number'=grant_ids, 'sdi'=-1)
grant_deets <- data.frame('Grant.Number'=grant_ids, 'sdi_CNH'=-1, 'sdi_interdisc'=-1, 'sdi_dis'=-1)


for (i in 1:length(grant_ids)) {
  if (length(pubs$Publication.Year[pubs$Grant.Number == grant_ids[i]]) > 2){
    grant_deets$sdi_CNH[i] <- diversity(pubs$CNH.Rubric.2[pubs$Grant.Number == grant_ids[i]], index="shannon")
    grant_deets$sdi_interdisc[i] <- diversity(pubs$Interdis.Rubric.1[pubs$Grant.Number == grant_ids[i]], index="shannon")
    #grant_deets$sdi_dis[i] <- diversity(pubs$Discipline[pubs$Grant.Number == grant_ids[i]], index="shannon")
  } else {grant_deets$sdi_CNH[i] <-NA
         grant_deets$sdi_interdisc[i] <-NA
  }
}
#QUITE A FEW NAs for the diversity that we need to figure out (not all 0 papers)

##### FOR SDI with publication discipline 

pubs <- pubs %>%    
  mutate(Journal = tolower(Journal))  

#pubs <- pubs[, -c(14:29)] 

pubs <- rename(pubs, "journal" = "Journal")
journals <- rename(journals, "journal" = "Journal")
journals <- journals %>%    
  mutate(journal = tolower(journal))

pubs<- left_join(pubs, journals, by= "journal")
pubs<-subset(pubs, Grant.Number != "")
pubs <- unique(pubs)

rub_sum_v3 <- pubs %>% group_by(Discipline) %>% summarize(count=n())

levels(pubs$Discipline) <- c(levels(pubs$Discipline), "1", "2", "3", "4", "5", "6", "7", "8", 
                             "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20") 
pubs$Discipline[pubs$Discipline== ""] <- "1"
pubs$Discipline[pubs$Discipline=="ses"] <- "2"
pubs$Discipline[pubs$Discipline=="ecology /biology"] <- "3"
pubs$Discipline[pubs$Discipline=="multi-disciplinary"] <- "4"
pubs$Discipline[pubs$Discipline=="biogeochemistry"] <- "5"
pubs$Discipline[pubs$Discipline=="atmospheric"] <- "6"
pubs$Discipline[pubs$Discipline=="geoscience"] <- "7"
pubs$Discipline[pubs$Discipline=="oceanography"] <- "8"
pubs$Discipline[pubs$Discipline=="hydrology"] <- "9"
pubs$Discipline[pubs$Discipline=="social sciences"] <- "10"
pubs$Discipline[pubs$Discipline=="health/disease"] <- "11"
pubs$Discipline[pubs$Discipline=="ag"] <- "12"
pubs$Discipline[pubs$Discipline=="humanities"] <- "13"
pubs$Discipline[pubs$Discipline=="geography"] <- "14"
pubs$Discipline[pubs$Discipline=="law"] <- "15"
pubs$Discipline[pubs$Discipline=="marine science"] <- "16"
pubs$Discipline[pubs$Discipline=="engineering"] <- "17"
pubs$Discipline[pubs$Discipline=="physics"] <- "18"
pubs$Discipline[pubs$Discipline=="remote sensing"] <- "19"
pubs$Discipline[pubs$Discipline=="mathematics"] <- "20"
pubs$Discipline <- as.numeric(as.character(pubs$Discipline))

data_subset <- pubs[ , c("Discipline")]  
pubs <- pubs[complete.cases(data_subset), ] 

for (i in 1:length(grant_ids)) {
  if (length(pubs$Publication.Year[pubs$Grant.Number == grant_ids[i]]) > 2){
    grant_deets$sdi_dis[i] <- diversity(pubs$Discipline[pubs$Grant.Number == grant_ids[i]], index="shannon")
  } else {grant_deets$sdi_dis[i] <-NA
  }
}


grants<- grants %>% left_join(grant_deets)
hist(grant_deets$sdi_dis, plot = T, xlab = "Shannon Diversity Index of Rubric for Each Grant", main='')

#histogram for diversity
p <- ggplot(grant_deets)+ geom_histogram(aes(x=sdi_CNH), binwidth = 0.1, fill = "navy") 
t <- p + labs(x = "Shannon Diversity for CNH ", y ="Count of Grants" ) +
  
  theme(
    panel.background = element_rect(fill = 'white', colour = 'black'),
    axis.text = element_text(size = 18),
    axis.text.x = element_text(colour = "gray30"),
    axis.text.y = element_text(colour = "gray30"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size =20),
    axis.title.y = element_text(size =18))
t
ggsave(filename= "Figures/Histo_Shannon_CHN.pdf", t, width=10, height=8)


#histogram for diversity by grant program
grants <- rename(grants, "GrantingProgram" = "Grant.Searched")

p <- ggplot(grants, aes(x=sdi_CNH))+
  geom_histogram(data=subset(grants, grants$GrantingProgram == "ES"), binwidth = 0.1, colour = "yellow3", fill = "yellow3", size = 1) + 
  geom_histogram(data=subset(grants, grants$GrantingProgram == "BE-CNH"),binwidth = 0.1, colour = "forestgreen", fill = "forestgreen", alpha = 0.3, size = 2)+
  geom_histogram(data=subset(grants, grants$GrantingProgram == "GEO-CHN"),binwidth = 0.1, colour = "navy", fill = "navy", alpha = 0.3, size = 1) 
t <- p + labs(x = "Shannon Diversity for CNH ", y ="Count of Grants" ) +
  
  theme(
    panel.background = element_rect(fill = 'white', colour = 'black'),
    axis.text = element_text(size = 18),
    axis.text.x = element_text(colour = "gray30"),
    axis.text.y = element_text(colour = "gray30"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size =20),
    axis.title.y = element_text(size =18), 
    legend.position = c(.95, .95),)
t
ggsave(filename= "Figures/Histo_Shannon_CHN_grantprogram.pdf", t, width=10, height=8)

#Figure for SDI CNH with subset programs
p <- ggplot(grants, aes(x=sdi_CNH))+
  geom_density(data=subset(grants, grants$Grant.Searched == "ES"), colour = "#a6cee3", fill = "yellow3", size = 2, alpha = 0, linetype = "longdash") + 
  geom_density(data=subset(grants, grants$Grant.Searched == "BE-CNH"), colour = "#33a02c", fill = "forestgreen", alpha = 0, size = 2, linetype = "dotted")+
  geom_density(data=subset(grants, grants$Grant.Searched == "GEO-CHN"), colour = "#b2df8a",  alpha = 0, size = 2, linetype = "dotted") +
  geom_density(data=subset(grants, grants$Grant.Searched == "Hydrology"), colour = "#1f78b4",  alpha = 0, size = 2, linetype = "longdash") +
  geom_density(data=grants, colour = "black",  alpha = 0, size = 2) 
t <- p + labs(x = "Shannon Diversity for CNH ", y ="Density of Grants" ) +
  
  theme(
    panel.background = element_rect(fill = 'white', colour = 'black'),
    axis.text = element_text(size = 18),
    axis.text.x = element_text(colour = "gray30"),
    axis.text.y = element_text(colour = "gray30"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size =20),
    axis.title.y = element_text(size =18), 
    legend.position = c(.95, .95),)+
    xlim(0, 3)
t
ggsave(filename= "Figures/Density_Shannon_CHN_grantprogram_9.17.20.pdf", t, width=10, height=8)


#Figure for SDI interdiscpline with subset programs
p <- ggplot(grants, aes(x=sdi_interdisc))+
  geom_density(data=subset(grants, grants$Grant.Searched == "ES"), colour = "#a6cee3", fill = "yellow3", size = 2, alpha = 0, linetype = "longdash") + 
  geom_density(data=subset(grants, grants$Grant.Searched == "BE-CNH"), colour = "#33a02c", fill = "forestgreen", alpha = 0, size = 2, linetype = "dotted")+
  geom_density(data=subset(grants, grants$Grant.Searched == "GEO-CHN"), colour = "#b2df8a",  alpha = 0, size = 2, linetype = "dotted") +
  geom_density(data=subset(grants, grants$Grant.Searched == "Hydrology"), colour = "#1f78b4",  alpha = 0, size = 2, linetype = "longdash") +
  geom_density(data=grants, colour = "black",  alpha = 0, size = 2) 
t <- p + labs(x = "Shannon Diversity for Interdisciplinary ", y ="Density of Grants" ) +
  
  theme(
    panel.background = element_rect(fill = 'white', colour = 'black'),
    axis.text = element_text(size = 18),
    axis.text.x = element_text(colour = "gray30"),
    axis.text.y = element_text(colour = "gray30"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size =20),
    axis.title.y = element_text(size =18), 
    legend.position = c(.95, .95),)+
  xlim(0,3)
t
ggsave(filename= "Figures/Density_Shannon_Interdis_grantprogram_9.17.20.pdf", t, width=10, height=8)

#Figure for SDI Journal Discpline with subset programs
p <- ggplot(grants, aes(x=sdi_dis))+
  geom_density(data=subset(grants, grants$Grant.Searched== "ES"), colour = "#a6cee3", fill = "yellow3", size = 2, alpha = 0, linetype = "longdash") + 
  geom_density(data=subset(grants, grants$Grant.Searched == "BE-CNH"), colour = "#33a02c", fill = "forestgreen", alpha = 0, size = 2, linetype = "dotted")+
  geom_density(data=subset(grants, grants$Grant.Searched == "GEO-CHN"), colour = "#b2df8a",  alpha = 0, size = 2, linetype = "dotted") +
  geom_density(data=subset(grants, grants$Grant.Searched == "Hydrology"), colour = "#1f78b4",  alpha = 0, size = 2, linetype = "longdash") +
  geom_density(data=grants, colour = "black",  alpha = 0, size = 2) 
t <- p + labs(x = "Shannon Diversity for Journal Discipline ", y ="Density of Grants" ) +
  
  theme(
    panel.background = element_rect(fill = 'white', colour = 'black'),
    axis.text = element_text(size = 18),
    axis.text.x = element_text(colour = "gray30"),
    axis.text.y = element_text(colour = "gray30"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size =20),
    axis.title.y = element_text(size =18), 
    legend.position = c(.95, .95),)+
  xlim(0.5, 2.5)
t
ggsave(filename= "Figures/Density_Shannon_discipline_grantprogram_9.3.20.pdf", t, width=10, height=8)



#### shannon diversity by funding 
### WHAT UNIT ARE THESE FUNDING AMOUNTS IN?
# grants$Funding.Amount <- as.numeric(grants$Funding.Amount)
# p <- ggplot(grants)+ geom_point(aes(x = grants$Funding.Amount, y=sdi_CNH), fill = "navy", alpha = 0.5, size =3) 
# t <- p + labs(x = "Funding Amount ", y ="Shannon Diversity Index - CNH" ) +
#   
#   theme(
#     panel.background = element_rect(fill = 'white', colour = 'black'),
#     axis.text = element_text(size = 18),
#     axis.text.x = element_text(colour = "gray30"),
#     axis.text.y = element_text(colour = "gray30"),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.title.x = element_text(size =20),
#     axis.title.y = element_text(size =18))
# t
# ggsave(filename= "Figures/FundingAmount_Shannon_CHN.pdf", t, width=10, height=8)


#################################################################
## citations by paper for different interdisciplinarity levels ##
#################################################################
# double check name for "check.of.Rubric2.score"
# histogram version
ggplot(pubs, aes(Citations)) +
  geom_histogram(data=filter(pubs, CNH.Rubric.2 == 1), fill = "red", alpha = 0.2) +
  #geom_histogram(data=filter(pubs, CNH.Rubric.2 == 2), fill = "green", alpha = 0.2) +
  geom_histogram(data=filter(pubs, CNH.Rubric.2 == 3), fill = "blue", alpha = 0.2) 

#density plot version
ggplot(pubs, aes(Citations)) +
  geom_density(data=filter(pubs, CNH.Rubric.2 == 3), colour = "#0400ff50", size = 1.5) +
  geom_density(data=filter(pubs, CNH.Rubric.2 == 2), colour = "#33ff0050", size = 1.5) +
  geom_density(data=filter(pubs, CNH.Rubric.2 == 1), colour = "#ff000050", size = 1.5) 

# violin plot version
dplyr::filter(pubs, is.na(CNH.Rubric.2)==F) %>% 
ggplot(aes(x=as.factor(CNH.Rubric.2),y=Citations)) +
  geom_violin()
#ANOVA  
Cite.by.interdiscip.aov <- aov(log(Citations+1)~as.factor(CNH.Rubric.2), data=pubs)
summary(Cite.by.interdiscip.aov)
TukeyHSD(Cite.by.interdiscip.aov)


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



#################################################################
#GRANTS by number of papers and dollar amount - THREE PANELED FIGURE
#################################################################

grants$Number.of.Papers <- as.numeric(grants$Number.of.Papers)
grants$Funding.Amount <- as.numeric(as.character(grants$Funding.Amount))
#grants2<-subset(grants, Funding.Amount != "") 

df <- grants %>%
  group_by(Grant.Searched) %>%
  summarise(count = n(), FundingAmount = sum(Funding.Amount), 
            MeanFunding = mean(Funding.Amount), MeanNumPapers = mean(Number.of.Papers..NSF.), MedNumPapers = median(Number.of.Papers..NSF.)) #numberpaper = sum(Number.of.Papers..NSF.)

## COLUMN plot
# df<-subset(df, Grant.Searched != "")
# p <- ggplot(data = df) +
#   geom_col(aes(x = Grant.Searched, y= df$count, fill = numberpaper)) 
# p <- p + theme(panel.background = element_rect(fill = 'white', colour = 'black'))
# t <- p + labs(x = "Grant Program", y ="Number of Grants" ) +
#   theme(
#     panel.background = element_rect(fill = 'white', colour = 'black'),
#     axis.text = element_text(size = 18),
#     axis.text.x = element_text(colour = "gray30", size =10),
#     axis.text.y = element_text(colour = "gray30"),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.title.x = element_text(size =20),
#     axis.title.y = element_text(size =18))+
#     scale_fill_gradientn(colours=c("navy", "yellow"))   #"cyan3", "yellow4")"green3", "grey50"  #"dodgerblue", "navy" #"darkcyan", "yellow4", "black" #"green4","olivedrab3", "darkorange1", # "red","navy", "dodgerblue" #"darkorchid4","lightskyblue4", "black"
# t
# ggsave(filename= "Figures/Grants_numberofpapers_columnchart.pdf", t, width=10, height=8)


df<-subset(df, Grant.Searched != "")
p <- ggplot(data = df) +
  geom_col(aes(x = Grant.Searched, y= df$count, fill = FundingAmount)) 
p <- p + theme(panel.background = element_rect(fill = 'white', colour = 'black'))
main <- p + labs(x = "Grant Program", y ="Number of Grants", fill = "Funding Amount (USD)") +
  theme(
    panel.background = element_rect(fill = 'white', colour = 'black'),
    axis.text = element_text(size = 18),
    axis.text.x = element_text(colour = "gray30", size =8),
    axis.text.y = element_text(colour = "gray30", size = 12),
    panel.grid.major = element_blank(),
    legend.text = element_text(size=8),
    legend.title = element_text(size=8),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.position = c(0.9, 0.7),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size =15),
    axis.title.y = element_text(size =15))+
  scale_fill_gradientn(colours=c("navy", "yellow")) +   #"cyan3", "yellow4")"green3", "grey50"  #"dodgerblue", "navy" #"darkcyan", "yellow4", "black" #"green4","olivedrab3", "darkorange1", # "red","navy", "dodgerblue" #"darkorchid4","lightskyblue4", "black"
  guides(fill = guide_colourbar(barwidth = 0.75, barheight = 10))
  main
#ggsave(filename= "Figures/Grants_fundingamount_columnchart.pdf", t, width=10, height=8)

### Mean papers by grant program
library(ggforce)

df2<-subset(df, Grant.Searched != "ML")    
p <- ggplot(data = df2) +
    geom_col(aes(x = Grant.Searched, y= log10(df2$MeanNumPapers)))+
      geom_point(data = df2, aes(x = Grant.Searched, y= log10(df2$MedNumPapers)))
    #facet_zoom(ylim = c(0,500))
papers <- p + labs(x = "Grant Program", y ="Logged Mean Number of Papers per Grant" ) +
    
    theme(
      panel.background = element_rect(fill = 'white', colour = 'black'),
      axis.text = element_text(size = 10),
      axis.text.x = element_text(colour = "gray30", size =6),
      axis.text.y = element_text(colour = "gray30", size =10),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.x = element_text(size =12),
      axis.title.y = element_text(size =10))+
  ylim(0,3)
  papers
  #ggsave(filename= "Figures/Grants_searched_numberpapers_barchart.pdf", papers, width=10, height=8)


  ### Mean citations by grant program
#join grants to pubs
Pubgrants <- left_join(pubs, grants, by= "Grant.Number")
Pubgrants <- subset(Pubgrants, Pubgrants$Grant.Searched != "")
Pubgrants <- subset(Pubgrants, Pubgrants$Citations != "NA")
data <- Pubgrants %>%
  group_by(Grant.Searched) %>%
  summarise(count = n(), CitMedian = median(Citations), CitMean = mean(Citations)) #Citations = sum(Citations)
data <- transform(data, CitationsPerPaper = (Citations/count))

  
  p <- ggplot(data = data) +
    geom_col(aes(x = Grant.Searched, y= data$CitMean))+
    geom_point(aes(x = Grant.Searched, y= data$CitMedian))
  cit <- p + labs(x = "Grant Program", y ="Mean Number of Citations per Paper" ) +
    
    theme(
      panel.background = element_rect(fill = 'white', colour = 'black'),
      axis.text = element_text(size = 10),
      axis.text.x = element_text(colour = "gray30", size =6),
      axis.text.y = element_text(colour = "gray30", size =10),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.x = element_text(size =12),
      axis.title.y = element_text(size =10))
  cit
  #ggsave(filename= "Figures/Grants_searched_barchart.pdf", t, width=10, height=8)
  

## To create the combination plot
  
figure <- ggdraw() +
    draw_plot(main, x = 0, y = 0, width = .65, height = 1) +
    draw_plot(papers, x = .65, y = .5, width = .35, height = .5) +
    draw_plot(cit, x = .65, y = 0, width = .35, height = 0.5)+
    draw_plot_label(label = c("A", "B", "C"), size = 15,
                    x = c(0, 0.96, 0.96), y = c(1, 0.99, 0.49))
figure
save_plot(filename= "Figures/ThreePanel_Bar_Mean_Median_9.29.20.pdf", figure, base_width=10, base_height=8)
### Need to figure out how to save the figure

#########################################################
######### Data for tables ##############################
#######################################################

#for total funding and papers across 
grants$Number.of.Papers <- as.numeric(grants$Number.of.Papers)
grants$Funding.Amount <- as.numeric(grants$Funding.Amount)

df <- grants %>%
  group_by(GrantingProgram) %>%
  summarise(count = n(), numberpaper = sum(Number.of.Papers), FundingAmount = sum(Funding.Amount))

df_grantpubs <- left_join(grants, grant_deets, by= "Grant.Number")

df_grantpubs <- rename(df_grantpubs, "sdi_CNH" = "sdi_CNH.x")
df_grantpubs <- rename(df_grantpubs, "sdi_interdisc" = "sdi_interdisc.x")
df_grantpubs$sdi_CNH <- as.numeric(df_grantpubs$sdi_CNH)
df_grantpubs$sdi_interdisc <- as.numeric(df_grantpubs$sdi_interdisc)
df_grantpubs$SDICNH_cat <- cut(df_grantpubs$sdi_CNH, c(0, 1, 2, 3))
df_grantpubs$SDIInt_cat <- cut(df_grantpubs$sdi_interdisc, c(0, 1, 2, 3))
df_grantpubs$SDIDis_cat <- cut(df_grantpubs$sdi_dis, c(0, 1, 2, 3))

#df_grantpubs[is.na(df_grantpubs)] = 0
df_grantpubs<-subset(grants, grants$sdi_dis != "NA")

grantpubsCH <- df_grantpubs %>%
  group_by(GrantingProgram, SDICNH_cat) %>%
  tally()

grantpubsIN <- df_grantpubs %>%
  group_by(GrantingProgram, SDIInt_cat) %>%
  tally()

grantpubsDIS <- df_grantpubs %>%
  group_by(Grant.Searched, SDIDis_cat) %>%
  tally()


pubs<- pubs %>% left_join(grants, by = c("Grant.Number"))
grantpubsCH <- pubs %>%
  group_by(Grant.Searched.x, Grant.Number, CNH.Rubric.2) %>%
  tally()

grantpubsIN <- pubs %>%
  group_by(Grant.Searched.x, Grant.Number, Interdis.Rubric.1) %>%
  tally()

grantpubsDIS <- pubs %>%
  group_by(Grant.Searched, Discipline) %>%
  tally()


totalgrants <-grants %>%
  group_by(Grant.Searched) %>%
  tally()


grants$Number.of.Papers <- as.numeric(grants$Number.of.Papers)
grants$Funding.Amount <- as.numeric(grants$Funding.Amount)

df <- grants %>%
  group_by(Grant.Searched) %>%
  summarise(count = n(), numberpaper = sum(Number.of.Papers), FundingAmount = sum(Funding.Amount), medianFund= median(Funding.Amount))

