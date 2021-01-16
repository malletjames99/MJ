#Required Packages
library(data.table)
library(lubridate)
library(scales)
library(ggplot2)
library(dplyr)
library(magrittr)
library(ggrepel)

#PSU Basketball Eras Project

psuH<-fread("PSUBBallHistoryKP.csv")

#Ranking
ggplot(aes(x=Year, y=KP_Rank, group = Coach, color = Coach), data = psuH) + geom_line(size=2) + geom_point(size=4)+ ylim(0,250)+scale_y_continuous(trans = "reverse")+
  labs(x = "Year", y = "KenPom Ranking", title = "Penn State Basketball KenPom Ranking", subtitle = "Rankings Since 1997", caption = "Created by: Mallet James Data: KenPom")+theme(plot.title = element_text(size = 22))+theme(plot.subtitle = element_text(size = 14))

allStat <- aggregate(psuH[, 5:30], list(psuH$Coach), mean)

mean(psuH$`FTR_O`)
mean(psuH$`OR_D`)

#Free Throw Rate
ggplot(allStat, aes(x=`Group.1`, y=FTR_D)) +
  geom_bar(stat='identity', aes(fill=allStat$FTR_D), show.legend = FALSE) +
  labs(x = "Coach", y="Defensive Free Throw Rate", title = "Penn State Defensive Free Throw Rate by Coach", subtitle = "KenPom Defensive FTR since 1997", fill="FTR") + labs(fill="Coach", caption = "Created by: Mallet James Data: KenPom") +
  geom_text(aes(label= signif(FTR_D, digits = 3)), size=10, nudge_y = 2) +
  theme(axis.text.x = element_text(size = 10,
                                   angle = 45,
                                   hjust = 1,
                                   vjust = 1)) 

#Adjusted Tempo
ggplot(data = psuH, aes(x= Year, y=AdjT, label = Coach)) +
  geom_point() +
  theme_minimal() +
  coord_fixed() +  
  labs(x = "Year", y="Adjusted Tempo", title = "PSU Basketball Adjusted Tempo", subtitle = "KenPom Adjusted Tempo since 1997")+
  geom_label_repel(aes(label = Year, fill = factor(Coach)), segment.color = "black", fontface = "bold") + labs(fill="Coach", caption = "Created by: Mallet James Data: KenPom")

#Efficiency
ggplot(data = psuH, aes(x= AdjO, y=AdjD, label = Year)) +
  geom_point() +
  lims(x=c(95,120),y=c(90,110)) +
  theme_minimal() +
  coord_fixed() +  
  geom_vline(xintercept = 106.779) + geom_hline(yintercept = 99.892) + labs(x = "Adjusted Offensive Efficiency", y="Adjusted Defensive Efficiency", title = "PSU Basketball Efficiency", subtitle = "KenPom Adjusted Offensive and Adjusted Defensive Efficiency since 1997")+
  geom_label_repel(aes(label = Year, fill = factor(Coach)), segment.color = "black", fontface = "bold") + labs(fill="Coach", caption = "Created by: Mallet James Data: KenPom")

#3-Point Percentage   
ggplot(data = psuH, aes(x= `3P_O`, y=`3P_D`, label = Year)) +
  geom_point() +
  lims(x=c(25,40),y=c(27.5,40)) +
  theme_minimal() +
  coord_fixed() +  
  geom_vline(xintercept = 33.9) + geom_hline(yintercept = 34.81) + labs(x = "Offensive 3-Point Percentage", y="Defensive 3-Point Percentage", title = "PSU Basketball 3-Point Percentage", subtitle = "3-Point Percentage since 1997")+
  geom_label_repel(aes(label = Year, fill = factor(Coach)), segment.color = "black", fontface = "bold") + labs(fill="Coach", caption = "Created by: Mallet James Data: KenPom")

#Offensive Rebounding Percentage
ggplot(data = psuH, aes(x= `OR_O`, y=`OR_D`, label = Year)) +
  geom_point() +
  lims(x=c(22,38),y=c(22,40)) +
  theme_minimal() +
  coord_fixed() +  
  geom_vline(xintercept = 31.21) + geom_hline(yintercept = 31.03) + labs(x = "Offensive Rebounding Percentage", y="Opponent Offensive Rebounding Percentage", title = "PSU Basketball Offensive Rebounding", subtitle = "Offensive Rebounding Percentage since 1997")+
  geom_label_repel(aes(label = Year, fill = factor(Coach)), segment.color = "black", fontface = "bold") + labs(fill="Coach", caption = "Created by: Mallet James Data: KenPom")

#Free Throw Rate
ggplot(data = psuH, aes(x= `FTR_O`, y=`FTR_D`, label = Year)) +
  geom_point() +
  lims(x=c(20,52),y=c(20,52)) +
  theme_minimal() +
  coord_fixed() +  
  geom_vline(xintercept = 34.31) + geom_hline(yintercept = 34.41) + labs(x = "Offensive Free Throw Rate", y="Defensive Free Throw Rate", title = "PSU Basketball Free Throw Rate", subtitle = "FTR since 1997")+
  geom_label_repel(aes(label = Year, fill = factor(Coach)), segment.color = "black", fontface = "bold") + labs(fill="Coach", caption = "Created by: Mallet James Data: KenPom")


