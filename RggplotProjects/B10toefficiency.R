library("data.table")
library("ncaahoopR")
library("rlang")
library("googlesheets4")
library("dplyr")
library("plyr")
library("ggrepel")

to<-fread("B10TimeoutEff2020.csv")

to$Team<-reorder(to$Team,-to$OffDiff)
#Full
ggplot(data = to, aes(x=Team, y=Timeout_Eff)) +
  geom_bar(stat = "identity") +  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5))+ 
  labs(x = "Team", y = "Efficiency in Possession after Timeout", title = "Big Ten Timeout Efficiency", subtitle = "Calculations Made on Possession after Elected Timeout 2020 Season", caption = "Created by: Mallet James Data: ncaahoopR") + annotation_custom(g, ymin = .12, ymax=.24, xmin = 4.45, xmax= 5.55)+
  annotation_custom(g3, ymin = .56, ymax=.68, xmin = .45, xmax= 1.55) + annotation_custom(g2, ymin = .33, ymax=.45, xmin = 1.45, xmax= 2.55) + annotation_custom(g4, ymin = .2, ymax=.3, xmin = 2.65, xmax= 3.35)+
  annotation_custom(g7, ymin = .15, ymax=.27, xmin = 3.5, xmax= 4.5) + annotation_custom(g9, ymin = .10, ymax=.20, xmin = 5.5, xmax= 6.5) +
  annotation_custom(g5, ymin = .025, ymax=.13, xmin = 6.5, xmax= 7.5) + annotation_custom(g10, ymin = .135, ymax=.235, xmin = 7.65, xmax= 8.35) +
  annotation_custom(g8, ymin = .115, ymax=.215, xmin = 8.65, xmax= 9.35) + annotation_custom(g11, ymin = .08, ymax=.19, xmin = 9.5, xmax= 10.5) +
  annotation_custom(g6, ymin = -.08, ymax=-.17, xmin = 10.65, xmax= 11.35) + annotation_custom(g12, ymin = -.11, ymax=-.22, xmin = 11.5, xmax= 12.5) +
  annotation_custom(g13, ymin = -.225, ymax=-.335, xmin = 12.65, xmax= 13.35) + annotation_custom(g14, ymin = -.625, ymax=-.725, xmin = 13.65, xmax= 14.35) +
  geom_text(aes(x = 1, y = .74), label = "+.708", color = "green4", size=6) + geom_text(aes(x = 2, y = .51), label = "+.474", color = "green4", size=6) +
  geom_text(aes(x = 3, y = .37), label = "+.331", color = "green4", size=6) + geom_text(aes(x = 4, y = .33), label = "+.293", color = "green4", size=6) +
  geom_text(aes(x = 5, y = .2875), label = "+.252", color = "green4", size=6) + geom_text(aes(x = 6, y = .255), label = "+.214", color = "green4", size=6) +
  geom_text(aes(x = 7, y = .175), label = "+.139", color = "green4", size=6) + geom_text(aes(x = 8, y = .095), label = "+.059", color = "green4", size=6) +
  geom_text(aes(x = 9, y = .075), label = "+.040", color = "green4", size=6) + geom_text(aes(x = 10, y = .04), label = "+.003", color = "green4", size=6) +
  geom_text(aes(x = 11, y = -.04), label = "-.006", color = "red4", size=6) + geom_text(aes(x = 12, y = -.075), label = "-.053", color = "red4", size=6) +
  geom_text(aes(x = 13, y = -.4), label = "-.368", color = "red4", size=6) + geom_text(aes(x = 14, y = -.785), label = "-.752", color = "red4", size=6)

ggplot(data = to, aes(x=Team, y=Timeout_Eff)) +
  geom_bar(stat = "identity") +  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5))+ geom_text(size=3, position = position_stack(vjust=0.5))+
  labs(x = "Team", y = "Efficiency in Possession after Timeout", title = "Big Ten Timeout Efficiency", subtitle = "Calculations Made on Possession after Elected Timeout Games Through Feb. 22nd", caption = "Created by: Mallet James Data: ncaahoopR")
#Minimal
ggplot(data = to, aes(x=Team, y=Timeout_Eff)) +
  geom_bar(stat = "identity") +  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5))+ 
  annotation_custom(g3, ymin = .56, ymax=.68, xmin = .45, xmax= 1.55) + annotation_custom(g2, ymin = .33, ymax=.45, xmin = 1.45, xmax= 2.55) + annotation_custom(g4, ymin = .2, ymax=.3, xmin = 2.65, xmax= 3.35)+
  annotation_custom(g7, ymin = .15, ymax=.27, xmin = 3.5, xmax= 4.5) + annotation_custom(g9, ymin = .10, ymax=.20, xmin = 5.5, xmax= 6.5) + annotation_custom(g, ymin = .12, ymax=.24, xmin = 4.45, xmax= 5.55)+
  annotation_custom(g5, ymin = .025, ymax=.13, xmin = 6.5, xmax= 7.5) + annotation_custom(g10, ymin = .135, ymax=.235, xmin = 7.65, xmax= 8.35) +
  annotation_custom(g8, ymin = .115, ymax=.215, xmin = 8.65, xmax= 9.35) + annotation_custom(g11, ymin = .08, ymax=.19, xmin = 9.5, xmax= 10.5) +
  annotation_custom(g6, ymin = -.08, ymax=-.17, xmin = 10.65, xmax= 11.35) + annotation_custom(g12, ymin = -.11, ymax=-.22, xmin = 11.5, xmax= 12.5) +
  annotation_custom(g13, ymin = -.225, ymax=-.335, xmin = 12.65, xmax= 13.35) + annotation_custom(g14, ymin = -.625, ymax=-.725, xmin = 13.65, xmax= 14.35) +
  geom_text(aes(x = 1, y = .74), label = "+.708", color = "green4", size=6) + geom_text(aes(x = 2, y = .51), label = "+.474", color = "green4", size=6) +
  geom_text(aes(x = 3, y = .37), label = "+.331", color = "green4", size=6) + geom_text(aes(x = 4, y = .33), label = "+.293", color = "green4", size=6) +
  geom_text(aes(x = 5, y = .2875), label = "+.252", color = "green4", size=6) + geom_text(aes(x = 6, y = .255), label = "+.214", color = "green4", size=6) +
  geom_text(aes(x = 7, y = .175), label = "+.139", color = "green4", size=6) + geom_text(aes(x = 8, y = .095), label = "+.059", color = "green4", size=6) +
  geom_text(aes(x = 9, y = .075), label = "+.040", color = "green4", size=6) + geom_text(aes(x = 10, y = .04), label = "+.003", color = "green4", size=6) +
  geom_text(aes(x = 11, y = -.04), label = "-.006", color = "red4", size=6) + geom_text(aes(x = 12, y = -.075), label = "-.053", color = "red4", size=6) +
  geom_text(aes(x = 13, y = -.4), label = "-.368", color = "red4", size=6) + geom_text(aes(x = 14, y = -.785), label = "-.752", color = "red4", size=6) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title = element_text(size = 20, face = "bold"), axis.text = element_text(size=14)) + labs(y="Timeout Efficiency", caption = "Created by: Mallet James Data: ncaahoopR")

ggplot(data = to, aes(x = Team, y = True_Timeout_Eff, label = True_Timeout_Eff)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  labs(x = "Team", y = "Efficiency in Possession after Timeout", title = "Big Ten Timeout Efficiency", subtitle = "Calculations Made on Possession after Elected Timeout Games Through Feb. 22nd", caption = "Created by: Mallet James Data: ncaahoopR")

teamsfill <- c("Ohio State" = "#BB0000", "Penn State" = "#093162", "Michigan" = "#00274C","Michigan State" = "#18453B",
               "Illinois" = "#13294B","Purdue" = "#CEB888","Wisconsin" = "#C5050C","Minnesota" = "#7A0019",
               "Rutgers" = "#CC0033", "Northwestern" = "#4E2A84","Nebraska" = "#E41C38","Iowa" = "#FFCD00",
               "Maryland" = "#E03A3E","Indiana" = "#990000")

teamscol <- c("Ohio State" = "white", "Penn State" = "#FFFFFF", "Michigan" = "#FFCB05","Michigan State" = "#FFFFFF",
              "Illinois" = "#E84A27","Purdue" = "#000000","Wisconsin" = "#FFFFFF","Minnesota" = "#FFCC33",
              "Rutgers" = "white", "Northwestern" = "#FFFFFF","Nebraska" = "white","Iowa" = "#000000",
              "Maryland" = "#FFD520","Indiana" = "#EEEDEB")

library("ggrepel")
#B10 Offense
ggplot(data = to, aes(x= Team_Timeout_Eff, y=Offeff, label = Team)) +
  geom_point() +
  lims(x=c(.5,1.75),y=c(.8,1.05)) +
  theme_minimal() +
  coord_fixed() +  
  geom_vline(xintercept = 1.182364286) + geom_hline(yintercept = 0.938857143) + labs(x = "Offensive Efficiency out of Timeout", y="Offensive Efficiency", title = "Offensive Efficiency")+
  geom_label_repel(aes(label = Team, fill = factor(Team), color = factor(Team)), segment.color = "black", fontface = "bold") +
  scale_fill_manual(values = teamsfill) + scale_color_manual(values = teamscol) + theme(legend.position = "none") +
  geom_text(aes(x = .5, y = .945), label = "AVG Off Eff", color="black", size = 4) + geom_text(aes(x = 1.175, y = .84), label = "AVG Off Eff out of TO", color="black", size = 4, angle = 90)+
  theme(title = element_text(size = 25, face = "bold"),axis.title = element_text(size = 20, face = "bold"), axis.text = element_text(size=14))

#B10 Defense
ggplot(data = to, aes(x= Opp_Timeout_Eff, y=Defeff, label = Team)) +
  geom_point() +
  lims(x=c(.75,1.5),y=c(.75,1)) +
  theme_minimal() +
  coord_fixed() +  
  geom_vline(xintercept = 1.0871) + geom_hline(yintercept = 0.840714286) + labs(x = "Defensive Efficiency out of Timeout", y="Defensive Efficiency", title = "Defensive Efficiency") + 
  geom_label_repel(aes(label = Team, fill = factor(Team), color = factor(Team)), segment.color = "black", fontface = "bold") +
  scale_fill_manual(values = teamsfill) + scale_color_manual(values = teamscol) + theme(legend.position = "none") + geom_text(aes(x = .75, y = .845), label = "AVG Def Eff", color="black", size = 4) + geom_text(aes(x = 1.0825, y = .965), label = "AVG Def Eff out of TO", color="black", size = 4, angle = 90)+
  theme(title = element_text(size = 25, face = "bold"),axis.title = element_text(size = 20, face = "bold"), axis.text = element_text(size=14))

to$Team<-reorder(to$Team,-to$OffDiff)
#Added Offense
O <- ggplot(data = to, aes(x=Team, y=OffDiff, fill = factor(Team), label = OffDiff)) +
  geom_bar(stat = "identity") + scale_fill_manual(values = teamsfill) + theme(legend.position = "none") +
  labs(x = "Team", y="Added Points per Timeout", title = "Offensive Points Added", subtitle="Points Added based on Efficiency in Possession after Timeout minus Average Efficiency") +
  theme(title = element_text(size = 25, face = "bold"),axis.title = element_text(size = 20, face = "bold"), axis.text = element_text(size=14))+
  geom_text(size = 5, position = position_dodge(width = 1), vjust = -1)

to$Team<-reorder(to$Team,to$DefDiff)
#Added Defense
D <- ggplot(data = to, aes(x=Team, y=DefDiff, fill = factor(Team), label = DefDiff)) +
  geom_bar(stat = "identity") + scale_fill_manual(values = teamsfill) + theme(legend.position = "none") +
  labs(x = "Team", y="Added Points per Timeout", title = "Defensive Points Added", subtitle="Points Added based on Efficiency in Possession after Timeout minus Average Efficiency") +
  theme(title = element_text(size = 25, face = "bold"),axis.title = element_text(size = 20, face = "bold"), axis.text = element_text(size=14))+
  geom_text(size = 5, position = position_dodge(width = 1), vjust = -1)

library("ggpubr")
final <- ggarrange(O,D,ncol=2,nrow=1)
final
O
D

## Project Code ##
library(dplyr)

filto <- to[!(to$Team == "Northwestern" | to$Team == "Nebraska"),]

toModel <- lm(avg_diff ~ Timeout_Eff + Offeff + Defeff, data  = to)

toModel2 <- lm(avg_diff ~ Timeout_Eff + o_eFG	+	o_TO + o_OR	+ o_FTRate + d_eFG + d_TO	+	d_OR +	d_FTRate, data = to)

toModel2.1 <- lm(avg_diff ~ Timeout_Eff + o_eFG	+	o_TO + o_OR	+ o_FTRate + d_eFG + d_TO	+	d_OR, data = to)

toModel2.2 <- lm(avg_diff ~ o_eFG +	o_TO + o_OR	+ o_FTRate + d_eFG + d_TO	+	d_OR, data = to)

toModel2.3 <- lm(avg_diff ~ Timeout_Eff + o_eFG	+	o_TO + o_OR + d_eFG	+	d_OR, data = to)

toModel2.4 <- lm(avg_diff ~ Timeout_Eff + o_eFG + o_OR + d_eFG + d_OR, data = to)

toModel2.5 <- lm(avg_diff ~ Timeout_Eff + o_eFG + o_OR + d_eFG, data = to)

toModel2.6 <- lm(avg_diff ~ o_eFG + o_OR + d_eFG, data = to)

toModel2.7 <- lm(avg_diff ~ o_eFG + o_OR, data = to)

toModel2.8 <- lm(avg_diff ~ d_eFG, data = to)

toModel2.9 <- lm(avg_diff ~ o_eFG + o_TO + o_OR + d_eFG + d_OR, data = to)

library(olsrr)
ols_mallows_cp(toModel2, toModel2.1)
ols_mallows_cp(toModel2, toModel2.2)
ols_mallows_cp(toModel2, toModel2.3)
ols_mallows_cp(toModel2, toModel2.4)
ols_mallows_cp(toModel2, toModel2.5)
ols_mallows_cp(toModel2, toModel2.6)
ols_mallows_cp(toModel2, toModel2.7)
ols_mallows_cp(toModel2, toModel2.8)
summary(toModel2.1)
summary(toModel2.2)
summary(toModel2.3)
summary(toModel2.4)
summary(toModel2.5)
summary(toModel2.6)
summary(toModel2.7)
summary(toModel2.8)
summary(toModel2.9)

toCore <- to[, c("Timeout_Eff","o_eFG","o_TO","o_OR","o_FTRate","d_eFG","d_TO","d_OR","d_FTRate")]

plot(toCore)

a <- car::qqPlot(
  x = toModel2$residuals,
  distribution = "norm",
  envelope = 0.97,
  ylab = "Margin of Victory",
  pch = 19
)

plot(toModel2, which = 1)

summary(toModel2)

m=as.matrix(cbind(to$Timeout_Eff,to$avg_diff),ncol=2)

cl = (kmeans(m,4))

to$cluster = factor(cl$cluster)

centers = as.data.frame(cl$centers)

#Margin of Victory and Timeout Efficiency K-Means
ggplot(data = to, aes(x= Timeout_Eff, y=avg_diff)) +
  geom_point(color=to$cluster) +
  geom_point(data=centers, aes(x=V1,y=V2), size=95, alpha = .1) +
  theme_minimal() +
  labs(x = "Efficiency out of Timeout", y="Average Margin of Victory", title = "Margin of Victory and Timeout Efficiency")+
  geom_label_repel(aes(label = to$Team, fill = factor(to$Team), color = factor(to$Team)), segment.color = "black", fontface = "bold") +
  scale_fill_manual(values = teamsfill) + scale_color_manual(values = teamscol) + theme(legend.position = "none") +
  theme(title = element_text(size = 25, face = "bold"),axis.title = element_text(size = 20, face = "bold"), axis.text = element_text(size=14))

#Margin of Victory and Efficiency
ggplot(data = to, aes(x= eff, y=avg_diff, label = Team)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Efficiency", y="Average Margin of Victory", title = "Margin of Victory and Efficiency")+
  geom_label_repel(aes(label = Team, fill = factor(Team), color = factor(Team)), segment.color = "black", fontface = "bold") +
  scale_fill_manual(values = teamsfill) + scale_color_manual(values = teamscol) + theme(legend.position = "none") +
  theme(title = element_text(size = 25, face = "bold"),axis.title = element_text(size = 20, face = "bold"), axis.text = element_text(size=14))

