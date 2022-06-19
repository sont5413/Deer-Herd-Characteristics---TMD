TMDdeer=read.csv(file.choose())

summary(TMDdeer)
TMDbucksmature=TMDdeer[TMDdeer$AGE>"3.5" & TMDdeer$SEX == "M",]

library(dplyr)
library(Rmisc)
library(tidyverse)
library(ggplot2)
library(hmi)
library(lattice)
library(survival)
library(Hmisc)


# Antler Size

# All sites:
TMDbucksmatureANTLERomit = subset(TMDbucksmature, !is.na(BEAM.T.MM))
TMDbucksmatureANTLERomit

antlervars = c("BEAM.T.MM","SITE","SEASONCAT","SEASON2")
newdata <- TMDbucksmatureANTLERomit[antlervars]
newdata$SEASONCAT = factor(newdata$SEASONCAT)
newdata
matureantler = summarySE(newdata, measurevar="BEAM.T.MM",groupvars = c("SITE","SEASON2"))
matureantler

ggplot(matureantler, aes(x = SEASON2,y= BEAM.T.MM, fill=SITE))+
  geom_point(aes(x=SEASON2,colour = SITE),size=3,position=position_dodge(width=0.7))+
  geom_errorbar(aes(ymin=BEAM.T.MM-ci,ymax=BEAM.T.MM+ci,width=.3, colour=SITE),size = 1,position=position_dodge(width=0.7))+ 
  scale_x_continuous(breaks=min(matureantler$SEASON2):max(matureantler$SEASON2), expand=c(0,0.1))+
  labs(title= "Antler Size of Mature Male White-tailed Deer at TMD Installations", y="Average Total Main Beam Length (mm)",x="Season")+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.x = element_text(size = 15),axis.title.y = element_text(size = 15))+
  theme(plot.title = element_text(size = 20))

