TMDdeer=read.csv(file.choose())
TMDdeer
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

# Body Size

# All sites:
bmkg_omit = subset(TMDbucksmature, !is.na(BM.KG))
bmkg_omit

bodyvars = c("BM.KG","SITE","SEASON2")
data <- bmkg_omit[bodyvars]

maturebody = summarySE(data, measurevar="BM.KG",groupvars = c("SITE","SEASON2"),conf.interval = .95)
maturebody

ggplot(maturebody, aes(x = SEASON2,y= BM.KG, fill=SITE))+
  geom_point(aes(x=SEASON2,colour = SITE), size=3,position=position_dodge(width=0.7))+
  geom_errorbar(aes(ymin=BM.KG-ci,ymax=BM.KG+ci,width=.3, colour=SITE),size = 1,position=position_dodge(width=0.7))+ 
  scale_x_continuous(breaks=min(maturebody$SEASON2):max(maturebody$SEASON2), expand=c(0,0.1))+
  labs(title= "Body Size of Mature Male White-tailed Deer at TMD Installations", y=" Average Dressed Body Mass (kg)",x="Season (year)")+
  theme(axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),axis.title.x = element_text(size = 15),axis.title.y = element_text(size = 15))+
  theme(plot.title = element_text(size = 20))
