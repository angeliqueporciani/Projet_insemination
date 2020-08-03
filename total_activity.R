## Script for editing graphics and analyse of total activity of the insemintation experiments.

library(tidyr)
library(dplyr)
library(hms)
library(ggplot2)
library(glmmTMB)
library(DHARMa)
library(car)
library(emmeans)
library(lubridate)
library(Rmisc)


# Load data 
activityDF <- readRDS("./data/R02/BDD_complete/actifDF.rds")

# Data wrangling
P <- c("29", "36", "47", "48", "50", "67", "70", "72", "75", "93", "134", "137", "184", "185")##ind acyclique/mort selectionné à partir du periodogramm
activityDF <- activityDF[!activityDF$ID %in% P, ]
activityDF$ID <- factor(activityDF$ID)
activityDF$winglength2 <-as.numeric(as.character(activityDF$Winglength))
activityDF <- subset(activityDF, Status!="Finseminated")# catégorie non prévue de base
activityDF <- subset(activityDF, day!="03")# suppression du dernier jour incomplet
#creation d'un facteur regroupant régime et food, pour les graphiques ca peut être pratique
activityDF$Combi <- as.factor(paste(activityDF$Food, activityDF$Status, sep="_"))
#ajout de la variable Day qui correspond au nombre de jour depuis le début de l'enregistrement 
start <- unique(date(activityDF$Time))[1] # first day of experimental period
activityDF <- mutate(activityDF, Day = day(as.period(interval(start, date(Time)), "days")))

## ajout d'une colonne Jour_nuit
# creation d'un vecteur qui permet d'avoir le creneau horaire qui nous interesse.(methode moche)
x <- c(18:23)
x2 <- c(0:5)
xt <- c(x,x2)

# ajout de la colonne 
activityDF <- mutate(activityDF, Jour_nuit = if_else(hour%in%xt, 
                                                     "Nuit", "Jour"))


## table of number of individuals keep for analysis

table <- activityDF%>%
  group_by(ID,Combi, lightStatus, Jour_nuit)%>%
  summarise(n= n()) %>%
  ungroup() %>%
  group_by(Combi, lightStatus, Jour_nuit)%>%
  summarise(n= n())%>% ungroup()%>% distinct(Combi,lightStatus,n)

kableExtra::kable(table, "markdown")

sum(table$n)#=200-control + dead


## database for graphics of brut activity

acttot <- activityDF %>%
  select(AC, ID, Time, Combi, lightStatus, hour, day, Jour_nuit) %>%
  group_by(Combi, Time, lightStatus) %>%
  mutate(sumAC= sum(AC,na.rm=TRUE),
         meanAC=mean(AC, na.rm=TRUE)) %>%
  ungroup()

acttot$time2 <- as_hms(acttot$Time)

## graphics of mean of activity per minute for all individual. 

### LD 

pLD <- ggplot(data=subset(acttot, lightStatus=="LD"), aes(x=Time, y=meanAC, colour=Combi))+
  geom_line(size=0.3)+
  scale_x_datetime(breaks=seq(as.POSIXct("2018-10-28 06:00:00"),
                              as.POSIXct("2018-11-02 18:00:00"), "6 hours"), date_labels = "%H")+
  ylab("Mean of activity")+
  xlab("Time")+
  ggtitle("Mean of activity (LD)")+
  theme(legend.position = "none")+
  facet_grid(Combi ~ .) 

pLD

ggsave(plot=pLD, filename="./img/Daily Median Mean Activity/sumetotLD.pdf")
ggsave(plot=pLD, filename="./img/Daily Median Mean Activity/sumetotLD.png")

## DD 

pDD <- ggplot(data=subset(acttot, lightStatus=="DD"), aes(x=Time, y=meanAC, group=Combi, colour=Combi))+
  geom_line(size=0.3)+
  scale_x_datetime(breaks=seq(as.POSIXct("2018-10-28 06:00:00"),
                              as.POSIXct("2018-11-02 18:00:00"), "6 hours"), date_labels = "%H")+
  ylab("Mean of activity")+
  xlab("Time")+
  ggtitle("Mean of activity (DD)")+
  theme(legend.position = "none")+
  facet_grid(Combi ~ .) 

pDD

ggsave(plot=pDD, filename="./img/Daily Median Mean Activity/sumetotDD.pdf")
ggsave(plot=pDD, filename="./img/Daily Median Mean Activity/sumetotDD.png")


## Mean of activity per day for brut data

data_avg <- activityDF %>% 
  select(ID, Combi, AC, lightStatus, Day, Jour_nuit) %>% 
  group_by(Combi, Jour_nuit, lightStatus, Day) %>% 
  mutate(MeanAC= mean(AC,na.rm=TRUE)) %>%
  separate(Combi, c("Food", "Status"), remove=FALSE) %>%
  distinct(Combi, MeanAC, lightStatus, Jour_nuit, Food, Status) %>%
  ungroup()

## Graphics of mean of activity per day 

pact <- ggplot(data=data_avg, aes(x=Day, y= MeanAC, group=Combi, color=Combi))+
  geom_line()+
  geom_point()+
  facet_grid(lightStatus+Jour_nuit~.)

pact

#############
##comparaison par individu moyenne des moyennes (mais c'est équivalent à ce qu'on a fait au dessus). 
#data_avg <- activityDF %>% 
#  select(ID, Combi, AC, lightStatus, Day, Jour_nuit) %>% 
#  group_by(ID, Day, Jour_nuit) %>% 
#  mutate(MeanAC= mean(AC,na.rm=TRUE)) %>%
#  distinct(ID,Combi, MeanAC, lightStatus, Jour_nuit) %>%
#  ungroup()%>%
#group_by(Combi, Jour_nuit, lightStatus, Day)%>%
#  mutate(MeanAC2=mean(MeanAC))%>%
#  separate(Combi, c("Food", "Status"), remove=FALSE) %>%
#  distinct(Combi, MeanAC, MeanAC2, lightStatus, Jour_nuit, Food, Status) %>%
#  ungroup()


#pact <- ggplot(data=data_avg, aes(x=Day, y= MeanAC2, group=Combi, color=Combi))+
 #geom_point()+
  #geom_line()+
#  facet_grid(lightStatus+Jour_nuit~.)

#pact


######################################################################
## Analysis with GLMM using brut data and mixed effect for Day and ID. 
# longitudinal data then glm negbinom 1
# split of database between LD status and Day and Night for simplify the model.

## DD & Day

DDJ <- subset(activityDF, Jour_nuit=="Jour" & lightStatus=="DD")
DDN <- subset(activityDF, Jour_nuit=="Nuit" & lightStatus=="DD")

mod1b <- glmmTMB(AC~Status*Food+(1|ID), family=nbinom1(link="log"), data=DDJ)
mod1c <- glmmTMB(AC~Status*Food*Day+(1|ID), family=nbinom1(link="log"), data=DDJ)
mod1d <- glmmTMB(AC~Status*Food+Day+(1|Day)+(1|ID), family=nbinom1(link="log"), data=DDJ)
mod1e <- glmmTMB(AC~Status*Food+(Day|ID), family=nbinom1(link="log"), data=DDJ)
mod1f <- glmmTMB(AC~Status*Food+Day+(Day|ID), family=nbinom1(link="log"), data=DDJ)
mod1g <- glmmTMB(AC~Status*Food*Day+(Day|ID), family=nbinom1(link="log"), data=DDJ)

anova(mod1b, mod1c, mod1d, mod1e, mod1f, mod1g)# choix du meilleur modèle

# validation with simulate residuals
res <- simulateResiduals(mod1f)
plot(res)

# prediction by the model for the graphics
s1 <- summary(emmeans(mod1g, pairwise ~ Food*Status|Day, at = list(Day=c(0,1,2,3,4,5))), type="response", infer=TRUE)
estim <- s1$emmeans
estim$Combi <- paste0(estim$Food,"-", estim$Status)

p5 <- ggplot()+
  geom_ribbon(data=estim , aes(x=Day, ymin=lower.CL, ymax=upper.CL, fill=Combi), alpha=0.2) +
  #geom_point(data=subset(actDD2, Jour_nuit=="Jour"), aes(x=Day, y=AC, shape=Combi))+
  geom_point(data=estim, aes(x=Day, y=response, colour=Combi)) +
  geom_line(data=estim, aes(x=Day, y=response, group=Combi, colour=Combi))+
  labs(colour= "Groups")+
  guides(fill = FALSE)+# supprime la 2eme légende des geom_ribbon
  ylab(element_blank())+
  xlab(element_blank())+
  ggtitle("Mean of diurnal activity per day for 
          mosquitoes in DD condition")+
  theme(plot.title = element_text(size=9))

ggsave(plot=p5, filename="./img/Daily Median Mean Activity/MeanDayDDmod.pdf")
ggsave(plot=p5, filename="./img/Daily Median Mean Activity/MeanDayDDmod.png")

## DD & Night 

mod1bN <- glmmTMB(AC~Status*Food+(1|ID), family=nbinom1(link="log"), data=DDN)
mod1cN <- glmmTMB(AC~Status*Food*Day+(1|ID), family=nbinom1(link="log"), data=DDN)
mod1dN <- glmmTMB(AC~Status*Food+Day+(1|Day)+(1|ID), family=nbinom1(link="log"), data=DDN)
mod1eN <- glmmTMB(AC~Status*Food+(Day|ID), family=nbinom1(link="log"), data=DDN)
mod1fN <- glmmTMB(AC~Status*Food+Day+(Day|ID), family=nbinom1(link="log"), data=DDN)
mod1gN <- glmmTMB(AC~Status*Food*Day+(Day|ID), family=nbinom1(link="log"), data=DDN)

anova(mod1bN, mod1cN, mod1dN, mod1eN, mod1fN, mod1gN)# selection of the best model

# validation with simulate residuals
res <- simulateResiduals(mod1gN)
plot(res)

# prediction by the model for the graphics
s2 <- summary(emmeans(mod1gN, pairwise ~ Food*Status|Day, at = list(Day=c(0,1,2,3,4,5))), type="response", infer=TRUE)
estim2 <- s2$emmeans
estim2$Combi <- paste0(estim2$Food,"-", estim2$Status)

p6 <- ggplot()+
  geom_ribbon(data=estim2 , aes(x=Day, ymin=lower.CL, ymax=upper.CL, fill=Combi), alpha=0.2) +
  geom_point(data=subset(data_avg, Jour_nuit=="Nuit"&lightStatus=="DD"), aes(x=Day, y=MeanAC, shape=Combi))+
  geom_point(data=estim2, aes(x=Day, y=response, group=Combi, colour=Combi)) +
  geom_line(data=estim2, aes(x=Day, y=response, group=Combi, colour=Combi)) +
  ylab(element_blank())+
  xlab(element_blank())+
  ggtitle("Mean of nocturnal activity per day 
          for mosquitoes in DD condition")+
  theme(plot.title = element_text(size=9))

p6

ggsave(plot=p6, filename="./img/Daily Median Mean Activity/MeanDayDDNmod.pdf")
ggsave(plot=p6, filename="./img/Daily Median Mean Activity/MeanDayDDNmod.png")


# Mod LD & Night 

LDJ <- subset(activityDF, Jour_nuit=="Jour" & lightStatus=="LD")
LDN <- subset(activityDF, Jour_nuit=="Nuit" & lightStatus=="LD")

mod2b <- glmmTMB(AC~Status*Food+(1|ID), family=nbinom1(link="log"), data=LDN)
mod2c <- glmmTMB(AC~Status*Food*Day+(1|ID), family=nbinom1(link="log"), data=LDN)
mod2d <- glmmTMB(AC~Status*Food+Day+(1|Day)+(1|ID), family=nbinom1(link="log"), data=LDN)
mod2e <- glmmTMB(AC~Status*Food+(Day|ID), family=nbinom1(link="log"), data=LDN)
mod2f <- glmmTMB(AC~Status*Food+Day+(Day|ID), family=nbinom1(link="log"), data=LDN)
mod2g <- glmmTMB(AC~Status*Food*Day+(Day|ID), family=nbinom1(link="log"), data=LDN)

anova(mod2b, mod2c, mod2d, mod2e, mod1f, mod1g)

# prediction by the model for the graphics
s3 <- summary(emmeans(mod2g, pairwise ~ Food*Status|Day, at = list(Day=c(0,1,2,3,4,5))), type="response", infer=TRUE)
estim3 <- s3$emmeans
estim3$Combi <- paste0(estim$Food,"-", estim$Status)

p7 <- ggplot()+
  geom_ribbon(data=estim3 , aes(x=Day, ymin=lower.CL, ymax=upper.CL, fill=Combi), alpha=0.2) +
  #geom_point(data=subset(actDD2, Jour_nuit=="Jour"), aes(x=Day, y=AC, shape=Combi))+
  geom_point(data=estim3, aes(x=Day, y=response, group=Combi, colour=Combi)) +
  geom_line(data=estim3, aes(x=Day, y=response, group=Combi, colour=Combi)) +
  ylab(element_blank())+
  ggtitle("Mean of nocturnal activity per day 
          for mosquitoes in LD condition")+
  theme(plot.title = element_text(size=9))

ggsave(plot=p7, filename="./img/Daily Median Mean Activity/MeanDayLDNmod.pdf")
ggsave(plot=p7, filename="./img/Daily Median Mean Activity/MeanDayLDNmod.png")

# Mod LD & Day

LDJ <- subset(activityDF, Jour_nuit=="Jour" & lightStatus=="LD")
LDN <- subset(activityDF, Jour_nuit=="Nuit" & lightStatus=="LD")

mod2bJ <- glmmTMB(AC~Status*Food+(1|ID), family=nbinom1(link="log"), data=LDJ)
mod2cJ <- glmmTMB(AC~Status*Food*Day+(1|ID), family=nbinom1(link="log"), data=LDJ)
mod2dJ <- glmmTMB(AC~Status*Food+Day+(1|Day)+(1|ID), family=nbinom1(link="log"), data=LDJ)
mod2eJ <- glmmTMB(AC~Status*Food+(Day|ID), family=nbinom1(link="log"), data=LDJ)
mod2fJ <- glmmTMB(AC~Status*Food+Day+(Day|ID), family=nbinom1(link="log"), data=LDJ)
mod2gJ <- glmmTMB(AC~Status*Food*Day+(Day|ID), family=nbinom1(link="log"), data=LDJ)

anova(mod2bJ, mod2cJ, mod2dJ, mod2eJ, mod2fJ, mod2gJ)

# prediction by the model for the graphics
s4 <- summary(emmeans(mod2gJ, pairwise ~ Food*Status|Day, at = list(Day=c(0,1,2,3,4,5))), type="response", infer=TRUE)
estim4 <- s4$emmeans
estim4$Combi <- paste0(estim$Food,"-", estim$Status)

p8 <- ggplot()+
  geom_ribbon(data=estim4 , aes(x=Day, ymin=lower.CL, ymax=upper.CL, fill=Combi), alpha=0.2) +
  #geom_point(data=subset(actDD2, Jour_nuit=="Jour"), aes(x=Day, y=AC, shape=Combi))+
  geom_point(data=estim4, aes(x=Day, y=response, group=Combi, colour=Combi)) +
  geom_line(data=estim4, aes(x=Day, y=response, group=Combi, colour=Combi)) +
  ylab(element_blank())+
  ggtitle("Mean of diurnal activity per day 
          for mosquitoes in LD condition")+
  theme(plot.title = element_text(size=9))

ggsave(plot=p8, filename="./img/Daily Median Mean Activity/MeanDayLDJmod.pdf")
ggsave(plot=p8, filename="./img/Daily Median Mean Activity/MeanDayLDJmod.png")

library(gridExtra)
library(cowplot)

##function for tae legend from one graphics
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(p5)

##Delete legend form graph 
p1 <- p5+theme(legend.position="none", legend.title = element_blank())
p2 <- p6+theme(legend.position="none", legend.title = element_blank())
p3 <- p7+theme(legend.position="none", legend.title = element_blank())
p4 <- p8+theme(legend.position="none", legend.title = element_blank())


##grid with all graphs
plot_grid(p1, p2, p3, p4, ncol = 2, nrow = 2)
grid.arrange(p2, p1, legend, p3, p4, ncol=3, nrow=2)

