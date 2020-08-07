library(lubridate)
library(tidyverse)
library(xts)
library(TSstudio)
library(hms)
library(kableExtra)
library(glmmTMB)
library(emmeans)
source("./src/01 Subset Full Dataset.R")
source("./src/fun/extract.timepoints.R")
source("./src/fun/consolidate.activity.R")

 
# Heure à laquelle il y a le Maximum d'activité par jour (j) par individu (i)

  ## 1. LIGHT DARK (LD) REGIMEN

  ### 1.1 twilight period

ZT1130 <- ymd_hms("2018:10:28 17:30:00", tz = "GMT")
twilight <- vector(mode = "character")
for(i in 1:5) {
  twilight[i] <- paste0(ZT1130 + 86400 * (i-1), "/", (ZT1130 + 86400 * (i-1)) + 7200)
}


  ### 1.2. Virgin_Sugar_LD

crep.activity.max.VSLD <- data.frame()
for(i in seq_along(Virgin_Sugar_LD)) {
  for(j in seq_along(twilight)) {
    dtemp <- Virgin_Sugar_LD[[i]][twilight[j]]
    n <- which.max(dtemp)
    crep.activity.max.VSLD[i, j] <- as.POSIXct(index(dtemp[n]))
  }
}


crep.activity.max.VSLD$ID <- rep(1:35)
crep.activity.max.VSLD$Group <- rep("VS")
colnames(crep.activity.max.VSLD) <- c("1", "2", "3", "4","5", "ID","Group")
ess <- pivot_longer( crep.activity.max.VSLD, cols = 1:5, names_to= "Day", values_to = "Time")
#ess$Time <- as.POSIXct(ess$Time, tz="GMT", origin ="1970-01-01 01:00.00 CEST" )
ess$Time2 <- as_hms(ess$Time)

VS_DF <- ess

# graph
VSLD <- ggplot(VS_DF) +
  aes(x = Time2, y = Day) +
  geom_violin(fill='#A4A4A4', color="darkred") + 
  ggtitle("Virgin Sugar") +
  scale_y_discrete(labels=c("1", "2", "3", "4", "5"))+
  scale_x_time(name = "Time", breaks = waiver())+
  geom_jitter(shape=16, position=position_jitter(0.2)) + ## pour voir la distribution (les points)
  geom_boxplot(width=0.1)  ## Pour ajouter la mediane et les quartiles 
  

 ggsave("./img/Twilight_Max_Activity/Pdf/Virgin Sugar LD.pdf", plot = VSLD) 
 ggsave("./img/Twilight_Max_Activity/Png/Virgin Sugar LD.png", plot = VSLD)

   
 
  ### 1.3. Virgin_Blood_LD
# Méthode qui ne conserve que le nombre de minute depuis le ZT choisi. (ancienne methode)
 # je la laisse ici au cas où on voudrais faire machine arrière. 
 
#crep.activity.max.VBLD <- matrix(nrow = length(Virgin_Blood_LD), ncol = length(twilight))
#for(i in seq_along(Virgin_Blood_LD)) {
 # for(j in seq_along(twilight)) {
  #  crep.activity.max.VBLD[i, j] <- which.max(Virgin_Blood_LD[[i]][twilight[j]])
  #}
#}

#crep.activity.max.VBLD <- gather(as.data.frame(crep.activity.max.VBLD), "Day", "Minutes", 1:5)

#VBLD <- ggplot(filter(crep.activity.max.VBLD, Minutes > 1)) +
#  aes(x = Day, y = Minutes) +
#  geom_violin(fill='#A4A4A4', color="darkred") + 
#  scale_y_continuous(limits=c(0, 125)) +
#  ggtitle("Virgin Blood") +
#  geom_jitter(shape=16, position=position_jitter(0.2)) + 
# geom_boxplot(width=0.1) +
#  scale_x_discrete(breaks=c("V1","V2","V3","V4","V5"),
#                   labels=c("1", "2", "3","4","5"))

 #nouvelle methode qui permet d'obtenir les heures-min réelles. 
 
 crep.activity.max.VBLD <- data.frame()
 for(i in seq_along(Virgin_Blood_LD)) {
   for(j in seq_along(twilight)) {
     dtemp <- Virgin_Blood_LD[[i]][twilight[j]]
     n <- which.max(dtemp)
     crep.activity.max.VBLD[i, j] <- as.POSIXct(index(dtemp[n]))
   }
 }
 
 
 crep.activity.max.VBLD$ID <- rep(1:nrow(crep.activity.max.VBLD))
 crep.activity.max.VBLD$Group <- rep("VB")
 colnames (crep.activity.max.VBLD) <- c("1", "2", "3", "4","5", "ID","Group")
 ess <- pivot_longer( crep.activity.max.VBLD, cols = 1:5, names_to= "Day", values_to = "Time")
 #ess$Time <- as.POSIXct(ess$Time, tz="GMT", origin ="1970-01-01 01:00.00 CEST" )
ess$Time2 <- as_hms(ess$Time)
 
VB_DF <- ess
 
 # graph
 VBLD <- ggplot(VB_DF) +
   aes(x = Time2, y = Day) +
   geom_violin(fill='#A4A4A4', color="darkred") + 
   ggtitle("Virgin Blood") +
   scale_y_discrete(labels=c("1", "2", "3", "4", "5"))+
   scale_x_time(name = "Time", breaks = waiver())+
   geom_jitter(shape=16, position=position_jitter(0.2)) + ## pour voir la distribution (les points)
   geom_boxplot(width=0.1)  ## Pour ajouter la mediane et les quartiles 
 
 

ggsave("./img/Twilight_Max_Activity/Pdf/Virgin Blood LD.pdf", plot = VBLD) 
ggsave("./img/Twilight_Max_Activity/Png/Virgin Blood LD.png", plot = VBLD)

  ### 1.4. Inseminated_Sugar_LD


crep.activity.max.ISLD <- data.frame()
for(i in seq_along(Inseminated_Sugar_LD)) {
  for(j in seq_along(twilight)) {
    dtemp <- Inseminated_Sugar_LD[[i]][twilight[j]]
    n <- which.max(dtemp)
    crep.activity.max.ISLD[i, j] <- as.POSIXct(index(dtemp[n]))
  }
}


crep.activity.max.ISLD$ID <- rep(1:nrow(crep.activity.max.ISLD))
crep.activity.max.ISLD$Group <- rep("IS")
colnames (crep.activity.max.ISLD) <- c("1", "2", "3", "4","5", "ID","Group")
ess <- pivot_longer(crep.activity.max.ISLD, cols = 1:5, names_to= "Day", values_to = "Time")
#ess$Time <- as.POSIXct(ess$Time, tz="GMT", origin ="1970-01-01 01:00.00 CEST" )
ess$Time2 <- as_hms(ess$Time)

IS_DF <- ess

# graph
ISLD <- ggplot(IS_DF) +
  aes(x = Time2, y = Day) +
  geom_violin(fill='#A4A4A4', color="darkred") + 
  ggtitle("Inseminated Sugar") +
  scale_y_discrete(labels=c("1", "2", "3", "4", "5"))+
  scale_x_time(name = "Time", breaks = waiver())+
  geom_jitter(shape=16, position=position_jitter(0.2)) + ## pour voir la distribution (les points)
  geom_boxplot(width=0.1)  ## Pour ajouter la mediane et les quartiles 


ggsave("./img/Twilight_Max_Activity/Pdf/Inseminated Sugar LD.pdf", plot = ISLD) 
ggsave("./img/Twilight_Max_Activity/Png/Inseminated Sugar LD.png", plot = ISLD)

  ### 1.5. Inseminated_Blood_LD


crep.activity.max.IBLD <- data.frame()
for(i in seq_along(Inseminated_Blood_LD)) {
  for(j in seq_along(twilight)) {
    dtemp <- Inseminated_Blood_LD[[i]][twilight[j]]
    n <- which.max(dtemp)
    crep.activity.max.IBLD[i, j] <- as.POSIXct(index(dtemp[n]))
  }
}


crep.activity.max.IBLD$ID <- rep(1:nrow(crep.activity.max.IBLD))
crep.activity.max.IBLD$Group <- rep("IB")
colnames (crep.activity.max.IBLD) <- c("1", "2", "3", "4","5", "ID","Group")
ess <- pivot_longer(crep.activity.max.IBLD, cols = 1:5, names_to= "Day", values_to = "Time")
#ess$Time <- as.POSIXct(ess$Time, tz="GMT", origin ="1970-01-01 01:00.00 CEST" )
ess$Time2 <- as_hms(ess$Time)

IB_DF <- ess

# graph
IBLD <- ggplot(IB_DF) +
  aes(x = Time2, y = Day) +
  geom_violin(fill='#A4A4A4', color="darkred") + 
  ggtitle("Inseminated Blood") +
  scale_y_discrete(labels=c("1", "2", "3", "4", "5"))+
  scale_x_time(name = "Time", breaks = waiver())+
  geom_jitter(shape=16, position=position_jitter(0.2)) + ## pour voir la distribution (les points)
  geom_boxplot(width=0.1)  ## Pour ajouter la mediane et les quartiles 

  
ggsave("./img/Twilight_Max_Activity/Pdf/Inseminated Blood LD.pdf", plot = IBLD) 
ggsave("./img/Twilight_Max_Activity/Png/Inseminated Blood LD.png", plot = IBLD)

    ### 1.6. Regrouper tous les graphs sur une seule  figure

library(gridExtra)
library(cowplot)

LD_Twilight <- grid.arrange(VSLD, VBLD, ISLD, IBLD, ncol=2, nrow = 2)
# Faut vraiment résoudre les problème de légende des graphiques (illisible tel quel).

#ggsave("./img/Twilight_Max_Activity/Pdf/Groupe Twilight Max Activity LD.pdf", plot = LD_Twilight) 
#ggsave("./img/Twilight_Max_Activity/Png/Groupe Twilight Max Activity LD.png", plot = LD_Twilight)


## Construction tableau global en LD 
dfLD <- rbind(VS_DF, VB_DF, IB_DF, IS_DF)
dfLD$Light <- rep("LD")

#############################
## 2. DARK DARK (DD) REGIMEN
#############################  

  ### 2.1. twilight period in DD (à définir)
  
ZT1030 <- ymd_hms("2018:10:28 16:30:00", tz = "GMT")
twilight <- vector(mode = "character")
for(i in 1:5) {
  twilight[i] <- paste0(ZT1030 + 86400 * (i-1), "/", (ZT1030 + 86400 * (i-1)) + 10800)
}

  ### 2.2. Virgin_Sugar_DD

crep.activity.max.VSDD <- data.frame()
for(i in seq_along(Virgin_Sugar_DD)) {
  for(j in seq_along(twilight)) {
    dtemp <- Virgin_Sugar_DD[[i]][twilight[j]]
    n <- which.max(dtemp)
    crep.activity.max.VSDD[i, j] <- as.POSIXct(index(dtemp[n]))
  }
}


crep.activity.max.VSDD$ID <- rep(1:nrow(crep.activity.max.VSDD))
crep.activity.max.VSDD$Group <- rep("VS")
colnames(crep.activity.max.VSDD) <- c("1", "2", "3", "4","5", "ID","Group")
ess <- pivot_longer( crep.activity.max.VSDD, cols = 1:5, names_to= "Day", values_to = "Time")
#ess$Time <- as.POSIXct(ess$Time, tz="GMT", origin ="1970-01-01 01:00.00 CEST" )
ess$Time2 <- as_hms(ess$Time)

VSDD_DF <- ess

# graph
VSDD <- ggplot(VSDD_DF) +
  aes(x = Time2, y = Day) +
  geom_violin(fill='#A4A4A4', color="darkred") + 
  ggtitle("Virgin Sugar") +
  scale_y_discrete(labels=c("1", "2", "3", "4", "5"))+
  scale_x_time(name = "Time", breaks = waiver())+
  geom_jitter(shape=16, position=position_jitter(0.2)) + ## pour voir la distribution (les points)
  geom_boxplot(width=0.1)  ## Pour ajouter la mediane et les quartiles 


ggsave("./img/Twilight_Max_Activity/Pdf/Virgin Sugar DD.pdf", plot = VSDD) 
ggsave("./img/Twilight_Max_Activity/Png/Virgin Sugar DD.png", plot = VSDD)


  ### 2.3. Virgin_Blood_DD

crep.activity.max.VBDD <- data.frame()
for(i in seq_along(Virgin_Blood_DD)) {
  for(j in seq_along(twilight)) {
    dtemp <- Virgin_Blood_DD[[i]][twilight[j]]
    n <- which.max(dtemp)
    crep.activity.max.VBDD[i, j] <- as.POSIXct(index(dtemp[n]))
  }
}


crep.activity.max.VBDD$ID <- rep(1:nrow(crep.activity.max.VBDD))
crep.activity.max.VBDD$Group <- rep("VB")
colnames(crep.activity.max.VBDD) <- c("1", "2", "3", "4","5", "ID","Group")
ess <- pivot_longer( crep.activity.max.VBDD, cols = 1:5, names_to= "Day", values_to = "Time")
#ess$Time <- as.POSIXct(ess$Time, tz="GMT", origin ="1970-01-01 01:00.00 CEST" )
ess$Time2 <- as_hms(ess$Time)

VBDD_DF <- ess

# graph
VBDD <- ggplot(VBDD_DF) +
  aes(x = Time2, y = Day) +
  geom_violin(fill='#A4A4A4', color="darkred") + 
  ggtitle("Virgin Blood") +
  scale_y_discrete(labels=c("1", "2", "3", "4", "5"))+
  scale_x_time(name = "Time", breaks = waiver())+
  geom_jitter(shape=16, position=position_jitter(0.2)) + ## pour voir la distribution (les points)
  geom_boxplot(width=0.1)  ## Pour ajouter la mediane et les quartiles 


ggsave("./img/Twilight_Max_Activity/Pdf/Virgin Blood DD.pdf", plot = VBDD) 
ggsave("./img/Twilight_Max_Activity/Png/Virgin Blood DD.png", plot = VBDD)


  ### 2.4. Inseminated_Sugar_DD

crep.activity.max.ISDD <- data.frame()
for(i in seq_along(Inseminated_Sugar_DD)) {
  for(j in seq_along(twilight)) {
    dtemp <- Inseminated_Sugar_DD[[i]][twilight[j]]
    n <- which.max(dtemp)
    crep.activity.max.ISDD[i, j] <- as.POSIXct(index(dtemp[n]))
  }
}


crep.activity.max.ISDD$ID <- rep(1:nrow(crep.activity.max.ISDD))
crep.activity.max.ISDD$Group <- rep("IS")
colnames(crep.activity.max.ISDD) <- c("1", "2", "3", "4","5", "ID","Group")
ess <- pivot_longer(crep.activity.max.ISDD, cols = 1:5, names_to= "Day", values_to = "Time")
#ess$Time <- as.POSIXct(ess$Time, tz="GMT", origin ="1970-01-01 01:00.00 CEST" )
ess$Time2 <- as_hms(ess$Time)

ISDD_DF <- ess

# graph
ISDD <- ggplot(ISDD_DF) +
  aes(x = Time2, y = Day) +
  geom_violin(fill='#A4A4A4', color="darkred") + 
  ggtitle("Inseminated Sugar") +
  scale_y_discrete(labels=c("1", "2", "3", "4", "5"))+
  scale_x_time(name = "Time", breaks = waiver())+
  geom_jitter(shape=16, position=position_jitter(0.2)) + ## pour voir la distribution (les points)
  geom_boxplot(width=0.1)  ## Pour ajouter la mediane et les quartiles 


ggsave("./img/Twilight_Max_Activity/Pdf/Inseminated Sugar DD.pdf", plot = ISDD) 
ggsave("./img/Twilight_Max_Activity/Png/Inseminated Sugar DD.png", plot = ISDD)


  ### 2.5. Inseminated_Blood_DD

crep.activity.max.IBDD <- data.frame()
for(i in seq_along(Inseminated_Blood_DD)) {
  for(j in seq_along(twilight)) {
    dtemp <- Inseminated_Blood_DD[[i]][twilight[j]]
    n <- which.max(dtemp)
    crep.activity.max.IBDD[i, j] <- as.POSIXct(index(dtemp[n]))
  }
}


crep.activity.max.IBDD$ID <- rep(1:nrow(crep.activity.max.IBDD))
crep.activity.max.IBDD$Group <- rep("IB")
colnames(crep.activity.max.IBDD) <- c("1", "2", "3", "4","5", "ID","Group")
ess <- pivot_longer(crep.activity.max.IBDD, cols = 1:5, names_to= "Day", values_to = "Time")
#ess$Time <- as.POSIXct(ess$Time, tz="GMT", origin ="1970-01-01 01:00.00 CEST" )
ess$Time2 <- as_hms(ess$Time)

IBDD_DF <- ess

# graph
IBDD <- ggplot(IBDD_DF) +
  aes(x = Time2, y = Day) +
  geom_violin(fill='#A4A4A4', color="darkred") + 
  ggtitle("Inseminated Blood") +
  scale_y_discrete(labels=c("1", "2", "3", "4", "5"))+
  scale_x_time(name = "Time", breaks = waiver())+
  geom_jitter(shape=16, position=position_jitter(0.2)) + ## pour voir la distribution (les points)
  geom_boxplot(width=0.1)  ## Pour ajouter la mediane et les quartiles 



ggsave("./img/Twilight_Max_Activity/Pdf/Inseminated Blood DD.pdf", plot = IBDD) 
ggsave("./img/Twilight_Max_Activity/Png/Inseminated Blood DD.png", plot = IBDD)
  

## Construction tableau global en DD 
dfDD <- rbind(VSDD_DF, VBDD_DF, IBDD_DF, ISDD_DF)
dfDD$Light <- rep("DD")

## Construction tableau global 
dfTwilight <- rbind(dfLD, dfDD)
saveRDS(dfTwilight, "./output/Twiligthtot.rds")


dfTwilight<- readRDS("./output/Twiligthtot.rds")


 
## Graph of brut data
gDD1 <- ggplot(subset(dfTwilight, Light=="DD"), aes(x=Time2, y=Day))+
  geom_boxplot()+
  geom_point()+ 
  theme_gray()+
  facet_grid(Group ~ .)+
  ggtitle("Twilight Max activity Peak for the 4 groups in DD")+
  xlab("Time")

gDD1
ggsave("./img/Twilight_Max_Activity/TwilightDD.pdf", plot=gDD1)

gDD2 <- ggplot(subset(dfTwilight, Light=="DD"), aes(x=Time2, y=Group))+
  geom_boxplot()+
  geom_point()+ 
  scale_x_time()+
  theme_gray()+
  xlab("Time")+
  ggtitle("Twilight Max activity Peak for the 4 groups in DD")

gDD2

ggsave("./img/Twilight_Max_activity/TwilightDDGroup.pdf", plot=gDD2)

LD <- subset(dfTwilight, Light=="LD")
str(LD)
gLD1 <- ggplot(dfLD, aes(x=Time2, y=Day))+
  geom_boxplot()+
  geom_point() + 
  facet_grid(Group ~ .) +
  xlab("Time") +
  theme_gray() +
  ggtitle("Twilight Max activity Peak for the 4 groups in LD")

gLD1

ggsave("./img/Twilight_Max_activity/TwilightLD.pdf", plot=gLD1)

gLD2 <- ggplot(subset(dfLD, Light=="LD"), aes(x=Time2, y=Group))+
  geom_boxplot()+
  geom_point()+ 
  xlab("Time") +
  theme_gray()+
  ggtitle("Twilight Max activity Peak for the 4 groups in LD")

gLD2
ggsave("./img/Twilight_Max_activity/TwilightLDGroup.pdf", plot=gLD2)

###########################
## ANALYSIS ## 
###########################
library(car)
library(emmeans)
library(DHARMa)
library(bbmle)
library(sjPlot)
library(sjmisc)
detach(TSstudio)
library(stringr)
# Comparaison entre les groupes LD

dfLD <- subset(dfTwilight, Light=="LD")
dfLD$Status <- str_replace_all(dfLD$Group, c("V[S|B]" = "Virgin", "I[S|B]" = "Inseminated"))
dfLD$Food <- str_replace_all(dfLD$Group, c("[V|I]S" = "Sugar", "[V|I]B" = "Blood"))
dfLD$Group <- as.factor(dfLD$Group)
dfLD$Time3 <- as.numeric(dfLD$Time2)
hist(dfLD$Time3)
boxplot(dfLD$Time3~dfLD$Group)
shapiro.test(dfLD$Time3)# don't follow a gaussian curve

## GLM 
glm1 <- glmmTMB(Time3~Food*Status, data=dfLD)
glm2 <- glmmTMB(Time3~Food*Status, data=dfLD, family=poisson)
glm3 <- glmmTMB(Time3~Food*Status, data=dfLD, family=nbinom1)
glm4 <- glmmTMB(Time3~Food*Status*Day, data=dfLD, family=nbinom1)
glm5 <- glmmTMB(Time3~Food*Status*Day+(1|ID), data=dfLD, family=nbinom1)
glm6 <- glmmTMB(Time3~Food*Status*Day+(Day|ID), data=dfLD, family=nbinom1)
glm7 <- glmmTMB(Time3~Food*Status+Day+(Day|ID), data=dfLD, family=nbinom1)

anova(glm1, glm2, glm3, glm4, glm5, glm6, glm7)#glm5

# res
res <- simulateResiduals(glm5)
plot(res)


# constrast
s1 <- summary(emmeans(glm5, pairwise ~ Food*Status|Day),type="response", infer=TRUE)
kable(s1$contrasts, "markdown")
kable(Anova(glm5), "markdown")

tabLD <- s1$emmeans 
tabLD$Time <- as.POSIXct(tabLD$response, origin ="2018-10-28 23:00:00 ")
tabLD$Hour <- as_hms(tabLD$Time)
tabLD$Combi <- paste0(tabLD$Food,"_",tabLD$Status)
dfLD$Combi <- paste0(dfLD$Food,"_",dfLD$Status)

## Graph for comparison between group for each day
ggplot(data = tabLD, aes(x=Hour, y=Combi, colour=Combi))+
  geom_point()+
  geom_errorbar(aes(xmin = lower.CL, xmax = upper.CL))+
  facet_grid(Day~.)+
  geom_jitter(data=dfLD, aes(x=Time2, y=Combi, colour=Combi),width = 0.1, alpha=0.3)+
  ylab("Group")+
  xlab("Hour")+
  guides(colour = FALSE)+ # supprime la légende
  ggtitle("Mean of Onsetime for each groupin LD. Mean ±95CI estimated, point = brut data")+
  theme(plot.title = element_text(size=9))

ggsave("./img/Twilight_Max_Activity/TMLDgroup_mod.pdf")
ggsave("./img/Twilight_Max_Activity/TMLDgroup_mod.png")

## Graph for comparison between day for each group 
ggplot(data = tabLD, aes(x=Hour, y=Day, colour=Combi))+
  geom_point()+
  geom_errorbar(aes(xmin = lower.CL, xmax = upper.CL))+
  facet_grid(Combi~.)+
  geom_jitter(data=dfLD, aes(x=Time2, y=Day, colour=Combi),width = 0.1, alpha=0.3)+
  ylab("Day")+
  xlab("Hour")+
  guides(colour = FALSE)+ # supprime la légende
  ggtitle("Mean of Onsetime for each group and day in LD. Mean ±95CI estimated, point = brut data")+
  theme(plot.title = element_text(size=9))

# use the graph that you need knowing the contrast comparison made for graph 1. 

## DD 
# Comparaison entre les groupes DD
dfDD <- subset(dfTwilight, Light=="DD")
dfDD$Time3 <- as.numeric(dfDD$Time2)
hist(dfDD$Time3)
boxplot(dfDD$Time3~dfDD$Group)
shapiro.test(dfDD$Time3)# don't follow a gaussian curve
dfDD$Status <- str_replace_all(dfDD$Group, c("V[S|B]" = "Virgin", "I[S|B]" = "Inseminated"))
dfDD$Food <- str_replace_all(dfDD$Group, c("[V|I]S" = "Sugar", "[V|I]B" = "Blood"))

## GLM 
glm1 <- glmmTMB(Time3~Food*Status, data=dfDD)
glm2 <- glmmTMB(Time3~Food*Status, data=dfDD, family=poisson)
glm3 <- glmmTMB(Time3~Food*Status, data=dfDD, family=nbinom1)
glm4 <- glmmTMB(Time3~Food*Status*Day, data=dfDD, family=nbinom1)
glm5 <- glmmTMB(Time3~Food*Status*Day+(1|ID), data=dfDD, family=nbinom1)
glm6 <- glmmTMB(Time3~Food*Status*Day+(Day|ID), data=dfDD, family=nbinom1)
glm7 <- glmmTMB(Time3~Food*Status+Day+(Day|ID), data=dfDD, family=nbinom1)

anova(glm1, glm2, glm3)#nbinom
anova(glm1, glm2, glm3, glm4, glm5, glm6, glm7)#glm5

# res
res <- simulateResiduals(glm5)
plot(res)


# constrast
s1 <- summary(emmeans(glm5, pairwise ~ Food*Status|Day),type="response", infer=TRUE)
kable(s1$contrasts, "markdown")
kable(Anova(glm5), "markdown")

tabDD <- s1$emmeans 
tabDD$Time <- as.POSIXct(tabDD$response, origin ="2018-10-28 23:00:00 ")
tabDD$Hour <- as_hms(tabDD$Time)
tabDD$Combi <- paste0(tabDD$Food,"_",tabDD$Status)
dfDD$Combi <- paste0(dfDD$Food,"_",dfDD$Status)

## Graph for comparison between group for each day
ggplot(data = tabDD, aes(x=Hour, y=Combi, colour=Combi))+
  geom_point()+
  geom_errorbar(aes(xmin = lower.CL, xmax = upper.CL))+
  facet_grid(Day~.)+
  geom_jitter(data=dfDD, aes(x=Time2, y=Combi, colour=Combi),width = 0.1, alpha=0.3)+
  ylab("Group")+
  xlab("Hour")+
  guides(colour = FALSE)+ # supprime la légende
  ggtitle("Mean of Onsetime for each groupin DD. Mean ±95CI estimated, point = brut data")+
  theme(plot.title = element_text(size=9))

  ggsave("./img/Twilight_Max_Activity/TMDDgroup_mod.pdf")
  ggsave("./img/Twilight_Max_Activity/TMDDgroup_mod.png")
  
## Graph for comparison between day for each group 
ggplot(data = tabDD, aes(x=Hour, y=Day, colour=Combi))+
  geom_point()+
  geom_errorbar(aes(xmin = lower.CL, xmax = upper.CL))+
  facet_grid(Combi~.)+
  geom_jitter(data=dfDD, aes(x=Time2, y=Day, colour=Combi),width = 0.1, alpha=0.3)+
  ylab("Day")+
  xlab("Hour")+
  guides(colour = FALSE)+ # supprime la légende
  ggtitle("Mean of Onsetime for each group and day in DD. Mean ±95CI estimated, point = brut data")+
  theme(plot.title = element_text(size=9))

# use the graph that you need knowing the contrast comparison made for graph 1. 

