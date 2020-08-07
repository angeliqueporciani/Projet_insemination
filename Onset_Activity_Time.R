## Inseminated_Blood_LD Peak time
library(lubridate)
library(tidyverse)
library(xts)
library(TSstudio)
library(hms)
library(ggplot2)
library(dplyr)

source("./src/01 Subset Full Dataset.R")
source("./src/fun/extract.timepoints.R")
source("./src/fun/consolidate.activity.R")
source("./src/fun/plot.raw.activity.R")

rm(Control_DD,Control_LD)

################################
# Data in LD ###################
################################
# twilight period
ZT1130 <- ymd_hms("2018:10:28 17:30:00", tz = "GMT")
twilight <- vector(mode = "character")
for(i in 1:5) {
  twilight[i] <- paste0(ZT1130 + 86400 * (i-1), "/", (ZT1130 + 86400 * (i-1)) + 7200)
}


#######################################################
## VS ##

## Onset time for each day for each individuals for VS 
Onset <- vector()
Onsetime2 <- data_frame()

for(k in seq_along(twilight)){
  for (i in seq_along(Virgin_Sugar_LD)) {
    datatemp <-Virgin_Sugar_LD[[i]][twilight[k]]
    temp <-which(datatemp > 0)
    dtemp <- diff(temp)
    bemp <- logical()
    for (j in 1:(length(datatemp)-3)) {
      bemp[j] <- (dtemp[j] == 1 & dtemp[j+1] == 1 & dtemp[j+2] == 1 & dtemp[j+3] == 1)
      } 
      Onset[i] <- temp[which(bemp==1)[1]] 
      if(isTRUE(is.na(Onset[i]) == TRUE)){
        Onsetime2[i,k] <-  as.POSIXct("2018-10-28 12:00:00")
      } else {
    dt <- datatemp > 0
    Onsetime2[i,k]<- index(dt[Onset[i],]) }
  }
}

# transfo onsetime2 pour avoir les ID et le groupe. 

Onsetime2$ID <- rep(1:35)
Onsetime2$Group <- rep("VS")
colnames(Onsetime2) <- c("1", "2", "3", "4","5", "ID","Group")
ess <- pivot_longer(Onsetime2, cols = 1:5, names_to= "Day", values_to = "Time")
#ess$Time <- as.POSIXct(ess$Time, tz="GMT", origin ="1970-01-01 01:00.00 CEST" )
ess$Time2 <- as_hms(ess$Time)

VS_DF <- ess

## VB ##
## Onset time for each day for each individuals 

Onset <- vector()
Onsetime2 <- data_frame()

for(k in seq_along(twilight)){
  for (i in seq_along(Virgin_Blood_LD)) {
    datatemp <-Virgin_Blood_LD[[i]][twilight[k]]
    temp <-which(datatemp > 0)
    dtemp <- diff(temp)
    bemp <- logical()
    for (j in 1:(length(datatemp)-3)) {
      bemp[j] <- (dtemp[j] == 1 & dtemp[j+1] == 1 & dtemp[j+2] == 1 & dtemp[j+3] == 1)
    } 
    Onset[i] <- temp[which(bemp==1)[1]] 
    if(isTRUE(is.na(Onset[i]) == TRUE)){
      Onsetime2[i,k] <-  as.POSIXct("2018-10-28 12:00:00")
    } else {
      dt <- datatemp > 0
      Onsetime2[i,k]<- index(dt[Onset[i],]) }
  }
}

# transfo onsetime2 pour avoir les ID et le groupe. 

Onsetime2$ID <- rep(1:nrow(Onsetime2))
Onsetime2$Group <- rep("VB")
colnames(Onsetime2) <- c("1", "2", "3", "4","5", "ID","Group")
ess <- pivot_longer(Onsetime2, cols = 1:5, names_to= "Day", values_to = "Time")
#ess$Time <- as.POSIXct(ess$Time, tz="GMT", origin ="1970-01-01 01:00.00 CEST" )
ess$Time2 <- as_hms(ess$Time)

VB_DF <- ess

## IB ##
## Onset time for each day for each individuals 

Onset <- vector()
Onsetime2 <- data_frame()

for(k in seq_along(twilight)){
  for (i in seq_along(Inseminated_Blood_LD)) {
    datatemp <-Inseminated_Blood_LD[[i]][twilight[k]]
    temp <-which(datatemp > 0)
    dtemp <- diff(temp)
    bemp <- logical()
    for (j in 1:(length(datatemp)-3)) {
      bemp[j] <- (dtemp[j] == 1 & dtemp[j+1] == 1 & dtemp[j+2] == 1 & dtemp[j+3] == 1)
    } 
    Onset[i] <- temp[which(bemp==1)[1]] 
    if(isTRUE(is.na(Onset[i]) == TRUE)){
      Onsetime2[i,k] <-  as.POSIXct("2018-10-28 12:00:00")
    } else {
      dt <- datatemp > 0
      Onsetime2[i,k]<- index(dt[Onset[i],]) }
  }
}


# transfo onsetime2 pour avoir les ID et le groupe. 

Onsetime2$ID <- rep(1:nrow(Onsetime2))
Onsetime2$Group <- rep("IB")
colnames(Onsetime2) <- c("1", "2", "3", "4","5", "ID","Group")
ess <- pivot_longer(Onsetime2, cols = 1:5, names_to= "Day", values_to = "Time")
#ess$Time <- as.POSIXct(ess$Time, tz="GMT", origin ="1970-01-01 01:00.00 CEST" )
ess$Time2 <- as_hms(ess$Time)

IB_DF <- ess

## IS ##
## Onset time for each day for each individuals 

Onset <- vector()
Onsetime2 <- data_frame()

for(k in seq_along(twilight)){
  for (i in seq_along(Inseminated_Sugar_LD)) {
    datatemp <-Inseminated_Sugar_LD[[i]][twilight[k]]
    temp <-which(datatemp > 0)
    dtemp <- diff(temp)
    bemp <- logical()
    for (j in 1:(length(datatemp)-3)) {
      bemp[j] <- (dtemp[j] == 1 & dtemp[j+1] == 1 & dtemp[j+2] == 1 & dtemp[j+3] == 1)
    } 
    Onset[i] <- temp[which(bemp==1)[1]] 
    if(isTRUE(is.na(Onset[i]) == TRUE)){
      Onsetime2[i,k] <-  as.POSIXct("2018-10-28 12:00:00")
    } else {
      dt <- datatemp > 0
      Onsetime2[i,k]<- index(dt[Onset[i],]) }
  }
}

# transfo onsetime2 pour avoir les ID et le groupe. 

Onsetime2$ID <- rep(1:nrow(Onsetime2))
Onsetime2$Group <- rep("IS")
colnames(Onsetime2) <- c("1", "2", "3", "4","5", "ID","Group")
ess <- pivot_longer(Onsetime2, cols = 1:5, names_to= "Day", values_to = "Time")
#ess$Time <- as.POSIXct(ess$Time, tz="GMT", origin ="1970-01-01 01:00.00 CEST" )
ess$Time2 <- as_hms(ess$Time)

IS_DF <- ess

## construction tableau LD 

dfLD <- rbind(VS_DF, VB_DF, IB_DF, IS_DF)
dfLD$Light <- rep("LD")

################################
# Data in DD ###################
################################

# twilight period
ZT1130 <- ymd_hms("2018:10:28 16:30:00", tz = "GMT")# longer
twilight <- vector(mode = "character")
for(i in 1:5) {
  twilight[i] <- paste0(ZT1130 + 86400 * (i-1), "/", (ZT1130 + 86400 * (i-1)) + 7200)
}

#########################
## VS ##################
#########################

## Onset time for each day for each individuals for VS 
Onset <- vector()
Onsetime2 <- data.frame()

for(k in seq_along(twilight)){
  for (i in seq_along(Virgin_Sugar_DD)) {
    datatemp <-Virgin_Sugar_DD[[i]][twilight[k]]
    temp <-which(datatemp > 0)
    dtemp <- diff(temp)
    bemp <- logical()
    for (j in 1:(length(datatemp)-3)) {
      bemp[j] <- (dtemp[j] == 1 & dtemp[j+1] == 1 & dtemp[j+2] == 1 & dtemp[j+3] == 1)
    } 
    Onset[i] <- temp[which(bemp==1)[1]] 
    if(isTRUE(is.na(Onset[i]) == TRUE)){
      Onsetime2[i,k] <- as.POSIXct("2018-10-28 12:00:00")
    } else {
      dt <- datatemp > 0
      Onsetime2[i,k]<- index(dt[Onset[i],])}
  }
}
index(Virgin_Sugar_DD[[1]])
# transfo onsetime2 pour avoir les ID et le groupe. 

Onsetime2$ID <- rep(1:nrow(Onsetime2))
Onsetime2$Group <- rep("VS")
colnames(Onsetime2) <- c("1", "2", "3", "4","5", "ID","Group")
ess <- pivot_longer(Onsetime2, cols = 1:5, names_to= "Day", values_to = "Time")

ess$Time <- as.POSIXct(ess$Time, origin ="1970-01-01 00:00:00 CEST")
ess$Time2 <- as_hms(ess$Time)
#ess$Hour <- hour(ess$Time)

VS_DD_DF <- ess

## VB ##
## Onset time for each day for each individuals 

# twilight period

Onset <- vector()
Onsetime2 <- data.frame()

for(k in seq_along(twilight)){
  for (i in seq_along(Virgin_Blood_DD)) {
    datatemp <-Virgin_Blood_DD[[i]][twilight[k]]
    temp <-which(datatemp > 0)
    dtemp <- diff(temp)
    bemp <- logical()
    for (j in 1:(length(datatemp)-3)) {
      bemp[j] <- (dtemp[j] == 1 & dtemp[j+1] == 1 & dtemp[j+2] == 1 & dtemp[j+3] == 1)
    } 
    Onset[i] <- temp[which(bemp==1)[1]] 
    if(isTRUE(is.na(Onset[i]) == TRUE)){
      Onsetime2[i,k] <- as.POSIXct("2018-10-28 12:00:00")
    } else {
      dt <- datatemp > 0
      Onsetime2[i,k]<- index(dt[Onset[i],]) }
  }
}

# transfo onsetime2 pour avoir les ID et le groupe. 

Onsetime2$ID <- rep(1:nrow(Onsetime2))
Onsetime2$Group <- rep("VB")
colnames(Onsetime2) <- c("1", "2", "3", "4","5", "ID","Group")
ess <- pivot_longer(Onsetime2, cols = 1:5, names_to= "Day", values_to = "Time")
#ess$Time <- as.POSIXct(ess$Time, tz="GMT", origin ="1970-01-01 01:00:00 CEST" )
ess$Time2 <- as_hms(ess$Time)

VB_DD_DF <- ess

## IB ##
## Onset time for each day for each individuals 

Onset <- vector()
Onsetime2 <- data.frame()

for(k in seq_along(twilight)){
  for (i in seq_along(Inseminated_Blood_DD)) {
    datatemp <-Inseminated_Blood_DD[[i]][twilight[k]]
    temp <-which(datatemp > 0)
    dtemp <- diff(temp)
    bemp <- logical()
    for (j in 1:(length(datatemp)-3)) {
      bemp[j] <- (dtemp[j] == 1 & dtemp[j+1] == 1 & dtemp[j+2] == 1 & dtemp[j+3] == 1)
    } 
    Onset[i] <- temp[which(bemp==1)[1]] 
    if(isTRUE(is.na(Onset[i]) == TRUE)){
      Onsetime2[i,k] <- as.POSIXct("2018-10-28 12:00:00")
    } else {
      dt <- datatemp > 0
      Onsetime2[i,k]<- index(dt[Onset[i],]) }
  }
}

# transfo onsetime2 pour avoir les ID et le groupe. 

Onsetime2$ID <- rep(1:nrow(Onsetime2))
Onsetime2$Group <- rep("IB")
colnames(Onsetime2) <- c("1", "2", "3", "4","5", "ID","Group")
ess <- pivot_longer(Onsetime2, cols = 1:5, names_to= "Day", values_to = "Time")
#ess$Time <- as.POSIXct(ess$Time, tz="GMT", origin ="1970-01-01 01:00:00 CEST" )
ess$Time2 <- as_hms(ess$Time)

IB_DD_DF <- ess

## IS ##
## Onset time for each day for each individuals 

Onset <- vector()
Onsetime2 <- data.frame()

for(k in seq_along(twilight)){
  for (i in seq_along(Inseminated_Sugar_DD)) {
    datatemp <-Inseminated_Sugar_DD[[i]][twilight[k]]
    temp <-which(datatemp > 0)
    dtemp <- diff(temp)
    bemp <- logical()
    for (j in 1:(length(datatemp)-3)) {
      bemp[j] <- (dtemp[j] == 1 & dtemp[j+1] == 1 & dtemp[j+2] == 1 & dtemp[j+3] == 1)
    } 
    Onset[i] <- temp[which(bemp==1)[1]] 
    if(isTRUE(is.na(Onset[i]) == TRUE)){
      Onsetime2[i,k] <- as.POSIXct("2018-10-28 12:00:00")
    } else {
      dt <- datatemp > 0
      Onsetime2[i,k]<- index(dt[Onset[i],]) }
  }
}

# transfo onsetime2 pour avoir les ID et le groupe. 

Onsetime2$ID <- rep(1:nrow(Onsetime2))
Onsetime2$Group <- rep("IS")
colnames(Onsetime2) <- c("1", "2", "3", "4","5", "ID","Group")
ess <- pivot_longer(Onsetime2, cols = 1:5, names_to= "Day", values_to = "Time")
#ess$Time <- as.POSIXct(ess$Time, tz="GMT", origin ="1970-01-01 01:00:00 CEST" )
ess$Time2 <- as_hms(ess$Time)

IS_DD_DF <- ess
plot(Inseminated_Sugar_DD[[5]])
## Creation tableau DD 
#VS_DD_DF, 
dfDD <- rbind(VS_DD_DF,VB_DD_DF, IB_DD_DF, IS_DD_DF)
dfDD$Light <- rep("DD")

### Creation d'un tableau glob

dfOnset <- rbind(dfLD, dfDD)
saveRDS(dfOnset, "./output/Onsettot.rds")
dfOnset <- readRDS("./output/Onsettot.rds")

#################################
### GRAPHIQUES #################
#################################

# Graphics of bruts data 

# Observer un peu le nombre qui n'ont pas eu d'onset ID et nombre de jour (car on a tous conserver et pas fait une moyenne sur 5 jours par ID).

str(dfOnset)
dfOnset$Hour <- hour(dfOnset$Time)
dfOnset$Hour <- as.factor(dfOnset$Hour)

# count of number of individual without onset time for each group and each days
# LD 
m1 <- dfOnset%>%
  subset(Light=="LD")%>%
  group_by(Group, Day)%>%
  count(Hour==12)%>%ungroup()
colnames(m1) <- c("Group", "Day","HourTF", "n")
m1 <- m1%>%filter(HourTF=="TRUE")
m1

# DD
m2 <- dfOnset%>%
  subset(Light=="DD")%>%
  group_by(Group, Day)%>%
  count(Hour==12)%>%ungroup()

colnames(m2) <- c("Group", "Day","HourTF", "n")
m2 <- m2%>%filter(HourTF=="TRUE")
m2

# new df for graphics without id without onset time
dfOnset2 <- subset(dfOnset, Hour != "12")
dfOnset2$Hour <- droplevels(dfOnset2$Hour)
str(dfOnset2)


## GRAPHIQUES

gDD1 <- ggplot(subset(dfOnset2, Light=="DD"), aes(x=Time2, y=Day, colours=ID))+
  geom_boxplot()+
  geom_point()+ 
  theme_gray()+
  facet_grid(Group ~ .)

gDD1
ggsave("./img/Onset/OnsetDD.pdf", plot=gDD1)

gDD2 <- ggplot(subset(dfOnset2, Light=="DD"), aes(x=Time2, y=Group, colours=ID))+
  geom_boxplot()+
  geom_point()+ 
  scale_x_time()+
  theme_gray()

gDD2
ggsave("./img/Onset/OnsetDDgroup.pdf", plot=gDD2)

gLD1 <- ggplot(subset(dfOnset2, Light=="LD"), aes(x=Time2, y=Day))+
  geom_boxplot()+
  geom_point()+ 
  xlab("Time") +
  facet_grid(Group ~ .) +
  theme_gray()+
  ggtitle("Onset time for the 4 groups in LD")

gLD1

ggsave("./img/Onset/OnsetLD.pdf", plot=gLD1)

gLD2 <- ggplot(subset(dfOnset2, Light=="LD"), aes(x=Time2, y=Group))+
  geom_boxplot()+
  geom_point()+ 
  xlab("Time") +
  theme_gray()+
  ggtitle("Onset time for the 4 groups in LD")

gLD2
ggsave("./img/Onset/OnsetLDgroup.pdf", plot=gLD2)


## Récuperation de l'onset minimum sur les 5 jours (que pour LD)
# A mon avis pas une bonne strategie car on tire artificiellement les valeurs vers le bas sans tenir compte de la variation journalière de l'onset.


minOnset <- dfOnset2 %>%
  subset(Light=="LD")%>%
  group_by(ID, Group) %>%
  filter(Time2==min(Time2)) 

minOnset

gminLD <- ggplot(minOnset, aes(x=Time2, y=Group))+
  geom_boxplot()+
  geom_point(aes(colour=Day))+ 
  xlab("Time") +
  theme_gray()+
  ggtitle("Onset time for the 4 groups in LD")
gminLD 


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
library(glmmTMB)
library(kableExtra)

# data organisation
dfOnset2$Time3 <- as.numeric(dfOnset2$Time2)
str(dfOnset2$Day)

# creation de nouvelles covar Status & Food
dfOnset2$Status <- str_replace_all(dfOnset2$Group, c("V[S|B]" = "Virgin", "I[S|B]" = "Inseminated"))
dfOnset2$Food <- str_replace_all(dfOnset2$Group, c("[V|I]S" = "Sugar", "[V|I]B" = "Blood"))

##LD 
# Comparaison entre les groupes LD

dfLD <- subset(dfOnset2, Light=="LD")
dfLD$ID <- as.factor(dfLD$ID)
#dfLD$Day <- as.factor(dfLD$Day)

dfLD$Group <- as.factor(dfLD$Group)
hist(dfLD$Time3)
boxplot(dfLD$Time3~dfLD$Group)
shapiro.test(dfLD$Time3)# don't follow a gaussian curve
summary(dfLD$Time3)

## GLMM
# count of number of minutes since the origins then, poisson or nbinomial, but model fail to converge. Then I adjust gaussian model. 
#distrib even if there is no difference with gaussian when comparing models
glm1 <- glmmTMB(Time3~Food*Status, data=dfLD)
glm1b <- glmmTMB(Time3~Food*Status, data=dfLD, family=nbinom1)
anova(glm1, glm1b)

# res
res <- simulateResiduals(glm1)
plot(res)
res <- simulateResiduals(glm1b)
plot(res)

glm2 <- glmmTMB(Time3~Food*Status*Day, data=dfLD)
glm3 <- glmmTMB(Time3~Food*Status*Day+(1|ID), data=dfLD)
glm4 <- glmmTMB(Time3~Food*Status*Day+(Day|ID), data=dfLD)
glm5 <- glmmTMB(Time3~Food*Status*Day+(1|Day)+(1|ID), data=dfLD)
glm6 <- glmmTMB(Time3~Food*Status+Day+(1|Day)+(1|ID), data=dfLD)

anova(glm1, glm2, glm3, glm4, glm5, glm6)# glm3


# res
res <- simulateResiduals(glm3)
plot(res)


# constrast
s1 <- summary(emmeans(glm3, pairwise ~ Food*Status|Day), type="response",infer=TRUE)
kable(s1$contrasts, "markdown")
kable(Anova(glm3), "markdown")

# plot 

tabLD <- s1$emmeans 
tabLD$Time <- as.POSIXct(tabLD$emmean, origin ="2018-10-28 23:00:00 ")
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

ggsave("./img/Onset/OnsetLDgroup_mod.pdf")
ggsave("./img/Onset/OnsetLDgroup_mod.png")

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


# Comparison  entre les groupes DD
dfDD <- subset(dfOnset2, Light=="DD")
hist(dfDD$Time3)

glm1 <- glmmTMB(Time3~Food*Status, data=dfDD, family=nbinom1)
glm2 <- glmmTMB(Time3~Food*Status*Day, data=dfDD, family=nbinom1)
glm2b <- glmmTMB(Time3~Food*Status+(1|Day), data=dfDD, family=nbinom1)
glm3 <- glmmTMB(Time3~Food*Status*Day+(1|ID), data=dfDD, family=nbinom1)
glm4 <- glmmTMB(Time3~Food*Status*Day+(Day|ID), data=dfDD, family=nbinom1)
glm5 <- glmmTMB(Time3~Food*Status*Day+(1|Day)+(1|ID), data=dfDD, family=nbinom1)
glm6 <- glmmTMB(Time3~Food*Status+Day+(1|Day)+(1|ID), data=dfDD, family=nbinom1)

anova(glm1, glm2, glm2b,glm3, glm4, glm5, glm6)# glm6

# res
res <- simulateResiduals(glm6)
plot(res)

# constrast
s1 <- summary(emmeans(glm6, pairwise ~ Food*Status|Day), type="response",infer=TRUE)
kable(s1$contrasts, "markdown")
kable(Anova(glm2), "markdown")

# plot 

tabDD <- s1$emmeans 
tabDD$Time <- as.POSIXct(tabDD$response, origin ="2018-10-28 23:00:00 ")
tabDD$Hour <- as_hms(tabDD$Time)
tabDD$Combi <- paste0(tabDD$Food,"_",tabLD$Status)
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
  ggtitle("Mean of Onsetime for each group in DD. Mean ±95CI estimated, point = brut data")+
  theme(plot.title = element_text(size=9))

ggsave("./img/Onset/OnsetDDgroup_mod.pdf")
ggsave("./img/Onset/OnsetDDgroup_mod.png")

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


