## Periodes des inseminées pour voir si y a un rapport entre activité et période (hypothèse Amadou)

library(xts)
library(foreach); library(parallel); library(doParallel)
library(xsp)
library(hms)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)

# load functions, scripts & data
#source("./src/00 Input and Clean Data.R")
activityDF <- readRDS("./data/R02/BDD_complete/data_momentuHMM.rds")
activity <- readRDS("./data/R02/Raw-Data/Activity.rds")
Metadata <- readRDS("./data/R02/Raw-Data/Metadata.rds")

# data modification 
activityDF$Food <- str_to_title(activityDF$Food)
activityDF$Status <- str_to_title(activityDF$Status)
colnames(activityDF) <- str_to_title(colnames(activityDF))

activityDF <- subset(activityDF, Status!="Finseminated" )
activityDF$ID2 <- as.factor(paste0(activityDF$Channel, activityDF$Monitor))

# params
expt.start <- "2018-10-28 06:00:00" # starting point for data analysis in POSIXct format
expt.end <- "2018-11-03 06:00:00" # ending point for data analysis in POSIXct format

# parallelized calculation of Lomb-Scorgle periodograms

# get the number of CPU cores
ncores <- detectCores()

# register cluster
cl <- parallel::makeForkCluster(ncores)
doParallel::registerDoParallel(cl)
start <- Sys.time() # param to evaluate CPU time for calculation

chiSqPeriodograms <- foreach(i=1:length(activity), .packages = 'xsp') %dopar% {
  df <- data.frame(dateTime = seq_along(activity[[i]]), value = activity[[i]])
  
  df <- data.frame(attr(activity[[i]][paste(expt.start, expt.end, sep = "/")], which = "index"),
                   as.numeric(activity[[i]][paste(expt.start, expt.end, sep = "/")]))
  names(df) <- c("dateTime", "value")
  chiSqPeriodogram(df, res = 1)
}

end <- Sys.time() # param to evaluate CPU time for calculation
end - start

# find period
df2 <- vector()
for (i in seq_along(chiSqPeriodograms))
{
  ch <- chiSqPeriodogramPlot(chiSqPeriodograms[[i]])
  df2[i] <- ch$layers[[2]]$data$pos
}


for(j in seq_along(activity)) {
  
  metadata <- list()
  for (i in 1:dim(Metadata)[2]) {
    metadata[[i]] <- attr(x = activity[[j]], which = names(Metadata[i]))
  }
  
  metadata <- unlist(metadata)
  names(metadata) <- names(Metadata)
  attributes(chiSqPeriodograms[[j]]) <- as.list(metadata)
  
}

df <- list()
for(i in seq_along(chiSqPeriodograms))
{
  df[[i]] <- data.frame(cbind.data.frame(attributes(chiSqPeriodograms[[i]])))
}

y <- do.call(rbind.data.frame, df)

dfperiod <- cbind(y, df2)

## Deleting control and non inseminated
dfperiod <- subset(dfperiod, Status!="Non-inseminated" & Status!="Control" )
dfperiod$ID2 <- as.factor(paste0(dfperiod$Channel, dfperiod$Monitor))

# merging the 2 databases
data <- left_join(activityDF, dfperiod)

# database modification 
names(data)[names(data) == 'df2'] <- 'Period'# change the name of the variable for better notation
data$Combi <- as.factor(paste(activityDF$Food, activityDF$Status, sep="_"))# add of a combination variable for graphics
data$Winglength2 <-as.numeric(as.character(activityDF$Winglength))# transformation of the data for analysis 

# filter the data to works on night period only for activity 
datadf1 <- data %>%
  filter(Hour > 16 | Hour < 8)

data_avg <- datadf1 %>% 
  select(ID2, Combi, Period, Ac, Hour, Lightstatus, Winglength2) %>% 
  group_by(ID2) %>% 
  mutate(MeanAC= mean(Ac,na.rm=TRUE))%>%
  mutate(MeanR=MeanAC/Period)%>%ungroup()


avg_tot <- data_avg %>%
  group_by(ID2, Combi,MeanAC, MeanR, Lightstatus, Period,  Winglength2)%>%
  distinct(ID2)%>%ungroup()

## GRAPHICS brut data

t1 <- ggplot(avg_tot, aes(x = Combi, y=Period))+
  geom_boxplot() +
  ylab("Periods (h)")+
  facet_grid(Lightstatus~ .)

t1 
ggsave(plot=t1, "./img/Periode/Period.pdf")
ggsave(plot=t1,"./img/Periode/Period.png")

pavg1 <- ggplot(avg_tot, aes(x = Combi, y=MeanR))+
  geom_boxplot()+ 
  ylab("Activité/Periode")+
  facet_grid(Lightstatus~ .)
pavg1

ggsave(plot=pavg1, "./img/Periode/Period-Act.pdf")
ggsave(plot=pavg1,"./img/Periode/Period-Act.png")

pavg2 <- ggplot(avg_tot, aes(x = Combi, y=MeanAC)) +
  geom_boxplot()+ 
  ylab("Activité moyenne")+
  facet_grid(Lightstatus~ .)
pavg2

ggsave(plot=pavg2, "./img/Periode/Act.pdf")
ggsave(plot=pavg2,"./img/Periode/Act.png")

## Taking only individuals with 20-24h period 

avg_tot20 <- avg_tot %>%
  filter(Period > 20 & Period < 26)

pavg20 <- ggplot(avg_tot20, aes(x = Combi, y=MeanR))+
  geom_boxplot()+ 
  ylab("Activité/Periode")+
  facet_grid(Lightstatus~ .)

pavg20

ggsave(plot=pavg20, "./img/Periode/period_act20.pdf")
ggsave(plot=pavg20,"./img/Periode/period_act20.png")


pavg20p <- ggplot(avg_tot20, aes(x = Combi, y=Period))+
  geom_boxplot()+ 
  ylab("Periode (h)")+
  facet_grid(Lightstatus~ .)

pavg20p

ggsave(plot=pavg20p, "./img/Periode/Period20.pdf")
ggsave(plot=pavg20p,"./img/Periode/Period20.png")


## Analysis of period between group ## 

library(glmmTMB)
library(DHARMa)
library(emmeans)
library(car)
library(tidyr)
library(kableExtra)


# Working only with 20-25h period
hist(avg_tot$Period)# dispersion more complicated, I split the database
hist(avg_tot20$Period)

avg_tot20 <- avg_tot20 %>% separate(Combi, c("Food", "Status"), remove=FALSE)#separation of the combi for testing interaction later

## GLMM with gaussian distrib of error because we got continuous response variable. 
glm2 <- glmmTMB(Period~Food*Status*Lightstatus, data=avg_tot20)
glm2b <- glmmTMB(Period~Food*Status*Lightstatus+(1|ID2), data=avg_tot20)
glm2c <- glmmTMB(log10(Period)~Food*Status*Lightstatus, data=avg_tot20)# trying transformation for improve the model 
anova(glm2, glm2b, glm2c)# glm2 

glm3 <- glmmTMB(Period~Food*Status*Lightstatus*MeanAC, data=avg_tot20)
glm4 <- glmmTMB(Period~Food*Status*Lightstatus+MeanAC, data=avg_tot20)

summary(glm4)
Anova(glm4)# interaction at 3 levels signif

res <- simulateResiduals(glm2)
plot(res)# deviation still present, but model couldn't be improve with the available data. We consider it for analysis. 

res <- simulateResiduals(glm3)
plot(res)# deviation still present, but model couldn't be improve with the available data. We consider it for analysis. 
anova(glm3, glm4, glm2)

# multiple comparisons
s2 <- summary(emmeans(glm2, pairwise ~ Food*Status|Lightstatus),type="response", infer=TRUE)
kable(s2$contrasts, "markdown")

tab <- s2$emmeans# for graphics 
tab$Combi <- paste0(tab$Food,"_", tab$Status)


#PLOT for paper 
pmod <- ggplot(data = tab, aes(x=Combi, y=emmean, group=Combi, colour=Combi))+
  geom_point()+
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2)+
  geom_jitter(data=avg_tot20, aes(x=Combi, y=Period),width = 0.1, alpha=0.5)+
  facet_grid(Lightstatus~.)+
  ylab("Period (h)")+
  xlab("Groups")+
  guides(colour = FALSE)+ # supprime la légende
  ggtitle("Mean of period for each group. Mean±95CI estimated by the model and point = brut data")


ggsave(plot=pmod, "./img/Periode/Period20mod.pdf")
ggsave(plot=pmod,"./img/Periode/Period20mod.png")


## Q2 : relation between period and activity. 

# a/ we can test a simple correlation between period & activity by groups 
# or 
# b/ make a simple model (but with limitation in interpreation). I don't do that here. 

## correlation periode-activité par group 

VS <- subset(avg_tot, Combi=="Sugar_Virgin" &  Lightstatus=="LD")
VB <- subset(avg_tot, Combi=="Blood_Virgin" &  Lightstatus=="LD")
IB <- subset(avg_tot, Combi=="Blood_Inseminated" &  Lightstatus =="LD")
IS <- subset(avg_tot, Combi=="Sugar_Inseminated" &  Lightstatus =="LD")

cor.test(VS$MeanAC, VS$Period)
cor.test(IS$MeanAC, IS$Period)
cor.test(IB$MeanAC, IB$Period)
cor.test(VB$MeanAC, VB$Period)

VS <- subset(avg_tot, Combi=="Sugar_Virgin" &   Lightstatus=="DD")
VB <- subset(avg_tot, Combi=="Blood_Virgin" &  Lightstatus=="DD")
IB <- subset(avg_tot, Combi=="Blood_Inseminated" &  Lightstatus =="DD")
IS <- subset(avg_tot, Combi=="Sugar_Inseminated" &  Lightstatus =="DD")

cor.test(VS$MeanAC, VS$Period)
cor.test(IS$MeanAC, IS$Period)
cor.test(IB$MeanAC, IB$Period)
cor.test(VB$MeanAC, VB$Period)

## no significant correlation between Mean of activity and Period. 

