#mixedmodel2
library(readxl)
library(dplyr)
library(lme4)
#
#
#reading the data
edata<- read_excel("emotiondataforR.xlsx", col_names = TRUE)
edata <- as.data.frame(edata)
attach(edata)
##
# %%%%%% data for AKT %%%%%%%%%%%%%
#full mixed model
emotion.model<- lmer(AKT~ group + time+ (1|ID), data=edata,REML = FALSE)
# null model 
null.model<- lmer(AKT~1 + (1|ID), data=edata, REML = FALSE)
#model with  the interaction
inter.model <- lmer(AKT~group* time+(1|ID), data = edata,REML = FALSE)
# model for the effect of group
group.model <- lmer(AKT~ time+(1|ID), data = edata,REML = FALSE)
#model  for effect of time
time.model <- lmer(AKT~ group+(1|ID), data = edata,REML = FALSE)
###
##
#
# evaluating the effectiveness
#
# interaction
interaction<-anova(emotion.model, inter.model)
#group
group_with_nullmodel<-anova(group.model, null.model)
group_with_fullmodel <-anova (emotion.model,group.model)
#Time
time_with_nullmodel<-anova(time.model,null.model)
time_with_fullmodel <- anova(time.model,emotion.model)
###
##
#
#effect of time in each group
#
#for the experimental group
group1<- subset(edata, group==1)

time.model.experiment<- lmer(AKT~ time+ (1|ID)
                             ,data = group1,REML = FALSE)

null.model.experiment <- lmer(AKT~ 1+(1|ID),
                              data = group1,REML = FALSE)

time_with_nullmodel_experiment <- anova(time.model.experiment,null.model.experiment)
#
#for the control group
group2 <- subset(edata, group==2)
time.model.control<- lmer(AKT~ time+(1|ID) ,
                             data = edata,REML = FALSE)
null.model.control <- lmer(AKT~ 1+(1|ID),
                              data = edata,REML = FALSE)
time_with_nullmodel_control <- anova(time.model.control,null.model.control)


