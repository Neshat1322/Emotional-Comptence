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
# %%%%%% data for reg1 %%%%%%%%%%%%%
#full mixed model
emotion.model<- lmer(Reg1~ group + time+ (1|ID)+ (1|Age), data=edata,REML = FALSE)
# null model 
null.model<- lmer(Reg1~1 + (1|ID)+(1|Age), data=edata, REML = FALSE)
#model with  the interaction
inter.model <- lmer(Reg1~group* time+ (1|Age)+(1|ID), data = edata,REML = FALSE)
# model for the effect of group
group.model <- lmer(Reg1~ time+ (1|Age)+(1|ID), data = edata,REML = FALSE)
#model  for effect of time
time.model <- lmer(Reg1~ group+ (1|Age)+(1|ID), data = edata,REML = FALSE)
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
group1<- subset(edata, group==1)

time.model.experiment<- lmer(Reg1~ time+ (1|ID)
                             ,data = group1,REML = FALSE)

null.model.experiment <- lmer(Reg1~ 1+(1|ID),
                              data = group1,REML = FALSE)

time_with_nullmodel_experiment <- anova(time.model.experiment,null.model.experiment)
#
#for the control group
group2 <- subset(edata, group==2)

time.model.control<- lmer(Reg1~ time+(1|ID),
                          data = edata,REML = FALSE)

null.model.control <- lmer(Reg1~ 1+(1|ID),
                           data = edata,REML = FALSE)

time_with_nullmodel_control <- anova(time.model.control,null.model.control)


