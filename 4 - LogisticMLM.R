## 2018 APPLIED PUBLIC HEALTH STATISTICS BREAKFAST WORKSHOP ##
##	R EXAMPLE 4: LOGISTIC MULTILEVEL MODELING OF ILD	##


# SUMMER G. FRANK-PEARCE & TRENT L. LALONDE #




# R FILE FOR MULTILEVEL LOGISTIC MODELING OF ILD #

# CONTENTS:												#
#	ALL ANALYSES REPEATED FOR BOTH MJ AND SMK DATASETS	#
#
#	(1) CODE FOR VARIABLE LAGGING						#
#	(2) CODE FOR TDC DECOMPOSITION						#
#	(3) LOGISTIC MULTILEVEL MODEL						#
#	(4) ODDS RATIOS 									#

library(ggplot2)

library(lme4)
library(lmerTest)


# READ IN THE DATA #

# MJ DATA #
setwd('/Users/trent.lalonde/Documents/Research/Presentations/APHA/APHA ILD Workshop/Software/Data/')
WorkshopMJData = read.csv('WorkshopMJData.csv')

# SMK DATA #
setwd('/Users/trent.lalonde/Documents/Research/Presentations/APHA/APHA ILD Workshop/Software/Data/')
WorkshopSMKData = read.csv('WorkshopSMKData.csv')




# MJ DATA #

## MODEL:										##
##	OUTCOME:		Used (BINARY)				##
##
##	PREDICTORS:		Others (FACTOR)				##
##					Energy (CONTINUOUS) 		##
##
##	CONTROLS:		Day (Factor)				##
##					TimeInStudy (CONTINUOUS)	##
##					ID (RANDOM)					##


## DESCRIPTIVES ##
table(WorkshopMJData$Used, WorkshopMJData$ID)
prop.table(table(WorkshopMJData$Used, WorkshopMJData$ID),margin=2)



##########################
## CREATE LAGGED ENERGY ##
##########################

WorkshopMJData$Prior_Energy = c(rep(NA,(nrow(WorkshopMJData))))

# LOOP #
for(i in 2:(nrow(WorkshopMJData)))
{
	WorkshopMJData$Prior_Energy[i] = WorkshopMJData$Energy[i-1]
}

# REMOVE FIRST FOR EACH PARTICIPANT #
for(i in 2:(nrow(WorkshopMJData)))
{
	if(WorkshopMJData$ID[i] != WorkshopMJData$ID[i-1])
	{
		WorkshopMJData$Prior_Energy[i] = NA
	}
}


# WITH DECOMPOSITION OF ENERGY #

Prior_Energy_between = aggregate(Prior_Energy~ID,data=WorkshopMJData,mean)
colnames(Prior_Energy_between) = c('ID','Prior_Energy_between')
WorkshopMJData = merge(WorkshopMJData,data.frame(Prior_Energy_between),by.x="ID",by.y="ID")
WorkshopMJData$Prior_Energy_within = WorkshopMJData$Prior_Energy_between - WorkshopMJData$Prior_Energy


# LOGISTIC MULTILEVEL MODEL #

LogisticMLM = glmer(Used~as.factor(Others)+Prior_Energy_between+Prior_Energy_within+TimeInStudy+(1|ID),data= WorkshopMJData,family=binomial)
LogisticMLM
summary(LogisticMLM)



## INTERPRETATIONS: ODDS RATIOS ##
exp(summary(LogisticMLM)$coefficients[,1])
exp(-summary(LogisticMLM)$coefficients[,1])






# SMK DATA #

## MODEL:										##
##	OUTCOME:		USED (BINARY)				##
##
##	PREDICTORS:		WithOthers (FACTOR)			##
##					Stress (CONTINUOUS) 		##
##
##	CONTROLS:		TimeInStudy (CONTINUOUS)	##
##					ID (RANDOM)					##


WorkshopSMKData$Used = ifelse(WorkshopSMKData$Cigarettes>0,1,0)


## DESCRIPTIVES ##
table(WorkshopSMKData$Used, WorkshopSMKData$ID)
prop.table(table(WorkshopSMKData$Used, WorkshopSMKData$ID),margin=2)



##########################
## CREATE LAGGED STRESS ##
##########################

WorkshopSMKData$Prior_Stress = c(rep(NA,(nrow(WorkshopSMKData))))

# LOOP #
for(i in 2:(nrow(WorkshopSMKData)))
{
	WorkshopSMKData$Prior_Stress[i] = WorkshopSMKData$Stress[i-1]
}

# REMOVE FIRST FOR EACH PARTICIPANT #
for(i in 2:(nrow(WorkshopSMKData)))
{
	if(WorkshopSMKData$ID[i] != WorkshopSMKData$ID[i-1])
	{
		WorkshopSMKData$Prior_Stress[i] = NA
	}
}


# WITH DECOMPOSITION OF STRESS #

Prior_Stress_between = aggregate(Prior_Stress~ID,data=WorkshopSMKData,FUN=function(x)mean(x,na.rm=TRUE))
colnames(Prior_Stress_between) = c('ID','Prior_Stress_between')
WorkshopSMKData = merge(WorkshopSMKData,data.frame(Prior_Stress_between),by.x="ID",by.y="ID")
WorkshopSMKData$Prior_Stress_within = WorkshopSMKData$Prior_Stress_between - WorkshopSMKData$Prior_Stress



# LOGISTIC MULTILEVEL MODEL #

LogisticMLM = glmer(Used~as.factor(WithOthers)+Prior_Stress_between+Prior_Stress_within+TimeInStudy+(1|ID),data= WorkshopSMKData,family=binomial)
LogisticMLM
summary(LogisticMLM)



## INTERPRETATIONS: ODDS RATIOS ##
exp(summary(LogisticMLM)$coefficients[,1])
exp(-summary(LogisticMLM)$coefficients[,1])




