## 2018 APPLIED PUBLIC HEALTH STATISTICS BREAKFAST WORKSHOP ##
##	R EXAMPLE 3: CONTINUOUS MULTILEVEL MODELING OF ILD	##


# SUMMER G. FRANK-PEARCE & TRENT L. LALONDE #



# R FILE FOR CONTINUOUS MULTILEVEL MODELING OF ILD #


# CONTENTS:												#
#	ALL ANALYSES REPEATED FOR BOTH MJ AND SMK DATASETS	#
#
#	(1) CODE FOR VARIABLE LAGGING						#
#	(2) CODE FOR TDC DECOMPOSITION						#
#	(3) CONTINUOUS MULTILEVEL MODEL						#
#	(4) RESIDUAL PLOTS 									#


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
##	OUTCOME:		Desire (CONTINUOUS)			##
##
##	PREDICTORS:		Others (FACTOR)				##
##					Energy (CONTINUOUS) 		##
##
##	CONTROLS:		Day (Factor)				##
##					TimeInStudy (CONTINUOUS)	##
##					ID (RANDOM)					##



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



# CONTINUOUS MULTILEVEL MODEL #
ContinuousMLM = lmer(Desire~as.factor(Others)+Prior_Energy_between+Prior_Energy_within+as.factor(Day)+TimeInStudy+(1|ID),data= WorkshopMJData)
ContinuousMLM
summary(ContinuousMLM)


# HOW DOES IT LOOK? #

ContinuousMLMData = as.data.frame(cbind(fitted=fitted(ContinuousMLM),residuals=summary(ContinuousMLM)$resid,TimeInStudy= ContinuousMLM@frame$TimeInStudy,Energy_between=ContinuousMLM@frame$Prior_Energy_between))


# RESIDUAL PLOT #
pdf('ResidualPlot.pdf')
ggplot(ContinuousMLMData, aes(x= fitted,y= residuals)) +
	geom_point(col='grey45') + 
	geom_smooth(col='grey45') +
	ggtitle("Residual Plot for Continuous MLM") + 
	xlab("Predicted Values") + 
	ylab("Residuals") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))
dev.off()


# FITTED VALUES VERSUS STUDY TIME #
pdf('PromptPlot.pdf')
ggplot(ContinuousMLMData, aes(x= TimeInStudy,y= fitted)) +
	geom_point(col='grey45') + 
	geom_smooth(col='grey45') +
	ggtitle("Fitted Values over Study Time") + 
	xlab("Prompt Number") + 
	ylab("Predicted Value") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))
dev.off()


# FITTED VALUES VERSUS ENERGY BETWEEN #
pdf('EnergyBetweenPlot.pdf')
ggplot(ContinuousMLMData, aes(x= Energy_between,y= fitted)) +
	geom_point(col='grey45') + 
	geom_smooth(col='grey45') +
	ggtitle("Fitted Values by Energy Between") + 
	xlab("Energy Between") + 
	ylab("Predicted Value") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))
dev.off()





# SMK DATA #

## MODEL:										##
##	OUTCOME:		Cigarettes (CONTINUOUS)		##
##
##	PREDICTORS:		WithOthers (FACTOR)			##
##					Stress (CONTINUOUS) 		##
##
##	CONTROLS:		TimeInStudy (CONTINUOUS)	##
##					ID (RANDOM)					##



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



# CONTINUOUS MULTILEVEL MODEL #
ContinuousMLM = lmer(Cigarettes~as.factor(WithOthers)+Prior_Stress_between+Prior_Stress_within+TimeInStudy+(1|ID),data= WorkshopSMKData)
ContinuousMLM
summary(ContinuousMLM)


# HOW DOES IT LOOK? #

ContinuousMLMData = as.data.frame(cbind(fitted=fitted(ContinuousMLM),residuals=summary(ContinuousMLM)$resid,TimeInStudy= ContinuousMLM@frame$TimeInStudy,Stress_between=ContinuousMLM@frame$Prior_Stress_between))


# RESIDUAL PLOT #
pdf('ResidualPlot.pdf')
ggplot(ContinuousMLMData, aes(x= fitted,y= residuals)) +
	geom_point(col='grey45') + 
	geom_smooth(col='grey45') +
	ggtitle("Residual Plot for Continuous MLM") + 
	xlab("Predicted Values") + 
	ylab("Residuals") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))
dev.off()


# FITTED VALUES VERSUS STUDY TIME #
pdf('PromptPlot.pdf')
ggplot(ContinuousMLMData, aes(x= TimeInStudy,y= fitted)) +
	geom_point(col='grey45') + 
	geom_smooth(col='grey45') +
	ggtitle("Fitted Values over Study Time") + 
	xlab("Prompt Number") + 
	ylab("Predicted Value") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))
dev.off()


# FITTED VALUES VERSUS ENERGY BETWEEN #
pdf('StressBetweenPlot.pdf')
ggplot(ContinuousMLMData, aes(x= Stress_between,y= fitted)) +
	geom_point(col='grey45') + 
	geom_smooth(col='grey45') +
	ggtitle("Fitted Values by Stress Between") + 
	xlab("Stress Between") + 
	ylab("Predicted Value") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24), panel.background = element_rect(fill = "grey92"))
dev.off()




