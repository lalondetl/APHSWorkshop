## 2018 APPLIED PUBLIC HEALTH STATISTICS BREAKFAST WORKSHOP ##
##	R EXAMPLE 2: DESCRIPTIVES AND VISUALIZATION OF ILD	##


# SUMMER FRANK-PEARCE & TRENT L. LALONDE #



# R FILE FOR VISUALIZATION AND EXPLORATION OF ILD #


# CONTENTS:												#
#	ALL ANALYSES REPEATED FOR BOTH MJ AND SMK DATASETS	#
#
#	(1) DATA SUMMARY AND TABLES							#
#	(2) AGGREGATED SUMMARY STATISTICS					#
#	(3) BOX PLOTS FOR RAW DATA							#
#	(4) AGGREGATED SUMMARY STATISTICS					#
#	(5) HISTOGRAMS AND BOX PLOTS OF AGGREGATES			#
#	(6) TIME PLOTS										#
#	(7) SPAGHETTI PLOTS									#
#	(8)	VARIOGRAMS										#

library(ggplot2)


# READ IN THE DATA #

# MJ DATA #
setwd('/Users/trent.lalonde/Documents/Research/Presentations/APHA/APHA ILD Workshop/Software/Data/')
WorkshopMJData = read.csv('WorkshopMJData.csv')

# SMK DATA #
setwd('/Users/trent.lalonde/Documents/Research/Presentations/APHA/APHA ILD Workshop/Software/Data/')
WorkshopSMKData = read.csv('WorkshopSMKData.csv')


## BASIC EXPLORATION OF DATA ##

# MJ DATA #
dim(WorkshopMJData)
summary(WorkshopMJData)

table(WorkshopMJData$Used,useNA='ifany')
table(WorkshopMJData$Others,useNA='ifany')

# SMK DATA #
dim(WorkshopSMKData)
summary(WorkshopSMKData)

table(WorkshopSMKData$WithOthers,useNA='ifany')
table(WorkshopSMKData$Stress,useNA='ifany')




## MEANS AND VARIANCES BY RELEVANT TIME (DAY OF WEEK) ##

# MJ DATA #
(DailyFrequencyMeans = aggregate(Frequency~Day,data=WorkshopMJData,FUN=function(x)mean(x,na.rm=TRUE)))
(DailyFrequencyVariances = aggregate(Frequency~Day,data=WorkshopMJData,FUN=function(x)var(x,na.rm=TRUE)))


# SMK DATA #
(DailyCigaretteMeans = aggregate(Cigarettes~TimeInStudy,data=WorkshopSMKData,FUN=function(x)mean(x,na.rm=TRUE)))
(DailyCigaretteVariances = aggregate(Cigarettes~TimeInStudy,data=WorkshopSMKData,FUN=function(x)var(x,na.rm=TRUE)))




# BOXPLOTS BY DAY OF WEEK #


# MJ DATA #
# RE-ORDER DAYS #
WorkshopMJData$Day = factor(WorkshopMJData$Day, levels=levels(WorkshopMJData$Day)[c(2,6,7,5,1,3,4)])

png('BoxplotFrequencyByDay.png')
ggplot(WorkshopMJData, aes(x=as.factor(Day),y=Frequency)) + 
	geom_boxplot(notch=TRUE,outlier.colour='grey45',fill='grey45') +
	stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
	ggtitle("Box Plots of Frequency by Day") + 
	xlab("Day") + 
	ylab("Frequency") +
	theme(axis.text=element_text(size=12), axis.title=element_text(size=20), plot.title=element_text(size=24),legend.position="none")
dev.off()


# SMK DATA #
pdf('BoxplotCigarettesByDay.pdf')
ggplot(WorkshopSMKData, aes(x=as.factor(TimeInStudy),y=Cigarettes)) + 
	geom_boxplot(notch=TRUE,outlier.colour='grey45',fill='grey45') +
	stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
	ggtitle("Box Plots of Cigarettes by Time") + 
	xlab("Time") + 
	ylab("Cigarettes") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24),legend.position="none")
dev.off()




## DISTRIBUTIONS OF SUMMARY STATISTICS AMONG SUBJECTS ##

# SOMETIMES USE MEANS #

# MJ DATA #

(SubjectFrequencyMeans = aggregate(Frequency~ID,data=WorkshopMJData,FUN=function(x)mean(x,na.rm=TRUE)))

fivenum(SubjectFrequencyMeans$Frequency)

pdf('HistogramFrequency.pdf')
ggplot(SubjectFrequencyMeans, aes(Frequency)) + 
	geom_histogram(aes(y=..density..),col='grey45') + 
	geom_density() +
	ggtitle("Histogram of Frequency Means") + 
	xlab("Frequency of Use") + 
	ylab("Frequency") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24))
dev.off()

pdf('BoxPlotFrequency.pdf')
ggplot(SubjectFrequencyMeans, aes(x=1,y=Frequency)) + 
	geom_boxplot(notch=TRUE,outlier.colour='grey45',fill='grey45') +
	stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
	ggtitle("Box Plot of Frequency Means") + 
	xlab("Frequency Mean Box Plot") + 
	ylab("Frequency Mean") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24),legend.position="none")
dev.off()



# SMK DATA #

(SubjectCigaretteMeans = aggregate(Cigarettes~ID,data=WorkshopSMKData,FUN=function(x)mean(x,na.rm=TRUE)))

fivenum(SubjectCigaretteMeans$Cigarettes)

pdf('HistogramCigarettes.pdf')
ggplot(SubjectCigaretteMeans, aes(Cigarettes)) + 
	geom_histogram(aes(y=..density..),col='grey45') + 
	geom_density() +
	ggtitle("Histogram of Cigarette Means") + 
	xlab("Mean of Cigarettes") + 
	ylab("Cigarettes") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24))
dev.off()

pdf('BoxPlotCigarettes.pdf')
ggplot(SubjectCigaretteMeans, aes(x=1,y=Cigarettes)) + 
	geom_boxplot(notch=TRUE,outlier.colour='grey45',fill='grey45') +
	stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
	ggtitle("Box Plot of Cigarette Means") + 
	xlab("Cigarette Mean Box Plot") + 
	ylab("Cigarette Mean") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24),legend.position="none")
dev.off()





# SOMETIMES USE TOTALS #

# MJ DATA #

(SubjectUseTotals = aggregate(Used~ID,data=WorkshopMJData,FUN=function(x)sum(x,na.rm=TRUE)))

fivenum(SubjectUseTotals$Used)

pdf('HistogramUse.pdf')
ggplot(SubjectUseTotals, aes(Used)) + 
	geom_histogram(aes(y=..density..),col='grey45') + 
	geom_density() +
	ggtitle("Histogram of Total Use") + 
	xlab("Use") + 
	ylab("Frequency") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24))
dev.off()

pdf('BoxplotUse.pdf')
ggplot(SubjectUseTotals, aes(x=1,y=Used)) + 
	geom_boxplot(notch=TRUE,outlier.colour='grey45',fill='grey45') +
	stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
	ggtitle("Box Plot of Total Use") + 
	xlab("Total Use Box Plot") + 
	ylab("Total Use") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24),legend.position="none")
dev.off()



# MJ DATA #

(SubjectCigTotals = aggregate(Cigarettes~ID,data=WorkshopSMKData,FUN=function(x)sum(x,na.rm=TRUE)))

fivenum(SubjectCigTotals$Cigarettes)

pdf('HistogramCig.pdf')
ggplot(SubjectCigTotals, aes(Cigarettes)) + 
	geom_histogram(aes(y=..density..),col='grey45') + 
	geom_density() +
	ggtitle("Histogram of Total Cigarettes") + 
	xlab("Cigarettes") + 
	ylab("Frequency") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24))
dev.off()

pdf('BoxplotCig.pdf')
ggplot(SubjectCigTotals, aes(x=1,y=Cigarettes)) + 
	geom_boxplot(notch=TRUE,outlier.colour='grey45',fill='grey45') +
	stat_summary(fun.y=mean, geom="point", shape=18, size=5) +
	ggtitle("Box Plot of Total Cigarettes") + 
	xlab("Total Cigarettes Box Plot") + 
	ylab("Total Cigarettes") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24),legend.position="none")
dev.off()





# SOMETIMES COMPARE #

(UseData = merge(SubjectFrequencyMeans, SubjectUseTotals,by.x='ID',by.y='ID'))

pdf('ScatterPlotUse.pdf')
ggplot(UseData, aes(x=Used,y=Frequency)) +
	geom_point(col='grey45') + 
	geom_smooth(col='grey45') +
	ggtitle("Scatter Plot of Frequency Means Versus Total Use") + 
	xlab("Total Use") + 
	ylab("Mean Frequency") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=18))
dev.off()




## PLOTS USING RAW DATA ##


# TIME PLOT #

# MJ DATA #

pdf('ScatterPlotFrequency.pdf')
ggplot(WorkshopMJData, aes(x=TimeInStudy,y=Frequency)) +
	geom_point(col='grey45') + 
	geom_smooth(col='grey45') +
	ggtitle("Scatter Plot of Frequency Versus Time") + 
	xlab("Study Prompt") + 
	ylab("Frequency") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24))
dev.off()


# SMK DATA #

pdf('ScatterPlotCigarettes.pdf')
ggplot(WorkshopSMKData, aes(x=TimeInStudy,y=Cigarettes)) +
	geom_point(col='grey45') + 
	geom_smooth(col='grey45') +
	ggtitle("Scatter Plot of Cigarettes Versus Time") + 
	xlab("Study Prompt") + 
	ylab("Cigarettes") +
	theme(axis.text=element_text(size=16), axis.title=element_text(size=20), plot.title=element_text(size=24))
dev.off()



# SPAGHETTI PLOT #

# MJ DATA #
interaction.plot(x.factor=as.factor(WorkshopMJData$TimeInStudy),trace.factor=as.factor(WorkshopMJData$ID),response= WorkshopMJData$Frequency,fun=function(x)mean(x,na.rm=TRUE))


# SMK DATA #
interaction.plot(x.factor=as.factor(WorkshopSMKData$TimeInStudy),trace.factor=as.factor(WorkshopSMKData$ID),response= WorkshopSMKData$Cigarettes,fun=function(x)mean(x,na.rm=TRUE))



# VARIOGRAM #

library(joineR)
?plot









