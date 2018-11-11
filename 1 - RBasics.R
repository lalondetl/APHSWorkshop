## 2018 APPLIED PUBLIC HEALTH STATISTICS BREAKFAST WORKSHOP ##
##	R EXAMPLE 1: BASICS OF READING AND ACCESSING DATA	##


# SUMMER G. FRANK-PEARCE & TRENT L. LALONDE #



## R FILE FOR BASIC CALCULATIONS AND DATA MANIPULATION ##

# DOWNLOAD FROM www.r-project.org #
# HELP DOCUMENTATION AVAILABLE FROM www.r-project.org #



## SET WORKING DIRECTORY ##
## NOTE: FOR WINDOWS MACHINES, SWITCH ALL '/' TO '\' OR '//' ##
setwd('/PATH-TO-FOLDER/')



## TRY CLOSING R ##
## WORKSPACE IMAGE: R WILL RECORD ALL NAMED OBJECTS AND	##
##	MAKE THEM AVAILABLE WHEN R IS STARTED AGAIN	##

## CAN ALSO MANUALLY SAVE ALL OBJECTS USING HISTORY ##

savehistory(file='NAME.Rhistory') # TO WORKING DIRECTORY #

loadhistory(file='NAME.Rhistory') # FROM WORKING DIRECTORY #




## READING IN DATA: TEXT FILES ##
?read.table

## header: COLUMN LABELS ##
## na.strings: SYMBOL FOR MISSING VALUES ##
TextData = read.table('FILE-NAME.txt',header=TRUE,na.strings='.')

## READING IN DATA: CSV FILES ##
?read.csv
?read.csv2


## READING IN DATA: EXCEL FILES ##
library(xlsx)
?read.xlsx


## READING IN DATA: SAS (.sas7bdat) FILES ##
library(sas7bdat)
?read.sas7bdat

library(haven)
?read_sas




## READING IN DATA: SPSS (.sav) FILES ##
library(foreign)
?read.spss



## SAVING DATA ##
?write.table
?write.csv










