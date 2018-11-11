/* 2018 APPLIED PUBLIC HEALTH STATISTICS BREAKFAST WORKSHOP 	*/
/*	SAS EXAMPLE 4: LOGISTIC MULTILEVEL MODELING OF ILD	*/


/* SUMMER G. FRANK-PEARCE & TRENT L. LALONDE */



/* SAS FILE FOR MULTILEVEL LOGISTIC MODELING OF ILD */


/* CONTENTS:							*/
/*	ALL ANALYSES REPEATED FOR BOTH MJ AND SMK DATASETS	*/
/*
/*	(1) CODE FOR TDC DECOMPOSITION				*/
/*	(2) LOGISTIC MULTILEVEL MODEL				*/
/*	(3) ODDS RATIOS 					*/


/* READ IN THE DATA */


/* MJ DATA */

/* THE FIRST GROUP OF COMMANDS RE-FORMATS DATA FROM R (USING 'NA' FOR MISSING VALUES) TO DATA FOR SAS (USING ' ' FOR MISSING) */
/* NOT NECESSARY IF YOU ALREADY HAVE 'BLANKS' FOR MISSING VALUES */

data WorkshopData;
   infile 'C:\Users\trent.lalonde\Desktop\WorkshopMJData.csv' dsd truncover ;
   file 'SASWorkshopMJ.csv' dsd ;
   length word $200 ;
   do i=1 to 17;
      input word @;
      if word='NA' then word=' ';
      put word @;
   end;
   put;
run;

proc import datafile='SASWorkshopMJ.csv'
	out= WorkshopMJData
	dbms=csv replace;
	getnames=yes;
run;



/* SMK DATA */

/* THE FIRST GROUP OF COMMANDS RE-FORMATS DATA FROM R (USING 'NA' FOR MISSING VALUES) TO DATA FOR SAS (USING ' ' FOR MISSING) */
/* NOT NECESSARY IF YOU ALREADY HAVE 'BLANKS' FOR MISSING VALUES */

data WorkshopData;
   infile 'C:\Users\trent.lalonde\Desktop\WorkshopSMKData.csv' dsd truncover ;
   file 'SASWorkshopSMK.csv' dsd ;
   length word $200 ;
   do i=1 to 13;
      input word @;
      if word='NA' then word=' ';
      put word @;
   end;
   put;
run;

proc import datafile='SASWorkshopSMK.csv'
	out= WorkshopSMKData
	dbms=csv replace;
	getnames=yes;
run;


title '';



/* MJ DATA */

/* MODEL:								*/
/*	OUTCOME:		Used (BINARY)				*/
/*
/*	PREDICTORS:		Others (FACTOR)				*/
/*					Energy (CONTINUOUS) 		*/
/*
/*	CONTROLS:		Day (Factor)				*/
/*					TimeInStudy (CONTINUOUS)	*/
/*					ID (RANDOM)			*/


title '';


/* WITH DECOMPOSITION OF ENERGY */
proc sql;
	create table MJDataTDC as
	select ID as ID,
        	Prior_Energy as Prior_Energy,
        	avg(Prior_Energy) as Prior_Energy_between,
        	Prior_Energy-avg(Prior_Energy) as Prior_Energy_within from WorkshopMJData group by ID;
quit;

data WorkshopMJData;
	set MJDataTDC;
	set WorkshopMJData;
run;


proc freq data=WorkshopMJData;
	tables Used*ID;
run;


proc sort data=WorkshopMJData;
	by descending Used descending Day;
run;


/* LOGISTIC MULTILEVEL MODEL */

/* DEFAULT: DOES NOT CONVERGE */
proc glimmix data=WorkshopMJData order=data method=Laplace;
	class Others (ref=first) Day ID;
	model Used(event='1') = Others Prior_Energy_between Prior_Energy_within Day TimeInStudy / s distribution=binomial link=logit oddsratio;
	random Int / subject=ID;
	output out = LogisticMLM resid=residual pred = pred;
run;

/* LAPLACE ESIMATION CONVERGES */
proc glimmix data=WorkshopMJData order=data method=Laplace;
	class Others (ref=first) Day ID;
	model Used(event='1') = Others Prior_Energy_between Prior_Energy_within Day TimeInStudy / s distribution=binomial link=logit oddsratio;
	random Int / subject=ID;
	output out = LogisticMLM resid=residual pred = pred;
run;






/* SMK DATA */

/* MODEL:							*/
/*	OUTCOME:		Used (BINARY)			*/
/*
/*	PREDICTORS:		WithOthers (FACTOR)		*/
/*				Stress (CONTINUOUS) 		*/
/*
/*	CONTROLS:		TimeInStudy (CONTINUOUS)	*/
/*				ID (RANDOM)			*/


title '';


/* WITH DECOMPOSITION OF STRESS */
proc sql;
	create table SMKDataTDC as
	select ID as ID,
        	Prior_Stress as Prior_Stress,
        	avg(Prior_Stress) as Prior_Stress_between,
        	Prior_Stress-avg(Prior_Stress) as Prior_Stress_within from WorkshopSMKData group by ID;
quit;

data WorkshopSMKData;
	set SMKDataTDC;
	set WorkshopSMKData;
run;


data WorkshopSMKData;
	set WorkshopSMKData;
	Used = 0;
	if Cigarettes > 0 then Used = 1;
run;


proc freq data=WorkshopSMKData;
	tables Used*ID;
run;


proc sort data=WorkshoSMKData;
	by descending Used;
run;


/* LOGISTIC MULTILEVEL MODEL */
proc glimmix data=WorkshopSMKData order=data;
	class WithOthers ID;
	model Used(event='1') = WithOthers Prior_Stress_between Prior_Stress_within TimeInStudy / s distribution=binomial link=logit oddsratio;
	random Int / subject=ID;
	output out = LogisticMLM resid=residual pred = pred;
run;



quit;







