/* 2018 APPLIED PUBLIC HEALTH STATISTICS BREAKFAST WORKSHOP 	*/
/*	SAS EXAMPLE 3: CONTINUOUS MULTILEVEL MODELING OF ILD	*/


/* SUMMER G. FRANK-PEARCE & TRENT L. LALONDE */



/* SAS FILE FOR CONTINUOUS MULTILEVEL MODELING OF ILD */



/* CONTENTS:							*/
/*	ALL ANALYSES REPEATED FOR BOTH MJ AND SMK DATASETS	*/
/*
/*	(1) CODE FOR TDC DECOMPOSITION				*/
/*	(2) CONTINUOUS MULTILEVEL MODEL				*/
/*	(3) RESIDUAL PLOTS 					*/



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
/*	OUTCOME:		Desire (CONTINUOUS)			*/
/*
/*	PREDICTORS:		Others (FACTOR)				*/
/*					Energy (CONTINUOUS) 		*/
/*
/*	CONTROLS:		Day (Factor)				*/
/*					TimeInStudy (CONTINUOUS)	*/
/*					ID (RANDOM)			*/


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


title '';


/* CONTINUOUS MULTILEVEL MODEL */
proc mixed data=WorkshopMJData;
	class Others Day ID;
	model Desire = Others Prior_Energy_between Prior_Energy_within Day TimeInStudy / ddfm=SATTERTHWAITE solution residual outpred=RIModel;
	random intercept / subject=ID type=un;
	ods output Fitstatistics=FS_RIModel SolutionF=SF_RIModel;
	ods output CovParms=CovRIModel;
run;



/* HOW DOES IT LOOK? */

/* RESIDUAL PLOT */
proc sgplot data=RIModel;
	scatter y=resid x=PRED;
run;


/* FITTED VALUES VERSUS STUDY TIME */
proc sgplot data=RIModel;
	scatter y=PRED x=TimeInStudy;
run;


/* FITTED VALUES VERSUS ENERGY BETWEEN */
proc sgplot data=RIModel;
	scatter y=PRED x=Prior_Energy_between;
run;







/* SMK DATA */

/* MODEL:							*/
/*	OUTCOME:		Cigarettes (CONTINUOUS)		*/
/*
/*	PREDICTORS:		WithOthers (FACTOR)		*/
/*				Stress (CONTINUOUS) 		*/
/*
/*	CONTROLS:		TimeInStudy (CONTINUOUS)	*/
/*				ID (RANDOM)			*/


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


title '';


/* CONTINUOUS MULTILEVEL MODEL */
proc mixed data=WorkshopSMKData;
	class WithOthers ID;
	model Cigarettes = WithOthers Prior_Stress_between Prior_Stress_within TimeInStudy / ddfm=SATTERTHWAITE solution residual outpred=RIModel;
	random intercept / subject=ID type=un;
	ods output Fitstatistics=FS_RIModel SolutionF=SF_RIModel;
	ods output CovParms=CovRIModel;
run;



/* HOW DOES IT LOOK? */

/* RESIDUAL PLOT */
proc sgplot data=RIModel;
	scatter y=resid x=PRED;
run;


/* FITTED VALUES VERSUS STUDY TIME */
proc sgplot data=RIModel;
	scatter y=PRED x=TimeInStudy;
run;


/* FITTED VALUES VERSUS STRESS BETWEEN */
proc sgplot data=RIModel;
	scatter y=PRED x=Prior_Stress_between;
run;



quit;


