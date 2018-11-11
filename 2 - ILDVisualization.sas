/* 2018 APPLIED PUBLIC HEALTH STATISTICS BREAKFAST WORKSHOP 	*/
/*	SAS EXAMPLE 2: DESCRIPTIVES AND VISUALIZATION OF ILD	*/


/* SUMMER G. FRANK-PEARCE & TRENT L. LALONDE */



/* SAS FILE FOR VISUALIZATION AND EXPLORATION OF ILD */


/* CONTENTS:							*/
/*	ALL ANALYSES REPEATED FOR BOTH MJ AND SMK DATASETS	*/
/*
/*	(1) DATA SUMMARY AND TABLES				*/
/*	(2) AGGREGATED SUMMARY STATISTICS			*/
/*	(3) BOX PLOTS FOR RAW DATA				*/
/*	(4) AGGREGATED SUMMARY STATISTICS			*/
/*	(5) HISTOGRAMS AND BOX PLOTS OF AGGREGATES		*/
/*	(6) TIME PLOTS						*/
/*	(7) SPAGHETTI PLOTS					*/
/*	(8)	VARIOGRAMS					*/



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


/* BASIC EXPLORATION OF DATA */

/* MJ DATA */
proc contents data=WorkshopMJData;
run;

proc freq data=WorkshopMJData;
	tables Used Others;
run;


/* SMK DATA */
proc contents data=WorkshopSMKData;
run;

proc freq data=WorkshopSMKData;
	tables WithOthers Stress;
run;




/* MEANS AND VARIANCES BY RELEVANT TIME (DAY OF WEEK) */


/* MJ DATA */
proc means data=WorkshopMJData;
	class Day;
	var Frequency;
run;


/* SMK DATA */
proc means data=WorkshopSMKData;
	class TimeInStudy;
	var Cigarettes;
run;



/* BOXPLOTS BY DAY OF WEEK */
proc sgplot data=WorkshopMJData;
	vbox Frequency / category = Day;
run;



/* DISTRIBUTIONS OF SUMMARY STATISTICS AMONG SUBJECTS */

/* SOMETIMES USE MEANS */


/* MJ DATA */
proc means data=WorkshopMJData;
	class ID;
	var Frequency;
	output out=MeansOut mean=FrequencyMean;
run;

/* HISTOGRAM AND BOX PLOT */
proc sgplot data=MeansOut;
	histogram FrequencyMean;
run;

proc sgplot data=MeansOut;
	vbox FrequencyMean;
run;



/* SMK DATA */
proc means data=WorkshopSMKData;
	class ID;
	var Cigarettes;
	output out=CMeansOut mean=CigaretteMean;
run;

/* HISTOGRAM AND BOX PLOT */
proc sgplot data=CMeansOut;
	histogram CigaretteMean;
run;

proc sgplot data=CMeansOut;
	vbox CigaretteMean;
run;




/* SOMETIMES USE TOTALS */


/* MJ DATA */
proc means data=WorkshopMJData sum;
	class ID;
	var Frequency;
	output out=TotalsOut sum=FrequencySum;
run;

/* HISTOGRAM AND BOX PLOT */
proc sgplot data=TotalsOut;
	histogram FrequencySum;
run;

proc sgplot data=TotalsOut;
	vbox FrequencySum;
run;


/* SMK DATA */
proc means data=WorkshopSMKData sum;
	class ID;
	var Cigarettes;
	output out=CTotalsOut sum=CigaretteSum;
run;

/* HISTOGRAM AND BOX PLOT */
proc sgplot data= CTotalsOut;
	histogram CigaretteSum;
run;

proc sgplot data= CTotalsOut;
	vbox CigaretteSum;
run;





/* PLOTS USING RAW DATA */


/* MJ DATA */

/* TIME PLOT */
proc sgplot data=WorkshopMJData;
	scatter y=Frequency x=TimeInStudy;
	title 'Time plot';
run;

/* SPAGHETTI PLOT */
proc sgplot data=WorkshopMJData;
	series y=Frequency x=TimeInStudy / group=ID;
	title 'Spaghetti plot';
run;



/* SMK DATA */

/* TIME PLOT */
proc sgplot data=WorkshopSMKData;
	scatter y=Cigarettes x=TimeInStudy;
	title 'Time plot';
run;

/* SPAGHETTI PLOT */
proc sgplot data=WorkshopSMKData;
	series y=Cigarettes x=TimeInStudy / group=ID;
	title 'Spaghetti plot';
run;





/* VARIOGRAM */
/* MUST ELIMINATE MISSING VALUES */
proc glm data=WorkshopMJData noprint;
	class Day;
	model Frequency = Day TimeInStudy;
	output out=newMJdata r=resid;
run;

proc variogram data=newMJdata plots=(SEMIVAR(CLA));	
	title "Variogram";
	compute lagd=2 maxlag=20;
	coordinates xc=TimeInStudy yc=resid;
	var resid;
run;



quit;

