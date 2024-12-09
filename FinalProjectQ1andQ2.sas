*\ Final project STAT448 Fall 23*\;
*\ Data set was obtained from the UCI Machine Learning Repository:
https://archive.ics.uci.edu/dataset/501/beijing+multi+site+air+quality+data *\;
*\This code read air quality data from location Guanyuan as an example *\; 
*\This code read air quality data from location Guanyuan as an example *\; 

*Loaded data;
data AirQualityT;
	infile '/home/u63545327/sasuser.v94/Tiantan.csv' dsd missover firstobs=2;
	input Rowno $ No year month day hour PM25 PM10 SO2 NO2 CO O3 TEMP PRES DEWP RAIN wd $ WSPM station $;
*\ Drop variables you do not need*\;
    drop Rowno station;   
run;
data AirQualityT2016;
  set AirQualityT;
  where year = 2016 and hour=12;
run;
*Categorical variables: month, year, day, hour, wd

*Question 1;


data AirQuality;
  set AirQualityT; 
run;

/* OVERVIEW OF AIR POLLUTANTS */
proc means data=AirQualityT;
  var PM25 PM10 SO2 NO2 CO O3;
run;

/* OVERVIEW OF METEOROLOGICAL VARIABLES */
proc means data=AirQualityT;
  var TEMP PRES DEWP RAIN WSPM;
run;

/* PLOT FOR CONTINUOUS VARIABLES */
proc univariate data=AirQualityT;
  var PM25 PM10 SO2 NO2 CO O3 TEMP PRES DEWP RAIN WSPM;
  histogram / normal kernel;
run;

/* PLOT FOR WD */
proc freq data=AirQualityT;
  tables wd / plots=freqplot;
run;

/* VARIATION BY MONTH */
proc sgpanel data=AirQualityT;
  panelby month / layout=rowlattice columns=3 novarname;
  vbox TEMP PRES DEWP RAIN WSPM / category=month boxwidth=0.6;
run;

/* VARIATION BY MONTH FOR AIR POLLUTANTS */
proc sgpanel data=AirQualityT;
  panelby month / layout=rowlattice columns=3 no


/* MONTH CATEGORICAL VAR */

*Analyzing TEMP with month;
proc anova data=AirQualityT;
	class month;
	model TEMP = month;
	ods select OverallANOVA FitStatistics ModelANOVA;
run;
proc reg data=AirQualityT;
  model TEMP = month;
  output out=diagnostics cookd= cd;
run;
*Analyzing PRES with month;
proc anova data=AirQualityT;
	class month;
	model PRES = month;
	ods select OverallANOVA FitStatistics ModelANOVA;
run;
proc reg data=AirQualityT;
  model PRES = month;
  output out=diagnostics cookd= cd;
run;
*Analyzing PM25 with month;
proc anova data=AirQualityT;
	class month;
	model PM25 = month;
	ods select OverallANOVA FitStatistics ModelANOVA;
run;
proc reg data=AirQualityT;
  model PM25 = month;
  output out=diagnostics cookd= cd;
run;
*Analyzing PM10 with month;
proc anova data=AirQualityT;
	class month;
	model PM10 = month;
	ods select OverallANOVA FitStatistics ModelANOVA;
run;
proc reg data=AirQualityT;
  model PM10 = month;
  output out=diagnostics cookd= cd;
run;
*Analyzing SO2 with month;
proc anova data=AirQualityT;
	class month;
	model SO2 = month;
	ods select OverallANOVA FitStatistics ModelANOVA;
run;
proc reg data=AirQualityT;
  model SO2 = month;
  output out=diagnostics cookd= cd;
run;
*Analyzing NO2 with month;
proc anova data=AirQualityT;
	class month;
	model NO2 = month;
	ods select OverallANOVA FitStatistics ModelANOVA;
run;
proc reg data=AirQualityT;
  model NO2 = month;
  output out=diagnostics cookd= cd;
run;
*Analyzing CO with month;
proc anova data=AirQualityT;
	class month;
	model CO = month;
	ods select OverallANOVA FitStatistics ModelANOVA;
run;
proc reg data=AirQualityT;
  model CO = month;
  output out=diagnostics cookd= cd;
run;
*Analyzing O3 with month;
proc anova data=AirQualityT;
	class month;
	model O3 = month;
	ods select OverallANOVA FitStatistics ModelANOVA;
run;
proc reg data=AirQualityT;
  model O3 = month;
  output out=diagnostics cookd= cd;
run;
*Analyzing DEWP with month;
proc anova data=AirQualityT;
	class month;
	model DEWP = month;
	ods select OverallANOVA FitStatistics ModelANOVA;
run;
proc reg data=AirQualityT;
  model DEWP = month;
  output out=diagnostics cookd= cd;
run;
*Analyzing RAIN with month;
proc anova data=AirQualityT;
	class month;
	model RAIN = month;
	ods select OverallANOVA FitStatistics ModelANOVA;
run;
proc reg data=AirQualityT;
  model RAIN = month;
  output out=diagnostics cookd= cd;
run;
*Analyzing WSMP with month;
proc anova data=AirQualityT;
	class month;
	model WSMP = month;
	ods select OverallANOVA FitStatistics ModelANOVA;
run;
proc reg data=AirQualityT;
  model WSMP = month;
  output out=diagnostics cookd= cd;
run;

/* WD CATEGORICAL VAR; */
data AirQualityT_numeric;
    set AirQualityT;
    if wd="E" then wd_numeric=1;
	    else if wd="ENE" then wd_numeric=2;
	    else if wd="ESE" then wd_numeric=3;
	    else if wd="N" then wd_numeric=4;
	    else if wd="NE" then wd_numeric=5;
	    else if wd="NNE" then wd_numeric=6;
	    else if wd="NNW" then wd_numeric=7;
	    else if wd="NW" then wd_numeric=8;
	    else if wd="S" then wd_numeric=9;
	    else if wd="SE" then wd_numeric=10;
	    else if wd="SSE" then wd_numeric=11;
	    else if wd="SSW" then wd_numeric=12;
	    else if wd="SW" then wd_numeric=13;
	    else if wd="W" then wd_numeric=14;
	    else if wd="WNW" then wd_numeric=15;
	    else if wd="WSW" then wd_numeric=16;
	    else wd_numeric=.; /* set to missing if wd is not recognized */
run;
*Analyzing TEMP with wd;
proc anova data=AirQualityT;
	class wd;
	model TEMP = wd;
	ods select OverallANOVA FitStatistics ModelANOVA;
run;
proc reg data=AirQualityT_numeric;
  model TEMP = wd_numeric;
  output out=diagnostics cookd= cd;
run;
*Analyzing PRES with wd;
proc anova data=AirQualityT;
	class wd;
	model PRES = wd;
	ods select OverallANOVA FitStatistics ModelANOVA;
run;
proc reg data=AirQualityT_numeric;
  model PRES = wd_numeric;
  output out=diagnostics cookd= cd;
run;
*Analyzing PM25 with wd;
proc anova data=AirQualityT;
	class wd;
	model PM25 = wd;
	ods select OverallANOVA FitStatistics ModelANOVA;
run;
proc reg data=AirQualityT_numeric;
  model PM25 = wd_numeric;
  output out=diagnostics cookd= cd;
run;
*Analyzing PM10 with wd;
proc anova data=AirQualityT;
	class wd;
	model PM10 = wd;
	ods select OverallANOVA FitStatistics ModelANOVA;
run;
proc reg data=AirQualityT_numeric;
  model PM10 = wd_numeric;
  output out=diagnostics cookd= cd;
run;
*Analyzing SO2 with wd;
proc anova data=AirQualityT;
	class wd;
	model SO2 = wd;
	ods select OverallANOVA FitStatistics ModelANOVA;
run;
proc reg data=AirQualityT_numeric;
  model SO2 = wd_numeric;
  output out=diagnostics cookd= cd;
run;
*Analyzing NO2 with wd;
proc anova data=AirQualityT;
	class wd;
	model NO2 = wd;
	ods select OverallANOVA FitStatistics ModelANOVA;
run;
proc reg data=AirQualityT_numeric;
  model NO2 = wd_numeric;
  output out=diagnostics cookd= cd;
run;
*Analyzing CO with wd;
proc anova data=AirQualityT;
	class wd;
	model CO = wd;
	ods select OverallANOVA FitStatistics ModelANOVA;
run;
proc reg data=AirQualityT_numeric;
  model CO = wd_numeric;
  output out=diagnostics cookd= cd;
run;
*Analyzing O3 with wd;
proc anova data=AirQualityT;
	class wd;
	model O3 = wd;
	ods select OverallANOVA FitStatistics ModelANOVA;
run;
proc reg data=AirQualityT_numeric;
  model O3 = wd_numeric;
  output out=diagnostics cookd= cd;
run;
*Analyzing DEWP with wd;
proc anova data=AirQualityT;
	class wd;
	model DEWP = wd;
	ods select OverallANOVA FitStatistics ModelANOVA;
run;
proc reg data=AirQualityT_numeric;
  model DEWP = wd_numeric;
  output out=diagnostics cookd= cd;
run;
*Analyzing RAIN with wd;
proc anova data=AirQualityT;
	class wd;
	model RAIN = wd;
	ods select OverallANOVA FitStatistics ModelANOVA;
run;
proc reg data=AirQualityT_numeric;
  model RAIN = wd_numeric;
  output out=diagnostics cookd= cd;
run;
*Analyzing WSPM with wd;
proc anova data=AirQualityT;
	class wd;
	model WSMP = wd;
	ods select OverallANOVA FitStatistics ModelANOVA;
run;
proc reg data=AirQualityT_numeric;
  model WSPM = wd_numeric;
  output out=diagnostics cookd= cd;
run;

/* Year categorical variable */
*Analyzing TEMP with year;
proc anova data=AirQualityT;
	class year;
	model TEMP = year;
	ods select OverallANOVA FitStatistics ModelANOVA;
run;
proc reg data=AirQualityT;
  model TEMP = year;
  output out=diagnostics cookd= cd;
run;
*Analyzing PRES with year;
proc anova data=AirQualityT;
	class year;
	model PRES = year;
	ods select OverallANOVA FitStatistics ModelANOVA;
run;
proc reg data=AirQualityT;
  model PRES = year;
  output out=diagnostics cookd= cd;
run;
*Analyzing PM25 with year;
proc anova data=AirQualityT;
	class year;
	model PM25 = year;
	ods select OverallANOVA FitStatistics ModelANOVA;
run;
proc reg data=AirQualityT;
  model PM25 = year;
  output out=diagnostics cookd= cd;
run;
*Analyzing PM10 with year;
proc anova data=AirQualityT;
	class year;
	model PM10 = year;
	ods select OverallANOVA FitStatistics ModelANOVA;
run;
proc reg data=AirQualityT;
  model PM10 = year;
  output out=diagnostics cookd= cd;
run;
*Analyzing SO2 with year;
proc anova data=AirQualityT;
	class year;
	model SO2 = year;
	ods select OverallANOVA FitStatistics ModelANOVA;
run;
proc reg data=AirQualityT;
  model SO2 = year;
  output out=diagnostics cookd= cd;
run;
*Analyzing NO2 with year;
proc anova data=AirQualityT;
	class year;
	model NO2 = year;
	ods select OverallANOVA FitStatistics ModelANOVA;
run;
proc reg data=AirQualityT;
  model NO2 = year;
  output out=diagnostics cookd= cd;
run;
*Analyzing CO with year;
proc anova data=AirQualityT;
	class year;
	model CO = year;
	ods select OverallANOVA FitStatistics ModelANOVA;
run;
proc reg data=AirQualityT;
  model CO = year;
  output out=diagnostics cookd= cd;
run;
*Analyzing O3 with year;
proc anova data=AirQualityT;
	class year;
	model O3 = year;
	ods select OverallANOVA FitStatistics ModelANOVA;
run;
proc reg data=AirQualityT;
  model O3 = year;
  output out=diagnostics cookd= cd;
run;
*Analyzing DEWP with year;
proc anova data=AirQualityT;
	class year;
	model DEWP = year;
	ods select OverallANOVA FitStatistics ModelANOVA;
run;
proc reg data=AirQualityT;
  model DEWP = month;
  output out=diagnostics cookd= cd;
run;
*Analyzing RAIN with year;
proc anova data=AirQualityT;
	class month;
	model RAIN = year;
	ods select OverallANOVA FitStatistics ModelANOVA;
run;
proc reg data=AirQualityT;
  model RAIN = year;
  output out=diagnostics cookd= cd;
run;
*Analyzing WSMP with year;
proc anova data=AirQualityT;
	class year;
	model WSMP = year;
	ods select OverallANOVA FitStatistics ModelANOVA;
run;
proc reg data=AirQualityT;
  model WSMP = year;
  output out=diagnostics cookd= cd;
run;















*Question 2;

*correlation coefficients between each of the pollutants (PM2.5, PM10, SO2, etc.) 
and the meteorological variables (Temperature, Pressure, Dew Point, Rain, Wind Speed);
proc corr data=AirQualityT;
	var PM25 PM10 SO2 NO2 CO O3;
	with TEMP PRES DEWP RAIN WSPM;
run;

*model with significant interactions for PM25;
proc reg data=AirQualityT;
	model PM25 = TEMP DEWP RAIN WSPM;
run;
*check for multicollinearity;
proc reg data=AirQualityT;
	model PM25 = TEMP DEWP RAIN WSPM / VIF;
run;

*model with significant interactions for PM10;
proc reg data=AirQualityT;
	model PM10 = TEMP DEWP RAIN WSPM;
run;
*check for multicollinearity;
proc reg data=AirQualityT;
	model PM10 = TEMP DEWP RAIN WSPM / VIF;
run;

*model with significant interactions for SO2;
proc reg data=AirQualityT;
	model SO2 = TEMP DEWP RAIN WSPM;
run;
*check for multicollinearity;
proc reg data=AirQualityT;
	model SO2 = TEMP DEWP RAIN WSPM / VIF;
run;

*model with significant interactions for NO2;
proc reg data=AirQualityT;
	model NO2 = TEMP DEWP RAIN WSPM;
run;
*check for multicollinearity;
proc reg data=AirQualityT;
	model NO2 = TEMP DEWP RAIN WSPM / VIF;
run;

*model with significant interactions for CO;
proc reg data=AirQualityT;
	model CO = TEMP DEWP RAIN WSPM;
run;
*check for multicollinearity;
proc reg data=AirQualityT;
	model CO = TEMP DEWP RAIN WSPM / VIF;
run;

*model with significant interactions for O3;
proc reg data=AirQualityT;
	model O3 = TEMP DEWP RAIN WSPM;
run;
*check for multicollinearity;
proc reg data=AirQualityT;
	model O3 = TEMP DEWP RAIN WSPM / VIF;
run;