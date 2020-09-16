* Master do-file for the project fdz1975 (The Effect of Immigration along the Distribution of Native Wages: Evidence from the 2010s in Germany)
* Ararat Gocmen
* Version: 2020-09-01
* master.do

* Use the Stata version currently available at the FDZ
version 14
clear

* Turn off -more- messages
set more off

* Set linesize
// Maximum linesize for Internal Use mode
//set linesize 120  
// Maximum linesize for Presentation/Publication mode
set linesize 255

* Stop execution to upload do-files for on-site use 
//STOP               

* Install(ed) packages
//net install abar
//net install carryforward
//net install estout
//net install pspline
//net install ranktest
//net install ivreg2
//net install xtivreg2
//net install xtabond2

* Execute do-files
// Clean data, restrict sample, and calculate full-time weights according to Fitzenberger & Seidlitz (2020) 
do ${prog}/d0_clean.do
// Perform descriptive analysis, calculate relative densities, and generate annual regional aggregates (at the Bundesland level) of the native and immigrant full-time populations
do ${prog}/d1_describe.do
// Estimate the impact of immigration along the native wage distribution using annual regional aggregates (at the Bundesland level)
do ${prog}/d2_regress.do

* Create log file
// Close the existing log file
capture log close
// Create a new log file
log using "$log/filelist.log", replace
// List the files in every directory
dir ${prog}/
dir ${log}/
dir ${data}/
dir ${orig}/
// Save the log file
log close
clear
