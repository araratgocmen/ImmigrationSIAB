* Clean data, restrict sample, and calculate full-time weights according to Fitzenberger & Seidlitz (2020)
* Ararat Gocmen
* Version: 2020-09-01
* d0_clean.do

* Create log file
// Close the existing log file
capture log close
// Create a new log file
log using ${log}/d0_clean.log, replace

* Load data and drop unused variables
// Load the individual file
use ${orig}/siab_7517_v1.dta, clear
// Switch to English labels
label language en
// Only look at the Beschäftigtenhistorik (BeH)
keep if quelle == 1
// Drop variables that are unavailable for the BeH
drop famst kind estatvor estatnach profil art_kuend arbzeit restanspruch traeger 
 
* Define new variables
// Year
qui gen jahr = year(begepi)
label variable jahr "Year"
// Age
qui gen age = jahr - gebjahr
label variable age "Age"
// German citizenship
qui gen deutsch = nation == 0
label variable deutsch "German citizenship"
label define deutsch 0 "Immigrant" 1 "Native", replace
label values deutsch deutsch
// Continent of citizenship
qui gen continent = real(substr(string(nation), 1, 1)) if deutsch == 0
label variable continent "Region of foreign citizenship"
label define continent 1 "Europe" 2 "Africa" 3 "Americas" 4 "Middle East & Asia" 5 "Oceania" 9 "Other", replace
label values continent continent
// Working
qui gen zustand = inlist(erwstat, 101, 140, 143)
label variable zustand "Subject to social security"
label define zustand 0 "No" 1 "Yes", replace
label values zustand zustand
// Marginal part-time
qui gen marginal = inlist(erwstat, 109, 209)
label variable marginal "Marginal part-time"
label define marginal 0 "No" 1 "Yes", replace
label values marginal marginal

* Merge establishment data and define new variables
merge m:1 betnr jahr using ${orig}/siab_7517_v1_bhp_basis_v1.dta, keepusing(w08_3_gen ao_bula) keep(match master)
label variable w08_3_gen "Classification of economic activities 08, groups completed by extrapolation/imputation"
label variable ao_bula "Bundesland of work"
label values ao_bula wo_bula_en
drop _merge
// West Germany
qui gen west = inrange(ao_bula, 1, 10)
label variable west "West Germany"
label define west 0 "No" 1 "Yes", replace
label values west west
// East Germany
qui gen ost = inrange(ao_bula, 11, 16)
label variable ost "East Germany"
label define ost 0 "No" 1 "Yes", replace
label values ost ost
// Mining
qui gen knapp = inrange(w08_3_gen, 11, 32) | inrange(w08_3_gen, 51, 99) | inrange(w08_3_gen, 351, 353) | inrange(w08_3_gen, 360, 390)
label variable knapp "Mining"
label define knapp 0 "No" 1 "Yes", replace
label values knapp knapp
tab knapp

* Restrict sample based on year and age
// Drop data from before 2000
drop if jahr < 2000
// Keep only individuals between 25- and 55-years-old
keep if inrange(age, 25, 55)

* Account for basic missing data
// Standardize all missing values
foreach x of varlist betnr nation ausbildung ausbildung_imp schule tentgelt beruf beruf2010_3 niveau teilzeit stib erwstat gleitz leih befrist grund alo_beg alo_dau wo_bula wo_rd w08_3_gen ao_bula {
	qui replace `x' = . if `x' == .z
	qui replace `x' = . if `x' == .n
}
// Set earnings to zero if they are missing
replace tentgelt = 0 if tentgelt == .
// Drop records for which the employment status is missing
drop if erwstat == .

* Account for separate reporting of one-time payments starting in 2013
// Indicate one-time payments
qui gen onetime = 0
qui replace onetime = 1 if grund == 54
// Indicate separate one-time payments
sort persnr betnr begepi endepi onetime
qui by persnr betnr: gen sonetime = 1 if begepi == begepi[_n - 1] & endepi == endepi[_n - 1] & jahr >= 2013 & onetime == 1
// Indicate simultaneous spells at the same employer
qui by persnr betnr: replace sonetime = 0 if begepi == begepi[_n + 1] & endepi == endepi[_n + 1] & jahr >= 2013 & sonetime[_n + 1] == 1
// Add separate one-time payments to wages
qui by persnr betnr begepi endepi: egen sonetimetentgelt = total(tentgelt) if jahr >= 2013 & sonetime != .
qui replace tentgelt = sonetimetentgelt if jahr >= 2013 & sonetime != .
drop if sonetime == 1
drop onetime sonetime sonetimetentgelt

* Account for overlapping spells
// Indicate overlapping spells
sort persnr begepi endepi spell
qui by persnr: gen indi = 1 if begepi == begepi[_n + 1] & endepi == endepi[_n + 1]
// Number overlapping spells
qui by persnr: replace indi = indi[_n - 1] + 1 if begepi == begepi[_n - 1] & endepi == endepi[_n - 1]
tab indi if indi != 1, miss
// Indicate each instance of overlap
qui gen indi_help = 1 if indi == 1
// Number instances of overlap
sort persnr indi_help begepi endepi
qui by persnr: replace indi_help = indi_help[_n - 1] + 1 if _n > 1 & indi == 1
sort persnr begepi endepi indi_help
qui by persnr: replace indi_help = indi_help[_n - 1] if indi_help[_n - 1] != . & indi_help == . & indi != .
sort persnr begepi endepi indi
// Prioritize spells subject to social security with positive earnings
qui gen prio = 0 if indi_help != .
qui replace prio = 1 if indi_help != . & zustand == 1 & tentgelt > 0
// Choose overlapping spell with the highest priority and highest earnings
sort persnr begepi endepi indi_help prio tentgelt indi
qui by persnr begepi endepi indi_help: gen drop_var = 1 if _n < _N
tab indi drop_var
drop if drop_var == 1
drop indi indi_help prio drop_var
// Verify no remaining overlaps
sort persnr begepi endepi
qui by persnr: replace spell = _n
qui by persnr: gen indi = 1 if begepi == begepi[_n + 1] & endepi == endepi[_n + 1]
qui by persnr: replace indi = indi[_n - 1] + 1 if begepi == begepi[_n - 1] & endepi == endepi[_n - 1]
tab indi
drop indi
sort persnr spell

* Account for missing regional data
// Indicate missing regional data
recode ao_bula (. = 0)
tab ao_bula
tab west ost, miss
// Indicate whether people have ever worked in either region
foreach x in west ost {
        qui by persnr: egen t`x' = total(`x')
        qui recode t`x' (1/max = 1)
}
// Fill missing regions for those who have only ever worked in one
replace ost = 1 if tost == 1 & twest == 0
replace west = 1 if tost == 0 & twest == 1
// Interpolate between same-region spells, but otherwise do not fill missing values
qui gen a = spell if west == 1 | ost == 1
qui by persnr: egen lort = max(a)
sort persnr spell
by persnr: replace ost = 1 if west[_n] == 0 & ost[_n - 1] == 1
by persnr: replace west = 1 if ost[_n] == 0 & west[_n - 1] == 1
replace ost = 0 if ao_bula == . & spell > lort
replace west = 0 if ao_bula == . & spell > lort
qui by persnr: replace ost = -1 if ao_bula[_n] == . & west[_n + 1] == 1 & ost[_n] == 1
qui by persnr: replace west = -1 if ao_bula[_n]== . & ost[_n + 1] == 1 & west[_n] == 1
gsort persnr -spell
qui by persnr: replace ost = -1 if ao_bula[_n] == . & ost[_n - 1] == -1
qui by persnr: replace west = -1 if ao_bula[_n] == . & west[_n - 1] == -1
recode west ost (-1 = 0)
// Assume West Germany if the region is still missing
replace west = 1 if ost == 0
// Count records still missing regional data
tab west ost, miss
drop tost twest a lort

* Account for earnings ceilings (Beitragsbemessungsgrenze) and floors (Geringfügigkeitsgrenze) for social security contributions, based on published code from Fitzenberger & Seidlitz (2020) and on https://de.wikipedia.org/wiki/Beitragsbemessungsgrenze
// Define average month of spell and number of months between it and January 1960
qui gen monat = month((endepi + begepi) / 2)
label variable monat "Month"
qui gen smonat = (jahr - 1975) * 12 + 180 + monat - 1
format %tm smonat
sort persnr smonat
save ${data}/0_clean.dta, replace
// Create a temporary file to address earnings ceilings and floors
tempfile tag_entg
keep persnr smonat jahr tentgelt knapp west ost
// Define region- and mining-specific ceilings and floors
qui gen rm_bbg_west = .
qui gen rm_bbg2_west = .
qui gen rm_gfg_west = .
qui gen rm_bbg_ost = .
qui gen rm_bbg2_ost = .
qui gen rm_gfg_ost = .
// Specify ceilings for West Germany
local bw_b "281.97 286.03"
local bw_c "147.95 167.67 168.85 170.96 172.60 172.60 173.77 177.53 180.82 180.82 183.61 190.68 195.62 198.90 203.28 208.77"
local bw `bw_b' `bw_c'
// Specify mining-specific ceilings for West Germany
local bw2_b "347.54 351.78"
local bw2_c "182.47 205.48 208.2 210.41 212.05 212.05 214.75 218.63 223.56 221.92 226.23 233.42 240.00 244.93 250.82 258.08"
local bw2 `bw2_b' `bw2_c'
// Specify floors for West Germany
local gw_b "20.66 20.71"
local gw_c "10.68 13.15 13.11 13.15 13.15 13.15 13.11 13.15 13.15 13.15 13.15 14.79 14.79 14.79 14.75 14.79"
local gw `gw_b' `gw_c'
// Specify ceilings for East Germany
local bo_a "232.79 240"
local bo_b "123.29 139.73 142.62 144.66 144.66 149.59 147.54 149.59 152.88 157.80 157.38 161.10 164.38 170.96 177.05 187.34"
local bo `bo_a' `bo_b'
// Specify mining-specific ceilings for East Germany
local bo2_a "285.25 295.89"
local bo2_b "152.88 172.6 175.41 177.53 177.53 182.47 182.47 184.11 187.40 193.97 193.44 198.90 202.19 208.77 218.03 230.14"
local bo2 `bo2_a' `bo2_b'
// Specify floors for East Germany
local go_a "20.66 20.71"
local go_b "10.68 13.15 13.11 13.15 13.15 13.15 13.11 13.15 13.15 13.15 13.15 14.79 14.79 14.79 14.75 14.79"
local go `go_a' `go_b'
// Assign ceilings and floors to each observation
forvalues i = 1(1)18 {
	local b1 : word `i' of `bw'
	qui replace rm_bbg_west = `b1' if jahr == 1999 +`i'
}
forvalues i = 1(1)18 {
	local b1 : word `i' of `bw2'
	qui replace rm_bbg2_west = `b1' if jahr == 1999 +`i'
}
forvalues i = 1(1)18 {
	local b1 : word `i' of `gw'
	qui replace rm_gfg_west = `b1' if jahr == 1999 +`i'
}
forvalues i = 1(1)18 {
	local b1 : word `i' of `bo'
	qui replace rm_bbg_ost = `b1' if jahr == 1999 +`i'
}
forvalues i = 1(1)18{
	local b1 : word `i' of `bo2'
	qui replace rm_bbg2_ost = `b1' if jahr == 1999 +`i'
}
forvalues i = 1(1)18 {
	local b1 : word `i' of `go'
	qui replace rm_gfg_ost = `b1' if jahr == 1999 +`i'
}
// Convert 2000-2001 ceilings and floors from DM to EUR
foreach x of varlist rm_bbg_west rm_bbg2_west rm_gfg_west rm_bbg_ost rm_bbg2_ost rm_gfg_ost{
	qui replace `x' = `x'/1.95583 if jahr < 2002
	format %12.2f `x'
}
// Account for the April 2003 change in the floors
qui replace rm_gfg_west = 10.68 if smonat >= 516 & smonat <= 518
qui replace rm_gfg_ost = 10.68 if smonat >= 516 & smonat <= 518
// Account for changes in the ceilings and floor for East Germany during reunification
qui replace rm_bbg_ost = 50.43 if smonat >= 372 & smonat <= 377
qui replace rm_bbg2_ost = 50.43 if smonat >= 372 & smonat <= 377
qui replace rm_gfg_ost = 3.70 if smonat >= 372 & smonat <= 377
// Account for regions and rounding
qui gen rm_gfg = rm_gfg_west if west == 1
qui gen rm_bbg = rm_bbg_west - 0.01 if west == 1
qui gen rm_bbg2 = rm_bbg2_west - 0.01 if west == 1
qui replace rm_gfg = rm_gfg_ost if ost == 1
qui replace rm_bbg = rm_bbg_ost - 0.01 if ost == 1
qui replace rm_bbg2 = rm_bbg2_ost - 0.01 if ost == 1
// Define the appropriate ceiling and daily wages corrected for it
qui gen grenze = rm_bbg if knapp == 0
qui replace grenze = rm_bbg2 if knapp == 1
label variable grenze "Contribution threshold"
qui gen tag_entg = tentgelt
label variable tag_entg "Daily wage, accounting for the contribution threshold"
// Deflate wages >=1.5x the ceiling by 100 because they are likely erroneously reported
replace tag_entg = tag_entg / 100 if tag_entg >= grenze * 1.5 & tag_entg < .
// Indicate right-censored observations and cap them at the ceiling
qui gen cens = 0
qui replace cens = 1 if tag_entg >= grenze
tab cens
replace tag_entg = grenze if cens == 1
// Identify the marginally employed based on the appropriate floor
gen gfg = (tag_entg <= rm_gfg & tag_entg > 0 & tag_entg != .) if jahr >= 1975
label variable gfg "Marginal part-time, according to mini-job threshold"
label define gfg 0 "No" 1 "Yes", replace
label values gfg gfg
// Save the temporary file and merge it into the data
keep persnr smonat tag_entg gfg jahr grenze
sort persnr smonat
save "`tag_entg'", replace
use ${data}/0_clean.dta, clear
merge persnr smonat using "`tag_entg'"
drop _merge smonat
replace tag_entg = 0 if tentgelt == .
// Verify that most full-time employees are not marginally employed
tab teilzeit marginal, row
tab teilzeit gfg, row
// Compare marginal part-time indicators
tab marginal gfg, row

* Deflate nominal wages using OECD data on German CPI inflation from https://data.oecd.org/price/inflation-cpi.htm
qui gen cpi = .   
label variable cpi "CPI deflator"                       
qui replace cpi = 0.78369 if jahr == 2000
qui replace cpi = 0.79924 if jahr == 2001
qui replace cpi = 0.81060 if jahr == 2002
qui replace cpi = 0.81898 if jahr == 2003
qui replace cpi = 0.83262 if jahr == 2004
qui replace cpi = 0.84550 if jahr == 2005
qui replace cpi = 0.85884 if jahr == 2006
qui replace cpi = 0.87858 if jahr == 2007
qui replace cpi = 0.90167 if jahr == 2008
qui replace cpi = 0.90449 if jahr == 2009
qui replace cpi = 0.91447 if jahr == 2010
qui replace cpi = 0.93345 if jahr == 2011
qui replace cpi = 0.95220 if jahr == 2012
qui replace cpi = 0.96653 if jahr == 2013
qui replace cpi = 0.97529 if jahr == 2014
qui replace cpi = 0.98031 if jahr == 2015
qui replace cpi = 0.98513 if jahr == 2016
qui replace cpi = 1.00000 if jahr == 2017
qui gen tag_entgd = tag_entg / cpi
label variable tag_entgd "Real daily wage"

* Merge consecutive full- or part-time employment spells at the same employer in the same year
sort persnr spell
qui gen samejob = 0
// Identify consecutive spells
by persnr: replace samejob = 1 if betnr == betnr[spell - 1] & jahr == jahr[spell - 1] & begepi - 1 == endepi[spell - 1] & teilzeit == teilzeit[spell - 1] & gfg == gfg[spell - 1]
// Identify start of new jobs
qui by persnr: gen neujob = 1 if samejob == 0 & samejob[spell + 1] == 1
// Count consecutive spells
qui egen jobid = seq() if neujob == 1
qui by persnr: replace jobid = jobid[_n - 1] if samejob == 1
sort jobid spell
qui by jobid: gen n = _n
qui by jobid: gen N = _N
// Calculate average earnings across consecutive spells
qui gen days = endepi - begepi + 1
label variable days "Spell duration (number of days)"
qui gen zeitraumentgelt = days * tag_entgd
qui by jobid: egen totalentgelt = total(zeitraumentgelt) if jobid != .
qui by jobid: egen start = min(begepi) if jobid != .
replace tag_entgd = totalentgelt / (endepi - start + 1) if jobid != . & n == N
// Keep highest educational attainment over consecutive spells
qui by jobid: egen abschluss = max(ausbildung_imp)
replace ausbildung_imp = abschluss if jobid != .
// Keep only one consecutive spell
qui replace begepi = start if jobid != . & n == N
qui replace days = endepi - begepi + 1
drop if jobid != . & n != N
sort persnr spell
qui by persnr: gen o = _n
qui replace spell = o
drop samejob neujob jobid n N zeitraumentgelt totalentgelt start abschluss o

* Calculate day fractions
// Identify leap years
qui gen schalt = 0
forvalue x = 1976(4)2020{
        qui replace schalt = 1 if jahr == `x' 
}
// Spell duration as percentage of full year
qui gen daysfrac = days / 365 if schalt == 0
qui replace daysfrac = days / 366 if schalt == 1
label variable daysfrac "Spell duration (number of days as percentage of year)"
// Drop spells inaccurately longer than a year
drop if days > 366 | (days > 365 & schalt == 0)
drop schalt

* Drop spells subject to social security but with zero earnings due to, e.g., illness, maternity leave, or sabbaticals
drop if tentgelt == 0 & zustand == 1

* Take the natural logarithm of earnings
qui gen lentgelt = ln(tag_entgd)
label variable lentgelt "Log real daily wage"

* Define age categories
qui gen agecat = age
label variable agecat "Age category"
qui recode agecat (25/30 = 1)(31/35 = 2)(36/40 = 3)(41/45 = 4)(46/50 = 5)(51/55 = 6)(min/24 = .)(56/max = .)
label define agecat 1 "25-30 years" 2 "31-35 years" 3 "36-40 years" 4 "41-45 years" 5 "46-50 years" 6 "51-55 years"
label value agecat agecat
tab agecat, miss

* Define education categories
qui gen educ = 0
label variable educ "Education category"
qui replace educ = 1 if ausbildung_imp == 1
qui replace educ = 2 if inlist(ausbildung_imp, 2, 3, 4)
qui replace educ = 3 if inlist(ausbildung_imp, 5, 6)
label define educ 0 "Missing" 1 "Low" 2 "Medium" 3 "High", replace
label values educ educ
tab educ, miss

* Define occupation categories
qui gen berufcat = 1 if inrange(beruf, 11, 62)
qui replace berufcat = 2 if inrange(beruf, 601, 635)
qui replace berufcat = 3 if inrange(beruf, 71, 549)
qui replace berufcat = 4 if inrange(beruf, 681, 688)
qui replace berufcat = 5 if inrange(beruf, 691, 706)
qui replace berufcat = 6 if inrange(beruf, 711, 744)
qui replace berufcat = 7 if inrange(beruf, 751, 784)
qui replace berufcat = 8 if inrange(beruf, 841, 856)
qui replace berufcat = 9 if inrange(beruf, 861, 883)
qui replace berufcat = 10 if inrange(beruf, 791, 838) | inrange(beruf, 901, 937)
qui replace berufcat = 0 if berufcat == .
label variable berufcat "Occupation category"
label define berufcat 0 "Missing" 1 "I Agriculture" 2 "III Manufacturing occupations in industry, IV techinal occupations" 3 "II Mining, III Manufacturing occupations outside industry" 4 "V Service, trading goods" 5 "V Service, other trading occupations" 6 "V Service, logistics" 7 "V Service, administration" 8 "V Service, health" 9 "V Service, education" 10 "V Service, other" 
label value berufcat berufcat
tab berufcat, miss
// Evaluate inaccuracies caused by new occupation classification system introduced in December 2011
qui gen berufmiss = berufcat == 0
tab jahr berufmiss, row
qui gen beruf2011 = 0 if jahr == 2011
qui replace beruf2011 = 1 if jahr == 2011 & endorig > td(30nov2011)
tab beruf2011
drop berufmiss beruf2011

* Define industry categories
qui gen indcat = 1 if knapp == 1
qui replace indcat = 2 if inrange(w08_3_gen, 101, 120) | inrange(w08_3_gen, 131, 152) | inrange(w08_3_gen, 161, 182) | inrange(w08_3_gen, 310, 332) | inrange(w08_3_gen, 191, 192) | inrange(w08_3_gen, 201, 206) | inrange(w08_3_gen, 211, 212) | inrange(w08_3_gen, 221, 239) | inrange(w08_3_gen, 241, 259) | inrange(w08_3_gen, 261, 268) | inrange(w08_3_gen, 271, 279) | inrange(w08_3_gen, 281, 289) | inrange(w08_3_gen, 291, 309)
qui replace indcat = 3 if inrange(w08_3_gen, 411, 439)
qui replace indcat = 4 if inrange(w08_3_gen, 451, 479)
qui replace indcat = 5 if inrange(w08_3_gen, 491, 532)
qui replace indcat = 6 if inrange(w08_3_gen, 551, 563)
qui replace indcat = 7 if inrange(w08_3_gen, 581, 639) | inrange(w08_3_gen, 681, 829)
qui replace indcat = 8 if inrange(w08_3_gen, 641, 663)
qui replace indcat = 9 if inrange(w08_3_gen, 841, 843)
qui replace indcat = 10 if inrange(w08_3_gen, 851, 856)
qui replace indcat = 11 if inrange(w08_3_gen, 861, 889)
qui replace indcat = 12 if inrange(w08_3_gen, 900, 990)
qui replace indcat = 0 if w08_3_gen == .
label variable indcat "Industry category"
label define indcat 0 "Missing" 1 "Agriculture & mining" 2 "Manufacturing" 3 "Construction" 4 "Wholesale and retail trades" 5 "Transportation & storage" 6 "Accommodation & food service" 7 "Information, communication, real estate & business services" 8 "Finance & insurance" 9 "Public administration & defense" 10 "Education" 11 "Health & social work" 12 "Other services"
label value indcat indcat
tab indcat, miss

* Generate full-time weights for West Germany according to Fitzenberger & Seidlitz (2020)
// Select only non-marginal spells subject to social security, based on mini-job threshold
qui gen sample = (gfg == 0 & zustand == 1 & west == 1)
label variable sample "Sample for estimating full-time weights"
label define sample 0 "No" 1 "Yes", replace
label values sample sample
// Identify the quantile of the full-time distribution for each gender above which full-time spells are unlikely to be misreported before 2012
foreach p of numlist 10 25 50 80 {
	qui gen graphp`p'1= .
	qui gen lwg2000`p'1 = .
	label variable lwg2000`p'1 "`p'th percentile"
	forvalue y = 2000/2017 {
		_pctile lentgelt [pw = daysfrac] if jahr == `y' & frau == 1 & teilzeit == 0 & sample == 1, p(`p')
		if `y' == 2000 {
			qui gen graphp`p'1_2000 = r(r1) if frau == 1
		}
		qui replace graphp`p'1 = r(r1) if jahr == `y' & frau == 1
		qui replace lwg2000`p'1 = graphp`p'1 - graphp`p'1_2000
	}
}
foreach p of numlist 2.5 5 10 25 {
	local pdec = 10 * `p'
	qui gen graphp`pdec'0 = .
	qui gen lwg2000`pdec'0 = .
	label variable lwg2000`pdec'0 "`p'th percentile"
	forvalue y = 2000/2017 {
		_pctile lentgelt [pw = daysfrac] if jahr == `y' & frau == 0 & teilzeit == 0 & sample == 1, p(`p')
		if `y' == 2000 {
			qui gen graphp`pdec'0_2000 = r(r1) if frau == 0
		}
		qui replace graphp`pdec'0 = r(r1) if jahr == `y' & frau == 0
		qui replace lwg2000`pdec'0 = graphp`pdec'0 - graphp`pdec'0_2000
	}
}
*** NOTE FOR DISCLOSURE REVIEW: Reproducing Figures 2-3 from Fitzenberger & Seidlitz (2020), these graphs (exported as png-files) plot the change since 2000 of various percentiles of the full-time log real wage distribution for each gender, providing graphical evidence for the percentile below which the 2011 break in the part-time indicator had an impact. The tables immediately preceding them show the number of observations per year per gender used to estimate the percentiles.
// Women
sort jahr
tab jahr if frau == 1 & teilzeit == 0 & sample == 1
qui gen ptchangelow = -0.1 if inlist(jahr, 2010, 2012)
qui gen ptchangehigh = 0.2 if inlist(jahr, 2010, 2012)
twoway (rarea ptchangelow ptchangehigh jahr, color(gs12)) (line lwg2000101 lwg2000251 lwg2000501 lwg2000801 jahr), ytitle("Difference to log-earning percentile in 2000") ylabel(-0.1(0.05)0.2) ymtick(-0.1(0.05)0.2) xlabel(2000(5)2015 2017) legend(order(2 3 4 5))
graph export ${log}/d0_clean_ptgrowthf.png, replace
// Men
tab jahr if frau == 0 & teilzeit == 0 & sample == 0
qui replace ptchangelow = -0.2 if inlist(jahr, 2010, 2012)
qui replace ptchangehigh = 0 if inlist(jahr, 2010, 2012)
twoway (rarea ptchangelow ptchangehigh jahr, color(gs12)) (line lwg2000250 lwg2000500 lwg20001000 lwg20002500 jahr), ytitle("Difference to log-earning percentile in 2000") ylabel(-0.2(0.05)0) ymtick(-0.2(0.05)0) xlabel(2000(5)2015 2017) legend(order(2 3 4 5))
graph export ${log}/d0_clean_ptgrowthm.png, replace
drop ptchange* graphp* lwg2000*
// Specify the quantile of the full-time log real wage distribution for each gender above which full-time spells are unlikely to be misreported before 2012, which is 80% for women and 25% for men
_pctile lentgelt [pw = daysfrac] if jahr == 2012 & frau == 1 & teilzeit == 0 & sample == 1, p(80)
gen p80_2012 = r(r1)
_pctile lentgelt [pw = daysfrac] if jahr == 2012 & frau == 0 & teilzeit == 0 & sample == 1, p(25)
gen p25_2012 = r(r1)
// Identify observations below these quantiles
qui gen low_part = (lentgelt <= p25_2012) if jahr == 2012 & frau == 0 & teilzeit == 0 & sample == 1
qui replace low_part = 1 if lentgelt <= p80_2012 & jahr == 2012 & frau == 1 & teilzeit == 0 & sample == 1
// Identify the quantile of the total (combined full- and part-time) distribution for each gender that corresponds to the previously specified quantiles of the full-time distribution
qui gen quantile_upperbound = .
forvalue f = 0/1 {
	cumul lentgelt [aw = daysfrac] if jahr == 2012 & frau == `f' & sample == 1, gen(a`f'2012)
	qui egen quantile_x`f' = max(a`f'2012) if low_part == 1
	qui replace quantile_upperbound = quantile_x`f' if frau == `f' 
}
sort frau quantile_upperbound
qui by frau: carryforward quantile_upperbound, replace
drop quantile_x0 quantile_x1
// Define a variable to indicate whether an observation is used in the probit estimate of full-time probabilities in 2012
qui gen popprobit2012 = 0
qui replace popprobit2012 = 1 if ((teilzeit == 1 | teilzeit == 0) & a02012 <= quantile_upperbound & frau == 0 & jahr == 2012 & sample == 1)
qui replace popprobit2012 = 1 if ((teilzeit == 1 | teilzeit == 0) & a12012 <= quantile_upperbound & frau == 1 & jahr == 2012 & sample == 1)
// Define a variable for an observation's distance from the upper bound in 2012
qui gen theta = .
label variable theta "Rank distance from upper bound"
forvalue f = 0/1 {
	qui gen d`f'2012 = a`f'2012 - quantile_upperbound
	qui recode d`f'2012 (0/max = 0)
	qui replace d`f'2012 = abs(d`f'2012)
	qui replace theta = d`f'2012 if frau == `f' & jahr == 2012
}
// Define these variables for 2000-2011
forvalue y = 2000/2011 {
	qui gen popprobit`y' = 0
	forvalue f = 0/1 {
		cumul lentgelt [aw = daysfrac] if jahr == `y' & frau == `f' & sample == 1, gen(a`f'`y')
		qui replace popprobit`y' = 1 if ((teilzeit == 1 | teilzeit == 0) & a`f'`y' <= quantile_upperbound & jahr == `y' & sample == 1)
		qui gen d`f'`y' = a`f'`y' - quantile_upperbound
		qui recode d`f'`y' (0/max = 0)
		qui replace d`f'`y' = abs(d`f'`y')
		qui replace theta = d`f'`y' if frau == `f' & jahr == `y'
	}
}
// Define a variable for age squared
qui gen age2 = age^2
label variable age2 "Age squared"
// Define controls in probit estimate
local controls "age age2 i.educ i.berufcat i.indcat i.ao_bula"
// Define variables for the probit estimates
qui gen pt_probit2012 = .
qui gen pt_probit = .
// Estimate the probit models
*** NOTE FOR DISCLOSURE REVIEW: Reproducing Tables 1-2 from Fitzenberger & Seidlitz (2020), these tables (exported as tex-files) contain the number of observations used to estimate the probit regressions. The regressions of the part-time indicator (teilzeit) on the rank distance of an observation from the upper bound relevant for the part-time correction (theta), age (age), age squared (age2), education categories (i.educ), occupation categories (i.indcat), industry categories (i.indicat), and regions (i.ao_bula).
forvalue f = 0/1 {
	eststo clear
	capture probit teilzeit theta `controls' [pw = daysfrac] if popprobit2012 == 1 & frau == `f'
	if _rc != 0 {
		forvalue y = 2000/2011 {
		display `y'
		display "Error: " _rc
			qui gen b`f'`y' = 0 if popprobit`y' == 1 & jahr == `y' & teilzeit == 0 & frau == `f'
			replace pt_probit2012 = b`f'`y' if popprobit`y' == 1 & jahr == `y' & teilzeit == 0 & frau == `f'
		}
	}
	else {
		eststo m2012
		forvalue y = 2000/2011 {
			display `y'
			qui predict b`f'`y' if popprobit`y' == 1 & jahr == `y' & teilzeit == 0 & frau == `f', pr
			replace pt_probit2012 = b`f'`y' if popprobit`y' == 1 & jahr == `y' & teilzeit == 0 & frau == `f'
		}
	}
	forvalue y = 2000/2011 {
		display `y'
		capture probit teilzeit theta `controls' [pw = daysfrac] if popprobit`y' == 1 & frau == `f'
		if _rc != 0 {
			display "Error: " _rc
			qui gen c`f'`y' = 0 if popprobit`y' == 1 & jahr == `y' & frau == `f'
		}
		else {
			eststo m`y'
			qui predict c`f'`y' if popprobit`y' == 1 & jahr == `y' & frau == `f', pr
		}
		replace pt_probit = c`f'`y' if popprobit`y' == 1 & jahr == `y' & frau == `f'
	}
	capture esttab m2000 m2005 m2010 m2011 m2012, keep(theta age age2 1.educ 2.educ 3.educ) mtitles("2000" "2005" "2010" "2011" "2012") nostar b(%8.3f) t(%8.1f) replace label
	if _rc != 0 {
		display "Error: " _rc
	}
	else {
		esttab m2000 m2005 m2010 m2011 m2012, keep(theta age age2 1.educ 2.educ 3.educ) mtitles("2000" "2005" "2010" "2011" "2012") nostar b(%8.3f) t(%8.1f) label
		esttab m2000 m2005 m2010 m2011 m2012 using "${log}/d0_clean_probit`f'.tex", replace keep(theta age age2 1.educ 2.educ 3.educ) mtitles("2000" "2005" "2010" "2011" "2012") nostar b(%8.3f) t(%8.1f) label
	}
}
// Generate full-time weights
qui gen faktor = 1
forvalue y = 2000/2011 {
	display `y'
	replace faktor = ((1 - pt_probit2012) / (1 - pt_probit)) if popprobit`y' == 1 & jahr == `y' & teilzeit == 0
}
recode faktor (1/max = 1), gen(ft_weight)
qui gen weight_duration_ft = daysfrac * ft_weight
label variable ft_weight "Full-time weight"
label variable weight_duration_ft "Duration-adjusted full-time weight"
drop a0* a1* b0* b1* c0* c1* d0* d1* popprobit** low_part quantile_upperbound p80_2012 p25_2012 pt_probit2012 pt_probit theta faktor
eststo clear
sort persnr spell

* Save cleaned data and log file
// Automatically assign optimal data types on all variables
compress
// Save data set
save ${data}/0_clean.dta, replace
log close
clear
