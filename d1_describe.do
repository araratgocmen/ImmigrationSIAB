* Perform descriptive analysis, calculate relative densities, and generate annual regional aggregates (at the Bundesland level) of the native and immigrant full-time populations
* Ararat Gocmen
* Version: 2020-09-01
* d1_describe.do

* Manage files
// Close the existing log file
capture log close
// Create a new log file
log using ${log}/d1_describe.log, replace

* Load data
// Load the cleaned data file
use ${data}/0_clean.dta, clear

* Indicate and keep only full-time observations
// Define full-time variable
qui gen ft = 1 if sample == 1 & teilzeit == 0
label variable ft "Full-time"
label define ft 0 "No" 1 "Yes", replace
label values ft ft
// Verify that no full-time workers are marginally employed
tab marginal if ft == 1
// Observe unweighted and weighted full-time sample over time
tab jahr if ft == 1
tab jahr [iw = weight_duration_ft] if ft == 1
// Keep only full-time observations
keep if ft == 1

* Estimate the wage gap for immigrants across years before aggregation, accounting for occupation and industry categories
*** NOTE FOR DISCLOSURE REVIEW: These are regressions of log real wages (lentgelt) on age categories (i.agecat), education categories (i.educ), their interactions, time fixed effects (i.jahr), and immigration status (deutsch), as well as occupation categories (i.berufcat), industry categories (i.indcat), and their interactions in the second regression. The paramater of interest is the coefficient on deutsch. The results of the unweighted regressions precede those of the weighted ones.
// Ignore occupation and age categories
eststo clear
qui reg lentgelt i.agecat##i.educ i.jahr deutsch if educ != 0
eststo m1uw
qui reg lentgelt i.agecat##i.educ i.jahr deutsch [pw = weight_duration_ft] if educ != 0
eststo m1
// Include occupation and age categories
qui reg lentgelt i.agecat##i.educ i.jahr i.berufcat##i.indcat deutsch if educ != 0
eststo m2uw
qui reg lentgelt i.agecat##i.educ i.jahr i.berufcat##i.indcat deutsch [pw = weight_duration_ft] if educ != 0
eststo m2
// Report estimated coefficients from unweighted regressions
esttab m1uw m2uw, keep(deutsch) nostar b(%8.3f) mlabels(none) label
// Report estimated coefficients from weighted regressions
esttab m1 m2, keep(deutsch) nostar b(%8.3f) mlabels(none) label

* Aggregate wages for each individual across all jobs in each year
// Count the number of jobs per year
qui by persnr jahr, sort: gen jobcount = _N
tab jobcount
label variable jobcount "Annual full-time job count"
// Calculate the total full-time weight across jobs
qui by persnr jahr: egen tweight = total(weight_duration_ft)
label variable tweight "Total weight"
// Calculate the weighted real wage per full-time job
qui gen wrwage = exp(lentgelt) * weight_duration_ft / tweight
// Calculate and log the weighted-average real wage
qui by persnr jahr: egen warwage = total(wrwage)
label variable warwage "Weighted-average real wage"
qui gen lwarwage = ln(warwage)
label variable lwarwage "Log weighted-average real wage"
drop wrwage
// Indicate censored observations based on whether they are more than 6 EUR less than the contribution threshold
qui gen cens = 0
qui replace cens = 1 if (warwage * cpi >= grenze - 6)
label variable cens "Censored"
label define cens 0 "No" 1 "Yes", replace
label values cens cens
// Take the maximum education and skill level per person per year
by persnr jahr: egen meduc = max(educ)
label variable meduc "Education category"
label values meduc educ
by persnr jahr: egen mniveau = max(niveau)
label variable mniveau "Level of requirement - current/most recent job (KldB 2010)"
label values mniveau niveau_en

* Identify distinct individuals in each year and drop those missing education
// Identify and keep only those unique observations with education data
qui egen tag = tag(persnr jahr)
keep if tag & meduc != 0
// Observe unweighted and weighted unique sample over time
tab jahr if tag
tab jahr if tag [iw = tweight]
// Keep only the maximum education and skill levels
drop educ niveau
rename meduc educ
rename mniveau niveau

* Impute wages above the censoring threshold following Gartner (2005) and Dustmann, Ludsteck & Schönberg (2009)
*** NOTE FOR DISCLOSURE REVIEW: These are gender- and year-specific censored regressions of real wages (lwarwage) on age categories (i.agecat), education categories (i.educ), and their interactions. They are used to predict uncensored wages for ones censored according to the social security contribution threshold. The gender- and year-specific numbers of censored and uncensored observations used in these regressions are initially tabulated.
// Observe censoring quantiles in each year
tab jahr cens if frau == 0, row
tab jahr cens if frau == 0 [iw = tweight], row
tab jahr cens if frau == 0, row
tab jahr cens if frau == 1 [iw = tweight], row
// Cluster by age and education categories
qui egen cgroup = group(agecat educ)
// Define new variables
qui gen impwage1 = lwarwage
label variable impwage1 "Imputed log real wage according to Gartner (2005)"
qui gen impwage2 = lwarwage
label variable impwage2 "Imputed log real wage according to Dustmann, Ludsteck & Schönberg (2009)"
// Estimate a censored regression for each combination of gender and year
forvalue f = 0/1 {
	forvalue y = 2000/2017 {
		disp `y'
		capture xi: cnreg lwarwage i.agecat*i.educ if jahr == `y' & frau == `f' [pw = tweight], censored(cens) vce(cluster cgroup)
		if _rc != 0 {
			display "Error: " _rc
		}
		else {
			qui predict predwage`f'`y' if jahr == `y' & frau == `f', xb
			qui predict sigma`f'`y' if jahr == `y' & frau == `f', stdf
			// Generate the standardized ceiling for each record
			qui gen alpha`f'`y' = (ln(grenze) - predwage`f'`y') / sigma`f'`y' if jahr == `y' & frau == `f' & cens == 1
			qui gen impwage0`f'`y' = invnorm(uniform() * (1 - normal(alpha`f'`y')) + normal(alpha`f'`y')) * sigma`f'`y' + predwage`f'`y' if jahr == `y' & frau == `f' & cens == 1
			replace impwage1 = impwage0`f'`y' if jahr == `y' & frau == `f' & cens == 1 & impwage0`f'`y' != .
			replace impwage2 = predwage`f'`y' + rnormal(0, sigma`f'`y') if jahr == `y' & frau == `f' & cens == 1
		}
	}
}
drop cgroup _I* predwage* sigma* alpha* impwage0*

* Estimate the wage gap for immigrants across years after aggregation, using actual and imputed wages
*** NOTE FOR DISCLOSURE REVIEW: These are regressions of actual (lwarwage) and imputed (impwage1) log weighted-average real wages on age categories (i.agecat), education categories (i.educ), their interactions, time fixed effects (i.jahr), and immigration status (deutsch). The paramater of interest is the coefficient on deutsch. The results of the unweighted regressions precede those of the weighted ones.
// Actual
eststo clear
qui reg lwarwage i.agecat##i.educ i.jahr deutsch
eststo m1uw
qui reg lwarwage i.agecat##i.educ i.jahr deutsch [pw = tweight]
eststo m1
// Imputed
qui reg impwage1 i.agecat##i.educ i.jahr deutsch
eststo m2uw
qui reg impwage1 i.agecat##i.educ i.jahr deutsch [pw = tweight]
eststo m2
// Report estimated coefficients from unweighted regressions
esttab m1uw m2uw, keep(deutsch) nostar b(%8.3f) mlabels(none) label
// Report estimated coefficients from weighted regressions
esttab m1 m2, keep(deutsch) nostar b(%8.3f) mlabels(none) label

* Plot immigration shares, overall and by region of foreign citizenship, over time
*** NOTE FOR DISCLOSURE REVIEW: The unweighted counts of immigrants and the total population, overall and by region of foreign citizenship, are tabulated before the weighted ones used to calculate the graphed ratios.
// Overall
tab jahr deutsch, row
tab jahr deutsch [iw = tweight], row
qui by jahr, sort: egen atweight = total(tweight)
qui by jahr: egen imshare = total(tweight / atweight * 100 * (1 - deutsch))
label variable imshare "Immigrant share (%)"
twoway (line imshare jahr), xlabel(2000(5)2015 2017)
graph export ${log}/d1_describe_imshare.png, replace
// By continent
tab jahr continent, row
tab jahr continent [iw = tweight], row
qui gen eur = continent == 1
qui by jahr: egen eurshare = total(tweight / atweight * 100 * eur)
label variable eurshare "Europe"
qui gen afr = continent == 2
qui by jahr: egen afrshare = total(tweight / atweight * 100 * afr)
label variable afrshare "Africa"
qui gen amer = continent == 3
qui by jahr: egen amershare = total(tweight / atweight * 100 * amer)
label variable amershare "Americas"
qui gen asia = continent == 4
qui by jahr: egen asiashare = total(tweight / atweight * 100 * asia)
label variable asiashare "Middle East & Asia"
twoway (line eurshare jahr, yaxis(1)) (line asiashare jahr, yaxis(2)) (line amershare jahr, yaxis(2)) (line afrshare jahr, yaxis(2)), ytitle("Immigrant share (%)") xlabel(2000(5)2015 2017)
graph export ${log}/d1_describe_imsharecont.png, replace
drop eur* afr* amer* asia*

* Describe native vs. immigrant age, gender, and education characteristics
// Age and gender
*** NOTE FOR DISCLOSURE REVIEW: The mean, minimum, maximum, and standard deviation of age by immigration status are reported in the initial tabulations. The unweighted and weighted averaged ages and female percentages are then tabulated. The final table exported in the tex-file displays the weighted mean age and female percentage of the native and immigrant populations in various years. 
by deutsch, sort: summarize age if jahr == 2000
by deutsch: summarize age if jahr == 2010
by deutsch: summarize age if jahr == 2017
tab deutsch jahr if inlist(jahr, 2000, 2010, 2017), summarize(age) means obs
tab deutsch jahr if inlist(jahr, 2000, 2010, 2017) [aw = tweight], summarize(age) means obs
format frau %9.0g
tab deutsch jahr if inlist(jahr, 2000, 2010, 2017), summarize(frau) means obs
tab deutsch jahr if inlist(jahr, 2000, 2010, 2017) [aw = tweight], summarize(frau) means obs
format frau %12.0f
sort deutsch jahr
qui gen fraup = 100 * frau
label variable fraup "% Female"
qui egen tabgroup = group(deutsch jahr) if inlist(jahr, 2000, 2010, 2017)
label values tabgroup
eststo clear
forvalue g = 1/6 {
	qui estpost tabstat age fraup if tabgroup == `g' [aw = tweight], statistics(mean) columns(statistics)
	eststo m`g'
}
esttab m4 m5 m6 m1 m2 m3, main(mean) nonumber nonote label mgroups(Native Immigrant, pattern(1 0 0 1 0 0)) mlabels(2000 2010 2017 2000 2010 2017)
esttab m4 m5 m6 m1 m2 m3 using ${log}/d1_describe_agegeneduc.tex, replace main(mean) nonumber nonote label mgroups(Native Immigrant, pattern(1 0 0 1 0 0)) mlabels(2000 2010 2017 2000 2010 2017)
drop fraup tabgroup
// Education
*** NOTE FOR DISCLOSURE REVIEW: The unweighted and weighted percentages of each education level by immigration status are first tabulated. The final table exported in the tex-file displays the weighted percentages in various years.
tab educ deutsch if jahr == 2000, col
tab educ deutsch if jahr == 2000 [aw = tweight], col
tab educ deutsch if jahr == 2010, col
tab educ deutsch if jahr == 2010 [aw = tweight], col
tab educ deutsch if jahr == 2017, col
tab educ deutsch if jahr == 2017 [aw = tweight], col
eststo clear
forvalue d = 0/1 {
	qui estpost tabulate educ jahr if deutsch == `d' & inlist(jahr, 2000, 2010, 2017) [aw = tweight], nototal
	eststo m`d'
}
esttab m1 m0, cell(colpct(fmt(2))) unstack nonumber noobs mgroups(Native Immigrant, pattern(1 1)) collabels(none)
esttab m1 m0 using ${log}/d1_describe_agegeneduc.tex, append cell(colpct(fmt(2))) unstack nonumber mgroups(Native Immigrant, pattern(1 1)) collabels(none)

* Describe native vs. immigrant skill characteristics
// Overall
*** NOTE FOR DISCLOSURE REVIEW: The unweighted and weighted percentages of each skill level by immigration status are first tabulated. The mean, minimum, maximum, and standard deviation of real wages are then tabulated. The final table exported in the tex-file displays the weighted percentages and average wages of each skill level.
tab niveau deutsch if jahr == 2017, col
tab niveau deutsch if jahr == 2017 [aw = tweight], col
by niveau deutsch, sort: summarize warwage if jahr == 2017
tab niveau if jahr == 2017, summarize(warwage) mean
tab niveau if jahr == 2017 [aw = tweight], summarize(warwage) mean
eststo clear
forvalue d = 0/1 {
	qui estpost tabulate niveau if deutsch == `d' & jahr == 2017 [aw = tweight], nototal
	eststo m`d'
}
qui estpost tabstat warwage if jahr == 2017 [aw = tweight], by(niveau) statistics(mean) columns(statistics) nototal
eststo mw
esttab m1 m0 mw, cell(pct(fmt(2)) mean(fmt(2))) nonumber noobs mgroups(Native Immigrant "Average Wage", pattern(1 1 1)) collabels(none) mlabels(none) varwidth(30)
esttab m1 m0 mw using ${log}/d1_describe_skill.tex, replace cell(pct(fmt(2)) mean(fmt(2))) nonumber noobs mgroups(Native Immigrant "Average Wage", pattern(1 1 1)) collabels(none) mlabels(none) varwidth(30)
// By education
*** NOTE FOR DISCLOSURE REVIEW: The unweighted and weighted percentages of each combination of education and skill levels by immigration status are first tabulated. The final table exported in the tex-file displays the weighted percentages of each education-skill pair.
by educ, sort: tab niveau deutsch if jahr == 2017, col
by educ: tab niveau deutsch if jahr == 2017 [aw = tweight], col
eststo clear
forvalue e = 1/3 {
	qui estpost tabulate niveau deutsch if educ == `e' & jahr == 2017 [aw = tweight], nototal
	eststo m`e'
}
esttab m1 m2 m3, cell(colpct(fmt(2)) mean(fmt(2))) unstack nonumber noobs mgroups("Low" "Medium" "High", pattern(1 1 1)) collabels(none) mlabels(none) varwidth(30)
esttab m1 m2 m3 using ${log}/d1_describe_skill.tex, append cell(colpct(fmt(2)) mean(fmt(2))) unstack nonumber noobs mgroups("Low" "Medium" "High", pattern(1 1 1)) collabels(none) mlabels(none) varwidth(30)

* Estimate gender-specific regressions on native wages and predict wages for immigrants for each year
*** NOTE FOR DISCLOSURE REVIEW: These are native-only and gender- and year-specific regressions of imputed wages (impwage1) on age categories (i.agecat), education categories (i.educ), and their interactions. The purpose of these regressions are to estimate the returns to age and education for natives and, based on those estimates, to predict wages for immigrants if they enjoyed the same returns to age and education as natives. These predicted wages are used in the density plot below.
// Cluster by age and education categories
qui egen cgroup = group(agecat educ)
// Define new variable
qui gen impwagep = .
label variable impwagep "Predicted log real wage"
// Estimate a regression for each combination of gender and year
forvalue f = 0/1 {
	forvalue y = 2000/2017 {
		qui reg impwage1 i.agecat##i.educ if jahr == `y' & deutsch == 1 & frau == `f' [pw = tweight], vce(cluster cgroup)
		qui predict predwage`f'`y' if jahr == `y' & deutsch == 0 & frau == `f', xb
		qui predict sigma`f'`y' if jahr == `y' & deutsch == 0 & frau == `f', stdp
		qui replace sigma`f'`y' = sqrt(sigma`f'`y'^2 + (e(rmse))^2) if jahr == `y' & deutsch == 0 & frau == `f'
		qui replace impwagep = predwage`f'`y' + rnormal(0, sigma`f'`y') if jahr == `y' & deutsch == 0 & frau == `f'
	}
}
qui replace impwagep = impwage1 if deutsch == 1
drop cgroup predwage* sigma*

* Estimate the relative density of immigrants from 2011 to 2017
*** NOTE FOR DISCLOSURE REVIEW: The graph here plots the density of immigrants relative to natives along the native wage distribution, based on the weighted percentage of natives with actual wages smaller than the actual and predicted wages of each immigrant. The number of natives and immigrants used to estimate these densities in every year is initially tabulated.
// Estimate the native distribution
tab jahr deutsch
sort persnr jahr
qui gen natcdf = .
forvalue y = 2000/2017 {
	cumul impwage1 if deutsch == 1 & jahr == `y' [aw = tweight], generate(natcdf`y')
	qui replace natcdf = natcdf`y' if deutsch == 1 & jahr == `y'
	drop natcdf`y'
}
// Identify the first 85 percentiles at which densities will be estimated
qui egen ptil = seq() if _n <= 85
label variable ptil "Percentile of the native wage distribution"
qui replace ptil = ptil / 100
qui replace ptil = ln(ptil / (1 - ptil))
// Calculate the kernal density estimate of the log odds of the native distribution and apply the appropriate transformations, resulting in a native density of 1
qui gen logitnatcdf = ln(natcdf / (1 - natcdf))
kdensity logitnatcdf if deutsch == 1 & jahr >= 2011 [aw = tweight], generate(logitnatcdfp logitnatpdf) nograph at(ptil)
qui gen natcdfp = 100 * exp(logitnatcdfp) / (1 + exp(logitnatcdfp))
qui gen natpdf = logitnatpdf / abs(exp(logitnatcdfp) / (1 + exp(logitnatcdfp)) ^ 2)
label variable natpdf "Native"
// Calculated the weighted percentage of natives with a lower wage than each immigrant based on actual and predicted wages
qui gen imcdfimpwage1 = .
qui gen imcdfimpwagep = .
forvalue y = 2000/2017 {
	qui gen y`y' = jahr == `y'
	qui egen y = total(deutsch * tweight * y`y')
	foreach w in impwage1 impwagep {
		sort y`y' deutsch `w'
		qui gen x = sum(deutsch * tweight * y`y')
		gsort y`y' `w' -deutsch
		qui gen z = x[_n - 1] if deutsch == 0 & deutsch[_n - 1] == 1 & y`y' == 1
		qui gen a = 1 if x == 0 & x[_n + 1] == 0 & y`y' == 1
		qui replace a = a[_n - 1] + 1 if x == 0 & x[_n - 1] == 0 & y`y' == 1
		qui gen b = 1 if a == 1
		sort y`y' b `w'
		qui replace b = b[_n - 1] + 1 if b == 1 & y`y' == 1 & y`y'[_n - 1] != 0
		gsort y`y' `w' -deutsch
		qui replace b = b[_n - 1] if b[_n - 1] != . & b == . & a != . & y`y' == 1
		sort y`y' b a
		qui by y`y' b: egen c = max(z) if a != .
		qui replace z = c if z == .
		gsort y`y' `w' -deutsch
		qui replace imcdf`w' = (z + tweight) / (y + tweight) if deutsch == 0 & jahr == `y'
		drop x z a b c
	}
	drop y`y' y
}
rename imcdfimpwage1 imcdf
rename imcdfimpwagep imcdf2
sort persnr jahr
// Calculate the kernal density estimate of immigrants' wages rank in the native wage distribution based on their actual wages
qui gen logitimcdf = ln(imcdf / (1 - imcdf))
kdensity logitimcdf if deutsch == 0 & jahr >= 2011 [aw = tweight], generate(logitimcdfp logitimpdf) nograph at(ptil)
qui gen imcdfp = 100 * exp(logitimcdfp) / (1 + exp(logitimcdfp))
qui gen impdf = logitimpdf / abs(exp(logitimcdfp) / (1 + exp(logitimcdfp)) ^ 2)
qui gen relimpdf = impdf / natpdf
label variable relimpdf "Actual"
// Calculate the kernal density estimate of immigrants' wages rank in the native wage distribution based on their predicted wages
qui gen logitimcdf2 = ln(imcdf2 / (1 - imcdf2))
kdensity logitimcdf2 if deutsch == 0 & jahr >= 2011 [aw = tweight], generate(logitimcdfp2 logitimpdf2) nograph at(ptil)
qui gen imcdfp2 = 100 * exp(logitimcdfp2) / (1 + exp(logitimcdfp2))
qui gen impdf2 = logitimpdf2 / abs(exp(logitimcdfp2) / (1 + exp(logitimcdfp2)) ^ 2)
qui gen relimpdf2 = impdf2 / natpdf if impdf2 != .
label variable relimpdf2 "Predicted"
// Plot the densities
twoway (line relimpdf imcdfp if inrange(imcdfp, 5, 85)) (line natpdf natcdfp if inrange(natcdfp, 5, 85)) (line relimpdf2 imcdfp2 if inrange(imcdfp2, 5, 85)), yscale(range(0 2)) ylabel(0(0.5)2) ytitle("Density")  xlabel(0(20)100 5 85) xtitle("Percentile of the native wage distribution")
graph export ${log}/d1_describe_kernal.png, replace
// Perform test of rank insensitivity
*** NOTE FOR DISCLOSURE REVIEW: The graph here plots the ratio of the difference in the relative density of immigrants and of the difference in log real wages between adjacent percentiles of the native wage distribution. To satisfy model assumptions, this ratio must exceed a certain value at all percentiles, which this plot can be used to show.
sort ptil
qui replace ptil = round(100 * exp(ptil) / (1 + exp(ptil)))
qui replace relimpdf = relimpdf - 1
qui gen nwage = .
forvalues p = 1/85 {
	qui _pctile impwage1 [pw = tweight] if jahr >= 2011 & deutsch == 1, p(`p')
	qui replace nwage = r(r1) if _n == `p'
}
qui gen rinstest = (relimpdf - relimpdf[_n - 1]) / (nwage - nwage[_n - 1]) if inrange(_n, 2, 85)
twoway (line rinstest ptil if ptil >= 5), ytitle("Ratio") xlabel(0(20)100 5 85) xtitle("Percentile of the native wage distribution")
graph export ${log}/d1_describe_ranktest.png, replace
// Assign every 5th percentile to a year between 2001 and 2017 for later analysis in d2_regress.do
qui gen ptil2 = (jahr - 2000) * 5 if jahr > 2000
label variable ptil2 "Percentile of the native wage distribution"
qui gen rdens = relimpdf[ptil2]
label variable rdens "Relative density of immigrants"
drop ptil natcdf* natpdf* imcdf* impdf* logit* relim*
rename ptil2 ptil

* Generate aggregates by region
*** NOTE FOR DISCLOSURE REVIEW: The annual regional aggregates here are generated at the Bundesland level using weighted counts of natives and immigrants. All unweighted counts required for disclosure review are also reported, and any aggregated records for which the unweighted counts are less than 20 are dropped.
// Drop missing regions and display the total native and immigrant populations across regions and time
drop if ao_bula == 0
sort jahr ao_bula
tab jahr ao_bula if deutsch == 0
tab jahr ao_bula if deutsch == 0 [iw = tweight]
tab jahr ao_bula if deutsch == 1
tab jahr ao_bula if deutsch == 1 [iw = tweight]
// Calculate percentiles of the native wage distribution
forvalues p = 5(5)85{
	display "Percentile " `p'
	qui gen p`p' = .
	label variable p`p' "`p'th"
	forvalues y = 2000/2017 {
		//display "Year " `y'
		forvalues r = 1/10{
			//display "Region " `r'
			qui _pctile impwage1 [pw = tweight] if ao_bula == `r' & jahr == `y' & deutsch == 1, p(`p')
			qui replace p`p' = r(r1) if ao_bula == `r' & jahr == `y'
		}
	}
}
// Calculate immigration-to-native ratio
qui by jahr ao_bula: egen totpop = count(persnr)
label variable totpop "Total population, unweighted"
qui by jahr ao_bula: egen rtweight = total(tweight)
label variable rtweight "Total population, weighted"
qui by jahr ao_bula: egen natpop = total(deutsch)
label variable natpop "Native population, unweighted"
qui by jahr ao_bula: egen ntweight = total(tweight) if deutsch == 1
qui by jahr ao_bula: egen mntweight = max(ntweight)
qui replace ntweight = mntweight if ntweight == .
label variable ntweight "Native population, weighted"
qui by jahr ao_bula: egen impop = total(1 - deutsch)
label variable impop "Immigrant population, unweighted"
qui by jahr ao_bula: egen itweight = total(tweight) if deutsch == 0
qui by jahr ao_bula: egen mitweight = max(itweight)
qui replace itweight = mitweight if itweight == .
label variable itweight "Immigrant population, weighted"
qui gen imratio = itweight / ntweight
label variable imratio "Immigrant-to-native ratio"
// Calculate controls for changing age and education demographics
qui by jahr ao_bula: egen natage = total(tweight / ntweight * age) if deutsch == 1
qui by jahr ao_bula: egen mnatage = max(natage)
qui replace natage = mnatage if natage == .
label variable natage "Average native age"
qui by jahr ao_bula: egen image = total(tweight / itweight * age) if deutsch == 0
qui by jahr ao_bula: egen mimage = max(image)
qui replace image = mimage if image == .
label variable image "Average immigrant age"
qui gen leduc = educ == 1
qui by jahr ao_bula: egen leducpop = total(leduc * deutsch)
label variable leducpop "Low-educated native population"
qui gen meduc = educ == 2
qui by jahr ao_bula: egen meducpop = total(meduc * deutsch)
label variable meducpop "Medium-educated native population"
qui gen heduc = educ == 3
qui by jahr ao_bula: egen heducpop = total(heduc * deutsch)
label variable heducpop "High-educated native population"
qui by jahr ao_bula: egen leducshare = total(tweight / ntweight * leduc) if deutsch == 1
qui by jahr ao_bula: egen meducshare = total(tweight / ntweight * meduc) if deutsch == 1
qui by jahr ao_bula: egen heducshare = total(tweight / ntweight * heduc) if deutsch == 1
qui gen h2leduc = ln(heducshare / leducshare)
qui by jahr ao_bula: egen mh2leduc = max(h2leduc)
qui replace h2leduc = mh2leduc if h2leduc == .
label variable h2leduc "Log high- to low-educated native ratio"
qui gen m2leduc = ln(meducshare / leducshare)
qui by jahr ao_bula: egen mm2leduc = max(m2leduc)
qui replace m2leduc = mm2leduc if m2leduc == .
label variable m2leduc "Log medium- to low-educated native ratio"
// Calculate mean wages per region per year
qui by jahr ao_bula: egen avglog = total(tweight / ntweight * impwage1) if deutsch == 1
qui by jahr ao_bula: egen mavglog = max(avglog)
qui replace avglog = mavglog if avglog == .
label variable avg "Average log wage"
qui by jahr ao_bula: egen logavg = total(tweight / ntweight * exp(impwage1)) if deutsch == 1
qui replace logavg = ln(logavg)
qui by jahr ao_bula: egen mlogavg = max(logavg)
qui replace logavg = mlogavg if mlogavg == .
label variable avg "Log average wage"
// Keep one record per region per year
qui by jahr ao_bula: gen reg_id = 1 if _n == 1
keep if reg_id == 1
drop persnr
keep jahr ao_bula p* rdens totpop rtweight natpop ntweight impop itweight imratio natage image leducpop meducpop heducpop h2leduc m2leduc avglog logavg
sort ao_bula jahr
// Drop records for which unweighted counts are less than 20
count if totpop < 20
count if natpop < 20
count if impop < 20
count if leducpop < 20
count if meducpop < 20
count if heducpop < 20

* Save cleaned data and log file
// Automatically assign optimal data types on all variables
compress
// Save data set
save ${data}/1_describe.dta, replace
log close
clear
