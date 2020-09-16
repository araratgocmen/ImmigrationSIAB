* Estimate the impact of immigration along the native wage distribution using annual regional aggregates (at the Bundesland level)
* Ararat Gocmen
* Version: 2020-09-01
* d2_regress.do

* Manage files
// Close the existing log file
capture log close
// Create a new log file
log using ${log}/d2_regress.log, replace

* Load data
use ${data}/1_describe.dta, clear
sort ao_bula jahr
xtset ao_bula jahr
xtline imratio
graph export ${log}/d2_regress_imratio.png, replace

* Summary statistics
*** NOTE FOR DISCLOSURE REVIEW: The following two tables 
// Percentiles of the native wage distribution, immigrant-to-native ratio, annual absolute change in the ratio, and all controls
qui gen diffimratio = D.imratio
label variable diffimratio "Annual change in the immigrant-to-native ratio"
eststo clear
eststo: estpost tabstat p5-p85 imratio diffimratio natage image h2leduc m2leduc if jahr >= 2011, statistics(mean sd min max) columns(statistics)
esttab * using ${log}/d2_regress_summary.tex, replace cells("mean(fmt(3)) sd(fmt(3)) min(fmt(3)) max(fmt(3))") label varwidth(50) nonumbers noobs
// Annual relative change in the immigrant-to-native ratio
qui gen pctdiffimratio = 100 * D.imratio
label variable pctdiffimratio "Annual percentage-point change in immigrant-to-native ratio"
eststo clear
eststo: estpost tabstat pctdiffimratio if jahr >= 2001, by(jahr) statistics(mean sd min max) nototal
esttab * using ${log}/d2_regress_summary.tex, append cells("mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2))") nonumbers noobs
eststo clear
eststo: estpost tabstat pctdiffimratio if inrange(jahr, 2001, 2010), statistics(mean sd min max)
esttab * using ${log}/d2_regress_summary.tex, append cells("mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2))") nonumbers noobs rename(pctdiffimratio "Average 2001-2010")
eststo clear
eststo: estpost tabstat pctdiffimratio if inrange(jahr, 2011, 2017), statistics(mean sd min max)
esttab * using ${log}/d2_regress_summary.tex, append cells("mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2))") nonumbers noobs rename(pctdiffimratio "Average 2011-2017")
qui gen totpctdiffimratio = 100 * S7.imratio if jahr == 2017
eststo clear
eststo: estpost tabstat totpctdiffimratio if jahr == 2017, statistics(mean sd min max)
esttab * using ${log}/d2_regress_summary.tex, append cells("mean(fmt(2)) sd(fmt(2)) min(fmt(3)) max(fmt(2))") nonumbers noobs rename(totpctdiffimratio "2010-2017")
drop diffimratio pctdiffimratio totpctdiffimratio

* Regressions
*** NOTE FOR DISCLOSURE REVIEW: The following regressions are of the log wage growth at each percentile (e.g. D.p5) on the difference in the immigrant-to-native ratio (D.imratio), controlling for changes in the mean age of the native population (D.natage), the mean age of the immigrant population (D.image), the ratio of high- to low-educated natives (D.h2leduc), and the ratio of medium- to low-educated natives (D.m2leduc). Standard errors are clustered at the regional level
// Define new variables to store estimates and confidence intervals
qui gen coef = .
label variable coef "IV coefficient"
qui gen lower = .
label variable lower "95% confidence interval"
qui gen upper = .
qui gen ar1 = .
qui gen ar1p = .
qui gen ar2 = .
qui gen ar2p = .
local i = 5
// Run the regressions
foreach p of varlist p5-p85 {
	eststo clear
	local lb: variable label `p'
	// OLS, no controls
	qui xi: reg D.(`p' imratio) i.jahr, noc vce(cluster ao_bula)
	eststo m`p'OLS
	// OLS, controls
	qui xi: reg D.(`p' imratio natage image h2leduc m2leduc) i.jahr, noc vce(cluster ao_bula)
	eststo m`p'OLSc
	// IV, no controls
	qui xi: ivreg2 D.`p' i.jahr (D.imratio = L4.imratio), noc cluster(ao_bula) partial(i.jahr) first small
	eststo m`p'IV
	qui estadd scalar F_IV = e(widstat)
	// IV, controls
	qui xi: ivreg2 D.`p' D.natage D.image D.h2leduc D.m2leduc i.jahr (D.imratio = L4.imratio), noc cluster(ao_bula) partial(i.jahr) first small
	eststo m`p'IVc
	qui estadd scalar F_IV = e(widstat)
	// OLS, no controls, after 2010
	qui xi: reg D.(`p' imratio) i.jahr if jahr >= 2011, noc vce(cluster ao_bula)
	eststo m`p'OLS11
	// OLS, controls, after 2010
	qui xi: reg D.(`p' imratio natage image h2leduc m2leduc) i.jahr if jahr >= 2011, noc vce(cluster ao_bula)
	eststo m`p'OLS11c
	// IV, no controls, after 2010
	qui xi: ivreg2 D.`p' i.jahr (D.imratio = L4.imratio) if jahr >= 2011, noc cluster(ao_bula) partial(i.jahr) first small
	eststo m`p'IV11
	qui estadd scalar F_IV = e(widstat)
	// IV, controls, after 2010
	qui xi: ivreg2 D.`p' D.natage D.image D.h2leduc D.m2leduc i.jahr (D.imratio = L4.imratio) if jahr >= 2011, noc cluster(ao_bula) partial(i.jahr) first small
	eststo m`p'IV11c
	// Store the estimates from the IV regression with controls after 2010
	qui estadd scalar F_IV = e(widstat)
	qui replace coef = _b[D.imratio] if ptil == `i'
	qui replace lower = _b[D.imratio] - invttail(e(Fdf2), 0.025) * _se[D.imratio] if ptil == `i'
	qui replace upper = _b[D.imratio] + invttail(e(Fdf2), 0.025) * _se[D.imratio] if ptil == `i'
	// Perform Arellano-Bond tests for serial correlation in the residuals and store the results
	qui abar, lag(2)
	qui replace ar1 = r(ar1) if ptil == `i'
	qui replace ar1p = r(ar1p) if ptil == `i'
	qui replace ar2 = r(ar2) if ptil == `i'
	qui replace ar2p = r(ar2p) if ptil == `i'
	// Tabulate and save the results
	if `i' == 5 {
		esttab m*, keep(`lb') rename(D.imratio `lb') se star(* 0.10 ** 0.05 *** 0.01) nonote noobs noeqlines modelwidth(8) mgroups("OLS" "IV" "OLS>2010" "IV>2010", pattern(1 0 1 0 1 0 1 0)) mlabels(none) varwidth(14)
		esttab m* using ${log}/d2_regress_results.tex, replace keep(`lb') rename(D.imratio `lb') se star(* 0.10 ** 0.05 *** 0.01) nonote noobs noeqlines modelwidth(8) mgroups("OLS" "IV" "OLS>2010" "IV>2010", pattern(1 0 1 0 1 0 1 0)) mlabels(none) varwidth(14)
	}
	if (`i' != 5 & `i' != 85) {
		esttab m*, keep(`lb') rename(D.imratio `lb') se star(* 0.10 ** 0.05 *** 0.01) nonote noobs nolines modelwidth(8) nomtitles nonumber mlabels(none) varwidth(14)
		esttab m* using ${log}/d2_regress_results.tex, append keep(`lb') rename(D.imratio `lb') se star(* 0.10 ** 0.05 *** 0.01) nonote noobs nolines modelwidth(8) nomtitles nonumber mlabels(none) varwidth(14)
	}
	if `i' == 85 {
		esttab m*, keep(`lb') scalar(F_IV) rename(D.imratio `lb') se star(* 0.10 ** 0.05 *** 0.01) indicate("Year dummies = _I*" "Other controls = D.natage") modelwidth(8) nomtitles nonumber mlabels(none) varwidth(14)
		esttab m* using ${log}/d2_regress_results.tex, append keep(`lb') scalar(F_IV) rename(D.imratio `lb') se star(* 0.10 ** 0.05 *** 0.01) indicate("Year dummies = _I*" "Other controls = D.natage") modelwidth(8) nomtitles nonumber mlabels(none) varwidth(14)
	}
	local i = `i' + 5
}
sort ptil
twoway (line coef ptil) (line lower ptil, lpattern(dash) lcolor(navy)) (line upper ptil, lpattern(dash) lcolor(navy)), yline(0, lcolor(maroon)) xlabel(0(20)100 5 85) legend(order(1 2))
graph export ${log}/d2_regress_coef.png, replace

* Display the Arellano-Bond test results
// First-order serial correlation, with t-statistics in the first column and p-values in the second
eststo clear
eststo: estpost tabstat ar1 ar1p, by(ptil) statistics(mean) nototal
drop ar1 ar1p
rename ar2 ar1
// Second-order serial correlation, with t-statistics in the first column and p-values in the second
rename ar2p ar1p
eststo: estpost tabstat ar1 ar1p, by(ptil) statistics(mean) nototal
esttab * using ${log}/d2_regress_results.tex, append main(ar1) aux(ar1p) nonumbers noobs nonote mtitles("First-Order" "Second-Order")
drop ar1 ar1p

* Regress the IV coefficients on the relative density of immigrants to estimate the elasticity of subtitution between different labor types
// Regression
qui by ptil: gen elassubreg = 1 if _n == 1
eststo clear
qui reg coef rdens if elassubreg == 1
eststo m1
esttab m1, keep(rdens) se nostar b(%8.3f) mlabels(none) varwidth(40) label
// Structural parameter
display _b[rdens] + 1
// Elasticity of substitution
display - 1 / _b[rdens]
// Plot
twoway (scatter coef rdens) (lfit coef rdens) if elassubreg == 1, legend(off) ytitle("IV coefficient")
graph export ${log}/d2_regress_elassub.png, replace
drop coef lower upper elassubreg

* Save log file
log close
clear
