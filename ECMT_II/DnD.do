/*

Econometrics II, Spring 2017
Abdulmohsen Almuhaisen

Showing the importance of the common trends assumption that underlies 
difference-in-differences estimates.

*/

*** clean everything and set the working directory ***
clear all
drop _all
capture cd "/Users/aalmuhaisen/Documents/R/ecmt/ecmt676"

*** set the seed and write the program ***
set seed 1991
capture program drop ddsim
program ddsim, rclass
	syntax, d(real)
	drop _all
	set obs 4000
	generate id = int((_n-1)/10) +1	//it=11,12,13,....,NT
	egen year = fill(1 2 3 4 5 6 7 8 9 10 1 2 3 4 5 6 7 8 9 10)
	generate T=1 
	replace T=0 if id>200
	generate P=1
	replace P=0 if year<=5
	generate TP=T*P
	generate e = rnormal(0,2)
	generate Y=1.2+`d'*year*T+2*T+1.5*P+2*TP+e	
	regress Y T P TP if year==5|year==6 //Since beta0 is constant across all i & t, using  
	return scalar b3 = _b[TP] //pooled OLS results in consistant & unbiased estimator.
	return scalar t = (_b[TP]-2)/_se[TP]
	return scalar reject = abs(return(t))>invttail(798,.025)
end

*** simulate betas ***
scalar j = 0
forvalues i = -0.60(0.20)0.61 {
local j = `j' + 1
simulate beta3_`j'=r(b3) t_stat_`j'=r(t) rejected_`j'=r(reject) ///
	,saving(data`j') reps(500) nolegend nodots: ddsim, d(`i')
}
 
*** merge data and summarize it *** 
use data1, clear
forvalues i = 2/7{
quietly merge 1:1 _n using data`i'
drop _merge
}
save data_all, replace

summarize, separator(3)

// Trend slope: 1= -0.6, 2= -0.4, 3= -0.2, 4= 0.0, 5= 0.2, 6= 0.4 and 7= 0.6

*** Remove created files ***
capture forvalues i = 1/7{
rm data`i'.dta
}
capture rm data_all.dta
