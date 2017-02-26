/*

Econometrics II, Spring 2017
Abdulmohsen Almuhaisen

This Do file simulate the situaltion where OLS inconsistently estimate 
the coeffeicints when endogeneity exists and the solution using IV.

*/

*** clean everything and set the working directory ***
clear all
drop _all
capture cd "/Users/aalmuhaisen/Documents/R/ecmt/ecmt676"

*** set the seed and write the program ***
set seed 1991
capture program drop endosim
program endosim, rclass
	syntax, beta2(real)
	drop _all
	set obs 5000
	generate u = rnormal(0)
	generate e = rnormal(0)
	generate z = rnormal(0)
	generate x1 = 0.8*z+0.6*u				//eq. 1
	generate x2 = 0.8*z+0.1*u				//eq. 2
	generate y1 = 0.5+0.2*x1+`beta2'*u+e
	generate y2 = 0.5+0.2*x2+`beta2'*u+e
	regress y1 x1
	return scalar b1 = _b[x1]
	regress y2 x2
	return scalar b2 = _b[x2]
end

*** simulate beta 1 over different values of beta 2 many times using eq. 1 ***
scalar j = 0
forvalues beta2 = -1(.05)1.001 { 
local j = `j' + 1
simulate b16=r(b1) b2=(`beta2'),saving(first`j') reps(30) nolegend nodots: ///
	endosim, beta2(`beta2')
}

*** simulate beta 1 over different values of beta 2 many times using eq. 2 ***
scalar k = 0
forvalues beta2 = -1(.05)1.001 { 
local k = `k' + 1
simulate b11=r(b2) b2=(`beta2'),saving(second`k') reps(30) nolegend nodots: ///
	endosim, beta2(`beta2')
}

*** append files to each other to plot a scatter plot *** 
use first1, clear
forvalues i = 2/41{
append using first`i'
}
save first_all, replace

use second1, clear
forvalues i = 2/41{
append using second`i'
}
save second_all, replace

use first_all, clear
merge m:m b2 using second_all

*** Plot the scatter plot ***
twoway (scatter b16 b11 b2, msize(small small) msymbol(o s)), yline(0.2) ///
	title("Simulating Inconsistency Due To Model Misspecification", ///
	size(medium)) t1title(" ") ///
note("DGP: Y={&beta}{sub:0}+{&beta}{sub:1}X+{&beta}{sub:2}u+e" " " ///
	"Estimated model: Y={&beta}{sub:0}+{&beta}{sub:1}X+{&epsilon}") ///
ytitle({&beta}{sub:1}) ///
xtitle({&beta}{sub:2}) legend(label(1 "X=0.8Z+0.6u") label(2 "X=0.8Z+0.1u"))

*** Remove created files ***
forvalues i = 1/41{
rm first`i'.dta
rm second`i'.dta
}
rm first_all.dta
rm second_all.dta

***********************************************
*** Solving the issue by 2SLS using Z as IV ***
***********************************************

clear all
drop _all
capture cd "/Users/aalmuhaisen/Documents/R/ecmt/ecmt676"
set seed 1991
capture program drop endosim
program endosim, rclass
	syntax, beta2(real)
	drop _all
	set obs 5000
	generate u = rnormal(0)
	generate e = rnormal(0)
	generate z = rnormal(0)
	generate x1 = 0.8*z+0.6*u				//eq. 1
	generate x2 = 0.8*z+0.1*u				//eq. 2
	generate y1 = 0.5+0.2*x1+`beta2'*u+e
	generate y2 = 0.5+0.2*x2+`beta2'*u+e
	ivregress 2sls y1 (x1=z)
	return scalar b1 = _b[x1]
	ivregress 2sls y2 (x2=z)
	return scalar b2 = _b[x2]
end

*** simulate beta 1 over different values of beta 2 many times using eq. 1 ***
scalar j = 0
forvalues beta2 = -1(.05)1.001 { 
local j = `j' + 1
simulate b16=r(b1) b2=(`beta2'),saving(first`j') reps(30) nolegend nodots: ///
	endosim, beta2(`beta2')
}

*** simulate beta 1 over different values of beta 2 many times using eq. 2 ***
scalar k = 0
forvalues beta2 = -1(.05)1.001 { 
local k = `k' + 1
simulate b11=r(b2) b2=(`beta2'),saving(second`k') reps(30) nolegend nodots: ///
	endosim, beta2(`beta2')
}

*** append files to each other to plot a scatter plot *** 
use first1, clear
forvalues i = 2/41{
append using first`i'
}
save first_all, replace

use second1, clear
forvalues i = 2/41{
append using second`i'
}
save second_all, replace

use first_all, clear
merge m:m b2 using second_all

*** Plot the scatter plot ***
twoway (scatter b16 b11 b2, msize(small small) msymbol(o s)), yline(0.2) ///
	title("Simulating Consistency After Including Z as IV", ///
	size(medium)) t1title(" ") ///
note("DGP: Y={&beta}{sub:0}+{&beta}{sub:1}X+{&beta}{sub:2}u+e" " " ///
	"Estimated model: Step 1: X={&delta}Z+{&epsilon} Step 2: Y={&beta} `=ustrunescape("X\u0302")'+{&epsilon}") ///
ytitle({&beta}{sub:1}) ///
xtitle({&beta}{sub:2}) legend(label(1 "X=0.8Z+0.6u") label(2 "X=0.8Z+0.1u"))

*** Remove created files ***
forvalues i = 1/41{
rm first`i'.dta
rm second`i'.dta
}
rm first_all.dta
rm second_all.dta

*** The End! ***
