/*

Econometrics II, Spring 2017
Abdulmohsen Almuhaisen

Solving endogeneity using ivregress 2sls and manual 2sls and comparing the 
percentage of times this method results in an estimator equal to the true 
one (statistically) compared to simple OLS.
*/

*** clean everything and set the working directory ***
clear all
drop _all
capture cd "/Users/aalmuhaisen/Documents/R/ecmt/ecmt676"


*** set the seed and write the program ***
set seed 1991
capture program drop endosim
program endosim, rclass
	syntax, n(real)
	drop _all
	set obs `n'
	generate u = rnormal(0)
	generate e = rnormal(0)
	generate v = rnormal(0)
	generate z = rnormal(0)
	generate x = 0.4*z+0.7*u+v				
	generate y = 0.3+1.5*x+1.0*u+e
	regress y x z u 
	return scalar b1_dgp = _b[x]
	return scalar t_dgp = (_b[x]-1.5)/_se[x]
	return scalar reject_dgp = abs(return(t_dgp))>invttail(`n'-2,.025)
	regress y x
	return scalar b1_ols = _b[x]
	return scalar t_ols = (_b[x]-1.5)/_se[x]
	return scalar reject_ols = abs(return(t_ols))>invttail(`n'-2,.025)
	ivregress 2sls y (x=z)
	return scalar b1_iv = _b[x]
	return scalar t_iv = (_b[x]-1.5)/_se[x]
	return scalar reject_iv = abs(return(t_iv))>invttail(`n'-2,.025)
	regress x z
	predict double xhat
	regress y xhat
	return scalar b1_2sls = _b[xhat]
	return scalar t_2sls = (_b[xhat]-1.5)/_se[xhat]
	return scalar reject_2sls = abs(return(t_2sls))>invttail(`n'-2,.025)
	scalar rmse = e(rmse) //Calculate correct SE. 
	matrix varcov = e(V) //Credit goes to C. F. Baum https://goo.gl/ph1HcK
	scalar df = e(df_r)
	generate double residuals = (y - _b[xhat]*x - _b[_cons])^2
	quietly summarize residuals
	scalar rmsecorr = sqrt(r(sum) / df)
	scalar varcovcorr = (rmsecorr / rmse)^2 * varcov[1,1]
	return scalar b1_2sls_corr = _b[xhat]
	return scalar t_2sls_corr = (_b[xhat]-1.5)/sqrt(varcovcorr)
	return scalar reject_2sls_corr = abs(return(t_2sls_corr))>invttail(`n'-2,.025)
end

*** simulate betas ***
simulate b1_dgp=r(b1_dgp) tstat_dgp=r(t_dgp) reject_dgp=r(reject_dgp) ///
	b1_ols=r(b1_ols) tstat_ols=r(t_ols) reject_ols=r(reject_ols) ///
	b1_iv=r(b1_iv) tstat_iv=r(t_iv) reject_iv=r(reject_iv) b1_2sls=r(b1_2sls) ///
	tstat_2sls=r(t_2sls) reject_2sls=r(reject_2sls) b1_2sls_corr=r(b1_2sls_corr) ///
	tstat_2sls_corr=r(t_2sls_corr) reject_2sls_corr=r(reject_2sls_corr) ///
	,saving(coeffs) reps(1000) nolegend nodots: endosim, n(100)

summarize, separator(3)
 
/*
We can see that at 5% significance level, for the DGP, we reject the null 
(b1=1.5) only ~5%. For OLS, ~98% of the estimators weren't statistically equal 
to the true beta 1 (1.5). On the other hand, using ivregress 2sls or doing the 
process manually by regressing x on z then regressing y on xhat result in the 
same beta 1. However, since the manual 2sls doesn't account for the fact that
we are estimating beta 1 using xhat rather than x in the second step, the SE 
is not correct. After correcting the SE in the manual 2sls, we get almost 
identical percentage of rejection for ivregress & manual 2sls, which is ~4%. 
*/

*** Remove created files ***
capture rm coeffs.dta
