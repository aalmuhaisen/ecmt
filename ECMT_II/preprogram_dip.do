/*

Econometrics II, Spring 2017
Abdulmohsen Almuhaisen

Understanding the bias induced by an Ashenfelter Dip (preprogram dip) in a generalized 
difference in difference model.

*/

*** clean everything and set the working directory ***
clear all
drop _all
capture cd "/Users/aalmuhaisen/Documents/R/ecmt/ecmt676"

set seed 1991

*** the program ***
capture program drop ddsim
program ddsim, rclass
	drop _all
	tempfile id
	set obs 200
	gen id = _n
	gen a_i = rnormal(5,8)
	expand 15
	sort id
	save `id'
	drop _all
	
	tempfile year
	set obs 15
	gen year = _n
	gen a_t = rnormal(1,2)
	expand 200
	sort year
	egen id = fill(1(1)200 1(1)200)
	sort id
	save `year'
	drop _all

	
	use `id', clear
	merge 1:1 _n using `year'
	drop _merge
	sort id year
	
	gen u_it = rnormal(0,2)
	gen e_it = .
	forvalues i = 1(1)3000{
		if year[`i'] == 1{
			quietly replace e_it = u_it in `i'
		}
		else{
			quietly replace e_it = 0.5*u_it+0.25*e_it[`i'-1] in `i'
		}
	}
	
	gen Tr = 0
	forvalues i = 1(1)3000{
		if year[`i'] == 7{
			quietly replace Tr = uniform()<=0.5 in `i'
		}
	}

	forvalues i = 1(1)3000{
	local j = `i' + 1
	local k = `i' + 2
	local o = `i' + 3
	local p = `i' + 4
	local q = `i' + 5
	local r = `i' + 6
	local s = `i' + 7
	local t = `i' + 8
		if year[`i'] == 7{
			if Tr[`i'] == 1{
				quietly replace Tr = 1 in `j'
				quietly replace Tr = 1 in `k'
				quietly replace Tr = 1 in `o'
				quietly replace Tr = 1 in `p'
				quietly replace Tr = 1 in `q'
				quietly replace Tr = 1 in `r'
				quietly replace Tr = 1 in `s'
				quietly replace Tr = 1 in `t'
			}
		}
	}

	gen Ypre = a_i + a_t + e_it
	centile Ypre if year == 6
	gen medYpre = r(c_1)
	gen Tm = 0
	forvalues i = 1(1)3000{
	local j =`i' + 1
		if year[`i'] == 6{
			if Ypre[`i'] < medYpre{
			quietly replace Tm = 1 in `j'
			}
		}
	}

	forvalues i = 1(1)3000{
	local j = `i' + 1
	local k = `i' + 2
	local o = `i' + 3
	local p = `i' + 4
	local q = `i' + 5
	local r = `i' + 6
	local s = `i' + 7
	local t = `i' + 8
		if year[`i'] == 7{
			if Tm[`i'] == 1{
				quietly replace Tm = 1 in `j'
				quietly replace Tm = 1 in `k'
				quietly replace Tm = 1 in `o'
				quietly replace Tm = 1 in `p'
				quietly replace Tm = 1 in `q'
				quietly replace Tm = 1 in `r'
				quietly replace Tm = 1 in `s'
				quietly replace Tm = 1 in `t'
			}
		}
	}


	gen TC6 = 0
	forvalues i = 1(1)3000{
		if year[`i'] == 6{
			if Ypre[`i'] < medYpre{
				quietly replace TC6 = 1 in `i'
			}		
		}
	}

	gen Yr = a_i + a_t + 1.5*Tr + e_it
	gen Ym = a_i + a_t + 1.5*Tm + e_it

	xtset id year
	xtreg Yr Tr i.year, fe
		return scalar theta_ho = _b[Tr] 
		return scalar se_ho = _se[Tr]
		return scalar t_ho = (_b[Tr]-1.5)/_se[Tr]
		return scalar reject_ho = abs(return(t_ho))>invttail(2793,.025)
	reg Yr Tr i.year i.id,  robust
		return scalar theta_ht = _b[Tr] 
		return scalar se_ht = _se[Tr]
		return scalar t_ht = (_b[Tr]-1.5)/_se[Tr]
		return scalar reject_ht = abs(return(t_ht))>invttail(2793,.025)
	xtreg Yr Tr i.year, fe vce(cluster id)
		return scalar theta_cl = _b[Tr] 
		return scalar se_cl = _se[Tr]
		return scalar t_cl = (_b[Tr]-1.5)/_se[Tr]
		return scalar reject_cl = abs(return(t_cl))>invttail(2793,.025)
	xtreg Ym Tm i.year, fe vce(cluster id)
		return scalar theta_mcl = _b[Tm] 
		return scalar se_mcl = _se[Tm]
		return scalar t_mcl = (_b[Tm]-1.5)/_se[Tm]
		return scalar reject_mcl = abs(return(t_mcl))>invttail(2793,.025)	
	xtreg Ym Tm TC6 i.year, fe vce(cluster id) // Control for Year 6 for 
		return scalar beta_mclC = _b[TC6] 	   // treatment group only
		return scalar seb_mclC = _se[TC6]
		return scalar tb_mclC = (_b[TC6])/_se[TC6]
		return scalar rejectb_mclC = abs(return(tb_mclC))>invttail(2793,.025)
		return scalar theta_mcl6C = _b[Tm] 	  
		return scalar se_mcl6C = _se[Tm]
		return scalar t_mcl6C = (_b[Tm]-1.5)/_se[Tm]
		return scalar reject_mcl6C = abs(return(t_mcl6C))>invttail(2793,.025)
end

*** simulate betas ***

simulate theta_rho=r(theta_ho) se_ho=r(se_ho) t_stat_rho=r(t_ho) reject_rho=r(reject_ho) ///
		 theta_rht=r(theta_ht) se_rht=r(se_ht) t_stat_rht=r(t_ht) reject_rht=r(reject_ht) ///
		 theta_rcl=r(theta_cl) se_rcl=r(se_cl) t_stat_rcl=r(t_cl) reject_rcl=r(reject_cl) ///
		 theta_mcl=r(theta_mcl) se_mcl=r(se_mcl) t_stat_mcl=r(t_mcl) reject_mcl=r(reject_mcl) ///
		 beta_mclC=r(beta_mclC) seb_mclC=r(seb_mclC) tb_stat_mclC=r(tb_mclC) rejectb_mclC=r(rejectb_mclC) ///
		 theta_mcl6C=r(theta_mcl6C) se_mcl6C=r(se_mcl6C) t_stat_mcl6C=r(t_mcl6C) reject_mcl6C=r(reject_mcl6C) ///
		 ,reps(500) nolegend nodots: ddsim ///

sum, sep(4)

