/*

Econometrics II
Abdulmohsen Almuhaisen

replication of Almond, Doyle, Kowalski, and Williams (2010) with extensions. 
*/

*** clean everything and set the working directory ***
clear all
drop _all


use "runandjump_sample1500g.dta"

*** PART A: TESTING THE VALIDITY OF THE RESEARCH DESIGN ***

generate bwtnorm = bweight-1500
generate bin01=floor(bwtnorm/1)*1 + 0.5 
generate bin10=floor(bwtnorm/10)*10 + 5 
generate bin25=floor(bwtnorm/25)*25 + 12.5

* Check if obs are pooled across the threshold
summarize bwtnorm if bin01==-0.5
summarize bwtnorm if bin01==0.5
summarize bwtnorm if bin10==-5
summarize bwtnorm if bin10==5
summarize bwtnorm if bin25==-12.5
summarize bwtnorm if bin25==12.5

* Plot histograms (assume data discrete for easly interpreted plot. The threshold is strictly part of the control)
histogram bin01, discrete width(1) frequency barwidth(1) ylabel(, format(%9.0g)) xtitle(Birth Weight) xline(0, lwidth(medthin) lpattern(dash) lcolor(red)) xlabel(-150(10)150, angle(forty_five)) title(The Distribution* of Birth Weights Around the 1500-gram Cutoff, size(medsmall)) subtitle(Normalized to 0-gram and using 1-grams width, size(small) margin(medsmall)) note(*Assumed to be discontinuous for easier interpretation., size(small) margin(medsmall)) scheme(sj)
histogram bin10, discrete width(10) frequency barwidth(10) ylabel(, format(%9.0g)) xtitle(Birth Weight) xline(0, lwidth(medthin) lpattern(dash) lcolor(red)) xlabel(-150(10)160, angle(forty_five)) title(The Distribution* of Birth Weights Around the 1500-gram Cutoff, size(medsmall)) subtitle(Normalized to 0-gram and using 10-grams width, size(small) margin(medsmall)) note(*Assumed to be discontinuous for easier interpretation., size(small) margin(medsmall)) scheme(sj)
histogram bin25, discrete width(25) frequency barwidth(25) ylabel(, format(%9.0g)) xtitle(Birth Weight) xline(0, lwidth(medthin) lpattern(dash) lcolor(red)) xlabel(-150(25)175, angle(forty_five)) title(The Distribution* of Birth Weights Around the 1500-gram Cutoff, size(medsmall)) subtitle(Normalized to 0-gram and using 25-grams width, size(small) margin(medsmall)) note(*Assumed to be discontinuous for easier interpretation., size(small) margin(medsmall)) scheme(sj)


* A.2

preserve
contract bin01, freq(fbin01)
generate D = 0
replace D = 1 if bin01 < 0 
generate Dbin01 = D*bin01
generate NDbin01 = (1-D)*bin01
regress fbin01 D Dbin01 NDbin01 if bin01>=-149.5 & bin01<=150.5, robust
regress fbin01 D Dbin01 NDbin01 if bin01>=-99.5 & bin01<=100.5, robust
regress fbin01 D Dbin01 NDbin01 if bin01>=-49.5 & bin01<=50.5, robust
restore

preserve
contract bin10, freq(fbin10)
generate D = 0
replace D = 1 if bin10 < 0 
generate Dbin10 = D*bin10
generate NDbin10 = (1-D)*bin10
regress fbin10 D Dbin10 NDbin10 if bin10>=-145 & bin10<=155, robust
regress fbin10 D Dbin10 NDbin10 if bin10>=-95 & bin10<=95, robust
regress fbin10 D Dbin10 NDbin10 if bin10>=-45 & bin10<=45, robust
restore

preserve
contract bin25, freq(fbin25)
generate D = 0
replace D = 1 if bin25 < 0 
generate Dbin25 = D*bin25
generate NDbin25 = (1-D)*bin25
regress fbin25 D Dbin25 NDbin25 if bin25>=-137.5 & bin25<=162.5, robust
regress fbin25 D Dbin25 NDbin25 if bin25>=-87.5 & bin25<=87.5, robust
regress fbin25 D Dbin25 NDbin25 if bin25>=-37.5 & bin25<=37.5, robust
restore

// Since the jump at the threshold is not statistically significant in any of the considered cases, we can concluded that the running variable is smooth across the threshold.

* A.3

// Mother race is white
preserve
label drop _all
replace mom_race=0 if mom_race!=1
collapse (mean) mom_race, by(bwtnorm)
generate D = 0
replace D = 1 if bwtnorm < 0
generate Dbwtnorm = D*bwtnorm
generate NDbwtnorm = (1-D)*bwtnorm
lpoly mom_race bwtnorm if D==1, ker(tri) gen(L) at(bwtnorm) nograph
lpoly mom_race bwtnorm if D==0, ker(tri) gen(R) at(bwtnorm) nograph
generate mom_race_Tri = 0
replace mom_race_Tri=L if D==1
replace mom_race_Tri=R if D==0
lpoly mom_race bwtnorm if D==1, ker(rec) gen(LRc) at(bwtnorm) nograph
lpoly mom_race bwtnorm if D==0, ker(rec) gen(RRc) at(bwtnorm) nograph
generate mom_race_Rec = 0
replace mom_race_Rec=LRc if D==1
replace mom_race_Rec=RRc if D==0
regress mom_race_Rec D Dbwtnorm NDbwtnorm if bwtnorm>=-90 & bwtnorm <=90
regress mom_race_Rec D Dbwtnorm NDbwtnorm if bwtnorm>=-60 & bwtnorm <=60
regress mom_race_Rec D Dbwtnorm NDbwtnorm if bwtnorm>=-30 & bwtnorm <=30
regress mom_race_Tri D Dbwtnorm NDbwtnorm if bwtnorm>=-90 & bwtnorm <=90
regress mom_race_Tri D Dbwtnorm NDbwtnorm if bwtnorm>=-60 & bwtnorm <=60
regress mom_race_Tri D Dbwtnorm NDbwtnorm if bwtnorm>=-30 & bwtnorm <=30
restore

// Mother education less than highschool
preserve
label drop _all
replace mom_ed=1 if mom_ed<12
replace mom_ed=0 if mom_ed!=1
collapse (mean) mom_ed, by(bwtnorm)
generate D = 0
replace D = 1 if bwtnorm < 0
generate Dbwtnorm = D*bwtnorm
generate NDbwtnorm = (1-D)*bwtnorm

lpoly mom_ed bwtnorm if D==1, ker(tri) gen(L) at(bwtnorm) nograph
lpoly mom_ed bwtnorm if D==0, ker(tri) gen(R) at(bwtnorm) nograph
generate mom_ed_Tri = 0
replace mom_ed_Tri=L if D==1
replace mom_ed_Tri=R if D==0

lpoly mom_ed bwtnorm if D==1, ker(rec) gen(LRc) at(bwtnorm) nograph
lpoly mom_ed bwtnorm if D==0, ker(rec) gen(RRc) at(bwtnorm) nograph
generate mom_ed_Rec = 0
replace mom_ed_Rec=LRc if D==1
replace mom_ed_Rec=RRc if D==0

regress mom_ed_Rec D Dbwtnorm NDbwtnorm if bwtnorm>=-90 & bwtnorm <=90
regress mom_ed_Rec D Dbwtnorm NDbwtnorm if bwtnorm>=-60 & bwtnorm <=60
regress mom_ed_Rec D Dbwtnorm NDbwtnorm if bwtnorm>=-30 & bwtnorm <=30
regress mom_ed_Tri D Dbwtnorm NDbwtnorm if bwtnorm>=-90 & bwtnorm <=90
regress mom_ed_Tri D Dbwtnorm NDbwtnorm if bwtnorm>=-60 & bwtnorm <=60
regress mom_ed_Tri D Dbwtnorm NDbwtnorm if bwtnorm>=-30 & bwtnorm <=30
restore



* A.4
// Mother Race

preserve
label drop _all
replace mom_race=0 if mom_race!=1
collapse (max) ounce (mean) mom_race, by(bwtnorm)
generate D = 0
replace D = 1 if bwtnorm < 0
generate Dbwtnorm = D*bwtnorm
generate NDbwtnorm = (1-D)*bwtnorm

regress mom_race D Dbwtnorm NDbwtnorm if bwtnorm>=-25 & bwtnorm <=25 & bwtnorm != 1
restore


preserve
label drop _all
replace mom_race=0 if mom_race!=1
collapse (max) ounce (mean) mom_race, by(bwtnorm)
generate D = 0
replace D = 1 if bwtnorm < 0
generate Dbwtnorm = D*bwtnorm
generate NDbwtnorm = (1-D)*bwtnorm

regress mom_race D Dbwtnorm NDbwtnorm if bwtnorm>=-25 & bwtnorm <=25 & ounce != 1
restore

// Mother education less than highschool
preserve
label drop _all
replace mom_ed=1 if mom_ed<12
replace mom_ed=0 if mom_ed!=1
collapse (max) ounce (mean) mom_ed, by(bwtnorm)
generate D = 0
replace D = 1 if bwtnorm < 0
generate Dbwtnorm = D*bwtnorm
generate NDbwtnorm = (1-D)*bwtnorm

regress mom_ed D Dbwtnorm NDbwtnorm if bwtnorm>=-25 & bwtnorm <=25 & bwtnorm != 1
restore


preserve
label drop _all
replace mom_ed=1 if mom_ed<12
replace mom_ed=0 if mom_ed!=1
collapse (max) ounce (mean) mom_ed, by(bwtnorm)
generate D = 0
replace D = 1 if bwtnorm < 0
generate Dbwtnorm = D*bwtnorm
generate NDbwtnorm = (1-D)*bwtnorm

regress mom_ed D Dbwtnorm NDbwtnorm if bwtnorm>=-25 & bwtnorm <=25 & ounce != 1
restore


*** PART B: ESTIMATED EFFECTS ON OUTCOMES ***
* B.1
preserve
collapse (mean) agedth5, by(bwtnorm)
generate D = 0
replace D = 1 if bwtnorm < 0
generate Dbwtnorm = D*bwtnorm
generate NDbwtnorm = (1-D)*bwtnorm

lpoly agedth5 bwtnorm if D==1, ker(tri) gen(L) at(bwtnorm) nograph
lpoly agedth5 bwtnorm if D==0, ker(tri) gen(R) at(bwtnorm) nograph
gen agedth5_Tri = 0
replace agedth5_Tri=L if D==1
replace agedth5_Tri=R if D==0

lpoly agedth5 bwtnorm if D==1, ker(rec) gen(LRc) at(bwtnorm) nograph
lpoly agedth5 bwtnorm if D==0, ker(rec) gen(RRc) at(bwtnorm) nograph
gen agedth5_Rec = 0
replace agedth5_Rec=LRc if D==1
replace agedth5_Rec=RRc if D==0

regress agedth5_Rec D Dbwtnorm NDbwtnorm if bwtnorm>=-90 & bwtnorm <=90, robust
regress agedth5_Rec D Dbwtnorm NDbwtnorm if bwtnorm>=-60 & bwtnorm <=60, robust
regress agedth5_Rec D Dbwtnorm NDbwtnorm if bwtnorm>=-30 & bwtnorm <=30, robust
regress agedth5_Tri D Dbwtnorm NDbwtnorm if bwtnorm>=-90 & bwtnorm <=90, robust
regress agedth5_Tri D Dbwtnorm NDbwtnorm if bwtnorm>=-60 & bwtnorm <=60, robust
regress agedth5_Tri D Dbwtnorm NDbwtnorm if bwtnorm>=-30 & bwtnorm <=30, robust
restore


* B.2
preserve
drop if bwtnorm == 0
collapse (mean) agedth5, by(bwtnorm)
generate D = 0
replace D = 1 if bwtnorm < 0
generate Dbwtnorm = D*bwtnorm
generate NDbwtnorm = (1-D)*bwtnorm

lpoly agedth5 bwtnorm if D==1, ker(tri) gen(L) at(bwtnorm) nograph
lpoly agedth5 bwtnorm if D==0, ker(tri) gen(R) at(bwtnorm) nograph
gen agedth5_Tri = 0
replace agedth5_Tri=L if D==1
replace agedth5_Tri=R if D==0

lpoly agedth5 bwtnorm if D==1, ker(rec) gen(LRc) at(bwtnorm) nograph
lpoly agedth5 bwtnorm if D==0, ker(rec) gen(RRc) at(bwtnorm) nograph
gen agedth5_Rec = 0
replace agedth5_Rec=LRc if D==1
replace agedth5_Rec=RRc if D==0

regress agedth5_Rec D Dbwtnorm NDbwtnorm if bwtnorm>=-90 & bwtnorm <=90, robust
regress agedth5_Rec D Dbwtnorm NDbwtnorm if bwtnorm>=-60 & bwtnorm <=60, robust
regress agedth5_Rec D Dbwtnorm NDbwtnorm if bwtnorm>=-30 & bwtnorm <=30, robust
regress agedth5_Tri D Dbwtnorm NDbwtnorm if bwtnorm>=-90 & bwtnorm <=90, robust
regress agedth5_Tri D Dbwtnorm NDbwtnorm if bwtnorm>=-60 & bwtnorm <=60, robust
regress agedth5_Tri D Dbwtnorm NDbwtnorm if bwtnorm>=-30 & bwtnorm <=30, robust
restore


* B.3
preserve
drop if bwtnorm == 0
drop if ounce == 1
collapse (mean) agedth5, by(bwtnorm)
generate D = 0
replace D = 1 if bwtnorm < 0
generate Dbwtnorm = D*bwtnorm
generate NDbwtnorm = (1-D)*bwtnorm

lpoly agedth5 bwtnorm if D==1, ker(tri) gen(L) at(bwtnorm) nograph
lpoly agedth5 bwtnorm if D==0, ker(tri) gen(R) at(bwtnorm) nograph
gen agedth5_Tri = 0
replace agedth5_Tri=L if D==1
replace agedth5_Tri=R if D==0

lpoly agedth5 bwtnorm if D==1, ker(rec) gen(LRc) at(bwtnorm) nograph
lpoly agedth5 bwtnorm if D==0, ker(rec) gen(RRc) at(bwtnorm) nograph
gen agedth5_Rec = 0
replace agedth5_Rec=LRc if D==1
replace agedth5_Rec=RRc if D==0

regress agedth5_Rec D Dbwtnorm NDbwtnorm if bwtnorm>=-90 & bwtnorm <=90, robust
regress agedth5_Rec D Dbwtnorm NDbwtnorm if bwtnorm>=-60 & bwtnorm <=60, robust
regress agedth5_Rec D Dbwtnorm NDbwtnorm if bwtnorm>=-30 & bwtnorm <=30, robust
regress agedth5_Tri D Dbwtnorm NDbwtnorm if bwtnorm>=-90 & bwtnorm <=90, robust
regress agedth5_Tri D Dbwtnorm NDbwtnorm if bwtnorm>=-60 & bwtnorm <=60, robust
regress agedth5_Tri D Dbwtnorm NDbwtnorm if bwtnorm>=-30 & bwtnorm <=30, robust
restore


* B.4
preserve
drop if bwtnorm == 0
collapse (max) ounce (mean) agedth5, by(bwtnorm)
generate D = 0
replace D = 1 if bwtnorm < 0
generate Dbwtnorm = D*bwtnorm
generate NDbwtnorm = (1-D)*bwtnorm
generate ODbwtnorm = D*bwtnorm*ounce
generate ONDbwtnorm = (1-D)*bwtnorm*ounce
generate OD=ounce*D
generate OND=ounce*(1-D)

lpoly agedth5 bwtnorm if D==1, ker(tri) gen(L) at(bwtnorm) nograph
lpoly agedth5 bwtnorm if D==0, ker(tri) gen(R) at(bwtnorm) nograph
gen agedth5_Tri = 0
replace agedth5_Tri=L if D==1
replace agedth5_Tri=R if D==0

lpoly agedth5 bwtnorm if D==1, ker(rec) gen(LRc) at(bwtnorm) nograph
lpoly agedth5 bwtnorm if D==0, ker(rec) gen(RRc) at(bwtnorm) nograph
gen agedth5_Rec = 0
replace agedth5_Rec=LRc if D==1
replace agedth5_Rec=RRc if D==0

regress agedth5_Rec D Dbwtnorm NDbwtnorm ODbwtnorm ONDbwtnorm OD OND if bwtnorm>=-90 & bwtnorm <=90, robust
regress agedth5_Rec D Dbwtnorm NDbwtnorm ODbwtnorm ONDbwtnorm OD OND if bwtnorm>=-60 & bwtnorm <=60, robust
regress agedth5_Rec D Dbwtnorm NDbwtnorm ODbwtnorm ONDbwtnorm OD OND if bwtnorm>=-30 & bwtnorm <=30, robust
regress agedth5_Tri D Dbwtnorm NDbwtnorm ODbwtnorm ONDbwtnorm OD OND if bwtnorm>=-90 & bwtnorm <=90, robust
regress agedth5_Tri D Dbwtnorm NDbwtnorm ODbwtnorm ONDbwtnorm OD OND if bwtnorm>=-60 & bwtnorm <=60, robust
regress agedth5_Tri D Dbwtnorm NDbwtnorm ODbwtnorm ONDbwtnorm OD OND if bwtnorm>=-30 & bwtnorm <=30, robust
restore
