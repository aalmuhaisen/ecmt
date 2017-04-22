/*
Abdulmohsen Amuhaisen
Econometrics II

Replicating Krueger (1999)'s results with extension 

*/

clear

use "stardata.dta"

quietly  destring *,replace
quietly  recode * (9=.)
quietly  recode * (99=.)
quietly  recode * (999=.)
quietly  recode * (9999=.)
quietly  recode * (99999=.)
quietly  recode * (999999=.)
quietly  recode * (9999999=.)
quietly  recode * (99999999=.)
quietly  recode * (999999999=.)

*** Q1: Descriptive Statistics ***
* a. recode some variables
quietly recode ses* (2=0) //Free lunch
quietly recode race (2=0) 
quietly recode race (3=1) 
quietly recode race (4=0) 
quietly recode race (5=0) 
quietly recode race (6=0) 

generate agein1985 = 1985-yob+(9-((qob*3+(qob-1)*3)/2))/12 

gen attrk = 0
replace attrk = 1 if stark == 1 & star1 ==2 | star2 ==2 | star3 ==2
replace attrk = . if stark == 2

gen attr1 = 0
replace attr1 = 1 if stark ==2 & star1 ==1 & star2 ==2 | star3 ==2
replace attr1 = . if star1 == 2 | stark == 1

gen attr2 = 0
replace attr2 = 1 if stark ==2 & star1 ==2 & star2 ==1  & star3 ==2
replace attr2 = . if stark == 1 | star1 == 1 | star2 == 2

gen attr3 = 0
replace attr3 = 1 if stark ==2 & star1 ==2 & star2 ==2  & star3 ==1
replace attr3 = . if stark == 1 | star1 == 1 | star2 == 1 | star3 == 2

pctile pcreadk = readk if (ctypek==2 | ctypek==3), nq(100)
pctile pcmathk = mathk if (ctypek==2 | ctypek==3), nq(100)

xtile preadk = readk, cutpoints(pcreadk)
xtile pmathk = mathk, cutpoints(pcmathk)

gen avgk = (preadk+pmathk)/2
tabulate ctypek if attrk!=.,summarize(avgk)


kdensity avgk, nograph generate(x fx)
kdensity avgk if ctypek==1, nograph generate(fx0) at(x)
kdensity avgk if ctypek==2, nograph generate(fx1) at(x)
label var fx0 "Small"
label var fx1 "Regular"
line fx0 fx1 x, sort ytitle(Density)

//drop pcreadk pcmathk pcwordk avgk preadk pmathk pwordk
drop x fx fx0 fx1
*1
pctile pcread1 = read1 if (ctype1==2 | ctype1==3), nq(100)
pctile pcmath1 = math1 if (ctype1==2 | ctype1==3), nq(100)

xtile pread1 = read1 , cutpoints(pcread1)
xtile pmath1 = math1 , cutpoints(pcmath1)

gen avg1 = (pread1+pmath1)/2
tabulate ctype1 if attr1!=.,summarize(avg1)


kdensity avg1, nograph generate(x fx)
kdensity avg1 if ctype1==1, nograph generate(fx0) at(x)
kdensity avg1 if ctype1==2, nograph generate(fx1) at(x)
label var fx0 "Small"
label var fx1 "Regular"
line fx0 fx1 x, sort ytitle(Density)

//drop pcread1 pcmath1 pcword1 avg1 pread1 pmath1 pword1
drop x fx fx0 fx1


*2
pctile pcread2 = read2 if (ctype2==2 | ctype2==3), nq(100)
pctile pcmath2 = math2 if (ctype2==2 | ctype2==3), nq(100)

xtile pread2 = read2 , cutpoints(pcread2)
xtile pmath2 = math2 , cutpoints(pcmath2)

gen avg2 = (pread2+pmath2)/2
tabulate ctype2 if attr2!=.,summarize(avg2)


kdensity avg2, nograph generate(x fx)
kdensity avg2 if ctype2==1, nograph generate(fx0) at(x)
kdensity avg2 if ctype2==2, nograph generate(fx1) at(x)
label var fx0 "Small"
label var fx1 "Regular"
line fx0 fx1 x, sort ytitle(Density)

//drop pcread2 pcmath2 pcword2 avg2 pread2 pmath2 pword2
drop x fx fx0 fx1

*3
pctile pcread3 = read3 if (ctype3==2 | ctype3==3), nq(100)
pctile pcmath3 = math3 if (ctype3==2 | ctype3==3), nq(100)

xtile pread3 = read3 , cutpoints(pcread3)
xtile pmath3 = math3 , cutpoints(pcmath3)

gen avg3 = (pread3+pmath3)/2
tabulate ctype3 if attr3!=.,summarize(avg3)


kdensity avg3, nograph generate(x fx)
kdensity avg3 if ctype3==1, nograph generate(fx0) at(x)
kdensity avg3 if ctype3==2, nograph generate(fx1) at(x)
label var fx0 "Small"
label var fx1 "Regular"
line fx0 fx1 x, sort ytitle(Density)

//drop pcread3 pcmath3 pcword3 avg3 pread3 pmath3 pword3
drop x fx fx0 fx1


* b)
//Free Lunch
tabulate ctypek if attrk!=., summarize(sesk)
tabulate ctype1 if attr1!=., summarize(ses1)
tabulate ctype2 if attr2!=., summarize(ses2)
tabulate ctype3 if attr3!=., summarize(ses3)
//Race
tabulate ctypek if attrk!=., summarize(race)
tabulate ctype1 if attr1!=., summarize(race)
tabulate ctype2 if attr2!=., summarize(race)
tabulate ctype3 if attr3!=., summarize(race)
//age in 1985
tabulate ctypek if attrk!=., summarize(agein1985)
tabulate ctype1 if attr1!=., summarize(agein1985)
tabulate ctype2 if attr2!=., summarize(agein1985)
tabulate ctype3 if attr3!=., summarize(agein1985)
//Attrition
tabulate ctypek if attrk!=., summarize(attrk)
tabulate ctype1 if attr1!=., summarize(attr1)
tabulate ctype2 if attr2!=., summarize(attr2)
tabulate ctype3 if attr3!=., summarize(attr3)
//Class Size
tabulate ctypek if attrk!=., summarize(csizek)
tabulate ctype1 if attr1!=., summarize(csize1)
tabulate ctype2 if attr2!=., summarize(csize2)
tabulate ctype3 if attr3!=., summarize(csize3)
//Score
tabulate ctypek if attrk!=., summarize(avgk)
tabulate ctype1 if attr1!=., summarize(avg1)
tabulate ctype2 if attr2!=., summarize(avg2)
tabulate ctype3 if attr3!=., summarize(avg3)
* c)
/*
before recoding will creat some variables to be used in Q3
*/
gen tmask = 0
replace tmask = 1 if hdegk == 3
gen tmas1 = 0
replace tmas1 = 1 if hdeg1 == 3
gen tmas2 = 0
replace tmas2 = 1 if hdeg2 == 3
gen tmas3 = 0
replace tmas3 = 1 if hdeg3 == 3

gen whitek = 0
replace whitek = 1 if tracek == 1
gen white1 = 0
replace white1 = 1 if trace1 == 1
gen white2 = 0
replace white2 = 1 if trace2 == 1
gen white3 = 0
replace white3 = 1 if trace3 == 1


* Now we can recode teacher's race and highest degree
quietly recode trace* (2=0) 
quietly recode trace* (3=1) 
quietly recode trace* (4=0) 
quietly recode trace* (5=0) 
quietly recode trace* (6=0) 

quietly recode hdeg* (1=0)
quietly recode hdeg* (2=1)
quietly recode hdeg* (3=1)
quietly recode hdeg* (4=1)

//Teacher Race
tabulate ctypek if attrk!=., summarize(tracek)
tabulate ctype1 if attr1!=., summarize(trace1)
tabulate ctype2 if attr2!=., summarize(trace2)
tabulate ctype3 if attr3!=., summarize(trace3)

// Teacher Education
tabulate ctypek if attrk!=., summarize(hdegk)
tabulate ctype1 if attr1!=., summarize(hdeg1)
tabulate ctype2 if attr2!=., summarize(hdeg2)
tabulate ctype3 if attr3!=., summarize(hdeg3)

// Teacher Experince
tabulate ctypek if attrk!=., summarize(totexpk)
tabulate ctype1 if attr1!=., summarize(totexp1)
tabulate ctype2 if attr2!=., summarize(totexp2)
tabulate ctype3 if attr3!=., summarize(totexp3)


* d)
//Free Lunch
quietly reg sesk i.ctypek if attrk!=., vce(cluster sysidk)
test 1.ctypek=2.ctypek=3.ctypek=0
quietly reg ses1 i.ctype1 if attr1!=., vce(cluster sysid1)
test 1.ctype1=2.ctype1=3.ctype1=0
quietly reg ses2 i.ctype2 if attr2!=., vce(cluster sysid2)
test 1.ctype2=2.ctype2=3.ctype2=0
quietly reg ses3 i.ctype3 if attr3!=., vce(cluster sysid3)
test 1.ctype3=2.ctype3=3.ctype3=0

//Race
quietly reg race i.ctypek if attrk!=., vce(cluster sysidk)
test 1.ctypek=2.ctypek=3.ctypek=0
quietly reg race i.ctype1 if attr1!=., vce(cluster sysid1)
test 1.ctype1=2.ctype1=3.ctype1=0
quietly reg race i.ctype2 if attr2!=., vce(cluster sysid2)
test 1.ctype2=2.ctype2=3.ctype2=0
quietly reg race i.ctype3 if attr3!=., vce(cluster sysid3)
test 1.ctype3=2.ctype3=3.ctype3=0

//age in 1985
quietly reg agein1985 i.ctypek if attrk!=., vce(cluster sysidk)
test 1.ctypek=2.ctypek=3.ctypek=0
quietly reg agein1985 i.ctype1 if attr1!=., vce(cluster sysid1)
test 1.ctype1=2.ctype1=3.ctype1=0
quietly reg agein1985 i.ctype2 if attr2!=., vce(cluster sysid2)
test 1.ctype2=2.ctype2=3.ctype2=0
quietly reg agein1985 i.ctype3 if attr3!=., vce(cluster sysid3)
test 1.ctype3=2.ctype3=3.ctype3=0

//Attrition
quietly reg attrk i.ctypek if attrk!=., vce(cluster sysidk)
test 1.ctypek=2.ctypek=3.ctypek=0
quietly reg attr1 i.ctype1 if attr1!=., vce(cluster sysid1)
test 1.ctype1=2.ctype1=3.ctype1=0
quietly reg attr2 i.ctype2 if attr2!=., vce(cluster sysid2)
test 1.ctype2=2.ctype2=3.ctype2=0
quietly reg attr3 i.ctype3 if attr3!=., vce(cluster sysid3)
test 1.ctype3=2.ctype3=3.ctype3=0

//Class Size
quietly reg csizek i.ctypek if attrk!=., vce(cluster sysidk)
test 1.ctypek=2.ctypek=3.ctypek=0
quietly reg csize1 i.ctype1 if attr1!=., vce(cluster sysid1)
test 1.ctype1=2.ctype1=3.ctype1=0
quietly reg csize2 i.ctype2 if attr2!=., vce(cluster sysid2)
test 1.ctype2=2.ctype2=3.ctype2=0
quietly reg csize3 i.ctype3 if attr3!=., vce(cluster sysid3)
test 1.ctype3=2.ctype3=3.ctype3=0

//Score
quietly reg avgk i.ctypek if attrk!=., vce(cluster sysidk)
test 1.ctypek=2.ctypek=3.ctypek=0
quietly reg avg1 i.ctype1 if attr1!=., vce(cluster sysid1)
test 1.ctype1=2.ctype1=3.ctype1=0
quietly reg avg2 i.ctype2 if attr2!=., vce(cluster sysid2)
test 1.ctype2=2.ctype2=3.ctype2=0
quietly reg avg3 i.ctype3 if attr3!=., vce(cluster sysid3)
test 1.ctype3=2.ctype3=3.ctype3=0

//Teacher Race
quietly reg tracek i.ctypek if attrk!=., vce(cluster sysidk)
test 1.ctypek=2.ctypek=3.ctypek=0
quietly reg trace1 i.ctype1 if attr1!=., vce(cluster sysid1)
test 1.ctype1=2.ctype1=3.ctype1=0
quietly reg trace2 i.ctype2 if attr2!=., vce(cluster sysid2)
test 1.ctype2=2.ctype2=3.ctype2=0
quietly reg trace3 i.ctype3 if attr3!=., vce(cluster sysid3)
test 1.ctype3=2.ctype3=3.ctype3=0

// Teacher Education
quietly reg hdegk i.ctypek if attrk!=., vce(cluster sysidk)
test 1.ctypek=2.ctypek=3.ctypek=0
quietly reg hdeg1 i.ctype1 if attr1!=., vce(cluster sysid1)
test 1.ctype1=2.ctype1=3.ctype1=0
quietly reg hdeg2 i.ctype2 if attr2!=., vce(cluster sysid2)
test 1.ctype2=2.ctype2=3.ctype2=0
quietly reg hdeg3 i.ctype3 if attr3!=., vce(cluster sysid3)
test 1.ctype3=2.ctype3=3.ctype3=0

// Teacher Experince
quietly reg totexpk i.ctypek if attrk!=., vce(cluster sysidk)
test 1.ctypek=2.ctypek=3.ctypek=0
quietly reg totexp1 i.ctype1 if attr1!=., vce(cluster sysid1)
test 1.ctype1=2.ctype1=3.ctype1=0
quietly reg totexp2 i.ctype2 if attr2!=., vce(cluster sysid2)
test 1.ctype2=2.ctype2=3.ctype2=0
quietly reg totexp3 i.ctype3 if attr3!=., vce(cluster sysid3)
test 1.ctype3=2.ctype3=3.ctype3=0

*** Q2: Random Assignment Tests ***
//Free Lunch
quietly regress sesk i.ctypek i.sysidk if attrk!=., vce(cluster sysidk)
test 1.ctypek=2.ctypek=3.ctypek=0

quietly regress ses1 i.ctype1 i.sysid1 if attr1!=., vce(cluster sysid1)
test 1.ctype1=2.ctype1=3.ctype1=0

quietly regress ses2 i.ctype2 i.sysid2 if attr2!=., vce(cluster sysid2)
test 1.ctype2=2.ctype2=3.ctype2=0

quietly regress ses3 i.ctype3 i.sysid3 if attr3!=., vce(cluster sysid3)
test 1.ctype3=2.ctype3=3.ctype3=0


//Race
quietly regress race i.ctypek i.sysidk if attrk!=., vce(cluster sysidk)
test 1.ctypek=2.ctypek=3.ctypek=0

quietly regress race i.ctype1 i.sysid1 if attr1!=., vce(cluster sysid1)
test 1.ctype1=2.ctype1=3.ctype1=0

quietly regress race i.ctype2 i.sysid2 if attr2!=., vce(cluster sysid2)
test 1.ctype2=2.ctype2=3.ctype2=0

quietly regress race i.ctype3 i.sysid3 if attr3!=., vce(cluster sysid3)
test 1.ctype3=2.ctype3=3.ctype3=0


//age in 1985
quietly regress agein1985 i.ctypek i.sysidk if attrk!=., vce(cluster sysidk)
test 1.ctypek=2.ctypek=3.ctypek=0

quietly regress agein1985 i.ctype1 i.sysid1 if attr1!=., vce(cluster sysid1)
test 1.ctype1=2.ctype1=3.ctype1=0

quietly regress agein1985 i.ctype2 i.sysid2 if attr2!=., vce(cluster sysid2)
test 1.ctype2=2.ctype2=3.ctype2=0

quietly regress agein1985 i.ctype3 i.sysid3 if attr3!=., vce(cluster sysid3)
test 1.ctype3=2.ctype3=3.ctype3=0


//Attrition
quietly regress attrk i.ctypek i.sysidk if attrk!=., vce(cluster sysidk)
test 1.ctypek=2.ctypek=3.ctypek=0

quietly regress attr1 i.ctype1 i.sysid1 if attr1!=., vce(cluster sysid1)
test 1.ctype1=2.ctype1=3.ctype1=0

quietly regress attr2 i.ctype2 i.sysid2 if attr2!=., vce(cluster sysid2)
test 1.ctype2=2.ctype2=3.ctype2=0

quietly regress attr3 i.ctype3 i.sysid3 if attr3!=., vce(cluster sysid3)
test 1.ctype3=2.ctype3=3.ctype3=0


//Class Size
quietly regress csizek i.ctypek i.sysidk if attrk!=., vce(cluster sysidk)
test 1.ctypek=2.ctypek=3.ctypek=0

quietly regress csize1 i.ctype1 i.sysid1 if attr1!=., vce(cluster sysid1)
test 1.ctype1=2.ctype1=3.ctype1=0

quietly regress csize2 i.ctype2 i.sysid2 if attr2!=., vce(cluster sysid2)
test 1.ctype2=2.ctype2=3.ctype2=0

quietly regress csize3 i.ctype3 i.sysid3 if attr3!=., vce(cluster sysid3)
test 1.ctype3=2.ctype3=3.ctype3=0


//Score
quietly regress avgk i.ctypek i.sysidk if attrk!=., vce(cluster sysidk)
test 1.ctypek=2.ctypek=3.ctypek=0

quietly regress avg1 i.ctype1 i.sysid1 if attr1!=., vce(cluster sysid1)
test 1.ctype1=2.ctype1=3.ctype1=0

quietly regress avg2 i.ctype2 i.sysid2 if attr2!=., vce(cluster sysid2)
test 1.ctype2=2.ctype2=3.ctype2=0

quietly regress avg3 i.ctype3 i.sysid3 if attr3!=., vce(cluster sysid3)
test 1.ctype3=2.ctype3=3.ctype3=0


//Teacher Race
quietly regress tracek i.ctypek i.sysidk if attrk!=., vce(cluster sysidk)
test 1.ctypek=2.ctypek=3.ctypek=0

quietly regress trace1 i.ctype1 i.sysid1 if attr1!=., vce(cluster sysid1)
test 1.ctype1=2.ctype1=3.ctype1=0

quietly regress trace2 i.ctype2 i.sysid2 if attr2!=., vce(cluster sysid2)
test 1.ctype2=2.ctype2=3.ctype2=0

quietly regress trace3 i.ctype3 i.sysid3 if attr3!=., vce(cluster sysid3)
test 1.ctype3=2.ctype3=3.ctype3=0



// Teacher Education
quietly regress hdegk i.ctypek i.sysidk if attrk!=., vce(cluster sysidk)
test 1.ctypek=2.ctypek=3.ctypek=0

quietly regress hdeg1 i.ctype1 i.sysid1 if attr1!=., vce(cluster sysid1)
test 1.ctype1=2.ctype1=3.ctype1=0

quietly regress hdeg2 i.ctype2 i.sysid2 if attr2!=., vce(cluster sysid2)
test 1.ctype2=2.ctype2=3.ctype2=0

quietly regress hdeg3 i.ctype3 i.sysid3 if attr3!=., vce(cluster sysid3)
test 1.ctype3=2.ctype3=3.ctype3=0


// Teacher Experince
quietly regress totexpk i.ctypek i.sysidk if attrk!=., vce(cluster sysidk)
test 1.ctypek=2.ctypek=3.ctypek=0

quietly regress totexp1 i.ctype1 i.sysid1 if attr1!=., vce(cluster sysid1)
test 1.ctype1=2.ctype1=3.ctype1=0

quietly regress totexp2 i.ctype2 i.sysid2 if attr2!=., vce(cluster sysid2)
test 1.ctype2=2.ctype2=3.ctype2=0

quietly regress totexp3 i.ctype3 i.sysid3 if attr3!=., vce(cluster sysid3)
test 1.ctype3=2.ctype3=3.ctype3=0


*** Q3: Regression Estimates ***
* a)
gen smallk = 0
replace smallk = 1 if ctypek==1
gen regk = 0
replace regk = 1 if ctypek == 2
gen regaidk = 0
replace regaidk = 1 if ctypek==3
gen girl = 0
replace girl = 1 if sex == 2
* Class size No school fixed effect
qui reg avgk smallk regaidk if stark==1, vce(cluster sysidk)
est store firstk

* Class size with school fixed effect
qui reg avgk smallk regaidk i.sysidk if stark==1, vce(cluster sysidk)
est store secondk

* Class size with school fixed effect and Girl & free lunch
qui reg avgk smallk regaidk race girl sesk i.sysidk if stark==1, vce(cluster sysidk)
est store thirdk

* Class size with school fixed effect and Girl, free lunch & teacher variables
qui reg avgk smallk regaidk race girl sesk whitek totexpk tmask ///
	i.sysidk if stark==1, vce(cluster sysidk)
est store fourthk

est table firstk secondk thirdk fourthk, drop(i.sysidk)  b(%9.3f) se(%9.3f) stats(r2)

* 1
gen small1 = 0
replace small1 = 1 if ctype1==1
gen reg1 = 0
replace reg1 = 1 if ctype1 == 2
gen regaid1 = 0
replace regaid1 = 1 if ctype1==3
* Class size No school fixed effect
qui reg avg1 small1 regaid1 if star1==1, vce(cluster sysid1)
est store first1

* Class size with school fixed effect
qui reg avg1 small1 regaid1 i.sysid1 if star1==1, vce(cluster sysid1)
est store second1

* Class size with school fixed effect and Girl & free lunch
qui reg avg1 small1 regaid1 race girl ses1 i.sysid1 if star1==1, vce(cluster sysid1)
est store third1

* Class size with school fixed effect and Girl, free lunch & teacher variables
qui reg avg1 small1 regaid1 race girl ses1 white1 totexp1 tmas1 ///
	i.sysid1 if star1==1, vce(cluster sysidk)
est store fourth1

est table first1 second1 third1 fourth1, drop(i.sysid1)  b(%9.3f) se(%9.3f) stats(r2)
	
* 2
gen small2 = 0
replace small2 = 1 if ctype2==1
gen regaid2 = 0
replace regaid2 = 1 if ctype2==3
* Class size No school fixed effect
qui reg avg2 small2 regaid2 if star2==1, vce(cluster sysid2)
est store first2

* Class size with school fixed effect
qui reg avg2 small2 regaid2 i.sysid2 if star2==1, vce(cluster sysid2)
est store second2

* Class size with school fixed effect and Girl & free lunch
qui reg avg2 small2 regaid2 race girl ses2 i.sysid2 if star2==1, vce(cluster sysid2)
est store third2

* Class size with school fixed effect and Girl, free lunch & teacher variables
qui reg avg2 small2 regaid2 race girl ses2 white2 totexp2 tmas2 ///
	i.sysid2 if star2==1, vce(cluster sysid2)
est store fourth2
	
est table first2 second2 third2 fourth2, drop(i.sysid2)  b(%9.3f) se(%9.3f) stats(r2)


* 3
gen small3 = 0
replace small3 = 1 if ctype3==1
gen regaid3 = 0
replace regaid3 = 1 if ctype3==3
* Class size No school fixed effect
qui reg avg3 small3 regaid3 if star3==1, vce(cluster sysid3)
est store first3

* Class size with school fixed effect
qui reg avg3 small3 regaid3 i.sysid3 if star3==1, vce(cluster sysid3)
est store second3

* Class size with school fixed effect and Girl & free lunch
qui reg avg3 small3 regaid3 race girl ses3 i.sysid3 if star3==1, vce(cluster sysid3)
est store third3

* Class size with school fixed effect and Girl, free lunch & teacher variables
qui reg avg3 small3 regaid3 race girl ses3 white3 totexp3 tmas3 ///
	i.sysid3 if star3==1, vce(cluster sysid3)
est store fourth3

est table first3 second3 third3 fourth3, drop(i.sysid3)  b(%9.3f) se(%9.3f) stats(r2)


*** Q4: IV ***
* a)

* Class size No school fixed effect
qui reg ctype1 ctypek  if stark==1 & star1==1, vce(cluster sysid1)
est store first_assignment

* Class size with school fixed effect
qui reg ctype1 ctypek  i.sysid1 if stark==1 & star1==1, vce(cluster sysid1)
est store second_assignment

* Class size with school fixed effect and Girl & free lunch
qui reg ctype1 ctypek  race girl ses1 i.sysid1 if stark==1 & star1==1, vce(cluster sysid1)
est store third_assignment

* Class size with school fixed effect and Girl, free lunch & teacher variables
qui reg ctype1 ctypek  race girl ses1 white1 totexp1 tmas1 ///
	i.sysid1 if stark==1 & star1==1, vce(cluster sysid1)
est store fourth_assignment

est table first_assignment second_assignment third_assignment ///
		fourth_assignment, drop(i.sysid1)  b(%9.3f) se(%9.3f) stats(r2)

* c)
* Creat initial class type
gen initctype = 0
replace  initctype = ctypek if stark == 1
replace  initctype = ctype1 if stark == 2 & star1 == 1
replace  initctype = ctype2 if stark == 2 & star1 == 2 & star2 == 1
replace  initctype = ctype3 if stark == 2 & star1 == 2 & star2 == 2 & star3 == 1
gen small_init = 0
replace small_init = 1 if initctype == 1
gen regaid_init = 0
replace regaid_init = 1 if initctype == 3
* k 
// Note: since the initial class is the same as actual class, the only diff will
// be the standard errors.
* Class size No school fixed effect
qui ivregress 2sls avgk smallk regaidk if stark==1, vce(cluster sysidk)
est store firstk

* Class size with school fixed effect
qui ivregress 2sls avgk smallk regaidk i.sysidk if stark==1, vce(cluster sysidk)
est store secondk

* Class size with school fixed effect and Girl & free lunch
qui ivregress 2sls avgk smallk regaidk race girl sesk i.sysidk if stark==1, vce(cluster sysidk)
est store thirdk

* Class size with school fixed effect and Girl, free lunch & teacher variables
qui ivregress 2sls avgk smallk regaidk race girl sesk whitek totexpk tmask ///
	i.sysidk if stark==1, vce(cluster sysidk)
est store fourthk

est table firstk secondk thirdk fourthk, drop(i.sysidk)  b(%9.3f) se(%9.3f) stats(r2)


* 1
* Class size No school fixed effect
qui ivregress 2sls avg1 (small1 regaid1 = small_init regaid_init) if star1==1, vce(cluster sysid1)
est store first1

* Class size with school fixed effect
qui ivregress 2sls avg1 (small1 regaid1 = small_init regaid_init) i.sysid1 if star1==1, vce(cluster sysid1)
est store second1

* Class size with school fixed effect and Girl & free lunch
qui ivregress 2sls avg1 (small1 regaid1 = small_init regaid_init) race girl ses1 i.sysid1 if star1==1, vce(cluster sysid1)
est store third1

* Class size with school fixed effect and Girl, free lunch & teacher variables
qui ivregress 2sls avg1 (small1 regaid1 = small_init regaid_init) race girl ses1 white1 totexp1 tmas1 ///
	i.sysid1 if star1==1, vce(cluster sysid1)
est store fourth1

est table first1 second1 third1 fourth1, drop(i.sysid1)  b(%9.3f) se(%9.3f) stats(r2)
	
* 2
* Class size No school fixed effect
qui ivregress 2sls avg2 (small2 regaid2 = small_init regaid_init) if star2==1, vce(cluster sysid2)
est store first2

* Class size with school fixed effect
qui ivregress 2sls avg2 (small2 regaid2 = small_init regaid_init) i.sysid2 if star2==1, vce(cluster sysid2)
est store second2

* Class size with school fixed effect and Girl & free lunch
qui ivregress 2sls avg2 (small2 regaid2 = small_init regaid_init) race girl ses2 i.sysid2 if star2==1, vce(cluster sysid2)
est store third2

* Class size with school fixed effect and Girl, free lunch & teacher variables
qui ivregress 2sls avg2 (small2 regaid2 = small_init regaid_init) race girl ses2 white2 totexp2 tmas2 ///
	i.sysid2 if star2==1, vce(cluster sysid2)
est store fourth2
	
est table first2 second2 third2 fourth2, drop(i.sysid2)  b(%9.3f) se(%9.3f) stats(r2)

* 3
* Class size No school fixed effect
qui ivregress 2sls avg3 (small3 regaid3 = small_init regaid_init) if star3==1, ///
	vce(cluster sysid3)
est store first3

* Class size with school fixed effect
qui ivregress 2sls avg3 (small3 regaid3 = small_init regaid_init) i.sysid3 if star3==1, vce(cluster sysid3)
est store second3

* Class size with school fixed effect and Girl & free lunch
qui ivregress 2sls avg3 (small3 regaid3 = small_init regaid_init) race girl ses3 i.sysid3 if star3==1, vce(cluster sysid3)
est store third3

* Class size with school fixed effect and Girl, free lunch & teacher variables
qui ivregress 2sls avg3 (small3 regaid3 = small_init regaid_init) race girl ses3 white3 totexp3 tmas3 ///
	i.sysid3 if star3==1, vce(cluster sysid3)
est store fourth3

est table first3 second3 third3 fourth3, drop(i.sysid3)  b(%9.3f) se(%9.3f) stats(r2)



