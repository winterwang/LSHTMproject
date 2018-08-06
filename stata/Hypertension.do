// Analysing NDNS survey data in stata
// for CW3CB3 survey data analysis 
// date created: 2018-08-01
// manipulation of the data was done in R

// import data from CW3CB3_7sregss.dta

use "/home/wangcc-me/Downloads/UKDA-6533-stata11_se/stata11_se/CW3CB3_7regss.dta", clear



label define smoking 1 "current" 2 "ex-smoker" 3 "Never"
label values cigsta3 smoking
label define drinking 1 "yes" 2 "no"
label values dnnow drinking
label define gender 1 "Men" 2 "Women"
label values Sex gender
label define paid 1 "No" 2 "Yes"
label values paidemployment paid
label define ethnicity5 1 "White" 2 "Mixed" 3 "Black" 4 "Asian" 5 "Other"
label values ethgrp5 ethnicity5
label define ethnicity2 1 "White" 2 "non-White"
label values ethgrp2 ethnicity2

gen Married = 1 if MarStat == 2 | MarSt2 == 2 
replace Married = 1 if MarSt2 == 3
replace Married = 0 if Married !=1
tab Married
tab MarSt2
tab MarStat

label define Partner 0 "No" 1 "Yes"
label values Married Partner

gen Education = qual7 == 1
label define Ed 0 "lower than Degree" 1 "Degree or higher"
label values Education Ed

replace Education = . if qual7 >100
tab Educ


egen BMIcat = cut(bmival), at(10, 25, 30, 40, 60)
tab BMIcat


**********************************************************
// variables need to be log transfomred                 //
**********************************************************

gen logalc = ln(Alcoholg+1)
summ logalc, detail
gen logMVP = ln(MVPAtime+1)
summ logMVP, detail
gen logGlu = ln(Glucose)
summ logGlu, detail
gen logA1C = ln(A1C)
summ logA1C, detail
gen logChol = ln(Chol)
summ logChol, detail
gen logLDL = ln(LDL)
gen logHDL = ln(HDL)
gen logTG = ln(Trig)




*********************************************************
//  weighting use wti to see the individual results    //
//                                                     //
*********************************************************


// weighting with individual weights, area is primary sampling unit, gor is the cluster variable
svyset area [pweight = wti1to8], strata(gor)

svydescribe wti // describe the weighted data set




*********************************************************
//  re-weighting use wtn to see the BMI,wc measurements //
//                                                     //
*********************************************************

svyset area [pweight = wtn1to8], strata(gor)


gen Men = Sex == 1  //  n of men = 2537
gen Women = Sex == 2 // n of women = 3618

svy, subpop(Men): mean age, over(hibp)
test [age]1 = [age]0
svy, subpop(Women): mean age, over(hibp)
test [age]1 = [age]0

svy, subpop(Men): mean wst, over(hibp)
test [wstval]1 = [wstval]0
svy, subpop(Women): mean wst, over(hibp)
test [wstval]1 = [wstval]0

svy, subpop(Men): tabulate CB hibp, col se ci format(%7.3f)

svy, subpop(Women): tabulate CB hibp, col se ci format(%7.3f)


svy, subpop(Men): tabulate Country hibp, col se ci format(%7.3f)

svy, subpop(Women): tabulate Country hibp, col se ci format(%7.3f)

svy, subpop(Men): tabulate SurveyYear hibp, col se ci format(%7.3f)

svy, subpop(Women): tabulate SurveyYear hibp, col se ci format(%7.3f)

svy, subpop(Men): tabulate ethgrp2 hibp, col se ci format(%7.3f)

svy, subpop(Women): tabulate ethgrp2 hibp, col se ci format(%7.3f)


svy, subpop(Men): tabulate Edu hibp, col se ci format(%7.3f)

svy, subpop(Women): tabulate Edu hibp, col se ci format(%7.3f)

svy, subpop(Men): tabulate cigsta3 hibp, col se ci format(%7.3f)

svy, subpop(Women): tabulate cigsta3 hibp, col se ci format(%7.3f)




svy, subpop(Men): mean logMVP, over(hibp)
test [logMVP]1 = [logMVP]0


disp exp(.9234363 ) - 1    
dis exp(.8457101) -1 
dis exp(1.001163) -1

disp exp(.828635) - 1    
dis exp(.730244) -1 
dis exp( .9270261) -1


svy, subpop(Women): mean logMVP, over(hibp) 
test [logMVP]1 = [logMVP]0


disp exp(.5916676) - 1    
dis exp(.5473043) -1 
dis exp(.6360309) -1

disp exp(.4231103) - 1    
dis exp(.3536885) -1 
dis exp(.4925322) -1




svy: mean logalc, over(CB)


disp exp( 2.035795) - 1    
dis exp(1.933326) -1 
dis exp(2.138264) -1




svy, subpop(Men): mean bmi, over(hibp)
test [bmival]1 = [bmival]0
svy, subpop(Women): mean bmi, over(hibp)
test [bmival]1 = [bmival]0


svy, subpop(Men): mean Energy, over(hibp)
test [EnergykJ]1 = [EnergykJ]0
svy, subpop(Women): mean Energy, over(hibp)
test [EnergykJ]1 = [EnergykJ]0

	
svy, subpop(Men): mean Carbo, over(hibp)
test [Carbohydrateg]1 = [Carbohydrateg]0

svy, subpop(Women): mean Carbohydrateg, over(hibp)
test [Carbohydrateg]1 = [Carbohydrateg]0


svy, subpop(Men): mean Carbo, over(hibp)
test [Carbohydrateg]1 = [Carbohydrateg]0

svy, subpop(Women): mean Carbohydrateg, over(hibp)
test [Carbohydrateg]1 = [Carbohydrateg]0



svy: tabulate Sex hibp, col se ci format(%7.3f)

svy: logistic hibp i.CB
svy: logistic hibp i.CB#i.Sex

test 2.CB#2.Sex

