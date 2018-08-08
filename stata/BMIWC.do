// Analysing NDNS survey data in stata
// for CW3CB3 survey data analysis on obesity
// date created: 2018-08-06
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


egen BMIcat = cut(bmival), at(10, 25, 30, 60)
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
//  weighting use wtn to see the BMI,wc measurements   //
//                                                     //
*********************************************************

svyset area [pweight = wtn1to8], strata(gor)


gen Men = Sex == 1  //  n of men = 2537
gen Women = Sex == 2 // n of women = 3618

svy, subpop(Men): tab BMIcat, se ci format(%7.3f)
svy, subpop(Women): tab BMIcat, se ci format(%7.3f)



svy, subpop(Men): mean age, over(BMIcat)
test [age]10 = [age]25 = [age]30, mtest(b)
svy, subpop(Women): mean age, over(BMIcat)
test [age]10 = [age]25 = [age]30, mtest(b)



svy, subpop(Men): tabulate Country BMIcat, col se ci format(%7.3f)
svy, subpop(Men): tabulate Country BMIcat, row se ci format(%7.3f)

svy, subpop(Women): tabulate Country BMIcat, col se ci format(%7.3f)



svy, subpop(Men): tabulate ethgrp2 BMIcat, col se ci format(%7.3f)

svy, subpop(Women): tabulate ethgrp2 BMIcat, col se ci format(%7.3f)


svy, subpop(Men): tabulate Edu BMIcat, col se ci format(%7.3f)

svy, subpop(Women): tabulate Edu BMIcat, col se ci format(%7.3f)


svy, subpop(Men): tabulate cigsta3 BMIcat, col se ci format(%7.3f)

svy, subpop(Women): tabulate cigsta3 BMIcat, col se ci format(%7.3f)


svy, subpop(Men): mean logMVP, over(BMIcat)
test [logMVP]10 = [logMVP]25 = [logMVP]30, mtest(b)



disp exp(.9481714) - 1    
dis exp(.8491881) -1 
dis exp(1.047155) -1


disp exp(.8851696) - 1    
dis exp(.8062384) -1 
dis exp( .9641008) -1

disp exp(.8803297) - 1    
dis exp(.7666242) -1 
dis exp(.9940351) -1



svy, subpop(Women): mean logMVP, over(BMIcat) 
test [logMVP]10 = [logMVP]25 = [logMVP]30, mtest(b)



disp exp(.6072737) - 1    
dis exp(.5537236) -1 
dis exp(.6608238) -1


disp exp(.5342603) - 1    
dis exp(.4816373) -1 
dis exp(.5868832) -1

disp exp(.5009181) - 1    
dis exp(.4261666) -1 
dis exp(.5756695) -1



svy, subpop(Men): mean bmi, over(BMIcat)
test [bmival]10 = [bmival]25 = [bmival]30, mtest(b)
svy, subpop(Women): mean bmi, over(BMIcat)
test [bmival]10 = [bmival]25 = [bmival]30, mtest(b)


svy, subpop(Men): mean wst, over(BMIcat)
test [wstval]10 = [wstval]25 = [wstval]30, mtest(b)
svy, subpop(Women): mean wst, over(BMIcat)
test [wstval]10 = [wstval]25 = [wstval]30, mtest(b)


svy, subpop(Men): tabulate CB BMIcat, col se ci format(%7.3f)

svy, subpop(Women): tabulate CB BMIcat, col se ci format(%7.3f)



svy, subpop(Men): mean Energy, over(BMIcat)
test [EnergykJ]10 = [EnergykJ]25 = [EnergykJ]30, mtest(b)
svy, subpop(Women): mean Energy, over(BMIcat)
test [EnergykJ]10 = [EnergykJ]25 = [EnergykJ]30, mtest(b)



svy, subpop(Men): mean Carbo, over(BMIcat)
test [Carbohydrateg]10 = [Carbohydrateg]25 = [Carbohydrateg]30, mtest(b)

svy, subpop(Women): mean Carbohydrateg, over(BMIcat)
test [Carbohydrateg]10 = [Carbohydrateg]25 = [Carbohydrateg]30, mtest(b)
