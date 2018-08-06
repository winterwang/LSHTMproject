// Analysing NDNS survey data in stata
// for CW3CB3 survey data analysis on diabetes
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
//  re-weighting use wtb to see the blood test results //
//                                                     //
*********************************************************

svyset area [pweight = wtb1to8], strata(gor)


gen DM = A1C > 6.5 if !missing(A1C)


gen Men = Sex == 1  //  n of men = 2537
gen Women = Sex == 2 // n of women = 3618



svy, subpop(Men): tab DM, se ci format(%7.3f)
svy, subpop(Women): tab DM, se ci format(%7.3f)


svy, subpop(Men): mean age, over(DM)
test [age]1 = [age]0
svy, subpop(Women): mean age, over(DM)
test [age]1 = [age]0




svy, subpop(Men): tabulate Country DM, col se ci format(%7.3f)

svy, subpop(Women): tabulate Country DM, col se ci format(%7.3f)


svy, subpop(Men): tabulate ethgrp2 DM, col se ci format(%7.3f)

svy, subpop(Women): tabulate ethgrp2 DM, col se ci format(%7.3f)



svy, subpop(Men): tabulate Edu DM, col se ci format(%7.3f)

svy, subpop(Women): tabulate Edu DM, col se ci format(%7.3f)



svy, subpop(Men): tabulate cigsta3 DM, col se ci format(%7.3f)

svy, subpop(Women): tabulate cigsta3 DM, col se ci format(%7.3f)



svy, subpop(Men): mean logMVP, over(DM)
test [logMVP]0 = [logMVP]1 

disp exp(.9289955) - 1    
dis exp(.8594272) -1 
dis exp(.9985639) -1


disp exp(.7292149) - 1    
dis exp(.5056025) -1 
dis exp(.9528272) -1


svy, subpop(Women): mean logMVP, over(DM)
test [logMVP]0 = [logMVP]1 

disp exp(.567613) - 1    
dis exp(.5250093) -1 
dis exp(.6102167) -1


disp exp(.3201048) - 1    
dis exp(.1703055) -1 
dis exp(.469904) -1


svy, subpop(Men): mean bmi, over(DM)
test [bmival]0 = [bmival]1
svy, subpop(Women): mean bmi, over(DM)
test [bmival]0 = [bmival]1




svy, subpop(Men): mean wst, over(DM)
test [wstval]0 = [wstval]1
svy, subpop(Women): mean wst, over(DM)
test [wstval]0 = [wstval]1


svy, subpop(Men): tabulate CB DM, col se ci format(%7.3f)

svy, subpop(Women): tabulate CB DM, col se ci format(%7.3f)



svy, subpop(Men): mean Energy, over(DM)
test [EnergykJ]0 = [EnergykJ]1 
svy, subpop(Women): mean Energy, over(DM)
test [EnergykJ]0 = [EnergykJ]1 




svy, subpop(Men): mean Carbo, over(DM)
test [Carbohydrateg]0 = [Carbohydrateg]1

svy, subpop(Women): mean Carbohydrateg, over(DM)
test [Carbohydrateg]0 = [Carbohydrateg]1


svy, subpop(Men): mean logGlu, over(DM)
test [logGlu]1 = [logGlu]0 


dis exp(1.654354)
dis exp(1.644512)
dis exp(1.664196)


dis exp(2.23614)
dis exp(2.166446)
dis exp(2.305834)



svy, subpop(Women): mean logGlu, over(DM)
test [logGlu]1 = [logGlu]0 


dis exp(1.608516)
dis exp(1.601077)
dis exp(1.615955)
	

dis exp(2.135895)
dis exp(2.040622)
dis exp(2.231167)




svy, subpop(Men): mean logA1C, over(DM)
test [logA1C]1 = [logA1C]0

dis exp(1.701075)
dis exp(1.6951)
dis exp(1.707049)


dis exp(2.080089)
dis exp(2.03803)
dis exp(2.122147)



svy, subpop(Women): mean logA1C, over(DM)
test [logA1C]1 = [logA1C]0

dis exp(1.699205)
dis exp(1.694649)
dis exp(1.703761)


dis exp(2.038391)
dis exp(1.984181)
dis exp(2.092601)



svy, subpop(Men): mean logChol, over(DM)
test [logChol]1 = [logChol]0
dis exp(1.566713)
dis exp(1.548606)
dis exp(1.58482)

dis exp(1.420114)
dis exp(1.349519)
dis exp(1.49071)


svy, subpop(Women): mean logChol, over(DM)
test [logChol]1 = [logChol]0
dis exp(1.614143)
dis exp(1.599745)
dis exp(1.628541)

dis exp(1.511573)
dis exp(1.415861)
dis exp(1.607286)



svy, subpop(Men): mean logHDL, over(DM)
test [logHDL]1 = [logHDL]0

dis exp( .2139892 )
dis exp(.1926995)
dis exp(.235279)


dis exp(.0491728)  
dis exp(-.0212134)
dis exp( .1195591)


svy, subpop(Women): mean logHDL, over(DM)
test [logHDL]1 = [logHDL]0

dis exp( .4252508)
dis exp(.4070373)
dis exp(.4434644)


dis exp(.1623785 )  
dis exp(.0577416 )
dis exp( .2670154)



svy, subpop(Men): mean logLDL, over(DM)
test [logLDL]1 = [logLDL]0
dis exp(1.051105)
dis exp(1.02472)
dis exp(1.07749)


dis exp(.7703449)
dis exp(.663467)
dis exp(.8772228)


svy, subpop(Women): mean logLDL, over(DM)
test [logLDL]1 = [logLDL]0
dis exp(1.074377)
dis exp(1.0527)
dis exp(1.096054)


dis exp(.908265)
dis exp(.7496328)
dis exp(1.066897)


svy, subpop(Men): mean logTG, over(DM)
test [logTG]1 = [logTG]0
dis exp(.2048978)
dis exp(1.0527)
dis exp(1.096054)


dis exp(.908265)
dis exp(.7496328)
dis exp(1.066897)
