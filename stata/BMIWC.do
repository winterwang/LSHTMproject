// Analysing NDNS survey data in stata
// for CW3CB3 survey data analysis on obesity
// date created: 2018-08-06
// manipulation of the data was done in R

// import data from CW3CB3_7sregss.dta

use "/home/wangcc-me/Downloads/UKDA-6533-stata11_se/stata11_se/CW3CB3_7regss.dta", clear

//use "../Rcode/CW3CB3_7regss.dta", clear

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


gen DM = A1C > 6.5 if !missing(A1C)

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

svy, subpop(Men): tabulate Married BMIcat, col se ci format(%7.3f)

svy, subpop(Women): tabulate Married BMIcat, col se ci format(%7.3f)

svy, subpop(Men): tabulate hibp BMIcat, col se ci format(%7.3f)

svy, subpop(Women): tabulate hibp BMIcat, col se ci format(%7.3f)

svy, subpop(Men): tabulate DM BMIcat, col se ci format(%7.3f)

svy, subpop(Women): tabulate DM BMIcat, col se ci format(%7.3f)


svy, subpop(Men): mean eqvinc, over(BMIcat)
test [eqvinc]10 = [eqvinc]25 = [eqvinc]30, mtest(b)
svy, subpop(Women): mean eqvinc, over(BMIcat)
test [eqvinc]10 = [eqvinc]25 = [eqvinc]30, mtest(b)

svy, subpop(Men): mean Energy, over(BMIcat)
test [EnergykJ]10 = [EnergykJ]25 = [EnergykJ]30, mtest(b)
svy, subpop(Women): mean Energy, over(BMIcat)
test [EnergykJ]10 = [EnergykJ]25 = [EnergykJ]30, mtest(b)



svy, subpop(Men): mean Carbo, over(BMIcat)
test [Carbohydrateg]10 = [Carbohydrateg]25 = [Carbohydrateg]30, mtest(b)

svy, subpop(Women): mean Carbohydrateg, over(BMIcat)
test [Carbohydrateg]10 = [Carbohydrateg]25 = [Carbohydrateg]30, mtest(b)




********************************************************
********************************************************
**   Building the linear regression model 
**   date: 08/08/2018
**
**
********************************************************
********************************************************
svyset area [pweight = wtn1to8], strata(gor)


// crude association between CB and bmi

svy, subpop(Men): regress bmival i.CB
svy, subpop(if Men & DM != 1): regress bmival i.CB

svy, subpop(Women): regress bmival i.CB 
svy, subpop(if Women & DM != 1): regress bmival i.CB



// looking for confounder one by one
// Age: -> confounder
svy, subpop(Men): regress bmival i.CB age
test age
svy, subpop(Women): regress bmival i.CB age
test age

svy, subpop(Men): regress bmival i.CB##c.age
svy, subpop(Women): regress bmival i.CB##c.age
test 2.CB#c.age 3.CB#c.age // no interaction



// Partner -> confounder for men
svy, subpop(Men): regress bmival i.CB i.Married
test 1.Married

svy, subpop(Women): regress bmival i.CB i.Married
test 1.Married

svy, subpop(Men): regress bmival i.CB##i.Married
svy, subpop(Women): regress bmival i.CB##i.Married

test 2.CB#1.Married 3.CB#1.Married // -> no interaction in men interaction in women




// Income -> not confounder for men but confounder for women
svy, subpop(Men):  regress bmival i.CB eqvinc 
test eqvinc

svy, subpop(Women): regress bmival i.CB eqvinc
test eqvin

svy, subpop(Men): regress bmival i.CB##c.eqvinc 
svy, subpop(Women): regress bmival i.CB##c.eqvinc 

test 2.CB#c.eqvinc 3.CB#c.eqvinc // no interaction


// Education -> confounder
svy, subpop(Men): regress bmival i.CB i.Edu 

test 1.Edu

svy, subpop(Women): regress bmival i.CB i.Edu
test 1.Edu

svy, subpop(Men): regress bmival i.CB##i.Edu 
svy, subpop(Women): regress bmival i.CB##i.Edu 

test 2.CB#1.Edu 3.CB#1.Edu // no interaction

// hypertension -> confounder

svy, subpop(Men): regress bmival i.CB i.hibp

test 1.hibp

svy, subpop(Women): regress bmival i.CB i.hibp
test 1.hibp


svy, subpop(Men): regress bmival i.CB##i.hibp
svy, subpop(Women): regress bmival i.CB##i.hibp 

test 2.CB#1.hibp 3.CB#1.hibp // maybe some interaction in men; no interaction in women


// Smoking -> confounder


svy, subpop(Men):  regress bmival i.CB i.cigsta3

test 2.cig 3.cig

svy, subpop(Women):  regress bmival i.CB i.cigsta3

test 2.cig 3.cig


svy, subpop(Men): regress bmival i.CB##i.cigsta3
svy, subpop(Women): regress bmival i.CB##i.cigsta3

test 2.CB#2.cigsta3 2.CB#3.cigsta3 3.CB#2.cigsta3 3.CB#3.cigsta3 // no interaction in men
// interaction in women
 
// logMVP



// Total energy intake -> confounder

svy, subpop(Men):  regress bmival i.CB Energy

test Energy

svy, subpop(Women):  regress bmival i.CB Energy

test Energy


svy, subpop(Men): regress bmival i.CB##c.Energy
svy, subpop(Women): regress bmival i.CB##c.Energy

test 2.CB#c.EnergykJ 3.CB#c.EnergykJ // no interaction


// ethnicity -> not confounder

svy, subpop(Men): regress bmival i.CB i.ethgrp2
svy, subpop(Women): regress bmival i.CB i.ethgrp2

svy, subpop(Men): regress bmival i.CB##i.ethgrp2
svy, subpop(Women): regress bmival i.CB##i.ethgrp2
test 2.CB#2.ethgrp2 // no interaction


// Alcohol -> confounder for both men and women 


svy, subpop(Men): regress bmival i.CB Alcoholg
test Alcoholg
svy, subpop(Women): regress bmival i.CB Alcoholg
test Alcoholg


svy, subpop(Men): regress bmival i.CB##c.Alcoholg
svy, subpop(Women): regress bmival i.CB##c.Alcoholg
test 2.CB#c.Alcoholg 3.CB#c.Alcoholg //  interaction in men  no interaction in women


//  Preliminary model includes all possible confounders in Men
svy, subpop(Men): regress bmival i.CB age i.Married i.Edu i.hibp i.cig Energy Alcoholg

svy, subpop(if Men & DM != 1): regress bmival i.CB age i.Married i.Edu i.hibp i.cig Energy Alcoholg

svy, subpop(Women): regress bmival i.CB##i.Married age eqvinc i.Edu i.hibp i.cig Energy Alcoholg

test 2.CB#1.Married 3.CB#1.Married 

svy, subpop(if Women & Married == 1): regress bmival i.CB age eqvinc i.Edu i.hibp i.cig Energy Alcoholg
svy, subpop(if Women & Married == 0): regress bmival i.CB age eqvinc i.Edu i.hibp i.cig Energy Alcoholg

// strong interaction of married or not in women 
linktest

svy, subpop(if Women & DM != 1 ): regress bmival i.CB##i.Married age eqvinc i.Edu i.hibp i.cig Energy
linktest

svy, subpop(if Women & Married == 1 & DM != 1): regress bmival i.CB age eqvinc i.Edu i.hibp i.cig Energy Alcoholg
svy, subpop(if Women & Married == 0 & DM != 1): regress bmival i.CB age eqvinc i.Edu i.hibp i.cig Energy Alcoholg


// crude association between CB and bmicat
svy, subpop(Men): ologit BMIcat i.CB, eform
svy, subpop(Women): ologit BMIcat i.CB
svy, subpop(Men): ologit BMIcat i.CB age i.Married i.Edu i.hibp i.cig Energy Alcoholg, eform
svy, subpop(if Men & DM !=1): ologit BMIcat i.CB age i.Married i.Edu i.hibp i.cig Energy Alcoholg, eform

svy, subpop(Women): ologit BMIcat i.CB##i.Married age eqvinc i.Edu i.hibp i.cig Energy, eform
test 2.CB#1.Married 3.CB#1.Married // interaction

svy, subpop(Women): ologit BMIcat i.CB##i.Married age eqvinc i.Edu i.hibp i.cig Energy, eform



********************************************************
********************************************************
**   Building the linear regression model  wst
**   date: 08/08/2018
**
**
********************************************************
********************************************************
svyset area [pweight = wtn1to8], strata(gor)


// crude association between CB and wst

svy, subpop(Men): regress wst i.CB

svy, subpop(if Men & DM != 1): regress wst i.CB


svy, subpop(Women): regress wst i.CB 

svy, subpop(if Women & DM != 1): regress wst i.CB 


// looking for confounder one by one
// Age: -> confounder
svy, subpop(Men): regress wst i.CB age
test age
svy, subpop(Women): regress wst i.CB age
test age


svy, subpop(Men): regress wst i.CB##c.age
svy, subpop(Women): regress wst i.CB##c.age
test 2.CB#c.age 3.CB#c.age // no interaction




// Partner -> confounder for both
svy, subpop(Men): regress wst i.CB i.Married
test 1.Married

svy, subpop(Women): regress wst i.CB i.Married
test 1.Married

svy, subpop(Men): regress wst i.CB##i.Married
svy, subpop(Women): regress wst i.CB##i.Married

test 2.CB#1.Married 3.CB#1.Married // -> no interaction in men interaction in women



// Income -> not confounder for women but confounder for men
svy, subpop(Men):  regress wst i.CB eqvinc 
test eqvinc

svy, subpop(Women): regress wst i.CB eqvinc
test eqvin

svy, subpop(Men): regress wst i.CB##c.eqvinc 
svy, subpop(Women): regress wst i.CB##c.eqvinc 

test 2.CB#c.eqvinc 3.CB#c.eqvinc // no interaction



// Education -> confounder
svy, subpop(Men): regress wst i.CB i.Edu 

test 1.Edu

svy, subpop(Women): regress wst i.CB i.Edu
test 1.Edu

svy, subpop(Men): regress wst i.CB##i.Edu 
svy, subpop(Women): regress wst i.CB##i.Edu 

test 2.CB#1.Edu 3.CB#1.Edu // no interaction


// hypertension -> confounder

svy, subpop(Men): regress wst i.CB i.hibp

test 1.hibp

svy, subpop(Women): regress wst i.CB i.hibp
test 1.hibp

svy, subpop(Men): regress wst i.CB##i.hibp
svy, subpop(Women): regress wst i.CB##i.hibp 

test 2.CB#1.hibp 3.CB#1.hibp // maybe some interaction in men; no interaction in women

// Smoking -> confounder


svy, subpop(Men):  regress wst i.CB i.cigsta3

test 2.cig 3.cig

svy, subpop(Women):  regress wst i.CB i.cigsta3

test 2.cig 3.cig

svy, subpop(Men): regress wst i.CB##i.cigsta3
svy, subpop(Women): regress wst i.CB##i.cigsta3

test 2.CB#2.cigsta3 2.CB#3.cigsta3 3.CB#2.cigsta3 3.CB#3.cigsta3 // no interaction in men
// no interaction in women
 

// Total energy intake -> confounder

svy, subpop(Men):  regress wst i.CB Energy

test Energy

svy, subpop(Women):  regress wst i.CB Energy

test Energy


svy, subpop(Men): regress wst i.CB##c.Energy
svy, subpop(Women): regress wst i.CB##c.Energy

test 2.CB#c.EnergykJ 3.CB#c.EnergykJ // no interaction



// Alcohol -> confounder for both men and women 


svy, subpop(Men): regress wst i.CB Alcoholg
test Alcoholg
svy, subpop(Women): regress wst i.CB Alcoholg
test Alcoholg


svy, subpop(Men): regress wst i.CB##c.Alcoholg
svy, subpop(Women): regress wst i.CB##c.Alcoholg
test 2.CB#c.Alcoholg 3.CB#c.Alcoholg // no interaction in men  no interaction in women




//  Preliminary model includes all possible confounders 
gen age2 = age^2

svy, subpop(Men): regress wst i.CB  age age2 i.Married eqvinc i.Edu i.hibp i.cig Energy Alcoholg
linktest

svy, subpop(if Men & DM != 1): regress wst i.CB  age age2 i.Married eqvinc i.Edu i.hibp i.cig Energy Alcoholg
linktest


svy, subpop(Women): regress wst i.CB  age i.Married i.Edu i.hibp i.cig Energy Alcoholg
linktest


svy, subpop(Women): regress wst i.CB##i.Married age i.Edu i.hibp i.cig Energy Alcoholg

test 2.CB#1.Married 3.CB#1.Married 

svy, subpop(if Women & Married == 1): regress wst i.CB age age2 i.Edu i.hibp i.cig Energy Alcoholg
linktest
svy, subpop(if Women & Married == 0): regress wst i.CB age age2 i.Edu i.hibp i.cig Energy Alcoholg
linktest


// strong interaction of married or not in women 

svy, subpop(if Women & Married == 1 & DM != 1): regress  wst i.CB age age2 i.Edu i.hibp i.cig Energy Alcoholg
svy, subpop(if Women & Married == 0 & DM != 1): regress  wst i.CB age age2 i.Edu i.hibp i.cig Energy Alcoholg

