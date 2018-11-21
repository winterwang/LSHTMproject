// Analysing NDNS survey data in stata
// for CW3CB3 survey data analysis on obesity
// date created: 2018-08-06
// manipulation of the data was done in R

// import data from CW3CB3_7sregss.dta

// use "/home/wangcc-me/Downloads/UKDA-6533-stata11_se/stata11_se/CW3CB3_7regss.dta", clear

// date updated: 2018-11-21
// accepting markers idea using gender as an interaction term

log using "/home/takeshi/ドキュメント/githubprojects/LSHTMproject/stata/forAJCN.txt", append

// use "/home/wangcc-me/Downloads/UKDA-6533-stata11_se/stata11_se/CW3CB3_7regss.dta", clear
use "/home/takeshi/ドキュメント/githubprojects/LSHTMproject/Rcode/CW3CB3_7regss.dta", clear

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

gen Obesity = BMI >=  30
tab Obesity


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

svyset area [pweight = wtn1to8], strata(gor) vce(linearized) singleunit(missing)


svy: logistic Obesity i.CB

gen DM = A1C > 6.5 if !missing(A1C)
gen nonDM = A1C <= 6.5 if !missing(A1C)



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

svy, subpop(Men): mean EnergykJ, over(BMIcat)
test [EnergykJ]10 = [EnergykJ]25 = [EnergykJ]30, mtest(b)
svy, subpop(Women): mean EnergykJ, over(BMIcat)
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

svy: regress bmival i.CB

svy, subpop(Men): regress bmival i.CB
svy, subpop(if Men & DM != 1): regress bmival i.CB

svy, subpop(Women): regress bmival i.CB 
svy, subpop(if Women & DM != 1): regress bmival i.CB

svy, subpop(nonDM): regress bmival i.CB


// looking for confounder one by one
// Sex: 
svy: regress bmival i.CB i.Sex
test 2.Sex
svy: regress bmival i.CB##i.Sex
test 2.CB#2.Sex 3.CB#2.Sex // -> there is no need to stratify or use gender as an interaction





// Age: -> confounder
svy: regress bmival i.CB age

svy, subpop(Men): regress bmival i.CB age
test age
svy, subpop(Women): regress bmival i.CB age
test age

svy: regress bmival i.CB##c.age

svy, subpop(Men): regress bmival i.CB##c.age
svy, subpop(Women): regress bmival i.CB##c.age
test 2.CB#c.age 3.CB#c.age // no interaction



// Partner -> confounder for men

svy: regress bmival i.CB i.Married

svy, subpop(Men): regress bmival i.CB i.Married
test 1.Married

svy, subpop(Women): regress bmival i.CB i.Married
test 1.Married

svy: regress bmival i.CB##i.Married

svy, subpop(Men): regress bmival i.CB##i.Married
svy, subpop(Women): regress bmival i.CB##i.Married

test 2.CB#1.Married 3.CB#1.Married // -> no interaction in men interaction in women




// Income -> not confounder for men but confounder for women
svy:  regress bmival i.CB eqvinc 


svy, subpop(Men):  regress bmival i.CB eqvinc 
test eqvinc

svy, subpop(Women): regress bmival i.CB eqvinc
test eqvin

svy: regress bmival i.CB##c.eqvinc 

svy, subpop(Men): regress bmival i.CB##c.eqvinc 
svy, subpop(Women): regress bmival i.CB##c.eqvinc 

test 2.CB#c.eqvinc 3.CB#c.eqvinc // no interaction


// Education -> confounder
svy: regress bmival i.CB i.Edu 

svy, subpop(Men): regress bmival i.CB i.Edu 

test 1.Edu

svy, subpop(Women): regress bmival i.CB i.Edu
test 1.Edu

svy: regress bmival i.CB##i.Edu 

svy, subpop(Men): regress bmival i.CB##i.Edu 
svy, subpop(Women): regress bmival i.CB##i.Edu 

test 2.CB#1.Edu 3.CB#1.Edu // no interaction

// hypertension -> confounder
svy: regress bmival i.CB i.hibp


svy, subpop(Men): regress bmival i.CB i.hibp

test 1.hibp

svy, subpop(Women): regress bmival i.CB i.hibp
test 1.hibp

svy: regress bmival i.CB##i.hibp

svy, subpop(Men): regress bmival i.CB##i.hibp
svy, subpop(Women): regress bmival i.CB##i.hibp 

test 2.CB#1.hibp 3.CB#1.hibp // maybe some interaction in men; no interaction in women


// Smoking -> confounder

svy:  regress bmival i.CB i.cigsta3

svy, subpop(Men):  regress bmival i.CB i.cigsta3

test 2.cig 3.cig

svy, subpop(Women):  regress bmival i.CB i.cigsta3

test 2.cig 3.cig

svy: regress bmival i.CB##i.cigsta3

svy, subpop(Men): regress bmival i.CB##i.cigsta3
svy, subpop(Women): regress bmival i.CB##i.cigsta3

test 2.CB#2.cigsta3 2.CB#3.cigsta3 3.CB#2.cigsta3 3.CB#3.cigsta3 // no interaction in men
// interaction in women
 
// logMVP

svy:  regress bmival i.CB logMVP


svy, subpop(Men):  regress bmival i.CB logMVP

test logMVP

svy, subpop(Women):  regress bmival i.CB logMVP

test logMVP

svy:  regress bmival i.CB##c.logMVP

test 2.CB#c.logMVP 3.CB#c.logMVP

// Total EnergykJ intake -> confounder
svy:  regress bmival i.CB EnergykJ

svy, subpop(Men):  regress bmival i.CB EnergykJ

test EnergykJ

svy, subpop(Women):  regress bmival i.CB EnergykJ

test EnergykJ

svy: regress bmival i.CB##c.EnergykJ

svy, subpop(Men): regress bmival i.CB##c.EnergykJ
svy, subpop(Women): regress bmival i.CB##c.EnergykJ

test 2.CB#c.EnergykJ 3.CB#c.EnergykJ // no interaction


// ethnicity -> not confounder
svy: regress bmival i.CB i.ethgrp2

svy, subpop(Men): regress bmival i.CB i.ethgrp2
svy, subpop(Women): regress bmival i.CB i.ethgrp2

test 2.ethgrp2

svy: regress bmival i.CB##i.ethgrp2

svy, subpop(Men): regress bmival i.CB##i.ethgrp2
svy, subpop(Women): regress bmival i.CB##i.ethgrp2
test 2.CB#2.ethgrp2 // no interaction


// Alcohol -> confounder for both men and women 

svy: regress bmival i.CB Alcoholg

svy, subpop(Men): regress bmival i.CB Alcoholg
test Alcoholg
svy, subpop(Women): regress bmival i.CB Alcoholg
test Alcoholg

svy: regress bmival i.CB##c.Alcoholg

svy, subpop(Men): regress bmival i.CB##c.Alcoholg
svy, subpop(Women): regress bmival i.CB##c.Alcoholg
test 2.CB#c.Alcoholg 3.CB#c.Alcoholg //  interaction in men  no interaction in women

// paid
svy: regress bmival i.CB i.paid
test 2.paid

svy: regress bmival i.CB##i.paid

test 2.CB#2.paid 3.CB#2.paid //  interaction in men  no interaction in women

//  Preliminary model includes all possible confounders in total sample

svy: regress bmival i.CB i.Sex age i.Married eqvinc i.Edu i.hibp i.cig EnergykJ Alcoholg
svy: regress bmival i.CB##i.Married i.Sex age eqvinc i.Edu i.hibp i.cig EnergykJ Alcoholg

test 2.CB#1.Married 3.CB#1.Married // -> no interaction anymore

svy: regress bmival i.CB##i.hibp  i.Married i.Sex age eqvinc i.Edu i.cig EnergykJ Alcoholg

test 2.CB#1.hibp 3.CB#1.hibp // -> strong evidence for hibp interaction 

lincom 2.CB 

lincom 3.CB

lincom 2.CB + 2.CB#1.hibp

lincom 3.CB + 3.CB#1.hibp


svy, subpop(nonDM): regress bmival i.CB##i.hibp  i.Married i.Sex age eqvinc i.Edu i.cig EnergykJ Alcoholg
lincom 2.CB 

lincom 3.CB

lincom 2.CB + 2.CB#1.hibp

lincom 3.CB + 3.CB#1.hibp





tab hibp CB, summarize(bmi) mean 
bysort hibp CB: su bmi

tab hibp CB, summarize(wst) mean 
bysort hibp CB: su wst



svy: regress bmival i.CB##c.Alcoholg i.hibp  i.Married i.Sex age eqvinc i.Edu i.cig EnergykJ 

test 2.CB#c.Alcoholg 3.CB#c.Alcoholg // -> strong evidence for alc interaction 



//  Preliminary model includes all possible confounders in Men
svy, subpop(Men): regress bmival i.CB age i.Married i.Edu i.hibp i.cig EnergykJ Alcoholg

svy, subpop(if Men & DM != 1): regress bmival i.CB age i.Married i.Edu i.hibp i.cig EnergykJ Alcoholg

svy, subpop(Women): regress bmival i.CB##i.Married age eqvinc i.Edu i.hibp i.cig EnergykJ Alcoholg

test 2.CB#1.Married 3.CB#1.Married 

svy, subpop(if Women & Married == 1): regress bmival i.CB age eqvinc i.Edu i.hibp i.cig EnergykJ Alcoholg
svy, subpop(if Women & Married == 0): regress bmival i.CB age eqvinc i.Edu i.hibp i.cig EnergykJ Alcoholg

// strong interaction of married or not in women 
linktest

svy, subpop(if Women & DM != 1 ): regress bmival i.CB##i.Married age eqvinc i.Edu i.hibp i.cig EnergykJ
linktest

svy, subpop(if Women & Married == 1 & DM != 1): regress bmival i.CB age eqvinc i.Edu i.hibp i.cig EnergykJ Alcoholg
svy, subpop(if Women & Married == 0 & DM != 1): regress bmival i.CB age eqvinc i.Edu i.hibp i.cig EnergykJ Alcoholg


// crude association between CB and bmicat
svy: ologit BMIcat i.CB, eform
svy, subpop(Men): ologit BMIcat i.CB, eform
svy, subpop(Women): ologit BMIcat i.CB
svy, subpop(Men): ologit BMIcat i.CB age i.Married i.Edu i.hibp i.cig EnergykJ Alcoholg, eform
svy, subpop(if Men & DM !=1): ologit BMIcat i.CB age i.Married i.Edu i.hibp i.cig EnergykJ Alcoholg, eform

svy, subpop(Women): ologit BMIcat i.CB##i.Married age eqvinc i.Edu i.hibp i.cig EnergykJ, eform
test 2.CB#1.Married 3.CB#1.Married // interaction

svy, subpop(Women): ologit BMIcat i.CB##i.Married age eqvinc i.Edu i.hibp i.cig EnergykJ, eform



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

svy: regress wst i.CB

svy, subpop(nonDM): regress wst i.CB

svy, subpop(Men): regress wst i.CB

svy, subpop(if Men & DM != 1): regress wst i.CB

svy, subpop(nonDM): regress wst i.CB

svy, subpop(Women): regress wst i.CB 

svy, subpop(if Women & DM != 1): regress wst i.CB 


// looking for confounder one by one
// Age: -> confounder
svy: regress wst i.CB age


svy, subpop(Men): regress wst i.CB age
test age
svy, subpop(Women): regress wst i.CB age
test age

svy: regress wst i.CB##c.age

svy, subpop(Men): regress wst i.CB##c.age
svy, subpop(Women): regress wst i.CB##c.age
test 2.CB#c.age 3.CB#c.age // no interaction

// Sex: 

svy: regress wst i.CB i.Sex
test 2.Sex
svy: regress wst i.CB##i.Sex
test 2.CB#2.Sex 3.CB#2.Sex // no interaction


// Partner -> confounder for both
svy: regress wst i.CB i.Married
test 1.Married


svy, subpop(Men): regress wst i.CB i.Married
test 1.Married

svy, subpop(Women): regress wst i.CB i.Married
test 1.Married

svy: regress wst i.CB##i.Married

svy, subpop(Men): regress wst i.CB##i.Married
svy, subpop(Women): regress wst i.CB##i.Married

test 2.CB#1.Married 3.CB#1.Married // -> no interaction in men interaction in women



// Income -> not confounder for women but confounder for men

svy:  regress wst i.CB eqvinc 


svy, subpop(Men):  regress wst i.CB eqvinc 
test eqvinc

svy, subpop(Women): regress wst i.CB eqvinc
test eqvin

svy: regress wst i.CB##c.eqvinc 

svy, subpop(Men): regress wst i.CB##c.eqvinc 
svy, subpop(Women): regress wst i.CB##c.eqvinc 

test 2.CB#c.eqvinc 3.CB#c.eqvinc // no interaction



// Education -> confounder

svy: regress wst i.CB i.Edu 


svy, subpop(Men): regress wst i.CB i.Edu 

test 1.Edu

svy, subpop(Women): regress wst i.CB i.Edu
test 1.Edu

svy: regress wst i.CB##i.Edu 


svy, subpop(Men): regress wst i.CB##i.Edu 
svy, subpop(Women): regress wst i.CB##i.Edu 

test 2.CB#1.Edu 3.CB#1.Edu // no interaction


// hypertension -> confounder
svy: regress wst i.CB i.hibp


svy, subpop(Men): regress wst i.CB i.hibp

test 1.hibp

svy, subpop(Women): regress wst i.CB i.hibp
test 1.hibp


svy: regress wst i.CB##i.hibp

svy, subpop(Men): regress wst i.CB##i.hibp
svy, subpop(Women): regress wst i.CB##i.hibp 

test 2.CB#1.hibp 3.CB#1.hibp // maybe some interaction in men; no interaction in women

// Smoking -> confounder

svy:  regress wst i.CB i.cigsta3

svy, subpop(Men):  regress wst i.CB i.cigsta3

test 2.cig 3.cig

svy, subpop(Women):  regress wst i.CB i.cigsta3

test 2.cig 3.cig

svy: regress wst i.CB##i.cigsta3

svy, subpop(Men): regress wst i.CB##i.cigsta3
svy, subpop(Women): regress wst i.CB##i.cigsta3

test 2.CB#2.cigsta3 2.CB#3.cigsta3 3.CB#2.cigsta3 3.CB#3.cigsta3 // no interaction in men
// no interaction in women
 

// Total EnergykJ intake -> confounder
svy:  regress wst i.CB EnergykJ

svy, subpop(Men):  regress wst i.CB EnergykJ

test EnergykJ

svy, subpop(Women):  regress wst i.CB EnergykJ

test EnergykJ

svy: regress wst i.CB##c.EnergykJ

svy, subpop(Men): regress wst i.CB##c.EnergykJ
svy, subpop(Women): regress wst i.CB##c.EnergykJ

test 2.CB#c.EnergykJ 3.CB#c.EnergykJ // no interaction



// Alcohol -> confounder for both men and women 

svy: regress wst i.CB Alcoholg

svy, subpop(Men): regress wst i.CB Alcoholg
test Alcoholg
svy, subpop(Women): regress wst i.CB Alcoholg
test Alcoholg

svy: regress wst i.CB##c.Alcoholg

svy, subpop(Men): regress wst i.CB##c.Alcoholg
svy, subpop(Women): regress wst i.CB##c.Alcoholg
test 2.CB#c.Alcoholg 3.CB#c.Alcoholg // no interaction in men  no interaction in women

// logMVP  -> not confounder not interaction

svy: regress wst i.CB logMVP
test logMVP

svy: regress wst i.CB##c.logMVP
test 2.CB#c.logMVP 3.CB#c.logMVP

// ethnicity

svy: regress wst i.CB i.ethgrp2 
test 2.ethgrp2

svy: regress wst i.CB##i.ethgrp2 
test 2.CB#2.ethgrp2 3.CB#2.ethgrp2


//  Preliminary model includes all possible confounders 
gen age2 = age^2


svy: regress wst i.CB age i.Sex i.Married eqvinc i.Edu i.hibp i.cig i.ethgrp2

svy, subpop(nonDM): regress wst i.CB age i.Sex i.Married eqvinc i.Edu i.hibp i.cig i.ethgrp2

svy, subpop(nonDM): regress wst i.CB##i.Sex age  i.Married eqvinc i.Edu i.hibp i.cig i.ethgrp2
test 2.CB#2.Sex 3.CB#2.Sex

lincom 2.CB 

lincom 3.CB

lincom 2.CB + 2.CB#2.Sex

lincom 3.CB + 3.CB#2.Sex

tab Sex CB, summarize(wst) mean 
bysort Sex CB: su wst

svy: regress wst i.CB##i.hibp##i.Sex age i.Married eqvinc i.Edu i.cig i.ethgrp2
test 2.CB#1.hibp 3.CB#1.hibp
test 2.CB#2.Sex 3.CB#2.Sex
test 1.hibp#2.Sex 1.hibp#2.Sex
test 2.CB#1.hibp 3.CB#1.hibp 2.CB#2.Sex 3.CB#2.Sex 1.hibp#2.Sex 1.hibp#2.Sex



tab hibp CB, summarize(wst) mean 
bysort Sex CB: su wst


svy: regress wst i.CB##i.Edu i.hibp i.Sex age i.Married eqvinc  i.cig i.ethgrp2
test 2.CB#1.Edu 3.CB#1.Edu


svy, subpop(Men): regress wst i.CB  age age2 i.Married eqvinc i.Edu i.hibp i.cig EnergykJ Alcoholg
linktest

svy, subpop(if Men & DM != 1): regress wst i.CB  age age2 i.Married eqvinc i.Edu i.hibp i.cig EnergykJ Alcoholg
linktest


svy, subpop(Women): regress wst i.CB  age i.Married i.Edu i.hibp i.cig EnergykJ Alcoholg
svy, subpop(Women): regress wst i.CB  age age2 i.Married i.Edu i.hibp i.cig EnergykJ Alcoholg

linktest


svy, subpop(Women): regress wst i.CB##i.Married age i.Edu i.hibp i.cig EnergykJ Alcoholg
linktest

svy, subpop(Women): regress wst i.CB##i.Married age age2 i.Edu i.hibp i.cig EnergykJ Alcoholg
linktest

test 2.CB#1.Married 3.CB#1.Married 

svy, subpop(if Women & Married == 1): regress wst i.CB age age2 i.Edu i.hibp i.cig EnergykJ Alcoholg
linktest
svy, subpop(if Women & Married == 0): regress wst i.CB age age2 i.Edu i.hibp i.cig EnergykJ Alcoholg
linktest

svy, subpop(if Women & DM != 1): regress wst i.CB##i.Married age age2 i.Edu i.hibp i.cig EnergykJ Alcoholg
linktest
test 2.CB#1.Married 3.CB#1.Married 

// strong interaction of married or not in women 

svy, subpop(if Women & Married == 1 & DM != 1): regress  wst i.CB age age2 i.Edu i.hibp i.cig EnergykJ Alcoholg
linktest
svy, subpop(if Women & Married == 0 & DM != 1): regress  wst i.CB age age2 i.Edu i.hibp i.cig EnergykJ Alcoholg
linktest

// see the percentages of three types of carbohydrate eaters in women 
// separately by married 

svy, subpop(if Women): tabulate Married CB, col se ci format(%7.3f)

svy, subpop(if Women & Married == 1): mean wst, over(CB)
test [wstval]1 == [wstval]2 == [wstval]3
svy, subpop(if Women & Married == 0): mean wst, over(CB)
test [wstval]1 == [wstval]2 == [wstval]3

svy, subpop(if Men & Married == 1): mean wst, over(CB)
test [wstval]1 == [wstval]2 == [wstval]3
svy, subpop(if Men & Married == 0): mean wst, over(CB)
test [wstval]1 == [wstval]2 == [wstval]3


svy, subpop(if Women & Married == 1):  mean EnergykJ, over(CB)

test [EnergykJ]1 = [EnergykJ]2 = [EnergykJ]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy, subpop(if Women & Married == 1): mean Energy6, over(CB)
test [Energy6_9]1 = [Energy6_9]2 = [Energy6_9]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy, subpop(if Women & Married == 1): mean Energy9, over(CB) 
test [Energy9_12]1 = [Energy9_12]2 = [Energy9_12]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy, subpop(if Women & Married == 1): mean Energy12, over(CB)
test [Energy12_14]1 = [Energy12_14]2 = [Energy12_14]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy, subpop(if Women & Married == 1): mean Energy14, over(CB)
test [Energy14_17]1 = [Energy14_17]2 = [Energy14_17]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy, subpop(if Women & Married == 1): mean Energy17, over(CB)
test [Energy17_20]1 = [Energy17_20]2 = [Energy17_20]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy, subpop(if Women & Married == 1): mean Energy20, over(CB)
test [Energy20_22]1 = [Energy20_22]2 = [Energy20_22]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy, subpop(if Women & Married == 1): mean Energy22, over(CB)
test [Energy22_6]1 = [Energy22_6]2 = [Energy22_6]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison


svy, subpop(if Women & Married == 1): mean Alcoholg, over(CB)
svy, subpop(if Women & Married == 0): mean age, over(CB)
