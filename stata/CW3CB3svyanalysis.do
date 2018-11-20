// Analysing NDNS survey data in stata
// for CW3CB3 survey data analysis 
// date created: 2018-08-01
// manipulation of the data was done in R

// import data from CW3CB3_7sregss.dta

// date updated: 2018-11-20
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



svy: tabulate Sex CB, row se ci format(%7.3f)
svy: tabulate Sex CB, col se ci format(%7.3f)
svy: tabulate Country CB, col se ci format(%7.3f)
svy: tabulate Country CB, row se ci format(%7.3f)
svy: tabulate SurveyYear CB, col se ci format(%7.3f)
svy: tabulate SurveyYear CB, row se ci format(%7.3f)
svy: tabulate paid CB, col se ci format(%7.3f)
	
svy: tabulate MarSt2 CB
svy: tabulate MarStat CB
svy: tabulate Married CB, row se ci format(%7.3f)
svy: tabulate Married CB, col se ci format(%7.3f)
svy: mean eqvinc, over(CB)
test [eqvinc]1 = [eqvinc]2 = [eqvinc]3, mtest(b)

svy: tabulate ethgrp2 CB, row se ci format(%7.3f)
svy: tabulate ethgrp2 CB, col se ci format(%7.3f)
svy: tabulate Education CB, row se ci format(%7.3f)
svy: tabulate Education CB, col se ci format(%7.3f)


*********************************************************
//  nutritional distribution                           //
//                                                     //
*********************************************************


svy: mean EnergykJ, over(CB)
test [EnergykJ]1 = [EnergykJ]2 = [EnergykJ]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Energy6, over(CB)
test [Energy6_9]1 = [Energy6_9]2 = [Energy6_9]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Energy9, over(CB) 
test [Energy9_12]1 = [Energy9_12]2 = [Energy9_12]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Energy12, over(CB)
test [Energy12_14]1 = [Energy12_14]2 = [Energy12_14]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Energy14, over(CB)
test [Energy14_17]1 = [Energy14_17]2 = [Energy14_17]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Energy17, over(CB)
test [Energy17_20]1 = [Energy17_20]2 = [Energy17_20]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Energy20, over(CB)
test [Energy20_22]1 = [Energy20_22]2 = [Energy20_22]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Energy22, over(CB)
test [Energy22_6]1 = [Energy22_6]2 = [Energy22_6]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison


svy: mean Carbohydrateg, over(CB)
test [Carbohydrateg]1 = [Carbohydrateg]2 = [Carbohydrateg]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Carb6, over(CB)
test [Carb6_9]1 = [Carb6_9]2 = [Carb6_9]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Carb9, over(CB)
test [Carb9_12]1 = [Carb9_12]2 = [Carb9_12]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Carb12, over(CB)
test [Carb12_14]1 = [Carb12_14]2 = [Carb12_14]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Carb14, over(CB)
test [Carb14_17]1 = [Carb14_17]2 = [Carb14_17]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Carb17, over(CB)
test [Carb17_20]1 = [Carb17_20]2 = [Carb17_20]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison


svy: mean Carb20, over(CB)
test [Carb20_22]1 = [Carb20_22]2 = [Carb20_22]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Carb22, over(CB)
test [Carb22_6]1 = [Carb22_6]2 = [Carb22_6]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Sugar6, over(CB)
test [Sugar6_9]1 = [Sugar6_9]2 = [Sugar6_9]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison



svy: mean Sugar9, over(CB)
test [Sugar9_12]1 = [Sugar9_12]2 = [Sugar9_12]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Sugar12, over(CB)
test [Sugar9_12]1 = [Sugar9_12]2 = [Sugar9_12]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Sugar14, over(CB)
test [Sugar14_17]1 = [Sugar14_17]2 = [Sugar14_17]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Sugar17, over(CB)
test [Sugar17_20]1 = [Sugar17_20]2 = [Sugar17_20]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Sugar20, over(CB)
test [Sugar20_22]1 = [Sugar20_22]2 = [Sugar20_22]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Sugar22, over(CB)
test [Sugar22_6]1 = [Sugar22_6]2 = [Sugar22_6]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Starch6, over(CB)
test [Starch6_9]1 = [Starch6_9]2 = [Starch6_9]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Starch9, over(CB)
test [Sugar12_14]1 = [Sugar12_14]2 = [Sugar12_14]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Starch12, over(CB)
test [Starch12_14]1 = [Starch12_14]2 = [Starch12_14]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Starch14, over(CB)
test [Starch14_17]1 = [Starch14_17]2 = [Starch14_17]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Starch17, over(CB)
test [Starch17_20]1 = [Starch17_20]2 = [Starch17_20]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison


svy: mean Starch20, over(CB)
test [Starch20_22]1 = [Starch20_22]2 = [Starch20_22]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Starch22, over(CB)
test [Starch20_22]1 = [Starch20_22]2 = [Starch20_22]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Fibre6, over(CB)
test [Starch22_6]1 = [Starch22_6]2 = [Starch22_6]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

gen Fibreg = Fibre6 + Fibre9 + Fibre12 + Fibre14 + Fibre17 + Fibre20 + Fibre22
svy: mean Fibreg, over(CB)
test [Fibreg]1 = [Fibreg]2 = [Fibreg]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison


svy: mean Fibre9, over(CB)
test [Fibre9_12]1 = [Fibre9_12]2 = [Fibre9_12]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison


svy: mean Fibre12, over(CB)
test [Fibre12_14]1 = [Fibre12_14]2 = [Fibre12_14]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Fibre14, over(CB)
test [Fibre14_17]1 = [Fibre14_17]2 = [Fibre14_17]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Fibre17, over(CB)
test [Fibre17_20]1 = [Fibre17_20]2 = [Fibre17_20]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Fibre20, over(CB)
test [Fibre20_22]1 = [Fibre20_22]2 = [Fibre20_22]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Fibre22, over(CB)
test [Fibre22_6]1 = [Fibre22_6]2 = [Fibre22_6]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean NMES6, over(CB)
test [NMES6_9]1 = [NMES6_9]2 = [NMES6_9]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean NMES9, over(CB)
test [NMES9_12]1 = [NMES9_12]2 = [NMES9_12]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison


svy: mean NMES12, over(CB)
test [NMES12_14]1 = [NMES12_14]2 = [NMES12_14]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean NMES14, over(CB)
test [NMES14_17]1 = [NMES14_17]2 = [NMES14_17]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean NMES17, over(CB)
test [NMES17_20]1 = [NMES17_20]2 = [NMES17_20]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison


svy: mean NMES20, over(CB)
test [NMES20_22]1 = [NMES20_22]2 = [NMES20_22]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean NMES22, over(CB)


svy: mean CHO, over(CB)
test [CHOpctotE]1 = [CHOpctotE]2 = [CHOpctotE]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison


svy: mean Proteing, over(CB)
test [Proteing]1 = [Proteing]2 = [Proteing]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Prot6, over(CB)
test [Prot6_9]1 = [Prot6_9]2 = [Prot6_9]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Prot9, over(CB)
test [Prot9_12]1 = [Prot9_12]2 = [Prot9_12]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Prot12, over(CB)
test [Prot12_14]1 = [Prot12_14]2 = [Prot12_14]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Prot14, over(CB)
test [Prot14_17]1 = [Prot14_17]2 = [Prot14_17]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Prot17, over(CB)
test [Prot17_20]1 = [Prot17_20]2 = [Prot17_20]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Prot20, over(CB)
test [Prot20_22]1 = [Prot20_22]2 = [Prot20_22]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Prot22, over(CB)
test [Prot22_6]1 = [Prot22_6]2 = [Prot22_6]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison


svy: mean Proteinp, over(CB)
test [ProteinpctotE]1 = [ProteinpctotE]2 = [ProteinpctotE]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Fatg, over(CB)
test [Fatg]1 = [Fatg]2 = [Fatg]3, mtest(b) // bonferroni-adjusted p-values for multiple group Fatg


svy: mean Fat6, over(CB)
test [Fat6_9]1 = [Fat6_9]2 = [Fat6_9]3, mtest(b) // bonferroni-adjusted p-values for multiple group Fatg

svy: mean Fat9, over(CB)
test [Fat9_12]1 = [Fat9_12]2 = [Fat9_12]3, mtest(b) // bonferroni-adjusted p-values for multiple group Fatg


svy: mean Fat12, over(CB)
test [Fat12_14]1 = [Fat12_14]2 = [Fat12_14]3, mtest(b) // bonferroni-adjusted p-values for multiple group Fatg

svy: mean Fat14, over(CB)
test [Fat14_17]1 = [Fat14_17]2 = [Fat14_17]3, mtest(b) // bonferroni-adjusted p-values for multiple group Fatg

svy: mean Fat17, over(CB)
test [Fat17_20]1 = [Fat17_20]2 = [Fat17_20]3, mtest(b) // bonferroni-adjusted p-values for multiple group Fatg

svy: mean Fat20, over(CB)
test [Fat20_22]1 = [Fat20_22]2 = [Fat20_22]3, mtest(b) // bonferroni-adjusted p-values for multiple group Fatg

svy: mean Fat22, over(CB)
test [Fat22_6]1 = [Fat22_6]2 = [Fat22_6]3, mtest(b) // bonferroni-adjusted p-values for multiple group Fatg



svy: mean Fatp, over(CB)
test [FatpctotE]1 = [FatpctotE]2 = [FatpctotE]3, mtest(b) // bonferroni-adjusted p-values for multiple group Fatg

svy: mean Alcoholg, over(CB)
test [Alcoholg]1 = [Alcoholg]2 = [Alcoholg]3, mtest(b) // bonferroni-adjusted p-values for multiple group Fatg


svy: mean Alc6, over(CB)
test [Alc6_9]1 = [Alc6_9]2 = [Alc6_9]3, mtest(b) // bonferroni-adjusted p-values for multiple group Fatg


svy: mean Alc9, over(CB)
test [Alc9_12]1 = [Alc9_12]2 = [Alc9_12]3, mtest(b) // bonferroni-adjusted p-values for multiple group Fatg

svy: mean Alc12, over(CB)
test [Alc12_14]1 = [Alc12_14]2 = [Alc12_14]3, mtest(b) // bonferroni-adjusted p-values for multiple group Fatg

svy: mean Alc14, over(CB)
test [Alc14_17]1 = [Alc14_17]2 = [Alc14_17]3, mtest(b) // bonferroni-adjusted p-values for multiple group Fatg

svy: mean Alc17, over(CB)
test [Alc14_17]1 = [Alc14_17]2 = [Alc14_17]3, mtest(b) // bonferroni-adjusted p-values for multiple group Fatg

svy: mean Alc20, over(CB)
test [Alc14_17]1 = [Alc14_17]2 = [Alc14_17]3, mtest(b) // bonferroni-adjusted p-values for multiple group Fatg


svy: mean Alcoholp, over(CB)
test [AlcoholpctotE]1 = [AlcoholpctotE]2 = [AlcoholpctotE]3, mtest(b) // bonferroni-adjusted p-values for multiple group Fatg


svy: tabulate cigsta3 CB, col se ci format(%7.3f)
svy: tabulate dnnow CB, col se ci format(%7.3f)
svy: tabulate hibp CB, col se ci format(%7.3f)


sum MVP [weight=wti1to8] if CB ==1 , det
sum MVP [weight=wti1to8] if CB ==2 , det
sum MVP [weight=wti1to8] if CB ==3 , det
svy: mean MVP, over(CB)

svy: mean logMVP, over(CB) eform
test [logMVP]1 = [logMVP]2 = [logMVP]3, mtest(b) // bonferroni-adjusted p-values for multiple group Fatg


disp exp(.731059) - 1    
dis exp(.6768489) -1 
dis exp(.7852691) -1

disp exp(.6239265) - 1    
dis exp(.571165) -1 
dis exp( .6766879) -1

disp exp(.7273621) - 1    
dis exp(.684545) -1 
dis exp(.7701791) -1

svy: mean logalc, over(CB)


disp exp( 2.035795) - 1    
dis exp(1.933326) -1 
dis exp(2.138264) -1




*********************************************************
//  re-weighting use wtn to see the BMI,wc measurements //
//                                                     //
*********************************************************

svyset area [pweight = wtn1to8], strata(gor)
svy: mean wst, over(CB)
test [wstval]1 = [wstval]2 = [wstval]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

gen Men = Sex == 1 
svy, subpop(Men): mean wst, over(CB)
test [wstval]1 = [wstval]2 = [wstval]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

gen Women = Sex == 2
svy, subpop(Women): mean wst, over(CB)
test [wstval]1 = [wstval]2 = [wstval]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison



svy: mean bmi, over(CB)

test [bmival]1 = [bmival]2 = [bmival]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison


svy: tabulate 


*********************************************************
//  re-weighting use wtb to see the blood test results //
//                                                     //
*********************************************************

svyset area [pweight = wtb1to8], strata(gor)


svy: mean HDL, over(CB)
test [HDL]1 = [HDL]2 = [HDL]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Chol, over(CB)
test [Chol]1 = [Chol]2 = [Chol]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean LDL, over(CB)
test [LDL]1 = [LDL]2 = [LDL]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Trig, over(CB)
test [Trig]1 = [Trig]2 = [Trig]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison


gen DM = A1C <= 6.5 if !missing(A1C)


svy, subpop(DM): mean Glucose, over(CB)
test [Glucose]1 = [Glucose]2
test [Glucose]1 = [Glucose]2 = [Glucose]3, mtest(b)

svy, subpop(DM): mean A1C, over(CB)
test [A1C]1 = [A1C]2
test [A1C]1 = [A1C]2 = [A1C]3, mtest(b)

svy: tabulate DM CB, col se ci format(%7.3f)




svy, subpop(DM): mean Glucose, over(CB)
test [Glucose]1 = [Glucose]2
test [Glucose]1 = [Glucose]2 = [Glucose]3, mtest(b)
svy, subpop(DM): mean A1C, over(C)
test [A1C]1 = [A1C]2
test [A1C]1 = [A1C]2 = [A1C]3, mtest(b)

svy: tabulate DM C, col se ci format(%7.3f)



svy, subpop(DM): mean logGlu, over(CB)
test [logGlu]1 = [logGlu]2 = [logGlu]3, mtest(b)


dis exp(1.642848)
dis exp(1.632226)
dis exp(1.653471)

dis exp(1.620347)
dis exp(1.606447)
dis exp(1.634246)


dis exp(1.629356)
dis exp(1.620271)
dis exp(1.63844)


svy, subpop(DM): mean logA1C, over(CB)
test [logA1C]1 = [logA1C]2 = [logA1C]3, mtest(b)

dis exp(1.699581)
dis exp(1.69296)
dis exp(1.706203)


dis exp(1.691608)
dis exp(1.683897)
dis exp(1.699318)


dis exp(1.705623)
dis exp(1.700665)
dis exp(1.710581)


svy: mean logChol, over(CB)
dis exp(1.598818)
dis exp(1.577698)
dis exp(1.619939)


dis exp(1.55251)  
dis exp(1.530408)
dis exp(1.574613)

dis exp(1.599389)
dis exp(1.583391)
dis exp(1.615388)


test [logChol]1 = [logChol]2 = [logChol]3, mtest(b)




svy: mean logHDL, over(CB)
dis exp(.3293169)
dis exp(.3026793)
dis exp(.3559545)


dis exp(.2749379)  
dis exp(.2476816)
dis exp(.3021941)

dis exp(.3269002)
dis exp(.3062623)
dis exp(.3475381)


test [logHDL]1 = [logHDL]2 = [logHDL]3, mtest(b)



svy: mean logLDL, over(CB)
dis exp(1.058635)
dis exp(1.028391)
dis exp(1.08888)


dis exp(1.018181)  
dis exp(.984431)
dis exp(1.051931)

dis exp(1.075229)
dis exp(1.051369)
dis exp(1.09909)


test [logLDL]1 = [logLDL]2 = [logLDL]3, mtest(b)


svy: mean logTG, over(CB)
dis exp(.1273876)
dis exp(.0777152)
dis exp(.17706)


dis exp(.1012169)  
dis exp(.0460972)
dis exp(.1563366)

dis exp(.0983298)
dis exp(.0607423)
dis exp(.1359172)


test [logTG]1 = [logTG]2 = [logTG]3, mtest(b)


