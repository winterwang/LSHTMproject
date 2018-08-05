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



svy: mean Energy, over(CB)
test [EnergykJ]1 = [EnergykJ]2 = [EnergykJ]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison



svy: mean Carb, over(CB)
test [Carbohydrateg]1 = [Carbohydrateg]2 = [Carbohydrateg]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison



svy: mean CHO, over(CB)
test [CHOpctotE]1 = [CHOpctotE]2 = [CHOpctotE]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison


svy: mean Proteing, over(CB)
test [Proteing]1 = [Proteing]2 = [Proteing]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Proteinp, over(CB)
test [ProteinpctotE]1 = [ProteinpctotE]2 = [ProteinpctotE]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean Fatg, over(CB)
test [Fatg]1 = [Fatg]2 = [Fatg]3, mtest(b) // bonferroni-adjusted p-values for multiple group Fatg

svy: mean Fatp, over(CB)
test [FatpctotE]1 = [FatpctotE]2 = [FatpctotE]3, mtest(b) // bonferroni-adjusted p-values for multiple group Fatg

svy: mean Alcoholg, over(CB)
test [Alcoholg]1 = [Alcoholg]2 = [Alcoholg]3, mtest(b) // bonferroni-adjusted p-values for multiple group Fatg

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

