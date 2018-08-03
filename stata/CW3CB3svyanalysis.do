// Analysing NDNS survey data in stata
// for CW3CB3 survey data analysis 
// date created: 2018-08-01
// manipulation of the data was done in R

// import data from CW3CB3_7sregss.dta

use "/home/wangcc-me/Downloads/UKDA-6533-stata11_se/stata11_se/CW3CB3_7regss.dta", clear



label define smoking 1 "current" 2 "ex-smoker" 3 "Never"
label values cigsta3 smoking
label define drinking 1 "no" 2 "yes"
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

svy: mean eqvinc, over(CB)
test [eqvinc]1 = [eqvinc]2 = [eqvinc]3, mtest(b)

svy: tabulate ethgrp2 CB, row se ci format(%7.3f)
svy: tabulate Education CB, row se ci format(%7.3f)



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


svy: tabulate cigsta3 CB, col se ci format(%7.3f)
svy: tabulate dnnow CB, col se ci format(%7.3f)
svy: tabulate hibp CB, col se ci format(%7.3f)



*********************************************************
//  re-weighting use wtn to see the BMI,wc measurements //
//                                                     //
*********************************************************

svyset area [pweight = wtn1to8], strata(gor)
svy: mean wst, over(CB)
test [wstval]1 = [wstval]2 = [wstval]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean bmi, over(CB)

test [bmival]1 = [bmival]2 = [bmival]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison
