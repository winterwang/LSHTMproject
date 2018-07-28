
// Analysing NDNS survey data in stata
// 



//import delimited /home/wangcc-me/Downloads/UKDA-6533-stata11_se/stata11_se/CW3CB2_regss.dat, delimiter(space) 

//import delimited /home/wangcc-me/Downloads/UKDA-6533-stata11_se/stata11_se/CW2CB2_regss.dat, delimiter(space) clear
use "/home/wangcc-me/Downloads/UKDA-6533-stata11_se/stata11_se/CW2CB2_regss.dta", clear

use "/home/wangcc-me/Downloads/UKDA-6533-stata11_se/stata11_se/CW3CB2_regss.dta", clear


use "/home/wangcc-me/Downloads/UKDA-6533-stata11_se/stata11_se/LCGA_2class.dta", clear


use "/home/wangcc-me/Downloads/UKDA-6533-stata11_se/stata11_se/LCGA_3class.dta", clear


label define smoking 1 "current" 2 "ex-smoker" 3 "Never"
label values cigsta3 smoking
label define drinking 1 "no" 2 "yes"
label values dnnow drinking
label define gender 1 "Men" 2 "Women"
label values Sex gender



brow


*********************************************************
//  weighting use wti to see the BMI, WC    results    //
//                                                     //
*********************************************************


// weighting with individual weights, area is primary sampling unit, gor is the cluster variable
svyset area [pweight = wti1to8], strata(gor)

svydescribe wti // describe the weighted data set



svy: tabulate Sex CB, row se ci format(%7.3f)
svy: tabulate Sex CB, col se ci format(%7.3f)
svy: tabulate Country CB, col se ci format(%7.3f)
svy: tabulate SurveyYear CB, col se ci format(%7.3f)

// when dealing with LCGA classes use C instead of CB
svy: tabulate Sex C, row se ci format(%7.3f)
svy: tabulate Sex C, col se ci format(%7.3f)
svy: tabulate Country C, col se ci format(%7.3f)



svy: mean bmi, over(CB)
test [bmival]1 = [bmival]2 

svy: mean bmi, over(C)
test [bmival]1 = [bmival]2 
test [bmival]1 = [bmival]2 = [bmival]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison

svy: mean age, over(CB)
test [age]1 = [age]2

svy: mean age, over(C)
test [age]1 = [age]2
test [age]1 = [age]2 = [age]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison


*test [bmival]1 - [bmival]2 = -0.5

svy: mean wst, over(CB)
test [wstval]1 = [wstval]2

svy: mean wst, over(C)
test [wstval]1 = [wstval]2
test [wstval]1 = [wstval]2 = [wstval]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison


svy: mean Energy, over(CB)
svy: mean Energy, over(C)
test [EnergykJ]1 = [EnergykJ]2
test [EnergykJ]1 = [EnergykJ]2 = [EnergykJ]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison



svy: mean Carb, over(CB)
svy: mean Carb, over(C)
test [Carbohydrateg]1 = [Carbohydrateg]2
test [Carbohydrateg]1 = [Carbohydrateg]2 = [Carbohydrateg]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison



svy: mean CHO, over(CB)
svy: mean CHO, over(C)
test [CHOpctotE]1 = [CHOpctotE]2
test [CHOpctotE]1 = [CHOpctotE]2 = [CHOpctotE]3, mtest(b) // bonferroni-adjusted p-values for multiple group comparison


svy: tabulate cigsta3 CB, col se ci format(%7.3f)

	


svy: tabulate cigsta3 C, col se ci format(%7.3f)

svy: tabulate dnnow C, col se ci format(%7.3f)


// hypertension

svy: tabulate hibp CB, col se ci format(%7.3f)
svy: tabulate hibp C, col se ci format(%7.3f)





svy: regress bmival i.CB 

svy: regress bmival i.CB AGE SEX

svy: regress bmival cb age sex

svy: glm hibp cb age sex, family(binomial) link(logit)


svydescribe wti

svyset, clear

mean age
svy: regress bmival i.CB

svy: regress bmival i.CB age i.Sex i.cigsta3 i.dnnow


svy: regress wst i.CB age i.Sex i.cigsta3 i.dnnow


svy: regress wstval i.CB#i.Sex age  i.cigsta3 i.dnnow
test 2.CB#2.Sex
test 1.CB#2.Sex
test 2.CB#1.Sex




*********************************************************
//  re-weighting use wtb to see the blood test results //
//                                                     //
*********************************************************

svyset area [pweight = wtb1to8], strata(gor)

gen DM = A1C <= 6.5 if !missing(A1C)


svy, subpop(DM): mean Glucose, over(CB)
test [Glucose]1 = [Glucose]2

svy, subpop(DM): mean A1C, over(CB)
test [A1C]1 = [A1C]2

svy: tabulate DM CB, col se ci format(%7.3f)




svy, subpop(DM): mean Glucose, over(C)
test [Glucose]1 = [Glucose]2
test [Glucose]1 = [Glucose]2 = [Glucose]3, mtest(b)
svy, subpop(DM): mean A1C, over(C)
test [A1C]1 = [A1C]2
test [A1C]1 = [A1C]2 = [A1C]3, mtest(b)

svy: tabulate DM C, col se ci format(%7.3f)
