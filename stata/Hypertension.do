// Analysing NDNS survey data in stata
// for CW3CB3 survey data analysis on hypertension
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

svy, subpop(Men): tab hibp, se ci format(%7.3f)
svy, subpop(Women): tab hibp, se ci format(%7.3f)

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


svy, subpop(Men): tabulate Married hibp, col  se ci format(%7.3f)

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


svy, subpop(Men): mean Proteing, over(hibp)
test [Proteing]1 = [Proteing]0

svy, subpop(Women): mean Carbohydrateg, over(hibp)
test [Carbohydrateg]1 = [Carbohydrateg]0



svy: tabulate Sex hibp, col se ci format(%7.3f)

svy: logistic hibp i.CB
svy: logistic hibp i.CB#i.Sex

test 2.CB#2.Sex

svy: tabulate paid hibp, col se ci format(%7.3f)



gen DM = A1C > 6.5 if !missing(A1C)
svyset area [pweight = wtb1to8], strata(gor)

svy, subpop(Men): tabulate DM hibp, col se ci format(%7.3f)
svy, subpop(Women): tabulate DM hibp, col se ci format(%7.3f)


svy, subpop(Men): tabulate Married hibp, col se ci format(%7.3f)
svy, subpop(Women): tabulate Married hibp, col se ci format(%7.3f)

svy, subpop(Men): mean eqvinc, over(hibp)
test [eqvinc]1 = [eqvinc]0

svy, subpop(Women): mean eqvinc, over(hibp)
test [eqvinc]1 = [eqvinc]0


********************************************************
********************************************************
**   Building the GLM model 
**   date: 07/08/2018
**
**
********************************************************
********************************************************
svyset area [pweight = wtn1to8], strata(gor)


// crude association between CB and hypertension 

svy, subpop(Men): logistic hibp i.CB

svy, subpop(Women): logistic hibp i.CB


//Number of strata   =        12                  Number of obs     =      5,849
//Number of PSUs     =     1,411                  Population size   = 5,562.7226
//                                                Subpop. no. obs   =      1,190
//                                                Subpop. size      = 2,392.6803
//                                                Design df         =      1,399
//                                                F(   2,   1398)   =       7.23
//                                                Prob > F          =     0.0008
//
//------------------------------------------------------------------------------
//             |             Linearized
//   hibp140_2 | Odds Ratio   Std. Err.      t    P>|t|     [95% Conf. Interval]
//-------------+----------------------------------------------------------------
//          CB |
//          2  |   .4791696   .0938229    -3.76   0.000     .3263451    .7035606
//          3  |   .8158691   .1345127    -1.23   0.217       .59042    1.127405
//             |
//       _cons |   .5707395    .068461    -4.68   0.000      .451073    .7221528
//------------------------------------------------------------------------------


// in non DM 
svy, subpop(Men if DM != 1): logistic hibp i.CB
svy, subpop(Women if DM != 1): logistic hibp i.CB

//Number of strata   =        12                  Number of obs     =      5,858
//Number of PSUs     =     1,412                  Population size   = 5,577.8467
//                                                Subpop. no. obs   =      1,129
//                                                Subpop. size      = 2,283.1986
//                                                Design df         =      1,400
//                                                F(   2,   1399)   =       6.32
//                                                Prob > F          =     0.0018
//
//------------------------------------------------------------------------------
//             |             Linearized
//   hibp140_2 | Odds Ratio   Std. Err.      t    P>|t|     [95% Conf. Interval]
//-------------+----------------------------------------------------------------
//          CB |
//          2  |   .4894474   .0994399    -3.52   0.000     .3285639    .7291087
//          3  |   .8199078   .1389051    -1.17   0.241     .5880761    1.143132
//             |
//       _cons |    .530889   .0665263    -5.05   0.000     .4151895    .6788301
//------------------------------------------------------------------------------


// looking for confounder one by one
// Age: -> confounder
svy, subpop(Men): logistic hibp i.CB age
svy, subpop(Women): logistic hibp i.CB age
//Number of strata   =        12                  Number of obs     =      5,849
//Number of PSUs     =     1,411                  Population size   = 5,562.7226
//                                                Subpop. no. obs   =      1,190
//                                                Subpop. size      = 2,392.6803
//                                                Design df         =      1,399
//                                                F(   3,   1397)   =      49.75
//                                                Prob > F          =     0.0000
//
//------------------------------------------------------------------------------
//             |             Linearized
//   hibp140_2 | Odds Ratio   Std. Err.      t    P>|t|     [95% Conf. Interval]
//-------------+----------------------------------------------------------------
//          CB |
//          2  |   .7040224   .1535932    -1.61   0.108     .4589053    1.080065
//          3  |   .7140916   .1233923    -1.95   0.052     .5087943    1.002226
//             |
//         age |   1.060416   .0051716    12.03   0.000      1.05032     1.07061
//       _cons |    .026696   .0077765   -12.44   0.000     .0150757    .0472732
//------------------------------------------------------------------------------

test age


svy, subpop(Men): logistic hibp i.CB##c.age
svy, subpop(Women): logistic hibp i.CB##c.age
test 2.CB#c.age 3.CB#c.age // no interaction



svy, subpop(Men if DM != 1): logistic hibp i.CB age


//Number of strata   =        12                  Number of obs     =      5,858
//Number of PSUs     =     1,412                  Population size   = 5,577.8467
//                                                Subpop. no. obs   =      1,129
//                                                Subpop. size      = 2,283.1986
//                                                Design df         =      1,400
//                                                F(   3,   1398)   =      44.54
//                                                Prob > F          =     0.0000
//
//------------------------------------------------------------------------------
//             |             Linearized
//   hibp140_2 | Odds Ratio   Std. Err.      t    P>|t|     [95% Conf. Interval]
//-------------+----------------------------------------------------------------
//          CB |
//          2  |   .7058061   .1595794    -1.54   0.124     .4529668    1.099777
//          3  |   .6979756   .1235623    -2.03   0.042     .4931989     .987776
//             |
//         age |   1.058744   .0053173    11.37   0.000     1.048365    1.069227
//       _cons |   .0281335   .0084027   -11.96   0.000     .0156593    .0505446
//------------------------------------------------------------------------------


// Partner -> confounder
svy, subpop(Men): logistic hibp i.CB i.Married
svy, subpop(Women): logistic hibp i.CB i.Married

//Number of strata   =        12                  Number of obs     =      5,849
//Number of PSUs     =     1,411                  Population size   = 5,562.7226
//                                                Subpop. no. obs   =      1,190
//                                                Subpop. size      = 2,392.6803
//                                                Design df         =      1,399
//                                                F(   3,   1397)   =       6.77
//                                                Prob > F          =     0.0002
//
//------------------------------------------------------------------------------
//             |             Linearized
//   hibp140_2 | Odds Ratio   Std. Err.      t    P>|t|     [95% Conf. Interval]
//-------------+----------------------------------------------------------------
//          CB |
//          2  |   .5072514    .099617    -3.46   0.001     .3450758    .7456448
//          3  |   .7823626   .1279722    -1.50   0.134     .5676168    1.078353
//             |
//     Married |
//        Yes  |   1.452791   .2125828     2.55   0.011     1.090289     1.93582
//       _cons |   .4552754   .0688242    -5.21   0.000     .3384435    .6124381
//------------------------------------------------------------------------------

test 1.Married

svy, subpop(Men): logistic hibp i.CB##i.Married
svy, subpop(Women): logistic hibp i.CB##i.Married

test 2.CB#1.Married 3.CB#1.Married // -> no interaction




svy, subpop(Men if DM != 1): logistic hibp i.CB i.Married


// Income -> not confounder for men but confounder for women
svy, subpop(Men): logistic hibp i.CB eqvinc 
svy, subpop(Women): logistic hibp i.CB eqvinc 

//Number of strata   =        12                  Number of obs     =      5,711
//Number of PSUs     =     1,408                  Population size   = 5,271.3875
//                                                Subpop. no. obs   =      1,052
//                                                Subpop. size      = 2,101.3453
//                                                Design df         =      1,396
//                                                F(   3,   1394)   =       3.51
//                                                Prob > F          =     0.0148
//
//------------------------------------------------------------------------------
//             |             Linearized
//   hibp140_2 | Odds Ratio   Std. Err.      t    P>|t|     [95% Conf. Interval]
//-------------+----------------------------------------------------------------
//          CB |
//          2  |   .5209865   .1101245    -3.08   0.002     .3441486    .7886912
//          3  |   .8431483   .1473565    -0.98   0.329     .5984266    1.187947
//             |
//      eqvinc |   .9999951   3.22e-06    -1.52   0.130     .9999888    1.000001
//       _cons |   .6298741   .1118431    -2.60   0.009     .4446105    .8923348
//------------------------------------------------------------------------------

test eqvinc

svy, subpop(Men): logistic hibp i.CB##c.eqvinc 
svy, subpop(Women): logistic hibp i.CB##c.eqvinc 

test 2.CB#c.eqvinc 3.CB#c.eqvinc // 0.0437 maybe some interaction but ignore


svy, subpop(Men if DM != 1): logistic hibp i.CB eqvinc 



// Education -> confounder
svy, subpop(Men): logistic hibp i.CB i.Edu 
svy, subpop(Women): logistic hibp i.CB i.Edu 

//Number of strata   =        12                  Number of obs     =      5,836
//Number of PSUs     =     1,411                  Population size   = 5,539.4682
//                                                Subpop. no. obs   =      1,177
//                                                Subpop. size      = 2,369.4259
//                                                Design df         =      1,399
//                                                F(   3,   1397)   =       6.67
//                                                Prob > F          =     0.0002
//
//-----------------------------------------------------------------------------------
//                  |             Linearized
//        hibp140_2 | Odds Ratio   Std. Err.      t    P>|t|     [95% Conf. Interval]
//------------------+----------------------------------------------------------------
//               CB |
//               2  |   .4661833    .094143    -3.78   0.000     .3136987    .6927887
//               3  |   .7986059   .1332221    -1.35   0.178      .575723    1.107775
//                  |
//        Education |
//Degree or higher  |   .6026215    .102745    -2.97   0.003     .4313123    .8419716
//            _cons |   .6565735   .0864564    -3.20   0.001     .5071094    .8500902
//-----------------------------------------------------------------------------------

test 1.Edu

svy, subpop(Men): logistic hibp i.CB##i.Edu 
svy, subpop(Women): logistic hibp i.CB##i.Edu 

test 2.CB#1.Edu 3.CB#1.Edu // no interaction



svy, subpop(Men if DM != 1): logistic hibp i.CB i.Edu

//Number of strata   =        12                  Number of obs     =      5,845
//Number of PSUs     =     1,412                  Population size   = 5,554.5923
//                                                Subpop. no. obs   =      1,116
//                                                Subpop. size      = 2,259.9442
//                                                Design df         =      1,400
//                                                F(   3,   1398)   =       5.51
//                                                Prob > F          =     0.0009
//
//-----------------------------------------------------------------------------------
//                  |             Linearized
//        hibp140_2 | Odds Ratio   Std. Err.      t    P>|t|     [95% Conf. Interval]
//------------------+----------------------------------------------------------------
//               CB |
//               2  |   .4790785   .0999371    -3.53   0.000     .3181926    .7213124
//               3  |   .8071075   .1382883    -1.25   0.211     .5767159    1.129538
//                  |
//        Education |
//Degree or higher  |   .6305539   .1116721    -2.60   0.009     .4454948    .8924867
//            _cons |   .6024803   .0830197    -3.68   0.000     .4597785    .7894724
//-----------------------------------------------------------------------------------



// BMI -> confounder

svy, subpop(Men): logistic hibp i.CB bmi
svy, subpop(Women): logistic hibp i.CB bmi

//Number of strata   =        12                  Number of obs     =      5,783
//Number of PSUs     =     1,410                  Population size   = 5,465.6161
//                                                Subpop. no. obs   =      1,124
//                                                Subpop. size      = 2,295.5739
//                                                Design df         =      1,398
//                                                F(   3,   1396)   =      21.84
//                                                Prob > F          =     0.0000
//
//------------------------------------------------------------------------------
//             |             Linearized
//   hibp140_2 | Odds Ratio   Std. Err.      t    P>|t|     [95% Conf. Interval]
//-------------+----------------------------------------------------------------
//          CB |
//          2  |   .4783022   .0987749    -3.57   0.000     .3189828    .7171955
//          3  |   .9041457   .1559796    -0.58   0.559     .6445629    1.268269
//             |
//      bmival |   1.128589   .0190075     7.18   0.000     1.091912    1.166498
//       _cons |   .0172859   .0084289    -8.32   0.000     .0066416    .0449895
//------------------------------------------------------------------------------

test bmi

svy, subpop(Men): logistic hibp i.CB##c.bmi
svy, subpop(Women): logistic hibp i.CB##c.bmi

test 2.CB#c.bmival 3.CB#c.bmival // no ineraction



svy, subpop(Men  if DM != 1): logistic hibp i.CB bmi

// paid employment

svy, subpop(Men): logistic hibp i.CB i.paid
svy, subpop(Women): logistic hibp i.CB i.paid


svy, subpop(Men): logistic hibp i.CB##i.paid
svy, subpop(Women): logistic hibp i.CB##i.paid

test 2.CB#2.paid 3.CB#2.paid

// Smoking -> confounder


svy, subpop(Men): logistic hibp i.CB i.cigsta3
svy, subpop(Women): logistic hibp i.CB i.cigsta3


//Number of strata   =        12                  Number of obs     =      5,848
//Number of PSUs     =     1,411                  Population size   = 5,556.4986
//                                                Subpop. no. obs   =      1,189
//                                                Subpop. size      = 2,386.4563
//                                                Design df         =      1,399
//                                                F(   4,   1396)   =       7.57
//                                                Prob > F          =     0.0000
//
//------------------------------------------------------------------------------
//             |             Linearized
//   hibp140_2 | Odds Ratio   Std. Err.      t    P>|t|     [95% Conf. Interval]
//-------------+----------------------------------------------------------------
//          CB |
//          2  |   .5336363    .105102    -3.19   0.001     .3626205     .785305
//          3  |   .8374946   .1391206    -1.07   0.286     .6045915    1.160117
//             |
//     cigsta3 |
//  ex-smoker  |   2.216922   .4889972     3.61   0.000      1.43825    3.417171
//      Never  |   1.254094   .2614616     1.09   0.278     .8331294    1.887764
//             |
//       _cons |   .3819755   .0810677    -4.53   0.000     .2518989    .5792216
//------------------------------------------------------------------------------

test 2.cigsta3 3.cigsta3

svy, subpop(Men): logistic hibp i.CB##i.cigsta3
svy, subpop(Women): logistic hibp i.CB##i.cigsta3

test 2.CB#2.cigsta3 2.CB#3.cigsta3 3.CB#2.cigsta3 3.CB#3.cigsta3 // no interaction
 

svy, subpop(Men  if DM != 1): logistic hibp i.CB i.cigsta3



// Total energy intake -> confounder

svy, subpop(Men): logistic hibp i.CB Energy
svy, subpop(Women): logistic hibp i.CB Energy


//Number of strata   =        12                  Number of obs     =      5,849
//Number of PSUs     =     1,411                  Population size   = 5,562.7226
//                                                Subpop. no. obs   =      1,190
//                                                Subpop. size      = 2,392.6803
//                                                Design df         =      1,399
//                                                F(   3,   1397)   =      11.29
//                                                Prob > F          =     0.0000
//
//------------------------------------------------------------------------------
//             |             Linearized
//   hibp140_2 | Odds Ratio   Std. Err.      t    P>|t|     [95% Conf. Interval]
//-------------+----------------------------------------------------------------
//          CB |
//          2  |   .4019295   .0800105    -4.58   0.000     .2719931    .5939391
//          3  |   .7619188    .125625    -1.65   0.099     .5513676    1.052873
//             |
//    EnergykJ |   .9998584   .0000334    -4.23   0.000     .9997928     .999924
//       _cons |   2.097569   .6653538     2.34   0.020     1.125847    3.907988
//------------------------------------------------------------------------------



test Energy

svy, subpop(Men): logistic hibp i.CB##c.Energy
svy, subpop(Women): logistic hibp i.CB##c.Energy

test 2.CB#c.EnergykJ 3.CB#c.EnergykJ // no interaction



// ethnicity -> not confounder

svy, subpop(Men): logistic hibp i.CB i.ethgrp2
svy, subpop(Women): logistic hibp i.CB i.ethgrp2
//Number of strata   =        12                  Number of obs     =      5,848
//Number of PSUs     =     1,411                  Population size   = 5,560.2736
//                                                Subpop. no. obs   =      1,189
//                                                Subpop. size      = 2,390.2313
//                                                Design df         =      1,399
//                                                F(   3,   1397)   =       4.75
//                                                Prob > F          =     0.0027
//
//------------------------------------------------------------------------------
//             |             Linearized
//   hibp140_2 | Odds Ratio   Std. Err.      t    P>|t|     [95% Conf. Interval]
//-------------+----------------------------------------------------------------
//          CB |
//          2  |   .4835438   .0959988    -3.66   0.000     .3275661    .7137937
//          3  |   .8171955   .1348286    -1.22   0.221     .5912418    1.129502
//             |
//     ethgrp2 |
//  non-White  |   .9639389   .2749488    -0.13   0.898     .5508674    1.686755
//       _cons |   .5717565   .0688999    -4.64   0.000     .4513857    .7242267
//------------------------------------------------------------------------------

test 2.eth
svy, subpop(Men): logistic hibp i.CB##i.ethgrp2
svy, subpop(Women): logistic hibp i.CB##i.ethgrp2
test 2.CB#2.ethgrp2 // no interaction

// Alcohol -> not confounder for men but confounder for women


svy, subpop(Men): logistic hibp i.CB Alcoholg
svy, subpop(Women): logistic hibp i.CB Alcoholg

test Alcoholg

svy, subpop(Men): logistic hibp i.CB##c.Alcoholg
svy, subpop(Women): logistic hibp i.CB##c.Alcoholg
test 2.CB#c.Alcoholg 3.CB#c.Alcoholg // no interaction

// logMVP -> not confounder for men but confounder for women


svy, subpop(Men): logistic hibp i.CB logMVP
svy, subpop(Women): logistic hibp i.CB logMVP

test logMVP

svy, subpop(Men): logistic hibp i.CB##c.logMVP
svy, subpop(Women): logistic hibp i.CB##c.logMVP
test 2.CB#c.logMVP 3.CB#c.logMVP // no interaction



//  Preliminary model includes all possible confounders in Men

gen age2 = age^2


svy, subpop(Men): logistic hibp i.CB age i.Married i.Edu bmi i.cig Energy 
linktest
svy, subpop(Men): logistic hibp i.CB age i.Married i.Edu wst i.cig Energy 
linktest



svy, subpop(if Men & DM != 1): logistic hibp i.CB age i.Married i.Edu bmi i.cig Energy 
linktest
svy, subpop(if Men & DM != 1): logistic hibp i.CB age i.Married i.Edu wst i.cig Energy 
linktest



svy, subpop(Women): logistic hibp i.CB age i.Married eqvinc i.Edu bmi i.cig Energy Alcoholg 
linktest
svy, subpop(Women): logistic hibp i.CB age i.Married eqvinc i.Edu wst i.cig Energy Alcoholg 
linktest



svy, subpop(if Women & DM != 1): logistic hibp i.CB age i.Married eqvinc i.Edu bmi i.cig Energy Alcoholg
linktest

svy, subpop(if Women & DM != 1): logistic hibp i.CB age i.Married eqvinc i.Edu wst i.cig Energy Alcoholg
linktest




// svylogitgof



test 2.CB#2.Sex 1.CB#2.Sex 2.CB#1.Sex
test 1.CB#2.Sex
test 2.CB#1.Sex
