// Analysing NDNS survey data in stata
// for CW3CB3 survey data analysis on hypertension
// date created: 2018-09-05
// manipulation of the data was done in R
// These analysis are done for revision of paper submitted to Nutrients 
// import data from CW3CB3_7sregss.dta


log using "/Users/wangcc.me/Documents/LSHTMproject/stata/forAJCN.txt", append

use "/Users/wangcc.me/Documents/LSHTMproject/Rcode/CW3CB3_7regss.dta", clear
use "/Users/wangcc.me/Documents/LSHTMproject/Rcode/CW3CB3_7regss_withCHOdetail.dta", clear // with CHO details



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
gen Obesity = BMI >=  30
tab Obesity


gen DM = (A1C >= 6.5 | Glu >= 7 | DM4cat == 3) if !missing(A1C)
gen DM_undiag = DM4cat == 2 if !missing(DM4cat)
gen DM_forundiag = DM4cat != 3 if !missing(DM4cat)
gen nonDM = A1C <= 6.5 if !missing(A1C)
gen DM_a1conly = A1C >= 6.5 if !missing(A1C)

tab DM
tab DM_a1c
tab DM4cat



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




********************************************************
********************************************************
**   Building the GLM model 
**   date: 2019-09-05
**
**
********************************************************
********************************************************



// blood weighting for DM endpoint logistic regression 
svyset area [pweight = wtb1to8], strata(gor)
svydescribe wti // describe the weighted data set


// tabulation \
tabulate DM CB, col
svy: tabulate DM CB, col se ci format(%7.3f)


// crude association between CB and DM 

svy: logistic DM i.CB
svy: logistic DM ib3.CB


// Survey: Logistic regression
// 
// Number of strata   =        12                  Number of obs     =      6,020
// Number of PSUs     =     1,413                  Population size   = 5,809.6202
//                                                 Design df         =      1,401
//                                                 F(   2,   1400)   =       4.37
//                                                 Prob > F          =     0.0128
// 
// ------------------------------------------------------------------------------
//              |             Linearized
//           DM | Odds Ratio   Std. Err.      t    P>|t|     [95% Conf. Interval]
// -------------+----------------------------------------------------------------
//           CB |
//           2  |   .4908584   .1356104    -2.58   0.010     .2854893    .8439615
//           3  |   .5727715   .1345976    -2.37   0.018     .3612281    .9081994
//              |
//        _cons |   .0737548   .0124078   -15.50   0.000     .0530235    .1025916
// ------------------------------------------------------------------------------
// Note: _cons estimates baseline odds.




// looking for confounder one by one
// Age: -> confounder 

svy: logistic DM i.CB age
test age

// Survey: Logistic regression
// 
// Number of strata   =        12                  Number of obs     =      6,020
// Number of PSUs     =     1,413                  Population size   = 5,809.6202
//                                                 Design df         =      1,401
//                                                 F(   3,   1399)   =      17.90
//                                                 Prob > F          =     0.0000
// 
// ------------------------------------------------------------------------------
//              |             Linearized
//           DM | Odds Ratio   Std. Err.      t    P>|t|     [95% Conf. Interval]
// -------------+----------------------------------------------------------------
//           CB |
//           2  |    .657772   .1867198    -1.48   0.140     .3769106    1.147922
//           3  |   .5302728   .1288586    -2.61   0.009     .3292114    .8541297
//              |
//          age |   1.041354   .0065117     6.48   0.000     1.028658    1.054206
//        _cons |   .0080009   .0032002   -12.07   0.000     .0036507    .0175345
// ------------------------------------------------------------------------------
// Note: _cons estimates baseline odds.



// Sex  -->  confounder

svy: logistic DM i.CB i.Sex
test 2.Sex



// Survey: Logistic regression
// 
// Number of strata   =        12                  Number of obs     =      6,020
// Number of PSUs     =     1,413                  Population size   = 5,809.6202
//                                                 Design df         =      1,401
//                                                 F(   3,   1399)   =       5.73
//                                                 Prob > F          =     0.0007
// 
// ------------------------------------------------------------------------------
//              |             Linearized
//           DM | Odds Ratio   Std. Err.      t    P>|t|     [95% Conf. Interval]
// -------------+----------------------------------------------------------------
//           CB |
//           2  |   .4952995   .1379797    -2.52   0.012     .2867719     .855459
//           3  |   .5946595   .1425247    -2.17   0.030     .3716042    .9516036
//              |
//          Sex |
//       Women  |   .6012816   .1268155    -2.41   0.016     .3975547    .9094083
//        _cons |   .0912921   .0161155   -13.56   0.000     .0645718    .1290693
// ------------------------------------------------------------------------------
// Note: _cons estimates baseline odds.



// Partner -> not confounder

svy: logistic DM i.CB i.Married

test 1.Married


// Survey: Logistic regression
// 
// Number of strata   =        12                  Number of obs     =      6,020
// Number of PSUs     =     1,413                  Population size   = 5,809.6202
//                                                 Design df         =      1,401
//                                                 F(   3,   1399)   =       3.05
//                                                 Prob > F          =     0.0275
// 
// ------------------------------------------------------------------------------
//              |             Linearized
//           DM | Odds Ratio   Std. Err.      t    P>|t|     [95% Conf. Interval]
// -------------+----------------------------------------------------------------
//           CB |
//           2  |   .5104042    .141344    -2.43   0.015     .2964769    .8786939
//           3  |   .5666154   .1343461    -2.40   0.017     .3558702    .9021632
//              |
//      Married |
//         Yes  |   1.241162   .2688308     1.00   0.319     .8115256    1.898256
//        _cons |   .0649094   .0125716   -14.12   0.000     .0443921    .0949095
// ------------------------------------------------------------------------------



// Income ->  confounder
svy: logistic DM i.CB eqvinc 
test eqvinc


// Survey: Logistic regression
// 
// Number of strata   =        12                  Number of obs     =      5,738
// Number of PSUs     =     1,408                  Population size   = 5,061.9306
//                                                 Design df         =      1,396
//                                                 F(   3,   1394)   =       3.85
//                                                 Prob > F          =     0.0092
// 
// ------------------------------------------------------------------------------
//              |             Linearized
//           DM | Odds Ratio   Std. Err.      t    P>|t|     [95% Conf. Interval]
// -------------+----------------------------------------------------------------
//           CB |
//           2  |   .4774794   .1442432    -2.45   0.015     .2639912    .8636143
//           3  |   .6728864   .1739186    -1.53   0.126     .4052693    1.117223
//              |
//       eqvinc |   .9999855   5.93e-06    -2.45   0.014     .9999738    .9999971
//        _cons |   .0986657   .0253355    -9.02   0.000     .0596216    .1632784
// ------------------------------------------------------------------------------
// Note: _cons estimates baseline odds.



// 
// Education -> confounder

svy: logistic DM i.CB i.Edu
test 1.Edu

// Survey: Logistic regression
// 
// Number of strata   =        12                  Number of obs     =      5,993
// Number of PSUs     =     1,413                  Population size   = 5,742.1866
//                                                 Design df         =      1,401
//                                                 F(   3,   1399)   =       6.67
//                                                 Prob > F          =     0.0002
// 
// -----------------------------------------------------------------------------------
//                   |             Linearized
//                DM | Odds Ratio   Std. Err.      t    P>|t|     [95% Conf. Interval]
// ------------------+----------------------------------------------------------------
//                CB |
//                2  |   .4679858   .1310621    -2.71   0.007     .2701734      .81063
//                3  |   .5490867   .1300445    -2.53   0.011     .3450403    .8737999
//                   |
//         Education |
// Degree or higher  |   .3394829   .1065521    -3.44   0.001     .1834103    .6283652
//             _cons |   .0941728    .016843   -13.21   0.000     .0663062    .1337508
// -----------------------------------------------------------------------------------



// BMI -> confounder
svy: logistic DM i.CB bmi
test bmi

// Survey: Logistic regression
// 
// Number of strata   =        12                  Number of obs     =      5,912
// Number of PSUs     =     1,410                  Population size   = 5,573.5979
//                                                 Design df         =      1,398
//                                                 F(   3,   1396)   =      18.82
//                                                 Prob > F          =     0.0000
// 
// ------------------------------------------------------------------------------
//              |             Linearized
//           DM | Odds Ratio   Std. Err.      t    P>|t|     [95% Conf. Interval]
// -------------+----------------------------------------------------------------
//           CB |
//           2  |   .4882773    .146931    -2.38   0.017     .2705848    .8811089
//           3  |   .6469837   .1602828    -1.76   0.079     .3979561    1.051845
//              |
//       bmival |   1.139135   .0198948     7.46   0.000     1.100769    1.178838
//        _cons |   .0014918    .000799   -12.15   0.000     .0005217    .0042656
// ------------------------------------------------------------------------------
// Note: _cons estimates baseline odds.



// paid employment -> confounder
svy: logistic DM i.CB i.paid

test 2.paid

// // Smoking -> not confounder

svy: logistic DM i.CB i.cigsta3

test 2.cigsta3 3.cigsta3


// Survey: Logistic regression
// 
// Number of strata   =        12                  Number of obs     =      6,019
// Number of PSUs     =     1,413                  Population size   = 5,809.0191
//                                                 Design df         =      1,401
//                                                 F(   4,   1398)   =       2.48
//                                                 Prob > F          =     0.0425
// 
// ------------------------------------------------------------------------------
//              |             Linearized
//           DM | Odds Ratio   Std. Err.      t    P>|t|     [95% Conf. Interval]
// -------------+----------------------------------------------------------------
//           CB |
//           2  |    .504932    .142524    -2.42   0.016      .290243    .8784237
//           3  |   .5758068   .1349457    -2.36   0.019     .3635945     .911877
//              |
//      cigsta3 |
//   ex-smoker  |   1.238773   .4217819     0.63   0.530      .635215    2.415808
//       Never  |    .938022   .2828533    -0.21   0.832     .5191799     1.69476
//              |
//        _cons |   .0713952    .021584    -8.73   0.000      .039456    .1291887
// ------------------------------------------------------------------------------
// Note: _cons estimates baseline odds.



// Total energy intake -> confounder
svy: logistic DM i.CB EnergykJ


// Survey: Logistic regression
// 
// Number of strata   =        12                  Number of obs     =      6,020
// Number of PSUs     =     1,413                  Population size   = 5,809.6202
//                                                 Design df         =      1,401
//                                                 F(   3,   1399)   =       4.78
//                                                 Prob > F          =     0.0025
// 
// ------------------------------------------------------------------------------
//              |             Linearized
//           DM | Odds Ratio   Std. Err.      t    P>|t|     [95% Conf. Interval]
// -------------+----------------------------------------------------------------
//           CB |
//           2  |   .4405679   .1201658    -3.01   0.003     .2580151     .752282
//           3  |   .5474338   .1313757    -2.51   0.012     .3418847    .8765638
//              |
//     EnergykJ |   .9998655   .0000562    -2.39   0.017     .9997552    .9999759
//        _cons |   .2128967   .1010365    -3.26   0.001     .0839182    .5401091
// ------------------------------------------------------------------------------


test EnergykJ



// ethnicity -> not confounder
svy: logistic DM i.CB i.ethgrp2

test 2.eth

// Survey: Logistic regression
// 
// Number of strata   =        12                  Number of obs     =      6,020
// Number of PSUs     =     1,413                  Population size   = 5,809.6202
//                                                 Design df         =      1,401
//                                                 F(   3,   1399)   =       4.49
//                                                 Prob > F          =     0.0038
// 
// ------------------------------------------------------------------------------
//              |             Linearized
//           DM | Odds Ratio   Std. Err.      t    P>|t|     [95% Conf. Interval]
// -------------+----------------------------------------------------------------
//           CB |
//           2  |   .4504773   .1190232    -3.02   0.003     .2682733    .7564291
//           3  |   .5565858   .1312077    -2.49   0.013      .350509    .8838225
//              |
//      ethgrp2 |
//   non-White  |   1.619064   .5215873     1.50   0.135     .8606155    3.045922
//        _cons |   .0717933   .0123851   -15.27   0.000     .0511819    .1007051
// ------------------------------------------------------------------------------



// Alcohol -> confounder 

svy: logistic DM i.CB Alcoholg
test Alcoholg


// Survey: Logistic regression
// 
// Number of strata   =        12                  Number of obs     =      6,020
// Number of PSUs     =     1,413                  Population size   = 5,809.6202
//                                                 Design df         =      1,401
//                                                 F(   3,   1399)   =       7.52
//                                                 Prob > F          =     0.0001
// 
// ------------------------------------------------------------------------------
//              |             Linearized
//           DM | Odds Ratio   Std. Err.      t    P>|t|     [95% Conf. Interval]
// -------------+----------------------------------------------------------------
//           CB |
//           2  |   .3723622   .1047227    -3.51   0.000     .2144706    .6464925
//           3  |   .4305311    .105113    -3.45   0.001     .2666897    .6950289
//              |
//     Alcoholg |   .9590136   .0121257    -3.31   0.001     .9355197    .9830975
//        _cons |   .1273092   .0256875   -10.22   0.000     .0856963    .1891285
// ------------------------------------------------------------------------------
// Note: _cons estimates baseline odds.




// logMVP -> confounder 

svy: logistic DM i.CB logMVP
test logMVP

// Survey: Logistic regression
// 
// Number of strata   =        12                  Number of obs     =      5,085
// Number of PSUs     =     1,377                  Population size   = 3,440.6385
//                                                 Design df         =      1,365
//                                                 F(   3,   1363)   =       3.92
//                                                 Prob > F          =     0.0085
// 
// ------------------------------------------------------------------------------
//              |             Linearized
//           DM | Odds Ratio   Std. Err.      t    P>|t|     [95% Conf. Interval]
// -------------+----------------------------------------------------------------
//           CB |
//           2  |   .3342087   .1373474    -2.67   0.008     .1492434    .7484114
//           3  |    .690585   .2043248    -1.25   0.211     .3864977    1.233921
//              |
//       logMVP |   .5655254   .1657353    -1.94   0.052      .318254    1.004917
//        _cons |   .0976077   .0270833    -8.39   0.000     .0566357      .16822
// ------------------------------------------------------------------------------
// Note: _cons estimates baseline odds.


////  Preliminary model includes all possible confounders in total sample

svy: logistic DM i.CB i.Sex age i.Edu bmi i.paid EnergykJ logMVP
svy: logistic DM i.CB i.Sex age bmi i.paid EnergykJ logMVP
svy: logistic DM i.CB i.Sex age bmi i.paid EnergykJ
svy: logistic DM i.CB i.Sex age bmi i.cigsta3 Alcoholg EnergykJ 
svy: logistic DM ib3.CB i.Sex age bmi i.cigsta3 Alcoholg EnergykJ 
svy: logistic DM_a ib3.CB i.Sex age bmi i.cigsta3 Alcoholg EnergykJ 
svy: logistic DM ib3.CB i.Sex age wstval i.cigsta3 Alcoholg EnergykJ 
svy: logistic DM_a ib3.CB i.Sex age wstval i.cigsta3 Alcoholg EnergykJ 




// same analysis for undiagnosed DM

tabulate DM4cat CB, col
tabulate DM_undiag CB if DM_forundi, col 
svy: tabulate DM_undiag CB , col se ci format(%7.3f)


svy, subpop(DM_forundi): logistic DM_undiag ib3.CB
svy, subpop(DM_forundi): logistic DM_undiag ib3.CB i.Sex age bmi i.cigsta3 Alcoholg EnergykJ 
svy, subpop(DM_forundi): logistic DM_undiag ib3.CB i.Sex age wstval i.cigsta3 Alcoholg EnergykJ 


// individual weighting for Diabetes diagnosed endpoint logistic regression 
table DM_1
svyset area [pweight = wti1to8], strata(gor)
svydescribe wti // describe the weighted data set

gen DM_dia = DM4cat == 3 if !missing(DM4cat)
tab DM_dia CB, col
svy: tabulate DM_dia CB, col se ci format(%7.3f)

svy: logistic DM_dia ib3.CB
svy: logistic DM_dia ib3.CB i.Sex age bmi i.cigsta3 Alcoholg EnergykJ 
svy: logistic DM_dia ib3.CB i.Sex age wstval i.cigsta3 Alcoholg EnergykJ 




**********************************************************
// obesity outcome                                     //
**********************************************************



// individual weighting for obesity endpoint logistic regression 
svyset area [pweight = wti1to8], strata(gor)
svydescribe wti // describe the weighted data set

// tabulation 

tabulate Obesity CB, col
svy: tabulate Obesity CB, col se ci format(%7.3f)


// crude association between CB and DM 
svy: logistic Obesity i.CB
svy: logistic Obesity ib3.CB

// Age confounder 
svy: logistic Obesity i.CB age
test age

 // Survey: Logistic regression
 // 
 // Number of strata   =        12                  Number of obs     =      6,155
 // Number of PSUs     =     1,414                  Population size   =      6,155
 //                                                 Design df         =      1,402
 //                                                 F(   3,   1400)   =      22.44
 //                                                 Prob > F          =     0.0000
 // 
 // ------------------------------------------------------------------------------
 //              |             Linearized
 //      Obesity | Odds Ratio   Std. Err.      t    P>|t|     [95% Conf. Interval]
 // -------------+----------------------------------------------------------------
 //           CB |
 //           2  |   1.020557   .1190483     0.17   0.862     .8118176    1.282968
 //           3  |   .7752201   .0818364    -2.41   0.016     .6302168    .9535864
 //              |
 //          age |   1.018847   .0024506     7.76   0.000     1.014052    1.023666
 //        _cons |   .1930625   .0298814   -10.63   0.000     .1425078    .2615515
 // ------------------------------------------------------------------------------
 // Note: _cons estimates baseline odds.

 
// Sex -> not confounder 

svy: logistic Obesity i.CB i.Sex
test 2.Sex


// partner  -> confounder


svy: logistic Obesity i.CB i.Married
test 1.Married


// equivinc  -> confounder

svy: logistic Obesity i.CB eqvinc

test eqvinc 


// education ->  confounder 

svy: logistic Obesity i.CB i.Edu

test 1.Edu


// HbA1c -> confounder

svy: logistic Obesity i.CB logA1C

test logA1C



// DM  -> confounder 

svy: logistic Obesity i.CB i.DM



// Smoking -> confoudner 

svy: logistic Obesity i.CB i.cigsta3

test 2.cigsta3 3.cigsta3


// paid employment -> confounder
svy: logistic Obesity i.CB i.paid

test 2.paid



// Total energy intake -> confounder
svy: logistic Obesity i.CB EnergykJ

test Energy



// ethnicity -> not confounder
svy: logistic Obesity i.CB i.ethgrp2

test 2.eth


// Alcohol -> confounder 

svy: logistic Obesity i.CB Alcoholg
test Alcoholg



// logMVP -> confounder 

svy: logistic Obesity i.CB logMVP
test logMVP


////  Preliminary model includes all possible confounders in total sample


svy: logistic Obesity i.CB age i.Sex i.Educ ///
 i.cigsta3  EnergykJ Alcoholg logMVP
 
 svy: logistic Obesity ib3.CB age i.Sex i.Educ ///
 i.cigsta3  EnergykJ Alcoholg logMVP
 
//BMI continuous outcome 

//  Preliminary model includes all possible confounders in total sample

svy: regress bmival i.CB i.Sex age i.Edu i.cig EnergykJ Alcoholg logMVP
svy: regress bmival i.CB##i.Sex age i.Edu i.cig EnergykJ Alcoholg logMVP

test 2.CB#2.Sex 3.CB#2.Sex　 // -> there is no need to stratify or use gender as an interaction




//  Preliminary model includes all possible confounders in Men and women
svy, subpop(Men): regress bmival i.CB age i.Edu i.cig EnergykJ Alcoholg logMVP
svy, subpop(Women): regress bmival i.CB  age i.Edu i.cig EnergykJ Alcoholg logMVP


// A1C continuous outcome 


svyset area [pweight = wtb1to8], strata(gor)
svyset area [pweight = wtn1to8], strata(gor)

svy: regress A1C i.CB
svy, subpop(nonDM): regress A1C i.CB


svy: regress A1C i.CB i.Sex 
svy: regress A1C i.CB i.Sex age 
svy: regress A1C i.CB i.Sex age bmi 
svy: regress A1C i.CB i.Sex age bmi i.cigsta3
svy: regress A1C i.CB i.Sex age bmi i.cigsta3 Alcoholg  
svy, subpop(nonDM): regress A1C i.CB i.Sex age bmi i.cigsta3 Alcoholg EnergykJ 
svy, subpop(nonDM): regress A1C i.CB i.Sex age bmi i.cigsta3 Alcoholg EnergykJ 



log close
