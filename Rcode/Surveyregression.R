library(haven)
CW3CB3_7regss <- read_dta("Rcode/CW3CB3_7regss.dta")
library(survey)


NDNSsvy <- svydesign(id = ~area, strat = ~gor, 
                     weights=~wtn1to8, data = CW3CB3_7regss, nest = TRUE)
# area and gor are the PSU and stratum identifiers
# nest = TRUE is used to indicate that the PSU identifier needs to be 
# interpreted as nested within stratum. (same PSU id is recycled in different strata)

svymean(~age, subset(NDNSsvy, Sex == 1))


# hypertension binomial ---------------------------------------------------

m <- svyglm(hibp140_2 ~ factor(CB), design = subset(NDNSsvy, Sex == 1), 
            family = quasibinomial())
summary(m)


# BMI(cont.) linear regression --------------------------------------------

m.BMI <- svyglm(bmival ~  age, subset(NDNSsvy, Sex == 1))
summary(m.BMI)
nonmissing <- NDNSsvy[m.BMI$na.action]
plot(m.BMI, panel = make.panel.svysmooth(nonmissing),
     partial = TRUE, se = TRUE, 
     smooth = make.panel.svysmooth(nonmissing))
plot(svysmooth(bmival ~ age, subset(NDNSsvy, Sex == 1)))

lines(svysmooth(bmival ~ age, subset(NDNSsvy, Sex == 2)))

f.BMI <- svyglm(bmival ~ factor(CB) + age, subset(NDNSsvy, Sex == 2))
summary(f.BMI)

plot(m.BMI)



# survey plots ------------------------------------------------------------

svyboxplot(bmival~factor(CB), subset(NDNSsvy, Sex == 1), col="gray80",
           varwidth=TRUE, ylab="Body mass index (kg/m^2)",
           xlab="Carbohydrate eaters groups")
svyboxplot(bmival~factor(CB), subset(NDNSsvy, Sex == 2), col="gray80",
           varwidth=TRUE, ylab="Body mass index (kg/m^2)",
           xlab="Carbohydrate eaters groups")
svyboxplot(wstval~factor(CB), subset(NDNSsvy, Sex == 1), col="gray80",
           varwidth=TRUE, ylab="Body mass index (kg/m^2)",
           xlab="Carbohydrate eaters groups")

svyboxplot(wstval~factor(CB), subset(NDNSsvy, Sex == 2), col="gray80",
           varwidth=TRUE, ylab="Body mass index (kg/m^2)",
           xlab="Carbohydrate eaters groups")

bys <- svyby(~hibp140_2, ~Sex+CB, design = NDNSsvy)
plot(svysmooth(hibp140_2~age, design = subset(NDNSsvy, Sex == 1 & CB == 1), 
     ylim =c(0, 1)))
lines(svysmooth(hibp140_2~age, design = subset(NDNSsvy, Sex == 1 & CB ==2)))
lines(svysmooth(hibp140_2~age, design = subset(NDNSsvy, Sex == 1 & CB ==3)))
lines(svysmooth(hibp140_2~age, design = subset(NDNSsvy, Sex == 1 & CB ==2)))
lines(svysmooth(hibp140_2~age, design = subset(NDNSsvy, Sex == 2 & CB ==1)))
lines(svysmooth(hibp140_2~age, design = subset(NDNSsvy, Sex == 2 & CB ==2)))
lines(svysmooth(hibp140_2~age, design = subset(NDNSsvy, Sex == 2 & CB ==3)))
barplot(bys)
