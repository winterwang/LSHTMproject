library(foreign)
demo <- read.xport("/home/wangcc-me/Downloads/NHANES/demo_c.xpt")
names(demo)


library(survey)
data(api)
srs_design <- svydesign(id = ~1, fpc = ~fpc, data = apisrs)
srs_design

svytotal(~enroll, srs_design)
svymean(~enroll, srs_design)

# summ(apisrs$enroll)
# sqrt(sd(apisrs$enroll))

nofpc <- svydesign(id = ~1, weights = ~pw, data = apisrs)
nofpc

svytotal(~enroll, nofpc)
svymean(~enroll, nofpc)


svytotal(~stype, srs_design)


strat_design <- svydesign(id = ~1, strata = ~stype, fpc = ~fpc, data = apistrat)
strat_design
