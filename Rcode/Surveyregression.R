library(haven)
CW3CB3_7regss <- read_dta("Rcode/CW3CB3_7regss.dta")
library(survey)


NDNSsvy <- svydesign(id = ~area, strat = ~gor, 
                     weights=~wtn1to8, data = CW3CB3_7regss, nest = TRUE)
# area and gor are the PSU and stratum identifiers
# nest = TRUE is used to indicate that the PSU identifier needs to be 
# interpreted as nested within stratum. (same PSU id is recycled in different strata)

