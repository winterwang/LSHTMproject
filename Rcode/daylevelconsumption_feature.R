library(RColorBrewer)
library(plyr)
library(epiDisplay)
library(tidyverse)
library(dplyr)
library(scales)
library(readr)
library(kableExtra)
library(haven)
library(naniar)


food14 <- read_dta("~/Downloads/UKDA-6533-stata11_se/stata11_se/ndns_rp_yr1-4a_dayleveldietarydata_nutrients_uk.dta")

food56 <- read_dta("~/Downloads/UKDA-6533-stata11_se/stata11_se/ndns_rp_yr5-6a_dayleveldietarydata_nutrients.dta")

food78 <- read_dta("~/Downloads/UKDA-6533-stata11_se/stata11_se/ndns_rp_yr7-8a_dayleveldietarydata_nutrients.dta")


names(food14)


names(food78)[names(food78)=="seriali"] <- "ID"
names(food56)[names(food56)=="seriali"] <- "ID"
names(food14)[names(food14)=="seriali"] <- "ID"



Energy14 <- food14 %>% 
  select(ID, DayofWeek, Country, SurveyYear, EnergykJ, Carbohydrateg, Proteing, 
         Fatg, Alcoholg, Totalsugarsg, Starchg, Nonmilkextrinsicsugarsg, Fruitg, 
         FoodQuantity, YellowRedGreeng)
Energy14$Country[Energy14$Country == "Northern Ireland"] <- "NI"

Energy56 <- food56 %>% 
  select(ID, DayofWeek, Country, Surveyyear, EnergykJ, Carbohydrateg, Proteing, 
         Fatg, Alcoholg, Totalsugarsg, Starchg, Nonmilkextrinsicsugarsg, Fruitg, 
         FoodQuantity, YellowRedGreeng) %>% 
  rename(SurveyYear = Surveyyear)

Energy78 <- food78 %>% 
  select(ID, DayofWeek, Country, SurveyYear, EnergykJ, Carbohydrateg, Proteing, 
         Fatg, Alcoholg, Totalsugarsg, Starchg, Nonmilkextrinsicsugarsg, Fruitg, 
         UsualDrinkQuantity, YellowRedGreeng) %>% 
  rename(FoodQuantity = UsualDrinkQuantity)


Energy <- bind_rows(Energy14, Energy56, Energy78)






CW3CB3 <- read_table2("~/Documents/LSHTMproject/results/Timeslots/NDNSslot_CW3CB3.txt",  
                      col_names = FALSE)# change the path to your own path

names(CW3CB3) <- c("Breakfast", "Morning.snack", "Lunch",
                   "Afternoon.snack", "Dinner", "Before.bedtime.snack",
                   "Midnight.food", "ID_DAY", "AGE", "SEX", "CPROB1",
                   "CPROB2", "CPROB3", "CPROB4", "CPROB5", "CPROB6", 
                   "CPROB7", "CPROB8", "CPROB9", 
                   "CB", "CW", "MLCJOINT", "ID")


dta_NDNS <- read_csv("/home/wangcc-me/Documents/LSHTMproject/Rcode/dta_NDNS.csv") # extract the day of week data

CW3idday <- CW3CB3 %>% 
  select(ID.x, ID_DAY, CW, CB, MLCJOINT, AGE, SEX) %>% 
  rename(ID = ID.x)

CW3idday$DayNo <- ave(CW3idday$ID_DAY, CW3idday$ID, FUN = seq_along) # adding the day no 

dta_DayofWeek <- dta_NDNS %>% 
  select(id, id_dy, DayofWeek)

dta_DayofWeek$DayNo <- unlist(strsplit(dta_DayofWeek$id_dy,"D"))[c(FALSE, TRUE)] # creating a day no
names(dta_DayofWeek)[1] <- "ID"
dta_DayofWeek$DayNo <- as.numeric(dta_DayofWeek$DayNo)


CW3idday <-   CW3idday %>%  
  left_join(dta_DayofWeek, by= c("ID", "DayNo"))

CW3idday$DayofWeek[CW3idday$ID_DAY == 40714261000] <- "Saturday"
CW3idday$DayofWeek[CW3idday$ID_DAY == 112050710000] <- "Tuesday"
CW3idday$DayofWeek[CW3idday$ID_DAY == 310122510000] <- "Wednesday"
CW3idday$DayofWeek[CW3idday$ID_DAY == 5050616100] <- "Monday"
CW3idday$DayofWeek[CW3idday$ID_DAY == 50506161000] <- "Tuesday"
CW3idday$DayofWeek[CW3idday$ID_DAY == 505061610000] <- "Wednesday"
CW3idday$DayofWeek[CW3idday$ID_DAY == 7090824100] <- "Monday"
CW3idday$DayofWeek[CW3idday$ID_DAY == 70908241000] <- "Tuesday"
CW3idday$DayofWeek[CW3idday$ID_DAY == 709082410000] <- "Wednesday"
CW3idday$DayofWeek[CW3idday$ID_DAY == 807021910000] <- "Monday"
CW3idday$DayofWeek[CW3idday$ID_DAY == 812101310000] <- "Sunday"


with(CW3idday, tabpct(DayofWeek, CW))
with(CW3idday, chisq.test(DayofWeek, CW))


CW3idday <- CW3idday %>% 
  left_join(Energy, by = c("ID", "DayofWeek")) %>% 
  mutate(DayofWeek = factor(DayofWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))

write.csv(CW3idday, file = "/home/wangcc-me/Documents/LSHTMproject/Tablecsv/CW3idday.csv")

CW3idday$Weekend <- ifelse((CW3idday$DayofWeek == "Sunday" | CW3idday$DayofWeek == "Saturday"), TRUE, FALSE)
# association of day level nutrients and CW3 ------------------------------

# 
# with(CW3idday, summ(EnergykJ, by = CW))
# with(CW3idday, summ(Carbohydrateg, by = CW))
# with(CW3idday, summ(Carbohydrateg, by = CW))



vars <- c("Country", "DayofWeek", "Weekend", "EnergykJ", "Carbohydrateg", "Proteing", 
          "Fatg", "Alcoholg", "Totalsugarsg", "Starchg", "Nonmilkextrinsicsugarsg", "Fruitg", 
          "FoodQuantity", "YellowRedGreeng")

CW3idday$CW_new <- 0
CW3idday$CW_new[CW3idday$CW == 1] <- 3
CW3idday$CW_new[CW3idday$CW == 2] <- 1
CW3idday$CW_new[CW3idday$CW == 3] <- 2



Tableone <- tableone::CreateTableOne(vars = vars, strata = "CW_new", data = CW3idday)
Tableone
summary(Tableone)
print(Tableone, quote = TRUE)

tab3Mat <- print(Tableone, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(tab3Mat, file = "/home/wangcc-me/Documents/LSHTMproject/Tablecsv/Daylevel.csv")
