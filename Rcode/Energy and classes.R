
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



personlevel <- read_dta("/home/wangcc-me/Downloads/UKDA-6533-stata11_se/stata11_se/ndns_rp_yr1-4a_personleveldietarydata_uk.dta")
personlevel56 <- read_dta("/home/wangcc-me/Downloads/UKDA-6533-stata11_se/stata11_se/ndns_rp_yr5-6a_personleveldietarydata.dta")
personlevel78 <- read_dta("/home/wangcc-me/Downloads/UKDA-6533-stata11_se/stata11_se/ndns_rp_yr7-8a_personleveldietarydata.dta")
head(personlevel$Ndays)


names(personlevel)[names(personlevel)=="seriali"] <- "ID"
names(personlevel56)[names(personlevel56)=="seriali"] <- "ID"
names(personlevel78)[names(personlevel78)=="seriali"] <- "ID"

personlevel <- personlevel %>% 
  select(ID, Ndays)

personlevel56 <- personlevel56 %>% 
  select(ID, Ndays)

personlevel78 <- personlevel78 %>% 
  select(ID, NDays) %>% 
  rename(Ndays = NDays)

NDAYS <- rbind(personlevel, personlevel56, personlevel78)
NDAYS$ID <- as.numeric(NDAYS$ID)




CW3idday <- read_csv("/home/wangcc-me/Documents/LSHTMproject/Tablecsv/CW3idday.csv")
CW3idday$DayofWeek[CW3idday$ID_DAY == 31012251000] <- "Tuesday"
Energy_slots <- read_csv("/home/wangcc-me/Documents/LSHTMproject/Tablecsv/Energy_slots.csv")
Energy_slots <- Energy_slots %>% 
  rename(ID = id)


CW3idday$CW_new <- 0
CW3idday$CW_new[CW3idday$CW == 1] <- 3
CW3idday$CW_new[CW3idday$CW == 2] <- 1
CW3idday$CW_new[CW3idday$CW == 3] <- 2





Energy <- Energy_slots %>% 
  left_join(CW3idday, by = c("ID", "DayofWeek"))



Energy$Slot3g <- as.character(Energy$TimeSlot)
Energy$Slot3g[Energy$TimeSlot == "[6,9)" | Energy$TimeSlot == "[9,12)"] <- "Before 12 noon"
Energy$Slot3g[Energy$TimeSlot == "[12,14)" | Energy$TimeSlot == "[14,17)" | 
                Energy$TimeSlot == "[17,20)"] <- "Between 12 noon to 8 pm"
Energy$Slot3g[Energy$TimeSlot == "[22, 6)" | Energy$TimeSlot == "[20,22)"] <- "After 8 pm"

Energy$Slot3g <- factor(Energy$Slot3g, levels = c("Before 12 noon", 
                                                  "Between 12 noon to 8 pm", 
                                                  "After 8 pm"))



Energy$TimeSlot <- factor(Energy$TimeSlot, levels = c("[6,9)", "[9,12)", "[12,14)", "[14,17)",
                                                      "[17,20)", "[20,22)", "[22, 6)"))

# 
# FoodbyCW <- Energy %>% 
#   group_by(CW_new, TimeSlot) %>% 
#   summarise(MeanEnergy = mean(Tot_Energ), sdEnergy = sd(Tot_Energ),
#             MeanCarb = mean(Tot_Carb), sdCarb = sd(Tot_Carb), 
#             MeanSugar = mean(Tot_Sugar), sdSugar = sd(Tot_Sugar), 
#             MeanStarch = mean(Tot_Starch), sdStarch = sd(Tot_Starch), 
#             meanFibre = mean(Tot_Fibre), sdFibre = sd(Tot_Fibre))
# 
# 
# FoodbyCB <- Energy %>% 
#   group_by(CB, TimeSlot) %>% 
#   summarise(MeanEnergy = mean(Tot_Energ), sdEnergy = sd(Tot_Energ),
#             MeanCarb = mean(Tot_Carb), sdCarb = sd(Tot_Carb), 
#             MeanSugar = mean(Tot_Sugar), sdSugar = sd(Tot_Sugar), 
#             MeanStarch = mean(Tot_Starch), sdStarch = sd(Tot_Starch), 
#             meanFibre = mean(Tot_Fibre), sdFibre = sd(Tot_Fibre))
# 
# 
# 
# FoodbyCW_slots <- Energy %>% 
#   group_by(CW_new, Slot3g) %>% 
#   summarise(MeanEnergy = mean(Tot_Energ), sdEnergy = sd(Tot_Energ),
#             MeanCarb = mean(Tot_Carb), sdCarb = sd(Tot_Carb), 
#             MeanSugar = mean(Tot_Sugar), sdSugar = sd(Tot_Sugar), 
#             MeanStarch = mean(Tot_Starch), sdStarch = sd(Tot_Starch), 
#             meanFibre = mean(Tot_Fibre), sdFibre = sd(Tot_Fibre))
# 

Energy %>% 
  group_by(ID, TimeSlot) %>% 
  summarise(Tot_Energ = sum(Tot_Energ)/4)



FoodbyCB_7slots <- Energy %>% 
  group_by(ID, TimeSlot) %>% 
  summarise(sumEnergy = sum(Tot_Energ),
            sumCarb = sum(Tot_Carb), 
            sumSugar = sum(Tot_Sugar), 
            sumStarch = sum(Tot_Starch),
            sumFibre = sum(Tot_Fibre), 
            sumNMES = sum(Tot_NMES),
            sumFat = sum(Tot_Fat),
            sumProt = sum(Tot_Prot), 
            sumAlc = sum(Tot_Alc))

FoodbyCB_3slots <- Energy %>% 
  group_by(ID, Slot3g) %>% 
  summarise(sumEnergy = sum(Tot_Energ),
            sumCarb = sum(Tot_Carb), 
            sumSugar = sum(Tot_Sugar), 
            sumStarch = sum(Tot_Starch),
            sumFibre = sum(Tot_Fibre), 
            sumNMES = sum(Tot_NMES),
            sumFat = sum(Tot_Fat),
            sumProt = sum(Tot_Prot), 
            sumAlc = sum(Tot_Alc))


Carbsum <- FoodbyCB_7slots %>% 
  select(ID, TimeSlot, sumCarb) %>% 
  spread(key = TimeSlot, 
         value = sumCarb)

Carbsum[is.na(Carbsum)] <- 0

names(Carbsum) <- c("ID", "Carb6_9", "Carb9_12", "Carb12_14", "Carb14_17",
                     "Carb17_20", "Carb20_22", "Carb22_6")

Energysum <- FoodbyCB_7slots %>% 
  select(ID, TimeSlot, sumEnergy) %>% 
  spread(key = TimeSlot, 
         value = sumEnergy)
Energysum[is.na(Energysum)] <- 0


names(Energysum) <- c("ID", "Energy6_9", "Energy9_12", "Energy12_14", "Energy14_17",
                     "Energy17_20", "Energy20_22", "Energy22_6")

Starchsum <- FoodbyCB_7slots %>% 
  select(ID, TimeSlot, sumStarch) %>% 
  spread(key = TimeSlot, 
         value = sumStarch)
Starchsum[is.na(Starchsum)] <- 0


names(Starchsum) <- c("ID", "Starch6_9", "Starch9_12", "Starch12_14", "Starch14_17",
                       "Starch17_20", "Starch20_22", "Starch22_6")


Sugarsum <- FoodbyCB_7slots %>% 
  select(ID, TimeSlot, sumSugar) %>% 
  spread(key = TimeSlot, 
         value = sumSugar)
Sugarsum[is.na(Sugarsum)] <- 0


names(Sugarsum) <- c("ID", "Sugar6_9", "Sugar9_12", "Sugar12_14", "Sugar14_17",
                      "Sugar17_20", "Sugar20_22", "Sugar22_6")


Fibresum <- FoodbyCB_7slots %>% 
  select(ID, TimeSlot, sumFibre) %>% 
  spread(key = TimeSlot, 
         value = sumFibre)
Fibresum[is.na(Fibresum)] <- 0


names(Fibresum) <- c("ID", "Fibre6_9", "Fibre9_12", "Fibre12_14", "Fibre14_17",
                      "Fibre17_20", "Fibre20_22", "Fibre22_6")


NMESsum <- FoodbyCB_7slots %>% 
  select(ID, TimeSlot, sumNMES) %>% 
  spread(key = TimeSlot, 
         value = sumNMES)
NMESsum[is.na(NMESsum)] <- 0


names(NMESsum) <- c("ID", "NMES6_9", "NMES9_12", "NMES12_14", "NMES14_17",
                      "NMES17_20", "NMES20_22", "NMES22_6")

Fatsum <- FoodbyCB_7slots %>% 
  select(ID, TimeSlot, sumFat) %>% 
  spread(key = TimeSlot, 
         value = sumFat)
Fatsum[is.na(Fatsum)] <- 0


names(Fatsum) <- c("ID", "Fat6_9", "Fat9_12", "Fat12_14", "Fat14_17",
                     "Fat17_20", "Fat20_22", "Fat22_6")


Protsum <- FoodbyCB_7slots %>% 
  select(ID, TimeSlot, sumProt) %>% 
  spread(key = TimeSlot, 
         value = sumProt)
Protsum[is.na(Protsum)] <- 0


names(Protsum) <- c("ID", "Prot6_9", "Prot9_12", "Prot12_14", "Prot14_17",
                    "Prot17_20", "Prot20_22", "Prot22_6")


Alcsum <- FoodbyCB_7slots %>% 
  select(ID, TimeSlot, sumAlc) %>% 
  spread(key = TimeSlot, 
         value = sumAlc)
Alcsum[is.na(Alcsum)] <- 0


names(Alcsum) <- c("ID", "Alc6_9", "Alc9_12", "Alc12_14", "Alc14_17",
                     "Alc17_20", "Alc20_22", "Alc22_6")


# person level dietary intake ---------------------------------------------

# 
# FoodbyCB_ID <- Energy %>% 
#   group_by(ID, DayofWeek) %>% 
#   summarise(MeanEnergy = sum(Tot_Energ),
#             MeanCarb = sum(Tot_Carb), 
#             MeanSugar = sum(Tot_Sugar), 
#             MeanStarch = sum(Tot_Starch),
#             MeanFibre = sum(Tot_Fibre), 
#             MeanNMES = sum(Tot_NMES),
#             MeanFat = sum(Tot_Fat),
#             MeanProt = sum(Tot_Prot), 
#             MeanAlc = sum(Tot_Alc))
# 
# FoodbyCB_ID <- FoodbyCB_ID %>% 
#   group_by(ID) %>% 
#   summarise(MeanEnergy = mean(MeanEnergy),
#             MeanCarb = mean(MeanCarb), 
#             MeanSugar = mean(MeanSugar), 
#             MeanStarch = mean(MeanStarch),
#             MeanFibre = mean(MeanFibre), 
#             MeanNMES = mean(MeanNMES),
#             MeanFat = mean(MeanFat),
#             MeanProt = mean(MeanProt), 
#             MeanAlc = mean(MeanAlc))

IntakeSlots <- Energysum %>% 
  left_join(Carbsum, by = "ID") %>% 
  left_join(Sugarsum, by = "ID") %>% 
  left_join(Starchsum, by = "ID") %>% 
  left_join(Fibresum, by = "ID") %>% 
  left_join(Fatsum, by = "ID") %>% 
  left_join(Protsum, by = "ID") %>% 
  left_join(NMESsum, by = "ID") %>% 
  left_join(Alcsum, by = "ID") %>% 
  # left_join(FoodbyCB_ID, by = "ID") %>% 
  left_join(NDAYS, by = "ID")

IntakeSlots$ID <- as.numeric(IntakeSlots$ID)


IntakeSlots$Energy6_9 <- IntakeSlots$Energy6_9/(IntakeSlots$Ndays)

for (i in 3:57){
  IntakeSlots[, i] <- IntakeSlots[, i]/(IntakeSlots$Ndays)
}




library(haven)
CW3CB3_7regss <- read_dta("Rcode/CW3CB3_7regss.dta")
CW3CB3_7regss <- CW3CB3_7regss %>% 
  left_join(IntakeSlots, by = "ID")


CW3CB3_7regss <- CW3CB3_7regss[order(CW3CB3_7regss$ID),]
IntakeSlots <- IntakeSlots[order(IntakeSlots$ID),]


write_dta(CW3CB3_7regss, "/home/wangcc-me/Downloads/UKDA-6533-stata11_se/stata11_se/CW3CB3_7regss.dta")




# 
# names(Energymean) <- c("ID", "Energy[6,9)", "Energy[9,12)", "Energy[12,14)", "Energy[14,17)",
#                        "Energy[17,20)", "Energy[20,22)", "Energy[22, 6)")
# 
# 
# var <- c("Tot_Energ", "Tot_Carb", "Tot_Sugar", "Tot_Starch", "Tot_Fibre")
# 
# 
# 
# Tableone <- tableone::CreateTableOne(vars = vars, strata = "CW_new", data = Energy)
# Tableone
# 
# 
# write.csv(FoodbyCW, file = "/home/wangcc-me/Documents/LSHTMproject/Tablecsv/FoodbyCW.csv")
# write.csv(FoodbyCB, file = "/home/wangcc-me/Documents/LSHTMproject/Tablecsv/FoodbyCB.csv")
# write.csv(FoodbyCW_slots, file = "/home/wangcc-me/Documents/LSHTMproject/Tablecsv/FoodbyCW_slots.csv")
# write.csv(FoodbyCB_slots, file = "/home/wangcc-me/Documents/LSHTMproject/Tablecsv/FoodbyCB_slots.csv")


# 
# # long to wide data -------------------------------------------------------
# 
# x <- Energy %>% 
#   select(id_dy.x, Tot_Carb, TimeSlot, ID, DayofWeek)
# 
# x_wide <- x %>% 
#   spread(key = TimeSlot, value = Tot_Carb)
# 
# 
# CW3idday <- CW3idday %>% 
#   left_join(x_wide, by = c("ID", "DayofWeek"))
# 
# 
# FoodbyCW <- CW3idday %>% 
#   group_by(CW_new) %>% 
#   summarise(MeanCarb = mean(CW3idday$`[6,9)`, na.rm = T), sdCarb = sd(CW3idday$`[6,9)`, na.rm = T))
# 
# 
# Energy %>% 
#   group_by(ID) %>% 
#   summarise(SumEnergy = sum(Tot_Energ)) %>% 
#   summarise(meanener = mean(SumEnergy))
#   