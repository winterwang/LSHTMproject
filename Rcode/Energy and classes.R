
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


FoodbyCW <- Energy %>% 
  group_by(CW_new, TimeSlot) %>% 
  summarise(MeanEnergy = mean(Tot_Energ), sdEnergy = sd(Tot_Energ),
            MeanCarb = mean(Tot_Carb), sdCarb = sd(Tot_Carb), 
            MeanSugar = mean(Tot_Sugar), sdSugar = sd(Tot_Sugar), 
            MeanStarch = mean(Tot_Starch), sdStarch = sd(Tot_Starch), 
            meanFibre = mean(Tot_Fibre), sdFibre = sd(Tot_Fibre))


FoodbyCB <- Energy %>% 
  group_by(CB, TimeSlot) %>% 
  summarise(MeanEnergy = mean(Tot_Energ), sdEnergy = sd(Tot_Energ),
            MeanCarb = mean(Tot_Carb), sdCarb = sd(Tot_Carb), 
            MeanSugar = mean(Tot_Sugar), sdSugar = sd(Tot_Sugar), 
            MeanStarch = mean(Tot_Starch), sdStarch = sd(Tot_Starch), 
            meanFibre = mean(Tot_Fibre), sdFibre = sd(Tot_Fibre))



FoodbyCW_slots <- Energy %>% 
  group_by(CW_new, Slot3g) %>% 
  summarise(MeanEnergy = mean(Tot_Energ), sdEnergy = sd(Tot_Energ),
            MeanCarb = mean(Tot_Carb), sdCarb = sd(Tot_Carb), 
            MeanSugar = mean(Tot_Sugar), sdSugar = sd(Tot_Sugar), 
            MeanStarch = mean(Tot_Starch), sdStarch = sd(Tot_Starch), 
            meanFibre = mean(Tot_Fibre), sdFibre = sd(Tot_Fibre))


FoodbyCB_slots <- Energy %>% 
  group_by(CB, Slot3g) %>% 
  summarise(MeanEnergy = mean(Tot_Energ), sdEnergy = sd(Tot_Energ),
            MeanCarb = mean(Tot_Carb), sdCarb = sd(Tot_Carb), 
            MeanSugar = mean(Tot_Sugar), sdSugar = sd(Tot_Sugar), 
            MeanStarch = mean(Tot_Starch), sdStarch = sd(Tot_Starch), 
            meanFibre = mean(Tot_Fibre), sdFibre = sd(Tot_Fibre))

var <- c("Tot_Energ", "Tot_Carb", "Tot_Sugar", "Tot_Starch", "Tot_Fibre")



Tableone <- tableone::CreateTableOne(vars = vars, strata = "CW_new", data = Energy)
Tableone


write.csv(FoodbyCW, file = "/home/wangcc-me/Documents/LSHTMproject/Tablecsv/FoodbyCW.csv")
write.csv(FoodbyCB, file = "/home/wangcc-me/Documents/LSHTMproject/Tablecsv/FoodbyCB.csv")
write.csv(FoodbyCW_slots, file = "/home/wangcc-me/Documents/LSHTMproject/Tablecsv/FoodbyCW_slots.csv")
write.csv(FoodbyCB_slots, file = "/home/wangcc-me/Documents/LSHTMproject/Tablecsv/FoodbyCB_slots.csv")



# long to wide data -------------------------------------------------------

x <- Energy %>% 
  select(id_dy.x, Tot_Carb, TimeSlot, ID, DayofWeek)

x_wide <- x %>% 
  spread(key = TimeSlot, value = Tot_Carb)


CW3idday <- CW3idday %>% 
  left_join(x_wide, by = c("ID", "DayofWeek"))


FoodbyCW <- CW3idday %>% 
  group_by(CW_new) %>% 
  summarise(MeanCarb = mean(CW3idday$`[6,9)`, na.rm = T), sdCarb = sd(CW3idday$`[6,9)`, na.rm = T))


Energy %>% 
  group_by(ID) %>% 
  summarise(SumEnergy = sum(Tot_Energ)) %>% 
  summarise(meanener = mean(SumEnergy))
  