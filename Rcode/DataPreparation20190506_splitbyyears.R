
# data preparation 20190506  ----------------------------------------------


# NDNS analysis, data management ------------------------------------------


# Change the data path accordingly ----------------------------------------

# setwd("/home/wangcc-me/Downloads/UKDA-6533-stata11_se/stata11_se/") # on Ubuntu
setwd("/home/takeshi/ドキュメント/NDNS/UKDA-6533-stata/stata/stata11_se/") # on Ubuntu 16.04 in Japan
# setwd("C:/Users/lsh1701745/Downloads/6533STATA11_SE/UKDA-6533-stata11_se/stata11_se/")


library(epiDisplay)
library(plyr)
library(tidyverse)


# Read the data into memory -----------------------------------------------

library(haven)

# Year 1-4 

# data <- read_dta("ndns_rp_yr1-4a_foodleveldietarydata_uk_v2.dta")
data14v1 <- read_dta("/home/takeshi/ドキュメント/NDNS/6533STATA11/UKDA-6533-stata11_se/stata11_se/ndns_rp_yr1-4a_foodleveldietarydata_uk.dta")


names(data14v1)
names(data14v1)[names(data14v1)=="seriali"] <- "id"

df14d <- data14v1[,c(113, 1,2,3,4,5,6,7,8,9,21, 22, 23, 24,53, 55,57,58,59,60,61,62,63,64,65)]

var <- names(df14d) # var for later extract for 5,6 and 7,8 years


dfs2 <- df14d[df14d$Age>=19,]

# Calculate the time (minute and hour) when they eat ----------------------

dfs2$MealTime_chr <- as.character(dfs2$MealTime)
dfs2$MealTime_hm <- unlist(strsplit(dfs2$MealTime_chr," "))[c(FALSE, TRUE)]


dfs2$MealHourN <- as.numeric(unlist(strsplit(dfs2$MealTime_hm,":"))[c(TRUE, FALSE, FALSE)])
dfs2$MealMinN <- as.numeric(unlist(strsplit(dfs2$MealTime_hm,":"))[c(FALSE, TRUE, FALSE)])
dfs2$MealMinN0 <- (60*dfs2$MealHourN)+dfs2$MealMinN


dfs3 <- dfs2[order(dfs2$id,dfs2$DayNo,dfs2$MealMinN0),]


length(unique(dfs3$id)) ## number of participants = 3450


# Create a subset data with only the first observation of each participant --------

NDNS <- dfs3[!duplicated(dfs3$id), ]


with(NDNS, tab1(SurveyYear, graph = FALSE, decimal = 2))

# SurveyYear : 
          #   Frequency Percent Cum. percent
# NDNS Year 1       801   23.22        23.22
# NDNS Year 2       812   23.54        46.75
# NDNS Year 3       782   22.67        69.42
# NDNS Year 4      1055   30.58       100.00
  # Total          3450  100.00       100.00


# how many men and women --------------------------------------------------

with(NDNS, tab1(Sex, graph = FALSE, decimal = 2))

# Sex : 
      #   Frequency Percent Cum. percent
# 1            1443   41.83        41.83
# 2            2007   58.17       100.00
  # Total      3450  100.00       100.00

# create a variable combine id and day No ---------------------------------


dfs3 <- dfs3 %>% 
  mutate(id_dy = paste(id, DayNo, sep = "D"))



# For each subject, the total energy/carbohydrate intake for each eating time can be calculated --------

old<-Sys.time()
Energy <- ddply(dfs3, .(id_dy, id, SurveyYear, DayNo, Age, Sex, DiaryDaysCompleted, MealHourN, DayofWeek),  
                summarise, Tot_Energ = sum(EnergykJ), 
                Tot_Carb = sum(Carbohydrateg), 
                Tot_Sugar = sum(Totalsugarsg), 
                Tot_Starch = sum(Starchg), 
                Tot_Fibre = sum(Englystfibreg),
                Tot_Fat   = sum(Fatg), 
                Tot_Prot  = sum(Proteing), 
                Tot_Alc   = sum(Alcoholg), 
                Tot_NMES  = sum(Nonmilkextrinsicsugarsg))
new<-Sys.time()-old
print(new)

# Time difference of 57.70057 secs  "2019-05-06 12:27:15 JST"


head(Energy)


# reset the time intervals into time slots --------------------------------

### Breakfast:         6am to 9am
### morning snack:   9am to 12noon
### lunch:           12noon to 2pm
### afternoon snack: 2pm to 5pm
### dinner:          5pm to 8pm
### night snack:     8pm to 10pm
### midnight:        10pm to 6am


Energy <- Energy %>% 
  mutate(TimeSlot = cut(MealHourN, breaks = c(6, 9, 12, 14, 17, 20, 22), right = FALSE))


levels(Energy$TimeSlot) <- c(levels(Energy$TimeSlot), "[22, 6)")

Energy$TimeSlot[is.na(Energy$TimeSlot)] <- "[22, 6)"

tab1(Energy$TimeSlot)



# For each subject, the total energy/carbohydrate intake for each 
# time slot can be calculated--------


old<-Sys.time()
Energy <- ddply(Energy, .(id_dy, id, SurveyYear, DayNo, Age, Sex, DiaryDaysCompleted, TimeSlot, DayofWeek),  
                summarise, Tot_Energ = sum(Tot_Energ), Tot_Carb = sum(Tot_Carb), Tot_Sugar = sum(Tot_Sugar),
                Tot_Starch = sum(Tot_Starch), Tot_Fibre = sum(Tot_Fibre), Tot_Fat   = sum(Tot_Fat), 
                Tot_Prot  = sum(Tot_Prot), 
                Tot_Alc   = sum(Tot_Alc), 
                Tot_NMES  = sum(Tot_NMES))
new<-Sys.time()-old
print(new)

# Time difference of 37.63512 secs "2019-05-06 12:28:18 JST"


# Calculate the energy from total carbohydrates ---------------------------

Energy <- Energy %>% 
  mutate(KJcarbo = Tot_Carb*16) %>% 
  mutate(CarKJpercentage = KJcarbo/Tot_Energ) %>% 
  mutate(Carbo = cut(CarKJpercentage, breaks = c(0, 0.50, 2), right = FALSE)) #%>% 
 


Energy0 <- Energy[!(Energy$Tot_Energ == 0), ] # some food consumption does not contain any energy
Energy0$Carbo <- factor(Energy0$Carbo, labels = c("< 50%", ">= 50%"))
with(Energy0, tab1(Carbo))
#Carbo : 
        # Frequency Percent Cum. percent
# < 50%       33056    49.4         49.4
# >= 50%      33878    50.6        100.0
  # Total     66934   100.0        100.0

with(Energy0, tab1(DayofWeek))


# Filter the data by observation day----------------------------------------------------------

dta_day1 <- Energy0 %>% 
  filter(DayNo == 1) %>% 
  select(c("id", "DayNo", "Age", "Sex", "DayofWeek",  "TimeSlot", "Carbo")) %>%  #"MealHourN", "Tot_Carb", "Tot_Energ"
  #"Carbo", "Cbfb_ratio")) %>% 
  mutate(DayofWeek = factor(DayofWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))

dta_day2 <- Energy0 %>% 
  filter(DayNo == 2) %>% 
  select(c("id", "DayNo", "Age", "Sex", "DayofWeek",  "TimeSlot", "Carbo")) %>%  #"MealHourN", "Tot_Carb", "Tot_Energ"
  #"Carbo", "Cbfb_ratio")) %>% 
  mutate(DayofWeek = factor(DayofWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))

dta_day3 <- Energy0 %>% 
  filter(DayNo == 3) %>% 
  select(c("id", "DayNo", "Age", "Sex", "DayofWeek",  "TimeSlot", "Carbo")) %>%  #"MealHourN", "Tot_Carb", "Tot_Energ"
  #"Carbo", "Cbfb_ratio")) %>% 
  mutate(DayofWeek = factor(DayofWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))

dta_day4 <- Energy0 %>% 
  filter(DayNo == 4) %>% 
  select(c("id", "DayNo", "Age", "Sex", "DayofWeek",  "TimeSlot", "Carbo")) %>%  #"MealHourN",  "Tot_Carb", "Tot_Energ"
  #"Carbo", "Cbfb_ratio")) %>% 
  mutate(DayofWeek = factor(DayofWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))



vecid<-unique(Energy$id) # n = 3450
vecid1<-unique(dta_day1$id) # n = 3450
vecid2<-unique(dta_day2$id) # n = 3448
vecid3<-unique(dta_day3$id) # n = 3448
vecid4<-unique(dta_day4$id) # n = 3384

Noday1 <- setdiff(vecid, vecid1) # n = 0
Noday2 <- setdiff(vecid, vecid2) # n = 2, id = 31012251 40714261 with no data on day 2
Noday3 <- setdiff(vecid, vecid3) # n = 2, id = 10914251 11205071 with no data on day 3
Noday4 <- setdiff(vecid, vecid4) # n = 66, id =  [1] 10112011 10701161 10702161 10707261 10906181 10910111 10914251 20106041 20116171 20202081 20205081 20301211 20307041 20405101 20509211
# [16] 20602011 20615041 21002101 21011041 21107031 21113041 21211041 21211101 30113231 30205131 30205201 30402131 30404081 30411081 30417081
# [31] 30603071 30605131 30609131 30708201 30709031 30906071 30906201 30907251 30912021 31110201 40101011 40104021 40109221 40116011 40214081
# [46] 40221221 40315101 40402221 40410251 40504211 40506221 40516021 40710081 40710101 40714251 40714261 40803081 40803221 40808081 40814131
# [61] 40816011 40902051 40904021 41012081 41016131 41202051 with no data on day 4


# Long to wide data -------------------------------------------------------

dta_d1_wide <- dta_day1 %>% #[, -7]
  spread(key = TimeSlot, #MealHourN, #
         value = Carbo #Cbfb_ratio
  )

head(dta_d1_wide)
names(dta_d1_wide)[6:12] <- c("H6_9", "H9_12", "H12_14", "H14_17", "H17_20", "H20_22", "H22_6")
names(dta_d1_wide)


dta_d2_wide <- dta_day2 %>% #[, -7] 
  spread(key = TimeSlot, #MealHourN
         value = Carbo) #Cbfb_ratio)

head(dta_d2_wide)
names(dta_d2_wide)[6:12] <- c("H6_9", "H9_12", "H12_14", "H14_17", "H17_20", "H20_22", "H22_6")
names(dta_d2_wide)


dta_d3_wide <- dta_day3 %>% #[, -7]
  spread(key = TimeSlot, #, MealHourN
         value = Carbo) #Cbfb_ratio)

head(dta_d3_wide)
names(dta_d3_wide)[6:12] <- c("H6_9", "H9_12", "H12_14", "H14_17", "H17_20", "H20_22", "H22_6")
names(dta_d3_wide)


dta_d4_wide <- dta_day4 %>% #[, -7]
  spread(key = TimeSlot, #MealHourN, #, 
         value = Carbo) #Cbfb_ratio)

head(dta_d4_wide)
names(dta_d4_wide)[6:12] <- c("H6_9", "H9_12", "H12_14", "H14_17", "H17_20", "H20_22", "H22_6")
names(dta_d4_wide)


# recode NA to not eating -------------------------------------------------

for (i in 6:ncol(dta_d1_wide)) 
  if(is.factor(dta_d1_wide[,i])) 
    levels(dta_d1_wide[,i]) <- c("1", "2", "0")

dta_d1_wide[is.na(dta_d1_wide)] <- "0"


for (i in 6:ncol(dta_d2_wide)) 
  if(is.factor(dta_d2_wide[,i])) 
    levels(dta_d2_wide[,i]) <- c("1", "2",  "0")


dta_d2_wide[is.na(dta_d2_wide)] <- "0"


for (i in 6:ncol(dta_d3_wide)) 
  if(is.factor(dta_d3_wide[,i])) 
    levels(dta_d3_wide[,i]) <-  c("1", "2", "0")

dta_d3_wide[is.na(dta_d3_wide)] <- "0"


for (i in 6:ncol(dta_d4_wide)) 
  if(is.factor(dta_d4_wide[,i])) 
    levels(dta_d4_wide[,i]) <-  c("1", "2", "0")

dta_d4_wide[is.na(dta_d4_wide)] <- "0"


dta_all <- rbind(dta_d1_wide, dta_d2_wide, dta_d3_wide, dta_d4_wide)

dta_all <- dta_all[order(dta_all$id,dta_all$DayNo),]



# check the difference between newdataset and previous one ----------------

dta_all <- dta_all %>% 
  mutate(id_day = paste(id, DayNo, sep = "D"))

dta_NDNS_Tslots <- read_csv("~/pCloudDrive/LSHTM/LSHTM_remote_desktopfiles/Project/SAS/dta_NDNS_Tslots.csv", 
                            col_names = c("id", "id_day", "Age", "Sex", "DayofWeek", 
                                          "H6_9", "H9_12", "H12_14", "H14_17", "H17_20", "H20_22", "H22_6"))

CHECK <- dta_all %>% 
  left_join(dta_NDNS_Tslots, by = "id_day")

CHECK %>% filter(DayNo == 1) %>% 
  group_by(as.character(H6_9.x)) %>%
  summarise(n = n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 2), "%"))

CHECK %>% filter(DayNo == 1) %>% 
  group_by(H6_9.y) %>%
  summarise(n = n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 2), "%"))


CHECK %>% filter(DayNo == 1) %>% 
  group_by(as.character(H9_12.x)) %>%
  summarise(n = n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 2), "%"))

CHECK %>% filter(DayNo == 1) %>% 
  group_by(H9_12.y) %>%
  summarise(n = n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 2), "%"))

CHECK %>% filter(DayNo == 1) %>% 
  group_by(as.character(H12_14.x)) %>%
  summarise(n = n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 2), "%"))

CHECK %>% filter(DayNo == 1) %>% 
  group_by(H12_14.y) %>%
  summarise(n = n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 2), "%"))

setwd("~/ドキュメント/githubprojects/LSHTMproject/Rcode")

# export the data from R to Mplus -----------------------------------------

library(MplusAutomation)



CHECK <- CHECK %>% 
  mutate(id_day_n = paste(id.x, DayNo, sep = "0")) %>% 
  mutate(id_day_n = as.numeric(id_day_n)) %>% 
  mutate(Sex.x = as.numeric(Sex.x))

prepareMplusData(CHECK, "Mplusdata/NDNS_Tslots_Y14.dat", 
                 keepCols = c("id.x", "DayNo", "Age.x", "Sex.x", "H6_9.y",
                              "H9_12.y", "H12_14.y", "H14_17.y", "H17_20.y",   
                              "H20_22.y", "H22_6.y", "id_day_n"))


# prepare data for year 5-8 -----------------------------------------------
setwd("/home/takeshi/ドキュメント/NDNS/UKDA-6533-stata/stata/stata11_se/") # on Ubuntu 16.04 in Japan

data56 <- read_dta("ndns_rp_yr5-6a_foodleveldietarydata_v2.dta")

data78 <- read_dta("ndns_rp_yr7-8a_foodleveldietarydata.dta")

names(data56)[names(data56)=="seriali"] <- "id"
names(data78)[names(data78)=="seriali"] <- "id"

df56d <- data56 %>% 
  select(var)

df78d <- data78 %>% 
  select(var)


dfs1 <- rbind(df56d, df78d)



dfs2 <- dfs1[dfs1$Age>=19,]

# Calculate the time (minute and hour) when they eat ----------------------

dfs2$MealTime_chr <- as.character(dfs2$MealTime)
dfs2$MealTime_hm <- unlist(strsplit(dfs2$MealTime_chr," "))[c(FALSE, TRUE)]


dfs2$MealHourN <- as.numeric(unlist(strsplit(dfs2$MealTime_hm,":"))[c(TRUE, FALSE, FALSE)])
dfs2$MealMinN <- as.numeric(unlist(strsplit(dfs2$MealTime_hm,":"))[c(FALSE, TRUE, FALSE)])
dfs2$MealMinN0 <- (60*dfs2$MealHourN)+dfs2$MealMinN


dfs3 <- dfs2[order(dfs2$id,dfs2$DayNo,dfs2$MealMinN0),]


length(unique(dfs3$id)) ## number of participants = 2705


# Create a subset data with only the first observation of each participant --------

NDNS <- dfs3[!duplicated(dfs3$id), ]


with(NDNS, tab1(SurveyYear, graph = FALSE, decimal = 2))

# SurveyYear : 
            # Frequency Percent Cum. percent
# NDNS Year 5       625   23.11        23.11
# NDNS Year 6       663   24.51        47.62
# NDNS Year 7       703   25.99        73.60
# NDNS Year 8       714   26.40       100.00
  # Total          2705  100.00       100.00


# how many men and women --------------------------------------------------

with(NDNS, tab1(Sex, graph = FALSE, decimal = 2))

# Sex : 
        # Frequency Percent Cum. percent
# 1            1094   40.44        40.44
# 2            1611   59.56       100.00
  # Total      2705  100.00       100.00

# create a variable combine id and day No ---------------------------------


dfs3 <- dfs3 %>% 
  mutate(id_dy = paste(id, DayNo, sep = "D"))



# For each subject, the total energy/carbohydrate intake for each eating time can be calculated --------

old<-Sys.time()
Energy <- ddply(dfs3, .(id_dy, id, SurveyYear, DayNo, Age, Sex, DiaryDaysCompleted, MealHourN, DayofWeek),  
                summarise, Tot_Energ = sum(EnergykJ), 
                Tot_Carb = sum(Carbohydrateg), 
                Tot_Sugar = sum(Totalsugarsg), 
                Tot_Starch = sum(Starchg), 
                Tot_Fibre = sum(Englystfibreg),
                Tot_Fat   = sum(Fatg), 
                Tot_Prot  = sum(Proteing), 
                Tot_Alc   = sum(Alcoholg), 
                Tot_NMES  = sum(Nonmilkextrinsicsugarsg))
new<-Sys.time()-old
print(new)

# Time difference of 46.45997 secs  "2019-05-06 12:27:15 JST"


head(Energy)


# reset the time intervals into time slots --------------------------------

### Breakfast:         6am to 9am
### morning snack:   9am to 12noon
### lunch:           12noon to 2pm
### afternoon snack: 2pm to 5pm
### dinner:          5pm to 8pm
### night snack:     8pm to 10pm
### midnight:        10pm to 6am


Energy <- Energy %>% 
  mutate(TimeSlot = cut(MealHourN, breaks = c(6, 9, 12, 14, 17, 20, 22), right = FALSE))


levels(Energy$TimeSlot) <- c(levels(Energy$TimeSlot), "[22, 6)")

Energy$TimeSlot[is.na(Energy$TimeSlot)] <- "[22, 6)"

tab1(Energy$TimeSlot)



# For each subject, the total energy/carbohydrate intake for each 
# time slot can be calculated--------


old<-Sys.time()
Energy <- ddply(Energy, .(id_dy, id, SurveyYear, DayNo, Age, Sex, DiaryDaysCompleted, TimeSlot, DayofWeek),  
                summarise, Tot_Energ = sum(Tot_Energ), Tot_Carb = sum(Tot_Carb), Tot_Sugar = sum(Tot_Sugar),
                Tot_Starch = sum(Tot_Starch), Tot_Fibre = sum(Tot_Fibre), Tot_Fat   = sum(Tot_Fat), 
                Tot_Prot  = sum(Tot_Prot), 
                Tot_Alc   = sum(Tot_Alc), 
                Tot_NMES  = sum(Tot_NMES))
new<-Sys.time()-old
print(new)

# Time difference of 29.43014 secs "2019-05-06 12:28:18 JST"


# Calculate the energy from total carbohydrates ---------------------------

Energy <- Energy %>% 
  mutate(KJcarbo = Tot_Carb*16) %>% 
  mutate(CarKJpercentage = KJcarbo/Tot_Energ) %>% 
  mutate(Carbo = cut(CarKJpercentage, breaks = c(0, 0.50, 2), right = FALSE)) #%>% 



Energy0 <- Energy[!(Energy$Tot_Energ == 0), ] # some food consumption does not contain any energy
Energy0$Carbo <- factor(Energy0$Carbo, labels = c("< 50%", ">= 50%"))
with(Energy0, tab1(Carbo))
#Carbo : 
        # Frequency Percent Cum. percent
# < 50%       25464    49.1         49.1
# >= 50%      26371    50.9        100.0
  # Total     51835   100.0        100.0

with(Energy0, tab1(DayofWeek))


# Filter the data by observation day----------------------------------------------------------

dta_day1 <- Energy0 %>% 
  filter(DayNo == 1) %>% 
  select(c("id", "DayNo", "Age", "Sex", "DayofWeek",  "TimeSlot", "Carbo")) %>%  #"MealHourN", "Tot_Carb", "Tot_Energ"
  #"Carbo", "Cbfb_ratio")) %>% 
  mutate(DayofWeek = factor(DayofWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))

dta_day2 <- Energy0 %>% 
  filter(DayNo == 2) %>% 
  select(c("id", "DayNo", "Age", "Sex", "DayofWeek",  "TimeSlot", "Carbo")) %>%  #"MealHourN", "Tot_Carb", "Tot_Energ"
  #"Carbo", "Cbfb_ratio")) %>% 
  mutate(DayofWeek = factor(DayofWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))

dta_day3 <- Energy0 %>% 
  filter(DayNo == 3) %>% 
  select(c("id", "DayNo", "Age", "Sex", "DayofWeek",  "TimeSlot", "Carbo")) %>%  #"MealHourN", "Tot_Carb", "Tot_Energ"
  #"Carbo", "Cbfb_ratio")) %>% 
  mutate(DayofWeek = factor(DayofWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))

dta_day4 <- Energy0 %>% 
  filter(DayNo == 4) %>% 
  select(c("id", "DayNo", "Age", "Sex", "DayofWeek",  "TimeSlot", "Carbo")) %>%  #"MealHourN",  "Tot_Carb", "Tot_Energ"
  #"Carbo", "Cbfb_ratio")) %>% 
  mutate(DayofWeek = factor(DayofWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))



vecid<-unique(Energy$id) # n = 2705
vecid1<-unique(dta_day1$id) # n = 2703
vecid2<-unique(dta_day2$id) # n = 2705
vecid3<-unique(dta_day3$id) # n = 2703
vecid4<-unique(dta_day4$id) # n = 2642

Noday1 <- setdiff(vecid, vecid1) # n = 2, id = 50506161 70908241 with no data on day 1
Noday2 <- setdiff(vecid, vecid2) # n = 0
Noday3 <- setdiff(vecid, vecid3) # n = 2, id =  80702191 81210131 with no data on day 3
Noday4 <- setdiff(vecid, vecid4) # n = 63, id =  50104191 50105161 50306241 50310271 50501271 50504271 50710161 51002141 51002191 51004011 51102241 51203191 51205141 51208041 51209071
# [16] 60202081 60202261 60206161 60310131 60313021 60405161 60508071 60606271 60808161 60909271 61013261 61102251 61109081 70113191 70302241
# [31] 70305031 70309241 70311181 70311251 70407251 70613181 70703181 70714181 70802241 70812251 70815241 71101061 71206191 80108061 80301061
# [46] 80301281 80302191 80308251 80312241 80405181 80405281 80410131 80611131 80713281 80805191 81002251 81004251 81005191 81007061 81101221
# [61] 81110061 81110131 81203221


# Long to wide data -------------------------------------------------------

dta_d1_wide <- dta_day1 %>% #[, -7]
  spread(key = TimeSlot, #MealHourN, #
         value = Carbo #Cbfb_ratio
  )

head(dta_d1_wide)
names(dta_d1_wide)[6:12] <- c("H6_9", "H9_12", "H12_14", "H14_17", "H17_20", "H20_22", "H22_6")
names(dta_d1_wide)


dta_d2_wide <- dta_day2 %>% #[, -7] 
  spread(key = TimeSlot, #MealHourN
         value = Carbo) #Cbfb_ratio)

head(dta_d2_wide)
names(dta_d2_wide)[6:12] <- c("H6_9", "H9_12", "H12_14", "H14_17", "H17_20", "H20_22", "H22_6")
names(dta_d2_wide)


dta_d3_wide <- dta_day3 %>% #[, -7]
  spread(key = TimeSlot, #, MealHourN
         value = Carbo) #Cbfb_ratio)

head(dta_d3_wide)
names(dta_d3_wide)[6:12] <- c("H6_9", "H9_12", "H12_14", "H14_17", "H17_20", "H20_22", "H22_6")
names(dta_d3_wide)


dta_d4_wide <- dta_day4 %>% #[, -7]
  spread(key = TimeSlot, #MealHourN, #, 
         value = Carbo) #Cbfb_ratio)

head(dta_d4_wide)
names(dta_d4_wide)[6:12] <- c("H6_9", "H9_12", "H12_14", "H14_17", "H17_20", "H20_22", "H22_6")
names(dta_d4_wide)


# recode NA to not eating -------------------------------------------------

for (i in 6:ncol(dta_d1_wide)) 
  if(is.factor(dta_d1_wide[,i])) 
    levels(dta_d1_wide[,i]) <- c("1", "2", "0")

dta_d1_wide[is.na(dta_d1_wide)] <- "0"


for (i in 6:ncol(dta_d2_wide)) 
  if(is.factor(dta_d2_wide[,i])) 
    levels(dta_d2_wide[,i]) <- c("1", "2",  "0")


dta_d2_wide[is.na(dta_d2_wide)] <- "0"


for (i in 6:ncol(dta_d3_wide)) 
  if(is.factor(dta_d3_wide[,i])) 
    levels(dta_d3_wide[,i]) <-  c("1", "2", "0")

dta_d3_wide[is.na(dta_d3_wide)] <- "0"


for (i in 6:ncol(dta_d4_wide)) 
  if(is.factor(dta_d4_wide[,i])) 
    levels(dta_d4_wide[,i]) <-  c("1", "2", "0")

dta_d4_wide[is.na(dta_d4_wide)] <- "0"


dta_all <- rbind(dta_d1_wide, dta_d2_wide, dta_d3_wide, dta_d4_wide)

dta_all <- dta_all[order(dta_all$id,dta_all$DayNo),]



# check the difference between newdataset and previous one ----------------

dta_all <- dta_all %>% 
  mutate(id_day = paste(id, DayNo, sep = "D"))

dta_NDNS_Tslots <- read_csv("~/pCloudDrive/LSHTM/LSHTM_remote_desktopfiles/Project/SAS/dta_NDNS_Tslots.csv", 
                            col_names = c("id", "id_day", "Age", "Sex", "DayofWeek", 
                                          "H6_9", "H9_12", "H12_14", "H14_17", "H17_20", "H20_22", "H22_6"))

CHECK <- dta_all %>% 
  left_join(dta_NDNS_Tslots, by = "id_day")

CHECK %>% filter(DayNo == 1) %>% 
  group_by(as.character(H6_9.x)) %>%
  summarise(n = n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 2), "%"))

CHECK %>% filter(DayNo == 1) %>% 
  group_by(H6_9.y) %>%
  summarise(n = n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 2), "%"))


CHECK %>% filter(DayNo == 1) %>% 
  group_by(as.character(H9_12.x)) %>%
  summarise(n = n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 2), "%"))

CHECK %>% filter(DayNo == 1) %>% 
  group_by(H9_12.y) %>%
  summarise(n = n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 2), "%"))

CHECK %>% filter(DayNo == 1) %>% 
  group_by(as.character(H12_14.x)) %>%
  summarise(n = n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 2), "%"))

CHECK %>% filter(DayNo == 1) %>% 
  group_by(H12_14.y) %>%
  summarise(n = n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 2), "%"))

setwd("~/ドキュメント/githubprojects/LSHTMproject/Rcode")

# export the data from R to Mplus -----------------------------------------

library(MplusAutomation)



CHECK <- CHECK %>% 
  mutate(id_day_n = paste(id.x, DayNo, sep = "0")) %>% 
  mutate(id_day_n = as.numeric(id_day_n)) %>% 
  mutate(Sex.x = as.numeric(Sex.x))

prepareMplusData(CHECK, "Mplusdata/NDNS_Tslots_Y58.dat", 
                 keepCols = c("id.x", "DayNo", "Age.x", "Sex.x", "H6_9.y",
                              "H9_12.y", "H12_14.y", "H14_17.y", "H17_20.y",   
                              "H20_22.y", "H22_6.y", "id_day_n"))
