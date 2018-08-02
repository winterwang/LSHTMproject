
# NDNS analysis, data management ------------------------------------------


# Change the data path accordingly ----------------------------------------

setwd("/home/wangcc-me/Downloads/UKDA-6533-stata11_se/stata11_se/") # on Ubuntu


library(epiDisplay)
library(plyr)
library(dplyr)
library(tidyverse)


# Read the data into memory -----------------------------------------------

library(haven)


data <- read_dta("ndns_rp_yr1-4a_foodleveldietarydata_uk.dta")

data56 <- read_dta("ndns_rp_yr5-6a_foodleveldietarydata.dta")

data78 <- read_dta("ndns_rp_yr7-8a_foodleveldietarydata.dta")

names(data)

names(data56)

names(data78)

names(data)[names(data)=="seriali"] <- "id"
names(data56)[names(data56)=="seriali"] <- "id"
names(data78)[names(data78)=="seriali"] <- "id"



# Extract the data we needed ----------------------------------------------


df14d <- data[,c(113,1,2,3,5,6,7,8,9,21,24,55,57,58,59,60,61,62,63,64)]

var <- names(df14d)

df56d <- data56 %>% 
  dplyr::select(var)

df78d <- data78 %>% 
  dplyr::select(var)

dfs1 <- rbind(df14d, df56d, df78d)


dfs2 <- dfs1[dfs1$Age>=19,]
rm(data, data56, data78)

dfs2


# Calculate the time (minute and hour) when they eat ----------------------

dfs2$MealTime_chr <- as.character(dfs2$MealTime)
dfs2$MealTime_hm <- unlist(strsplit(dfs2$MealTime_chr," "))[c(FALSE, TRUE)]


dfs2$MealHourN <- as.numeric(unlist(strsplit(dfs2$MealTime_hm,":"))[c(TRUE, FALSE, FALSE)])
dfs2$MealMinN <- as.numeric(unlist(strsplit(dfs2$MealTime_hm,":"))[c(FALSE, TRUE, FALSE)])
dfs2$MealMinN0 <- (60*dfs2$MealHourN)+dfs2$MealMinN


dfs3 <- dfs2[order(dfs2$id,dfs2$DayNo,dfs2$MealMinN0),]


length(unique(dfs3$id)) ## number of participants = 6155



# Create a subset data with only the first observation of each participant --------

NDNS <- dfs3[!duplicated(dfs3$id), ]


with(NDNS, tab1(SurveyYear, graph = FALSE, decimal = 2))


# #SurveyYear : 
            # Frequency Percent Cum. percent
# NDNS Year 1       801   13.01        13.01
# NDNS Year 2       812   13.19        26.21
# NDNS Year 3       782   12.71        38.91
# NDNS Year 4      1055   17.14        56.05
# NDNS Year 5       625   10.15        66.21
# NDNS Year 6       663   10.77        76.98
# NDNS Year 7       703   11.42        88.40
# NDNS Year 8       714   11.60       100.00
  # Total          6155  100.00       100.00

save.image("NDNS.Rdata")



# start analysis from loading the extracted data --------------------------
setwd("~/Documents/LSHTMproject/Rcode")
load("NDNS.Rdata")



# how many men and women --------------------------------------------------

with(NDNS, tab1(Sex, graph = FALSE, decimal = 2))

# Sex : 
      #   Frequency Percent Cum. percent
# 1            2537   41.22        41.22 Men
# 2            3618   58.78       100.00 Women
  # Total      6155  100.00       100.00


# create a variable combine id and day No ---------------------------------


dfs3 <- dfs3 %>% 
  mutate(id_dy = paste(id, DayNo, sep = "D"))



# For each subject, the total energy/carbohydrate intake for each eating time can be calculated --------

old<-Sys.time()
Energy <- ddply(dfs3, .(id_dy, id, SurveyYear, DayNo, Age, Sex, DiaryDaysCompleted, MealHourN, DayofWeek),  
                summarise, Tot_Energ = sum(EnergykJ), Tot_Carb = sum(Carbohydrateg), Tot_Sugar = sum(Totalsugarsg), Tot_Starch = sum(Starchg))
new<-Sys.time()-old
print(new)
    
# Time difference of 3.876385 mins
# Time difference of 3.882332 mins #"2018-07-04 11:52:13 BST"
# Time difference of 3.765663 mins  "2018-07-04 14:47:13 BST"

# rm(df14d, df56d, df78d, dfs2)

head(Energy)


vecid<-unique(Energy$id)


# Calculate the energy from total carbohydrates ---------------------------

Energy <- Energy %>% 
  mutate(KJcarbo = Tot_Carb*16) %>% 
  mutate(CarKJpercentage = KJcarbo/Tot_Energ) %>% 
  # mutate(Carbo = cut(CarKJpercentage, breaks = c(0, 0.5, 2), right = FALSE))
   mutate(Carbo = cut(CarKJpercentage, breaks = c(0, 0.25, 0.50, 0.75, 2), right = FALSE)) #%>% 
  # mutate(Carbo2 = cut(CarKJpercentage, breaks = c(0, 0.26, 2), right = FALSE))

Energy0 <- Energy[!(Energy$Tot_Energ == 0), ] # some food consumption does not contain any carbohydrates

# Energy0$Carbo[is.na(Energy0$Carbo)] <- "[0.7,1.01)"
#

Energy0$Carbo <- factor(Energy0$Carbo, labels = c("1", "2", "3", "4"))

# Energy0$Carbo <- factor(Energy0$Carbo, labels = c("< 50%", ">= 50%"))

# Energy0$Carbo <- factor(Energy0$Carbo, labels = c("Low_carb", "Med_carb", "High_carb"))
# 
# Energy0$Carbo2 <- factor(Energy0$Carbo2, labels = c("Low_carb", "Med_or_high_carb"))


with(Energy0, tab1(Carbo))
# with(Energy0, summary(CarKJpercentage))
# with(Energy0, tab1(Carbo2))

with(Energy0, tab1(DayofWeek))



# Use data from each day ----------------------------------------------------------

dta_day1 <- Energy0 %>% 
  filter(DayNo == 1) %>% 
  dplyr::select(c("id", "Age", "Sex", "DayofWeek", "MealHourN", "Carbo"#, "Carbo2"
                  )) %>% 
  mutate(DayofWeek = factor(DayofWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))

dta_day2 <- Energy0 %>% 
  filter(DayNo == 2) %>% 
  dplyr::select(c("id", "Age", "Sex", "DayofWeek", "MealHourN", "Carbo"#, "Carbo2"
                  )) %>% 
  mutate(DayofWeek = factor(DayofWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))

dta_day3 <- Energy0 %>% 
  filter(DayNo == 3) %>% 
  dplyr::select(c("id", "Age", "Sex", "DayofWeek", "MealHourN", "Carbo"#, "Carbo2"
                  )) %>% 
  mutate(DayofWeek = factor(DayofWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))

dta_day4 <- Energy0 %>% 
  filter(DayNo == 4) %>% 
  dplyr::select(c("id", "Age", "Sex", "DayofWeek", "MealHourN", "Carbo"#, "Carbo2"
                  )) %>% 
  mutate(DayofWeek = factor(DayofWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))

vecid1<-unique(dta_day1$id) # n = 6153
vecid2<-unique(dta_day2$id) # n = 6153
vecid3<-unique(dta_day3$id) # n = 6151
vecid4<-unique(dta_day4$id) # n = 6026

# '%nin%' <- Negate('%in%')


# NDNS$id[NDNS$id %nin% vecid1]
Noday1 <- setdiff(vecid, vecid1) # two subjects did not have day 1 data

Noday2 <- setdiff(vecid, vecid2) # two subjects did not have day 2 data 

Noday3 <- setdiff(vecid, vecid3) # four subjects did not have day 3 data 

Noday4 <- setdiff(vecid, vecid4) # 129 subjects did not have day 4 data


# Long to wide data -------------------------------------------------------

dta_d1_wide <- dta_day1[, -7] %>% 
  spread(key = MealHourN, 
         value = Carbo)

head(dta_d1_wide)
names(dta_d1_wide)[5:28] <- paste(rep("H", 24), 0:23, sep = "")
names(dta_d1_wide)


dta_d2_wide <- dta_day2[, -7] %>% 
  spread(key = MealHourN, 
         value = Carbo)

head(dta_d2_wide)
names(dta_d2_wide)[5:28] <- paste(rep("H", 24), 0:23, sep = "")
names(dta_d2_wide)


dta_d3_wide <- dta_day3[, -7] %>% 
  spread(key = MealHourN, 
         value = Carbo)

head(dta_d3_wide)
names(dta_d3_wide)[5:28] <- paste(rep("H", 24), 0:23, sep = "")
names(dta_d3_wide)


dta_d4_wide <- dta_day4[, -7] %>% 
  spread(key = MealHourN, 
         value = Carbo)

head(dta_d4_wide)
names(dta_d4_wide)[5:28] <- paste(rep("H", 24), 0:23, sep = "")
names(dta_d4_wide)



# recode NA to not eating -------------------------------------------------

# levels <- levels(dta_d1_wide$H23)
# levels[length(levels) + 1] <- "Not eating"
# 
# dta_d1_wide$H0 <- factor(dta_d1_wide$H0, levels = levels)
# dta_d1_wide$H0[is.na(dta_d1_wide$H0)] <- "Not eating"

for (i in 5:ncol(dta_d1_wide)) 
  if(is.factor(dta_d1_wide[,i])) 
   levels(dta_d1_wide[,i]) <- c(levels(dta_d1_wide[,i]), "0")
      # levels(dta_d1_wide[,i]) <- c("1", "2", "0")
    #levels(dta_d1_wide[,i]) <- c(levels(dta_d1_wide[,i]), "Not_eating")

dta_d1_wide[is.na(dta_d1_wide)] <- "0"


for (i in 5:ncol(dta_d2_wide)) 
  if(is.factor(dta_d2_wide[,i])) 
    levels(dta_d2_wide[,i]) <- c(levels(dta_d2_wide[,i]), "0")
    # levels(dta_d2_wide[,i]) <- c("1", "2", "0")
    #levels(dta_d2_wide[,i]) <- c(levels(dta_d2_wide[,i]), "Not_eating")

dta_d2_wide[is.na(dta_d2_wide)] <- "0"


for (i in 5:ncol(dta_d3_wide)) 
  if(is.factor(dta_d3_wide[,i])) 
    levels(dta_d3_wide[,i]) <- c(levels(dta_d3_wide[,i]), "0")
    # levels(dta_d3_wide[,i]) <- c("1", "2", "0")
#    levels(dta_d3_wide[,i]) <- c(levels(dta_d3_wide[,i]), "Not_eating")

dta_d3_wide[is.na(dta_d3_wide)] <- "0"


for (i in 5:ncol(dta_d4_wide)) 
  if(is.factor(dta_d4_wide[,i])) 
    levels(dta_d4_wide[,i]) <- c(levels(dta_d4_wide[,i]), "0")
    # levels(dta_d4_wide[,i]) <- c("1", "2", "0")
#    levels(dta_d4_wide[,i]) <- c(levels(dta_d4_wide[,i]), "Not_eating")

dta_d4_wide[is.na(dta_d4_wide)] <- "0"




# Use data according to the day of the week -------------------------------




dta_Mon <- Energy0 %>% 
  filter(DayofWeek == "Monday") %>% 
  select(c("id", "Age", "Sex", "DayNo", "MealHourN", "Carbo", "Carbo2")) %>% 
  mutate(DayNo = factor(DayNo, levels = c("1", "2", "3", "4")))

dta_Tue <- Energy0 %>% 
  filter(DayofWeek == "Tuesday") %>% 
  select(c("id", "Age", "Sex", "DayNo", "MealHourN", "Carbo", "Carbo2")) %>% 
  mutate(DayNo = factor(DayNo, levels = c("1", "2", "3", "4")))

dta_Wed <- Energy0 %>% 
  filter(DayofWeek == "Wednesday") %>% 
  select(c("id", "Age", "Sex", "DayNo", "MealHourN", "Carbo", "Carbo2")) %>% 
  mutate(DayNo = factor(DayNo, levels = c("1", "2", "3", "4")))

dta_Thurs <- Energy0 %>% 
  filter(DayofWeek == "Thursday") %>% 
  select(c("id", "Age", "Sex", "DayNo", "MealHourN", "Carbo", "Carbo2")) %>% 
  mutate(DayNo = factor(DayNo, levels = c("1", "2", "3", "4")))

dta_Fri <- Energy0 %>% 
  filter(DayofWeek == "Friday") %>% 
  select(c("id", "Age", "Sex", "DayNo", "MealHourN", "Carbo", "Carbo2")) %>% 
  mutate(DayNo = factor(DayNo, levels = c("1", "2", "3", "4")))

dta_Sat <- Energy0 %>% 
  filter(DayofWeek == "Saturday") %>% 
  select(c("id", "Age", "Sex", "DayNo", "MealHourN", "Carbo", "Carbo2")) %>% 
  mutate(DayNo = factor(DayNo, levels = c("1", "2", "3", "4")))

dta_Sun <- Energy0 %>% 
  filter(DayofWeek == "Sunday") %>% 
  select(c("id", "Age", "Sex", "DayNo", "MealHourN", "Carbo", "Carbo2")) %>% 
  mutate(DayNo = factor(DayNo, levels = c("1", "2", "3", "4")))





# Long to wide data -------------------------------------------------------

dta_Mon_wide <- dta_Mon[, -7] %>% 
  spread(key = MealHourN, 
         value = Carbo)

head(dta_Mon_wide)
names(dta_Mon_wide)[5:28] <- paste(rep("H", 24), 0:23, sep = "")
names(dta_Mon_wide)


dta_Tue_wide <- dta_Tue[, -7] %>% 
  spread(key = MealHourN, 
         value = Carbo)

head(dta_Tue_wide)
names(dta_Tue_wide)[5:28] <- paste(rep("H", 24), 0:23, sep = "")
names(dta_Tue_wide)


dta_Wed_wide <- dta_Wed[, -7] %>% 
  spread(key = MealHourN, 
         value = Carbo)

head(dta_Wed_wide)
names(dta_Wed_wide)[5:28] <- paste(rep("H", 24), 0:23, sep = "")
names(dta_Wed_wide)


dta_Thurs_wide <- dta_Thurs[, -7] %>% 
  spread(key = MealHourN, 
         value = Carbo)

head(dta_Thurs_wide)
names(dta_Thurs_wide)[5:28] <- paste(rep("H", 24), 0:23, sep = "")
names(dta_Thurs_wide)


dta_Fri_wide <- dta_Fri[, -7] %>% 
  spread(key = MealHourN, 
         value = Carbo)

head(dta_Fri_wide)
names(dta_Fri_wide)[5:28] <- paste(rep("H", 24), 0:23, sep = "")
names(dta_Fri_wide)



dta_Sat_wide <- dta_Sat[, -7] %>% 
  spread(key = MealHourN, 
         value = Carbo)

head(dta_Sat_wide)
names(dta_Sat_wide)[5:28] <- paste(rep("H", 24), 0:23, sep = "")
names(dta_Sat_wide)


dta_Sun_wide <- dta_Sun[, -7] %>% 
  spread(key = MealHourN, 
         value = Carbo)

head(dta_Sun_wide)
names(dta_Sun_wide)[5:28] <- paste(rep("H", 24), 0:23, sep = "")
names(dta_Sun_wide)



# recode NA to not eating -------------------------------------------------

# levels <- levels(dta_d1_wide$H23)
# levels[length(levels) + 1] <- "Not eating"
# 
# dta_d1_wide$H0 <- factor(dta_d1_wide$H0, levels = levels)
# dta_d1_wide$H0[is.na(dta_d1_wide$H0)] <- "Not eating"

for (i in 5:ncol(dta_Mon_wide)) 
  if(is.factor(dta_Mon_wide[,i])) 
    levels(dta_Mon_wide[,i]) <- c(levels(dta_Mon_wide[,i]), "Not_eating")

dta_Mon_wide[is.na(dta_Mon_wide)] <- "Not_eating"


for (i in 5:ncol(dta_Tue_wide)) 
  if(is.factor(dta_Tue_wide[,i])) 
    levels(dta_Tue_wide[,i]) <- c(levels(dta_Tue_wide[,i]), "Not_eating")

dta_Tue_wide[is.na(dta_Tue_wide)] <- "Not_eating"


for (i in 5:ncol(dta_Wed_wide)) 
  if(is.factor(dta_Wed_wide[,i])) 
    levels(dta_Wed_wide[,i]) <- c(levels(dta_Wed_wide[,i]), "Not_eating")

dta_Wed_wide[is.na(dta_Wed_wide)] <- "Not_eating"


for (i in 5:ncol(dta_Thurs_wide)) 
  if(is.factor(dta_Thurs_wide[,i])) 
    levels(dta_Thurs_wide[,i]) <- c(levels(dta_Thurs_wide[,i]), "Not_eating")

dta_Thurs_wide[is.na(dta_Thurs_wide)] <- "Not_eating"



for (i in 5:ncol(dta_Fri_wide)) 
  if(is.factor(dta_Fri_wide[,i])) 
    levels(dta_Fri_wide[,i]) <- c(levels(dta_Fri_wide[,i]), "Not_eating")

dta_Fri_wide[is.na(dta_Fri_wide)] <- "Not_eating"




for (i in 5:ncol(dta_Sat_wide)) 
  if(is.factor(dta_Sat_wide[,i])) 
    levels(dta_Sat_wide[,i]) <- c(levels(dta_Sat_wide[,i]), "Not_eating")

dta_Sat_wide[is.na(dta_Sat_wide)] <- "Not_eating"



for (i in 5:ncol(dta_Sun_wide)) 
  if(is.factor(dta_Sun_wide[,i])) 
    levels(dta_Sun_wide[,i]) <- c(levels(dta_Sun_wide[,i]), "Not_eating")

dta_Sun_wide[is.na(dta_Sun_wide)] <- "Not_eating"



# LCA in day1 -------------------------------------------------------------

# noquote(paste(rep("H", 24), 0:23, sep = ""))
#H0  H1  H2  H3  H4  H5  H6  H7  H8  H9  H10 H11 H12 H13 H14 H15 H16 H17 H18 H19 H20 H21 H22 H23
library(poLCA)
f <- cbind(H0, H1, H2, H3, H4, H5, H6, H7, H8, H9, H10,H11,H12,H13,H14,H15,H16,H17,H18,H19,H20,H21,H22,H23) ~ 1
# lc3 <- poLCA(f, dta_d1_wide, nclass = 3, graphs = TRUE, maxiter = 10000, nrep = 25)

### save data by day 1-4 

save(dfs3, dta_d1_wide, dta_d2_wide, dta_d3_wide, dta_d4_wide, 
       dta_day1, dta_day2, dta_day3, dta_day4, file = "NDNSday1_4.Rdata")

### save data by day Mon-Sun 

save(dfs3, dta_Wed_wide, dta_Wed, dta_Tue_wide, dta_Tue, dta_Thurs_wide, 
     dta_Thurs, dta_Sun_wide, dta_Sun, dta_Sat_wide, dta_Sat, 
     dta_Fri, dta_Fri_wide, dta_Mon, dta_Mon_wide, file = "NDNSMon_Sun.Rdata")

#------ run a sequence of models with 1-8 classes and print out the model with the lowest BIC
max_II <- 100000
min_bic <- 210000
old <- Sys.time()
for(i in 1:8){
  lc <- poLCA(f, dta_d1_wide, nclass=i, maxiter=max_II, 
              tol=1e-5, na.rm=FALSE,  
              nrep=10, verbose=TRUE, calc.se=TRUE)
  if(lc$bic < min_bic){
    min_bic <- lc$bic
    LCA_best_model<-lc
  }
}    	
LCA_best_model
new <- Sys.time()-old
print(new)

#------ run a sequence of models with 1-8 classes and print out the model with the lowest BIC
max_II <- 100000
min_bic <- 210000
old <- Sys.time()
for(i in 1:8){
  lc <- poLCA(f, dta_d2_wide, nclass=i, maxiter=max_II, 
              tol=1e-5, na.rm=FALSE,  
              nrep=10, verbose=TRUE, calc.se=TRUE)
  if(lc$bic < min_bic){
    min_bic <- lc$bic
    LCA_best_model2<-lc
  }
}    	
LCA_best_model2
new <- Sys.time()-old
print(new)

# Estimated class population shares 
# 0.2727 0.2467 0.0977 0.3829 
# 
# Predicted class memberships (by modal posterior prob.) 
# 0.3151 0.2539 0.0712 0.3598 



#------ run a sequence of models with 1-8 classes and print out the model with the lowest BIC
max_II <- 100000
min_bic <- 210000
old <- Sys.time()
for(i in 1:8){
  lc <- poLCA(f, dta_d3_wide, nclass=i, maxiter=max_II, 
              tol=1e-5, na.rm=FALSE,  
              nrep=10, verbose=TRUE, calc.se=TRUE)
  if(lc$bic < min_bic){
    min_bic <- lc$bic
    LCA_best_model3<-lc
  }
}    	
LCA_best_model3
new <- Sys.time()-old
print(new)



#------ run a sequence of models with 1-8 classes and print out the model with the lowest BIC
max_II <- 100000
min_bic <- 210000
old <- Sys.time()
for(i in 1:8){
  lc <- poLCA(f, dta_d4_wide, nclass=i, maxiter=max_II, 
              tol=1e-5, na.rm=FALSE,  
              nrep=10, verbose=TRUE, calc.se=TRUE)
  if(lc$bic < min_bic){
    min_bic <- lc$bic
    LCA_best_model4<-lc
  }
}    	
LCA_best_model4
new <- Sys.time()-old
print(new)


# Generate tables for model comparison ------------------------------------


## models with different number of groups without covariates:
library(poLCA)
f <- cbind(H0, H1, H2, H3, H4, H5, H6, H7, H8, H9, H10,H11,H12,H13,H14,H15,H16,H17,H18,H19,H20,H21,H22,H23) ~ 1

set.seed(20180705)
max_II <- 100000
old <- Sys.time()
lc1 <- poLCA(f, data=dta_d1_wide, nclass=1, na.rm = FALSE, nrep=20, maxiter=max_II) #Loglinear independence model.
lc2 <- poLCA(f, data=dta_d1_wide, nclass=2, na.rm = FALSE, nrep=20, maxiter=max_II)
lc3 <- poLCA(f, data=dta_d1_wide, nclass=3, na.rm = FALSE, nrep=20, maxiter=max_II)
lc4 <- poLCA(f, data=dta_d1_wide, nclass=4, na.rm = FALSE, nrep=20, maxiter=max_II) 
lc5 <- poLCA(f, data=dta_d1_wide, nclass=5, na.rm = FALSE, nrep=20, maxiter=max_II)
lc6 <- poLCA(f, data=dta_d1_wide, nclass=6, na.rm = FALSE, nrep=20, maxiter=max_II)
lc7 <- poLCA(f, data=dta_d1_wide, nclass=7, na.rm = FALSE, nrep=20, maxiter=max_II)
lc8 <- poLCA(f, data=dta_d1_wide, nclass=8, na.rm = FALSE, nrep=20, maxiter=max_II)
new <- Sys.time()-old
print(new)  #Time difference of 32.71098 mins

# generate dataframe with fit-values

results <- data.frame(N_class=c("1"),
                      log_likelihood=lc1$llik,
                      df = lc1$resid.df,
                      AIC = lc1$aic,
                      BIC=lc1$bic,
                      ABIC=  (-2*lc1$llik) + ((log((lc1$N + 2)/24)) * lc1$npar),
                      CAIC = (-2*lc1$llik) + lc1$npar * (1 + log(lc1$N)), 
                      likelihood_ratio=lc1$Gsq)
results$N_class<-as.integer(results$N_class)
results[1,1]<-c("1")
results[2,1]<-c("2")
results[3,1]<-c("3")
results[4,1]<-c("4")
results[5,1]<-c("5")
results[6,1]<-c("6")
results[7,1]<-c("7")
results[8,1]<-c("8")

results[2,2]<-lc2$llik
results[3,2]<-lc3$llik
results[4,2]<-lc4$llik
results[5,2]<-lc5$llik
results[6,2]<-lc6$llik
results[7,2]<-lc7$llik
results[8,2]<-lc8$llik

results[2,3]<-lc2$resid.df
results[3,3]<-lc3$resid.df
results[4,3]<-lc4$resid.df
results[5,3]<-lc5$resid.df
results[6,3]<-lc6$resid.df
results[7,3]<-lc7$resid.df
results[8,3]<-lc8$resid.df

results[2,4]<-lc2$aic
results[3,4]<-lc3$aic
results[4,4]<-lc4$aic
results[5,4]<-lc5$aic
results[6,4]<-lc6$aic
results[7,4]<-lc7$aic
results[8,4]<-lc8$aic

results[2,5]<-lc2$bic
results[3,5]<-lc3$bic
results[4,5]<-lc4$bic
results[5,5]<-lc5$bic
results[6,5]<-lc6$bic
results[7,5]<-lc7$bic
results[8,5]<-lc8$bic

results[2,6]<-(-2*lc2$llik) + ((log((lc2$N + 2)/24)) * lc2$npar) #abic
results[3,6]<-(-2*lc3$llik) + ((log((lc3$N + 2)/24)) * lc3$npar)
results[4,6]<-(-2*lc4$llik) + ((log((lc4$N + 2)/24)) * lc4$npar)
results[5,6]<-(-2*lc5$llik) + ((log((lc5$N + 2)/24)) * lc5$npar)
results[6,6]<-(-2*lc6$llik) + ((log((lc6$N + 2)/24)) * lc6$npar)
results[7,6]<-(-2*lc7$llik) + ((log((lc7$N + 2)/24)) * lc7$npar)
results[8,6]<-(-2*lc8$llik) + ((log((lc8$N + 2)/24)) * lc8$npar)

results[2,7]<- (-2*lc2$llik) + lc2$npar * (1 + log(lc2$N)) #caic
results[3,7]<- (-2*lc3$llik) + lc3$npar * (1 + log(lc3$N))
results[4,7]<- (-2*lc4$llik) + lc4$npar * (1 + log(lc4$N))
results[5,7]<- (-2*lc5$llik) + lc5$npar * (1 + log(lc5$N))
results[6,7]<- (-2*lc6$llik) + lc6$npar * (1 + log(lc6$N))
results[7,7]<- (-2*lc7$llik) + lc7$npar * (1 + log(lc7$N))
results[8,7]<- (-2*lc8$llik) + lc8$npar * (1 + log(lc8$N))

results[2,8]<-lc2$Gsq
results[3,8]<-lc3$Gsq
results[4,8]<-lc4$Gsq
results[5,8]<-lc5$Gsq
results[6,8]<-lc6$Gsq
results[7,8]<-lc7$Gsq
results[8,8]<-lc8$Gsq




entropy<-function (p) sum(-p*log(p))

results$R2_entropy
results[1,9]<-c("-")

error_prior<-entropy(lc2$P) # class proportions model 2
error_post<-mean(apply(lc2$posterior,1, entropy),na.rm = TRUE)
results[2,9]<-round(((error_prior-error_post) / error_prior),3)

error_prior<-entropy(lc3$P) # class proportions model 3
error_post<-mean(apply(lc3$posterior,1, entropy),na.rm = TRUE)
results[3,9]<-round(((error_prior-error_post) / error_prior),3)

error_prior<-entropy(lc4$P) # class proportions model 4
error_post<-mean(apply(lc4$posterior,1, entropy),na.rm = TRUE)
results[4,9]<-round(((error_prior-error_post) / error_prior),3)

error_prior<-entropy(lc5$P) # class proportions model 5
error_post<-mean(apply(lc5$posterior,1, entropy),na.rm = TRUE)
results[5,9]<-round(((error_prior-error_post) / error_prior),3)

error_prior<-entropy(lc6$P) # class proportions model 6
error_post<-mean(apply(lc6$posterior,1, entropy),na.rm = TRUE)
results[6,9]<-round(((error_prior-error_post) / error_prior),3)


error_prior<-entropy(lc7$P) # class proportions model 7
error_post<-mean(apply(lc7$posterior,1, entropy),na.rm = TRUE)
results[7,9]<-round(((error_prior-error_post) / error_prior),3)


error_prior<-entropy(lc8$P) # class proportions model 8
error_post<-mean(apply(lc8$posterior,1, entropy),na.rm = TRUE)
results[8,9]<-round(((error_prior-error_post) / error_prior),3)

# combining results to a dataframe
colnames(results)<-c("N of Class","log-likelihood","resid. df", "AIC","BIC","aBIC","cAIC","likelihood-ratio","Entropy")
lca_results<-results

# Generate a HTML-TABLE and show it in the RSTUDIO-Viewer (for copy & paste) 

# view_kable <- function(x, ...){
#   tab <- paste(capture.output(knitr::kable(x, ...)), collapse = '\n')
#   tf <- tempfile(fileext = ".html")
#   writeLines(tab, tf)
#   rstudio::viewer(tf)
# }
# view_kable(lca_results, format = 'html', table.attr = "class=nofluid")
# 
# # Another possibility which is prettier and easier to do:
# install.packages("ztable")
# ztable::ztable(lca_results)

save(lc1, lc2, lc3, lc4, lc5, lc6, lc7, lc8, lca_results, file = "day1LCA1_6.Rdata")

# Figs --------------------------------------------------------------------


a <- (data.frame(lc3$probs))
a$Class <- c("Class1", "Class2", "Class3")

a_long <- a %>% 
  gather(Hour, Prob, -Class) %>% 
  separate(Hour, into = c("HourN", "Carbo"), sep = "\\.") 

a_long$HourN <- factor(a_long$HourN, levels = c("H0","H1" ,"H2", "H3", "H4", "H5", "H6", "H7", "H8", "H9", "H10","H11","H12","H13",
                                                "H14","H15","H16","H17","H18","H19","H20","H21","H22","H23"))

a_long <- a_long %>% 
  filter(Carbo != "Not_eating")


ggplot(a_long[a_long$Class == "Class1", ], aes(y = Prob, x=HourN, group = Carbo, color = Carbo)) + 
  geom_point() + 
  geom_line()

ggplot(a_long[a_long$Class == "Class2", ], aes(y = Prob, x=HourN, group = Carbo, color = Carbo)) + 
  geom_point() + 
  geom_line()

ggplot(a_long[a_long$Class == "Class3", ], aes(y = Prob, x=HourN, group = Carbo, color = Carbo)) + 
  geom_point() + 
  geom_line()

lc4 <- poLCA(f, dta_d1_wide, nclass = 4, graphs = TRUE, maxiter = 10000)


lc4_prob <- (data.frame(lc4$probs))
lc4_prob$Class <- c("Class1", "Class2", "Class3", "Class4")

lc4_long <- lc4_prob %>% 
  gather(Hour, Prob, -Class) %>% 
  separate(Hour, into = c("HourN", "Carbo"), sep = "\\.") 

lc4_long$HourN <- factor(lc4_long$HourN, levels = c("H0","H1" ,"H2", "H3", "H4", "H5", "H6", "H7", "H8", "H9", "H10","H11","H12","H13",
                                                "H14","H15","H16","H17","H18","H19","H20","H21","H22","H23"))

lc4_long <- lc4_long %>% 
  filter(Carbo != "Not_eating")


ggplot(lc4_long[lc4_long$Class == "Class1", ], aes(y = Prob, x=HourN, group = Carbo, color = Carbo)) + 
  geom_point() + 
  geom_line()

ggplot(lc4_long[lc4_long$Class == "Class2", ], aes(y = Prob, x=HourN, group = Carbo, color = Carbo)) + 
  geom_point() + 
  geom_line()

ggplot(lc4_long[lc4_long$Class == "Class3", ], aes(y = Prob, x=HourN, group = Carbo, color = Carbo)) + 
  geom_point() + 
  geom_line()

ggplot(lc4_long[lc4_long$Class == "Class4", ], aes(y = Prob, x=HourN, group = Carbo, color = Carbo)) + 
  geom_point() + 
  geom_line()




# LCA in day2 -------------------------------------------------------------


## models with different number of groups without covariates:
# library(poLCA)
f <- cbind(H0, H1, H2, H3, H4, H5, H6, H7, H8, H9, H10,H11,H12,H13,H14,H15,H16,H17,H18,H19,H20,H21,H22,H23) ~ 1

set.seed(20180705)
max_II <- 100000
old <- Sys.time()
lc1 <- poLCA(f, data=dta_d2_wide, nclass=1, na.rm = FALSE, nrep=20, maxiter=max_II) #Loglinear independence model.
lc2 <- poLCA(f, data=dta_d2_wide, nclass=2, na.rm = FALSE, nrep=20, maxiter=max_II)
lc3 <- poLCA(f, data=dta_d2_wide, nclass=3, na.rm = FALSE, nrep=20, maxiter=max_II)
lc4 <- poLCA(f, data=dta_d2_wide, nclass=4, na.rm = FALSE, nrep=20, maxiter=max_II) 
lc5 <- poLCA(f, data=dta_d2_wide, nclass=5, na.rm = FALSE, nrep=20, maxiter=max_II)
lc6 <- poLCA(f, data=dta_d2_wide, nclass=6, na.rm = FALSE, nrep=20, maxiter=max_II)
lc7 <- poLCA(f, data=dta_d2_wide, nclass=7, na.rm = FALSE, nrep=20, maxiter=max_II)
lc8 <- poLCA(f, data=dta_d2_wide, nclass=8, na.rm = FALSE, nrep=20, maxiter=max_II)
new <- Sys.time()-old
print(new)  #Time difference of 35.34783 mins

# generate dataframe with fit-values

results <- data.frame(N_class=c("1"),
                      log_likelihood=lc1$llik,
                      df = lc1$resid.df,
                      AIC = lc1$aic,
                      BIC=lc1$bic,
                      ABIC=  (-2*lc1$llik) + ((log((lc1$N + 2)/24)) * lc1$npar),
                      CAIC = (-2*lc1$llik) + lc1$npar * (1 + log(lc1$N)), 
                      likelihood_ratio=lc1$Gsq)
results$N_class<-as.integer(results$N_class)
results[1,1]<-c("1")
results[2,1]<-c("2")
results[3,1]<-c("3")
results[4,1]<-c("4")
results[5,1]<-c("5")
results[6,1]<-c("6")
results[7,1]<-c("7")
results[8,1]<-c("8")

results[2,2]<-lc2$llik
results[3,2]<-lc3$llik
results[4,2]<-lc4$llik
results[5,2]<-lc5$llik
results[6,2]<-lc6$llik
results[7,2]<-lc7$llik
results[8,2]<-lc8$llik

results[2,3]<-lc2$resid.df
results[3,3]<-lc3$resid.df
results[4,3]<-lc4$resid.df
results[5,3]<-lc5$resid.df
results[6,3]<-lc6$resid.df
results[7,3]<-lc7$resid.df
results[8,3]<-lc8$resid.df

results[2,4]<-lc2$aic
results[3,4]<-lc3$aic
results[4,4]<-lc4$aic
results[5,4]<-lc5$aic
results[6,4]<-lc6$aic
results[7,4]<-lc7$aic
results[8,4]<-lc8$aic

results[2,5]<-lc2$bic
results[3,5]<-lc3$bic
results[4,5]<-lc4$bic
results[5,5]<-lc5$bic
results[6,5]<-lc6$bic
results[7,5]<-lc7$bic
results[8,5]<-lc8$bic

results[2,6]<-(-2*lc2$llik) + ((log((lc2$N + 2)/24)) * lc2$npar) #abic
results[3,6]<-(-2*lc3$llik) + ((log((lc3$N + 2)/24)) * lc3$npar)
results[4,6]<-(-2*lc4$llik) + ((log((lc4$N + 2)/24)) * lc4$npar)
results[5,6]<-(-2*lc5$llik) + ((log((lc5$N + 2)/24)) * lc5$npar)
results[6,6]<-(-2*lc6$llik) + ((log((lc6$N + 2)/24)) * lc6$npar)
results[7,6]<-(-2*lc7$llik) + ((log((lc7$N + 2)/24)) * lc7$npar)
results[8,6]<-(-2*lc8$llik) + ((log((lc8$N + 2)/24)) * lc8$npar)

results[2,7]<- (-2*lc2$llik) + lc2$npar * (1 + log(lc2$N)) #caic
results[3,7]<- (-2*lc3$llik) + lc3$npar * (1 + log(lc3$N))
results[4,7]<- (-2*lc4$llik) + lc4$npar * (1 + log(lc4$N))
results[5,7]<- (-2*lc5$llik) + lc5$npar * (1 + log(lc5$N))
results[6,7]<- (-2*lc6$llik) + lc6$npar * (1 + log(lc6$N))
results[7,7]<- (-2*lc7$llik) + lc7$npar * (1 + log(lc7$N))
results[8,7]<- (-2*lc8$llik) + lc8$npar * (1 + log(lc8$N))

results[2,8]<-lc2$Gsq
results[3,8]<-lc3$Gsq
results[4,8]<-lc4$Gsq
results[5,8]<-lc5$Gsq
results[6,8]<-lc6$Gsq
results[7,8]<-lc7$Gsq
results[8,8]<-lc8$Gsq




entropy<-function (p) sum(-p*log(p))

results$R2_entropy
results[1,9]<-c("-")

error_prior<-entropy(lc2$P) # class proportions model 2
error_post<-mean(apply(lc2$posterior,1, entropy),na.rm = TRUE)
results[2,9]<-round(((error_prior-error_post) / error_prior),3)

error_prior<-entropy(lc3$P) # class proportions model 3
error_post<-mean(apply(lc3$posterior,1, entropy),na.rm = TRUE)
results[3,9]<-round(((error_prior-error_post) / error_prior),3)

error_prior<-entropy(lc4$P) # class proportions model 4
error_post<-mean(apply(lc4$posterior,1, entropy),na.rm = TRUE)
results[4,9]<-round(((error_prior-error_post) / error_prior),3)

error_prior<-entropy(lc5$P) # class proportions model 5
error_post<-mean(apply(lc5$posterior,1, entropy),na.rm = TRUE)
results[5,9]<-round(((error_prior-error_post) / error_prior),3)

error_prior<-entropy(lc6$P) # class proportions model 6
error_post<-mean(apply(lc6$posterior,1, entropy),na.rm = TRUE)
results[6,9]<-round(((error_prior-error_post) / error_prior),3)


error_prior<-entropy(lc7$P) # class proportions model 7
error_post<-mean(apply(lc7$posterior,1, entropy),na.rm = TRUE)
results[7,9]<-round(((error_prior-error_post) / error_prior),3)


error_prior<-entropy(lc8$P) # class proportions model 8
error_post<-mean(apply(lc8$posterior,1, entropy),na.rm = TRUE)
results[8,9]<-round(((error_prior-error_post) / error_prior),3)

# combining results to a dataframe
colnames(results)<-c("N of Class","log-likelihood","resid. df", "AIC","BIC","aBIC","cAIC","likelihood-ratio","Entropy")
lca_results<-results

# Generate a HTML-TABLE and show it in the RSTUDIO-Viewer (for copy & paste) 

# view_kable <- function(x, ...){
#   tab <- paste(capture.output(knitr::kable(x, ...)), collapse = '\n')
#   tf <- tempfile(fileext = ".html")
#   writeLines(tab, tf)
#   rstudio::viewer(tf)
# }
# view_kable(lca_results, format = 'html', table.attr = "class=nofluid")
# 
# # Another possibility which is prettier and easier to do:
# install.packages("ztable")
# ztable::ztable(lca_results)

save(lc1, lc2, lc3, lc4, lc5, lc6, lc7, lc8, lca_results, file = "day2LCA1_6.Rdata")



# LCA in day3 -------------------------------------------------------------


f <- cbind(H0, H1, H2, H3, H4, H5, H6, H7, H8, H9, H10,H11,H12,H13,H14,H15,H16,H17,H18,H19,H20,H21,H22,H23) ~ 1

set.seed(20180705)
max_II <- 100000
old <- Sys.time()
lc1 <- poLCA(f, data=dta_d3_wide, nclass=1, na.rm = FALSE, nrep=20, maxiter=max_II) #Loglinear independence model.
lc2 <- poLCA(f, data=dta_d3_wide, nclass=2, na.rm = FALSE, nrep=20, maxiter=max_II)
lc3 <- poLCA(f, data=dta_d3_wide, nclass=3, na.rm = FALSE, nrep=20, maxiter=max_II)
lc4 <- poLCA(f, data=dta_d3_wide, nclass=4, na.rm = FALSE, nrep=20, maxiter=max_II) 
lc5 <- poLCA(f, data=dta_d3_wide, nclass=5, na.rm = FALSE, nrep=20, maxiter=max_II)
lc6 <- poLCA(f, data=dta_d3_wide, nclass=6, na.rm = FALSE, nrep=20, maxiter=max_II)
lc7 <- poLCA(f, data=dta_d3_wide, nclass=7, na.rm = FALSE, nrep=20, maxiter=max_II)
lc8 <- poLCA(f, data=dta_d3_wide, nclass=8, na.rm = FALSE, nrep=20, maxiter=max_II)
new <- Sys.time()-old
print(new)  #Time difference of 31.60514 mins

# generate dataframe with fit-values

results <- data.frame(N_class=c("1"),
                      log_likelihood=lc1$llik,
                      df = lc1$resid.df,
                      AIC = lc1$aic,
                      BIC=lc1$bic,
                      ABIC=  (-2*lc1$llik) + ((log((lc1$N + 2)/24)) * lc1$npar),
                      CAIC = (-2*lc1$llik) + lc1$npar * (1 + log(lc1$N)), 
                      likelihood_ratio=lc1$Gsq)
results$N_class<-as.integer(results$N_class)
results[1,1]<-c("1")
results[2,1]<-c("2")
results[3,1]<-c("3")
results[4,1]<-c("4")
results[5,1]<-c("5")
results[6,1]<-c("6")
results[7,1]<-c("7")
results[8,1]<-c("8")

results[2,2]<-lc2$llik
results[3,2]<-lc3$llik
results[4,2]<-lc4$llik
results[5,2]<-lc5$llik
results[6,2]<-lc6$llik
results[7,2]<-lc7$llik
results[8,2]<-lc8$llik

results[2,3]<-lc2$resid.df
results[3,3]<-lc3$resid.df
results[4,3]<-lc4$resid.df
results[5,3]<-lc5$resid.df
results[6,3]<-lc6$resid.df
results[7,3]<-lc7$resid.df
results[8,3]<-lc8$resid.df

results[2,4]<-lc2$aic
results[3,4]<-lc3$aic
results[4,4]<-lc4$aic
results[5,4]<-lc5$aic
results[6,4]<-lc6$aic
results[7,4]<-lc7$aic
results[8,4]<-lc8$aic

results[2,5]<-lc2$bic
results[3,5]<-lc3$bic
results[4,5]<-lc4$bic
results[5,5]<-lc5$bic
results[6,5]<-lc6$bic
results[7,5]<-lc7$bic
results[8,5]<-lc8$bic

results[2,6]<-(-2*lc2$llik) + ((log((lc2$N + 2)/24)) * lc2$npar) #abic
results[3,6]<-(-2*lc3$llik) + ((log((lc3$N + 2)/24)) * lc3$npar)
results[4,6]<-(-2*lc4$llik) + ((log((lc4$N + 2)/24)) * lc4$npar)
results[5,6]<-(-2*lc5$llik) + ((log((lc5$N + 2)/24)) * lc5$npar)
results[6,6]<-(-2*lc6$llik) + ((log((lc6$N + 2)/24)) * lc6$npar)
results[7,6]<-(-2*lc7$llik) + ((log((lc7$N + 2)/24)) * lc7$npar)
results[8,6]<-(-2*lc8$llik) + ((log((lc8$N + 2)/24)) * lc8$npar)

results[2,7]<- (-2*lc2$llik) + lc2$npar * (1 + log(lc2$N)) #caic
results[3,7]<- (-2*lc3$llik) + lc3$npar * (1 + log(lc3$N))
results[4,7]<- (-2*lc4$llik) + lc4$npar * (1 + log(lc4$N))
results[5,7]<- (-2*lc5$llik) + lc5$npar * (1 + log(lc5$N))
results[6,7]<- (-2*lc6$llik) + lc6$npar * (1 + log(lc6$N))
results[7,7]<- (-2*lc7$llik) + lc7$npar * (1 + log(lc7$N))
results[8,7]<- (-2*lc8$llik) + lc8$npar * (1 + log(lc8$N))

results[2,8]<-lc2$Gsq
results[3,8]<-lc3$Gsq
results[4,8]<-lc4$Gsq
results[5,8]<-lc5$Gsq
results[6,8]<-lc6$Gsq
results[7,8]<-lc7$Gsq
results[8,8]<-lc8$Gsq




entropy<-function (p) sum(-p*log(p))

results$R2_entropy
results[1,9]<-c("-")

error_prior<-entropy(lc2$P) # class proportions model 2
error_post<-mean(apply(lc2$posterior,1, entropy),na.rm = TRUE)
results[2,9]<-round(((error_prior-error_post) / error_prior),3)

error_prior<-entropy(lc3$P) # class proportions model 3
error_post<-mean(apply(lc3$posterior,1, entropy),na.rm = TRUE)
results[3,9]<-round(((error_prior-error_post) / error_prior),3)

error_prior<-entropy(lc4$P) # class proportions model 4
error_post<-mean(apply(lc4$posterior,1, entropy),na.rm = TRUE)
results[4,9]<-round(((error_prior-error_post) / error_prior),3)

error_prior<-entropy(lc5$P) # class proportions model 5
error_post<-mean(apply(lc5$posterior,1, entropy),na.rm = TRUE)
results[5,9]<-round(((error_prior-error_post) / error_prior),3)

error_prior<-entropy(lc6$P) # class proportions model 6
error_post<-mean(apply(lc6$posterior,1, entropy),na.rm = TRUE)
results[6,9]<-round(((error_prior-error_post) / error_prior),3)


error_prior<-entropy(lc7$P) # class proportions model 7
error_post<-mean(apply(lc7$posterior,1, entropy),na.rm = TRUE)
results[7,9]<-round(((error_prior-error_post) / error_prior),3)


error_prior<-entropy(lc8$P) # class proportions model 8
error_post<-mean(apply(lc8$posterior,1, entropy),na.rm = TRUE)
results[8,9]<-round(((error_prior-error_post) / error_prior),3)

# combining results to a dataframe
colnames(results)<-c("N of Class","log-likelihood","resid. df", "AIC","BIC","aBIC","cAIC","likelihood-ratio","Entropy")
lca_results<-results

# Generate a HTML-TABLE and show it in the RSTUDIO-Viewer (for copy & paste) 

# view_kable <- function(x, ...){
#   tab <- paste(capture.output(knitr::kable(x, ...)), collapse = '\n')
#   tf <- tempfile(fileext = ".html")
#   writeLines(tab, tf)
#   rstudio::viewer(tf)
# }
# view_kable(lca_results, format = 'html', table.attr = "class=nofluid")
# 
# # Another possibility which is prettier and easier to do:
# install.packages("ztable")
# ztable::ztable(lca_results)

save(lc1, lc2, lc3, lc4, lc5, lc6, lc7, lc8, lca_results, file = "day3LCA1_6.Rdata")


# LCA in day4 -------------------------------------------------------------

f <- cbind(H0, H1, H2, H3, H4, H5, H6, H7, H8, H9, H10,H11,H12,H13,H14,H15,H16,H17,H18,H19,H20,H21,H22,H23) ~ 1

set.seed(20180705)
max_II <- 100000
old <- Sys.time()
lc1 <- poLCA(f, data=dta_d4_wide, nclass=1, na.rm = FALSE, nrep=20, maxiter=max_II) #Loglinear independence model.
lc2 <- poLCA(f, data=dta_d4_wide, nclass=2, na.rm = FALSE, nrep=20, maxiter=max_II)
lc3 <- poLCA(f, data=dta_d4_wide, nclass=3, na.rm = FALSE, nrep=20, maxiter=max_II)
lc4 <- poLCA(f, data=dta_d4_wide, nclass=4, na.rm = FALSE, nrep=20, maxiter=max_II) 
lc5 <- poLCA(f, data=dta_d4_wide, nclass=5, na.rm = FALSE, nrep=20, maxiter=max_II)
lc6 <- poLCA(f, data=dta_d4_wide, nclass=6, na.rm = FALSE, nrep=20, maxiter=max_II)
lc7 <- poLCA(f, data=dta_d4_wide, nclass=7, na.rm = FALSE, nrep=20, maxiter=max_II)
lc8 <- poLCA(f, data=dta_d4_wide, nclass=8, na.rm = FALSE, nrep=20, maxiter=max_II)
new <- Sys.time()-old
print(new)  #Time difference of 30.44217 mins

# generate dataframe with fit-values

results <- data.frame(N_class=c("1"),
                      log_likelihood=lc1$llik,
                      df = lc1$resid.df,
                      AIC = lc1$aic,
                      BIC=lc1$bic,
                      ABIC=  (-2*lc1$llik) + ((log((lc1$N + 2)/24)) * lc1$npar),
                      CAIC = (-2*lc1$llik) + lc1$npar * (1 + log(lc1$N)), 
                      likelihood_ratio=lc1$Gsq)
results$N_class<-as.integer(results$N_class)
results[1,1]<-c("1")
results[2,1]<-c("2")
results[3,1]<-c("3")
results[4,1]<-c("4")
results[5,1]<-c("5")
results[6,1]<-c("6")
results[7,1]<-c("7")
results[8,1]<-c("8")

results[2,2]<-lc2$llik
results[3,2]<-lc3$llik
results[4,2]<-lc4$llik
results[5,2]<-lc5$llik
results[6,2]<-lc6$llik
results[7,2]<-lc7$llik
results[8,2]<-lc8$llik

results[2,3]<-lc2$resid.df
results[3,3]<-lc3$resid.df
results[4,3]<-lc4$resid.df
results[5,3]<-lc5$resid.df
results[6,3]<-lc6$resid.df
results[7,3]<-lc7$resid.df
results[8,3]<-lc8$resid.df

results[2,4]<-lc2$aic
results[3,4]<-lc3$aic
results[4,4]<-lc4$aic
results[5,4]<-lc5$aic
results[6,4]<-lc6$aic
results[7,4]<-lc7$aic
results[8,4]<-lc8$aic

results[2,5]<-lc2$bic
results[3,5]<-lc3$bic
results[4,5]<-lc4$bic
results[5,5]<-lc5$bic
results[6,5]<-lc6$bic
results[7,5]<-lc7$bic
results[8,5]<-lc8$bic

results[2,6]<-(-2*lc2$llik) + ((log((lc2$N + 2)/24)) * lc2$npar) #abic
results[3,6]<-(-2*lc3$llik) + ((log((lc3$N + 2)/24)) * lc3$npar)
results[4,6]<-(-2*lc4$llik) + ((log((lc4$N + 2)/24)) * lc4$npar)
results[5,6]<-(-2*lc5$llik) + ((log((lc5$N + 2)/24)) * lc5$npar)
results[6,6]<-(-2*lc6$llik) + ((log((lc6$N + 2)/24)) * lc6$npar)
results[7,6]<-(-2*lc7$llik) + ((log((lc7$N + 2)/24)) * lc7$npar)
results[8,6]<-(-2*lc8$llik) + ((log((lc8$N + 2)/24)) * lc8$npar)

results[2,7]<- (-2*lc2$llik) + lc2$npar * (1 + log(lc2$N)) #caic
results[3,7]<- (-2*lc3$llik) + lc3$npar * (1 + log(lc3$N))
results[4,7]<- (-2*lc4$llik) + lc4$npar * (1 + log(lc4$N))
results[5,7]<- (-2*lc5$llik) + lc5$npar * (1 + log(lc5$N))
results[6,7]<- (-2*lc6$llik) + lc6$npar * (1 + log(lc6$N))
results[7,7]<- (-2*lc7$llik) + lc7$npar * (1 + log(lc7$N))
results[8,7]<- (-2*lc8$llik) + lc8$npar * (1 + log(lc8$N))

results[2,8]<-lc2$Gsq
results[3,8]<-lc3$Gsq
results[4,8]<-lc4$Gsq
results[5,8]<-lc5$Gsq
results[6,8]<-lc6$Gsq
results[7,8]<-lc7$Gsq
results[8,8]<-lc8$Gsq




entropy<-function (p) sum(-p*log(p))

results$R2_entropy
results[1,9]<-c("-")

error_prior<-entropy(lc2$P) # class proportions model 2
error_post<-mean(apply(lc2$posterior,1, entropy),na.rm = TRUE)
results[2,9]<-round(((error_prior-error_post) / error_prior),3)

error_prior<-entropy(lc3$P) # class proportions model 3
error_post<-mean(apply(lc3$posterior,1, entropy),na.rm = TRUE)
results[3,9]<-round(((error_prior-error_post) / error_prior),3)

error_prior<-entropy(lc4$P) # class proportions model 4
error_post<-mean(apply(lc4$posterior,1, entropy),na.rm = TRUE)
results[4,9]<-round(((error_prior-error_post) / error_prior),3)

error_prior<-entropy(lc5$P) # class proportions model 5
error_post<-mean(apply(lc5$posterior,1, entropy),na.rm = TRUE)
results[5,9]<-round(((error_prior-error_post) / error_prior),3)

error_prior<-entropy(lc6$P) # class proportions model 6
error_post<-mean(apply(lc6$posterior,1, entropy),na.rm = TRUE)
results[6,9]<-round(((error_prior-error_post) / error_prior),3)


error_prior<-entropy(lc7$P) # class proportions model 7
error_post<-mean(apply(lc7$posterior,1, entropy),na.rm = TRUE)
results[7,9]<-round(((error_prior-error_post) / error_prior),3)


error_prior<-entropy(lc8$P) # class proportions model 8
error_post<-mean(apply(lc8$posterior,1, entropy),na.rm = TRUE)
results[8,9]<-round(((error_prior-error_post) / error_prior),3)

# combining results to a dataframe
colnames(results)<-c("N of Class","log-likelihood","resid. df", "AIC","BIC","aBIC","cAIC","likelihood-ratio","Entropy")
lca_results<-results

# Generate a HTML-TABLE and show it in the RSTUDIO-Viewer (for copy & paste) 

# view_kable <- function(x, ...){
#   tab <- paste(capture.output(knitr::kable(x, ...)), collapse = '\n')
#   tf <- tempfile(fileext = ".html")
#   writeLines(tab, tf)
#   rstudio::viewer(tf)
# }
# view_kable(lca_results, format = 'html', table.attr = "class=nofluid")
# 
# # Another possibility which is prettier and easier to do:
# install.packages("ztable")
# ztable::ztable(lca_results)

save(lc1, lc2, lc3, lc4, lc5, lc6, lc7, lc8, lca_results, file = "day4LCA1_6.Rdata")



# save.image("LCA.Rdata")




# Combine fourdays data ---------------------------------------------------

# 
# CARB_50 <- Energy0 %>% 
#   # filter(DayNo == 4) %>% 
#  dplyr::select(c("id", "Age", "Sex", "DayofWeek", "MealHourN", "Carbo")) %>% 
#   mutate(DayofWeek = factor(DayofWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))
# 
# 
# 
# CARB_50_wide <- CARB_50 %>% 
#   spread(key = MealHourN, 
#          value = Carbo)
# 
# 
# head(CARB_50_wide)
# names(CARB_50_wide)[5:28] <- paste(rep("H", 24), 0:23, sep = "")
# names(CARB_50_wide)
# 
# 
# 
# for (i in 5:ncol(CARB_50_wide)) 
#   if(is.factor(CARB_50_wide[,i])) 
#     levels(CARB_50_wide[,i]) <- c(levels(CARB_50_wide[,i]), "Not_eating")
# 
# 
# 
# CARB_50_wide[is.na(CARB_50_wide)] <- "Not_eating"
# 


CARB_50 <- dta_d1_wide %>% 
  full_join(dta_d2_wide[,-c(2,3)], by = "id") 

CARB_50 <- CARB_50 %>% 
  full_join(dta_d3_wide[,-c(2,3)], by = "id")

CARB_50 <- CARB_50 %>% 
  full_join(dta_d4_wide[,-c(2,3)], by = "id")


head(CARB_50)

write_delim(CARB_50, "CARB_50.dat", na = ".", delim = " ")




CARB_255075 <- dta_d1_wide %>% 
  full_join(dta_d2_wide[,-c(2,3)], by = "id") 

CARB_255075 <- CARB_255075 %>% 
  full_join(dta_d3_wide[,-c(2,3)], by = "id")

CARB_255075 <- CARB_255075 %>% 
  full_join(dta_d4_wide[,-c(2,3)], by = "id")


head(CARB_255075)

write_delim(CARB_255075, "CARB_255075.dat", na = ".", delim = " ")



# posterior analysis classification with BMI etc. -------------------------

setwd("/home/wangcc-me/Downloads/UKDA-6533-stata11_se/stata11_se/") # on Ubuntu


library(epiDisplay)
library(plyr)
library(dplyr)
library(tidyverse)


library(haven)

# 
# data <- read_dta("ndns_rp_yr1-4a_foodleveldietarydata_uk.dta")
# 
# data56 <- read_dta("ndns_rp_yr5-6a_foodleveldietarydata.dta")
# 
# data78 <- read_dta("ndns_rp_yr7-8a_foodleveldietarydata.dta")

library(readr)
CW2CB2 <- read_table2("~/Documents/LSHTMproject/results/50NDNS_CW2CB2.txt", 
                              col_names = FALSE)


names(CW2CB2) <- c("H0",
                   "H1",
                   "H2",
                   "H3",
                   "H4",
                   "H5",
                   "H6",
                   "H7",
                   "H8",
                   "H9",
                   "H10",
                   "H11",
                   "H12",
                   "H13",
                   "H14",
                   "H15",
                   "H16",
                   "H17",
                   "H18",
                   "H19",
                   "H20",
                   "H21",
                   "H22",
                   "H23",
                   "ID_DAY",
                   "AGE",
                   "SEX",
                   "CPROB1",
                   "CPROB2",
                   "CPROB3",
                   "CPROB4",
                   # "CPROB5",
                   # "CPROB6",
                   "CB",
                   "CW",
                   "MLCJOINT",
                   "ID")


CW3CB2 <- read_table2("~/Documents/LSHTMproject/results/50NDNS_CW3CB2.txt", 
                      col_names = FALSE)

names(CW3CB2) <- c("H0",
                   "H1",
                   "H2",
                   "H3",
                   "H4",
                   "H5",
                   "H6",
                   "H7",
                   "H8",
                   "H9",
                   "H10",
                   "H11",
                   "H12",
                   "H13",
                   "H14",
                   "H15",
                   "H16",
                   "H17",
                   "H18",
                   "H19",
                   "H20",
                   "H21",
                   "H22",
                   "H23",
                   "ID_DAY",
                   "AGE",
                   "SEX",
                   "CPROB1",
                   "CPROB2",
                   "CPROB3",
                   "CPROB4",
                   "CPROB5",
                   "CPROB6",
                   "CB",
                   "CW",
                   "MLCJOINT",
                   "ID")


names(CW2CB2)
names(CW3CB2)

CW2CB2_reg <- CW2CB2[!duplicated(CW2CB2$ID), ]

CW2CB2_reg <- CW2CB2_reg %>% 
  select(ID, AGE, SEX, CB) # extract only the CB variable (Between individual classes == 1 or 2)

tab1(CW2CB2_reg$CB)

CW3CB2_reg <- CW3CB2[!duplicated(CW3CB2$ID), ]

CW3CB2_reg <- CW3CB2_reg %>% 
  select(ID, AGE, SEX, CB) # extract only the CB variable (Between individual classes == 1 or 2)


tab1(CW3CB2_reg$CB)

blood78 <- read_dta("ndns_rp_yr7-8a_indiv.dta")
blood56 <- read_dta("ndns_rp_yr5-6a_indiv.dta")
blood14 <- read_dta("ndns_rp_yr1-4a_indiv_uk.dta")

names(blood14)
library(naniar)
names(blood78)[names(blood78)=="seriali"] <- "ID"
names(blood56)[names(blood56)=="seriali"] <- "ID"
names(blood14)[names(blood14)=="seriali"] <- "ID"
BMI78 <- blood78 %>% 
  select(ID, bmival, wstval, Diabetes, bpmedc2, bpmedd2, hyper140_2, hibp140_2,
         Glucose, A1C, cigsta3, wti_Y78, wtn_Y78, wtb_Y78, cluster1, cluster2, cluster3, 
         cluster4, cluster5, area) %>% 
  rename(wti = wti_Y78, wtn = wtn_Y78, wtb = wtb_Y78) %>% 
  mutate(Years = "7-8") %>% 
  replace_with_na(replace = list(bmival = -1, 
                                 wstval = -1, 
                                 bpmedd2 = -1, 
                                 bpmedc2 = -1,
                                 hyper140_2 = -7, 
                                 # hyper140_2 = -1, 
                                 hibp140_2 = -7, 
                                 # hibp140_2 = -1, 
                                 Glucose = -1, 
                                 A1C  = -1, 
                                 cigsta3 = -1)) %>% 
  replace_with_na(replace = list(hyper140_2 = -1, hibp140_2 = -1))

BMI56 <- blood56 %>% 
  select(ID, area, bmival, wstval, Diabetes, bpmedc2, bpmedd2, hyper140_2, hibp140_2,
         Glucose, A1C, cigsta3, wti_Y56, wtn_Y56, wtb_Y56, cluster1, cluster2, cluster3, 
         cluster4, cluster5, area) %>% 
  mutate(Years = "5-6") %>% 
  rename(wti = wti_Y56, wtn = wtn_Y56, wtb = wtb_Y56) %>% 
  replace_with_na(replace = list(bmival = -1, 
                                 wstval = -1,
                                 bpmedd2 = -1, 
                                 bpmedc2 = -1,
                                 hyper140_2 = -7, 
                                 hyper140_2 = -1, 
                                 hibp140_2 = -7, 
                                 hibp140_2 = -1, 
                                 Glucose = -1, 
                                 A1C  = -1, 
                                 cigsta3 = -1)) %>% 
  replace_with_na(replace = list(hyper140_2 = -1, hibp140_2 = -1))

BMI14 <- blood14 %>% 
  select(ID, bmival, wstval, Diabetes, bpmedc, bpmedd, hyper140, hibp140,
         Glucose, A1C, cigsta3, wti_CY1234 , wtn_CY1234, wtb_CY1234, cluster, area) %>%
  rename(hyper140_2 = hyper140, hibp140_2 = hibp140, bpmedd2 = bpmedd, 
         bpmedc2 = bpmedc, cluster1 = cluster, 
         wti = wti_CY1234, wtn = wtn_CY1234, wtb =  wtb_CY1234) %>% 
  mutate(cluster2 = NA, cluster3 = NA, cluster4 = NA, cluster5 = NA, Years = "1-4") %>% 
  replace_with_na(replace = list(bmival = -1,
                                 wstval = -1,
                                 bpmedd2 = -1, 
                                 bpmedc2 = -1,
                                 hyper140_2 = -7, 
                                 hyper140_2 = -1, 
                                 hibp140_2 = -7, 
                                 hibp140_2 = -1, 
                                 Glucose = -1, 
                                 A1C  = -1, 
                                 cigsta3 = -1)) %>% 
  replace_with_na(replace = list(hyper140_2 = -1, hibp140_2 = -1))




BMI <- bind_rows(BMI14, BMI56, BMI78)

CW2CB2_regss <- CW2CB2_reg %>% 
  left_join(BMI, by = "ID")

CW3CB2_regss <- CW3CB2_reg %>% 
  left_join(BMI, by = "ID")



# rescale the weights

CW2CB2_regss %>% 
  # select(wti) %>% 
  filter(Years == "1-4") %>% 
  summarise(mean = mean(wti), sum1_4 = sum(wti) )

CW2CB2_regss %>% 
  # select(wti) %>% 
  filter(Years == "5-6") %>% 
  summarise(mean = mean(wti), sum5_6 = sum(wti))

CW2CB2_regss %>% 
  # select(wti) %>% 
  filter(Years == "7-8") %>% 
  summarise(mean = mean(wti), sum7_8 = sum(wti))


write_delim(CW2CB2_regss, "CW2CB2_regss.dat", na = ".", delim = " ")
write_delim(CW3CB2_regss, "CW3CB2_regss.dat", na = ".", delim = " ")


# 
# with(CW2CB2_reg, summ(bmival, by = CB))
# summary(lm(bmival ~ as.factor(CB) + factor(SEX) + AGE, data = CW2CB2_reg))


write_dta(LCGA_3class, "/home/wangcc-me/Downloads/UKDA-6533-stata11_se/stata11_se/LCGA_3class.dta")
write_dta(LCGA_2class, "/home/wangcc-me/Downloads/UKDA-6533-stata11_se/stata11_se/LCGA_2class.dta")
write_dta(CW3CB2_regss, "/home/wangcc-me/Downloads/UKDA-6533-stata11_se/stata11_se/CW3CB2_regss.dta")
write_dta(CW2CB2_regss, "/home/wangcc-me/Downloads/UKDA-6533-stata11_se/stata11_se/CW2CB2_regss.dta")




# CW3CB2CB3 and weekdays weekends & CW3----------------------------------------------------------

CW3CB2 <- read_table2("/home/wangcc-me/Documents/LSHTMproject/results/Timeslots/NDNSslot_CW3CB2.txt",
                      col_names = FALSE)

names(CW3CB2) <- c("Breakfast", "Morning.snack", "Lunch",
                   "Afternoon.snack", "Dinner", "Before.bedtime.snack",
                   "Midnight.food", "ID_DAY", "AGE", "SEX",
                   "CPROB1", "CPROB2", "CPROB3", "CPROB4",
                   "CPROB5", "CPROB6",
                   # "CPROB7",
                   # "CPROB8",
                   # "CPROB9",
                   "CB", "CW", "MLCJOINT", "ID")


CW3CB3 <- read_table2("/home/wangcc-me/Documents/LSHTMproject/results/Timeslots/NDNSslot_CW3CB3.txt",
                      col_names = FALSE)

names(CW3CB3) <- c("Breakfast",  "Morning.snack", "Lunch",
                   "Afternoon.snack", "Dinner", "Before.bedtime.snack",
                   "Midnight.food", "ID_DAY", "AGE", "SEX",
                   "CPROB1", "CPROB2", "CPROB3", "CPROB4",
                   "CPROB5", "CPROB6", "CPROB7", "CPROB8",
                   "CPROB9", "CB", "CW", "MLCJOINT", "ID")

tab1(CW3CB2$CW)
tab1(CW3CB3$CW)

CW3CB3$CW_new <- 0
CW3CB3$CW_new[CW3CB3$CW == 1] <- 3
CW3CB3$CW_new[CW3CB3$CW == 2] <- 1
CW3CB3$CW_new[CW3CB3$CW == 3] <- 2

CB2_in_CW3CB2 <- CW3CB2 %>% 
  select(ID, ID_DAY, CW, CB) %>% 
  rename(CW3_in_2 = CW, CB2_in_2 = CB)
CW3CB3 <- CW3CB3 %>% 
  left_join(CB2_in_CW3CB2, by = "ID_DAY")


pp <- CW3CB3 %>%  #CW3CB2 change this to check the level 1 classes changes
  group_by(CW) %>% 
  summarise(Breakfast_0 = sum(Breakfast == 0)/length(Breakfast), 
            Breakfast_1 = sum(Breakfast == 1)/length(Breakfast), 
            Breakfast_2 = sum(Breakfast == 2)/length(Breakfast),
            Morning.snack_0 = sum(Morning.snack == 0)/length(Morning.snack),
            Morning.snack_1 = sum(Morning.snack == 1)/length(Morning.snack), 
            Morning.snack_2 = sum(Morning.snack == 2)/length(Morning.snack),
            Lunch_0 = sum(Lunch == 0)/length(Lunch),
            Lunch_1 = sum(Lunch == 1)/length(Lunch), 
            Lunch_2 = sum(Lunch == 2)/length(Lunch),
            Afternoon.snack_0 = sum(Afternoon.snack == 0)/length(Afternoon.snack),
            Afternoon.snack_1 = sum(Afternoon.snack == 1)/length(Afternoon.snack), 
            Afternoon.snack_2 = sum(Afternoon.snack == 2)/length(Afternoon.snack),
            Dinner_0 = sum(Dinner == 0)/length(Dinner),
            Dinner_1 = sum(Dinner == 1)/length(Dinner), 
            Dinner_2 = sum(Dinner == 2)/length(Dinner),
            Before.bedtime.snack_0 = sum(Before.bedtime.snack == 0)/length(Before.bedtime.snack),
            Before.bedtime.snack_1 = sum(Before.bedtime.snack == 1)/length(Before.bedtime.snack), 
            Before.bedtime.snack_2 = sum(Before.bedtime.snack == 2)/length(Before.bedtime.snack),
            Midnight.food_0 = sum(Midnight.food == 0)/length(Midnight.food),
            Midnight.food_1 = sum(Midnight.food == 1)/length(Midnight.food), 
            Midnight.food_2 = sum(Midnight.food == 2)/length(Midnight.food))



pp_long <- pp %>% 
  gather(Hour, Prob, -CW) %>% 
  separate(Hour, into = c("Slots", "Carbo"), sep = "_") 




pp_long$Slots <- factor(pp_long$Slots, levels = c("Breakfast", 
                                                  "Morning.snack",
                                                  "Lunch",
                                                  "Afternoon.snack",
                                                  "Dinner",
                                                  "Before.bedtime.snack",
                                                  "Midnight.food"))

library(ggthemr)
ggthemr("fresh", layout = "scientific")
library(ggplot2)


pp_long$Time <- as.character(pp_long$Slots)
pp_long$Time[pp_long$Slots == "Breakfast"] <- 7.5
pp_long$Time[pp_long$Slots == "Morning.snack"] <- 10.5
pp_long$Time[pp_long$Slots == "Lunch"] <- 13
pp_long$Time[pp_long$Slots == "Afternoon.snack"] <- 15.5
pp_long$Time[pp_long$Slots == "Dinner"] <- 18.5
pp_long$Time[pp_long$Slots == "Before.bedtime.snack"] <- 21
pp_long$Time[pp_long$Slots == "Midnight.food"] <- 26
pp_long$Time <- as.numeric(pp_long$Time)


ggplot(pp_long[pp_long$CW == 1, ], aes(y = Prob, x=Time, group = Carbo, color = Carbo))  + 
  scale_x_continuous(limits = c(6, 29),
                     breaks = seq(6, 29, by = 1)) +
  annotate("rect", xmin = 6, xmax = 9, ymin = 0, ymax = 1.00, 
           alpha = 0.5, fill = "grey") + 
  annotate("rect", xmin = 12, xmax = 14, ymin = 0, ymax = 1.00, 
           alpha = 0.5, fill = "grey") + 
  annotate("rect", xmin = 17, xmax = 20, ymin = 0, ymax = 1.00, 
           alpha = 0.5, fill = "grey") + 
  annotate("rect", xmin = 22, xmax = 29, ymin = 0, ymax = 1.00, 
           alpha = 0.5, fill = "grey") +
  geom_point(size = 3) + 
  geom_line(size = 1.5) + 
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(size = 18), 
        axis.line = element_line(colour = "black"), 
        panel.border = element_blank(), 
        panel.background = element_blank(), 
        legend.text = element_text(size = 15), 
        legend.title = element_text(size = 15) 
        # legend.position = "bottom", 
        # legend.direction = "horizontal"
  ) + 
  # theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  labs(title = "Class 1 days", x = "Hours of the day (next day from hour 24)", y = "Probability",
       color = "Carbohydrate\nintake") + 
  scale_color_discrete(labels = c("Not eating", "< 50%", ">= 50%")) + 
  ylim(c(0,1))



ggplot(pp_long[pp_long$CW == 2, ], aes(y = Prob, x=Time, group = Carbo, color = Carbo)) + 
  scale_x_continuous(limits = c(6, 29),
                     breaks = seq(6, 29, by = 1)) +
  annotate("rect", xmin = 6, xmax = 9, ymin = 0, ymax = 1.00, 
           alpha = 0.5, fill = "grey") + 
  annotate("rect", xmin = 12, xmax = 14, ymin = 0, ymax = 1.00, 
           alpha = 0.5, fill = "grey") + 
  annotate("rect", xmin = 17, xmax = 20, ymin = 0, ymax = 1.00, 
           alpha = 0.5, fill = "grey") + 
  annotate("rect", xmin = 22, xmax = 29, ymin = 0, ymax = 1.00, 
           alpha = 0.5, fill = "grey") +
  geom_point(size = 3) + 
  geom_line(size = 1.5) + 
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(size = 18), 
        axis.line = element_line(colour = "black"), 
        panel.border = element_blank(), 
        panel.background = element_blank(), 
        legend.text = element_text(size = 15), 
        legend.title = element_text(size = 15) 
        # legend.position = "bottom", 
        # legend.direction = "horizontal"
  ) + 
  # theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  labs(title = "Class 2 days", x = "Hours of the day (next day from hour 24)", y = "Probability",
       color = "Carbohydrate\nintake") + 
  scale_color_discrete(labels = c("Not eating", "< 50%", ">= 50%")) + 
  ylim(c(0,1))




ggplot(pp_long[pp_long$CW == 3, ], aes(y = Prob, x=Time, group = Carbo, color = Carbo)) + 
  scale_x_continuous(limits = c(6, 29),
                     breaks = seq(6, 29, by = 1)) +
  annotate("rect", xmin = 6, xmax = 9, ymin = 0, ymax = 1.00, 
           alpha = 0.5, fill = "grey") + 
  annotate("rect", xmin = 12, xmax = 14, ymin = 0, ymax = 1.00, 
           alpha = 0.5, fill = "grey") + 
  annotate("rect", xmin = 17, xmax = 20, ymin = 0, ymax = 1.00, 
           alpha = 0.5, fill = "grey") + 
  annotate("rect", xmin = 22, xmax = 29, ymin = 0, ymax = 1.00, 
           alpha = 0.5, fill = "grey") +
  geom_point(size = 3) + 
  geom_line(size = 1.5) + 
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(size = 18), 
        axis.line = element_line(colour = "black"), 
        panel.border = element_blank(), 
        panel.background = element_blank(), 
        legend.text = element_text(size = 15), 
        legend.title = element_text(size = 15) 
        # legend.position = "bottom", 
        # legend.direction = "horizontal"
  ) + 
  # theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  labs(title = "Class 3 days", x = "Hours of the day (next day from hour 24)", y = "Probability",
       color = "Carbohydrate\nintake") + 
  scale_color_discrete(labels = c("Not eating", "< 50%", ">= 50%")) + 
  ylim(c(0,1))




chart.data <- CW3CB3 %>% 
  group_by(CB, CW) %>% 
  tally %>% 
  group_by(CB) %>% 
  mutate(pct = n/sum(n))


chart.data$CW[chart.data$CW == 1] <- 0
chart.data$CW[chart.data$CW == 3] <- 1
chart.data$CW[chart.data$CW == 0] <- 3

chart.data <- chart.data[order(chart.data$CB, chart.data$CW),]
chart.data <- ddply(chart.data, .(CB), 
                    transform, pos = cumsum(pct) - (0.5 * pct)) 



chart.data$CW3_2 <- factor(chart.data$CW, levels = c("3", "2", "1"), 
                        labels = c("Class 3 days", "Class 2 days", "Class 1 days"))
chart.data$CB <- factor(chart.data$CB, levels = c("1", "2", "3"), 
                        labels = c("Individual\nclass 1\n(28.2%)",  "Individual\nclass 2\n(28.9%)", 
                                   "Individual\nclass 3\n(42.8%)"))



library(ggthemr)
ggthemr("dust", layout = "scientific")
ggplot() + 
  geom_bar(aes(y = pct, x = CB, fill = CW3_2), data = chart.data, width = 0.6,
           stat="identity") + 
  geom_text(data=chart.data, aes(x = CB, y = pos, label = paste0(sprintf("%1.1f", pct*100),"%")),
            size=4, colour="white", family="Atlas Grotesk Medium") + 
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(), 
        axis.title = element_text(size = 18), 
        axis.text = element_text(size = 15), 
        axis.line = element_line(colour = "black"), 
        plot.title=element_text(family="Atlas Grotesk Medium"),
        text=element_text(family="Atlas Grotesk Light")) + 
  labs(title = "Multilevel latent class solution", x = "Between Individual classes", y = "Percentage") +
  scale_y_continuous(labels=percent)
