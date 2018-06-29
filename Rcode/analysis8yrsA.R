
# NDNS analysis, data management ------------------------------------------


# Change the data path accordingly ----------------------------------------

setwd("/home/wangcc-me/Downloads/UKDA-6533-stata11_se/stata11_se/") # on Ubuntu

library(tidyverse)
library(epiDisplay)

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
  select(var)

df78d <- data78 %>% 
  select(var)

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
