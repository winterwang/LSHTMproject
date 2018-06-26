# setwd("D:/Dropbox/LSHTM/study/project/6533STATA11/UKDA-6533-stata11_se/stata11_se/") # on Win 10
setwd("~/Dropbox/LSHTM/study/project/6533STATA11/UKDA-6533-stata11_se/stata11_se") # on Ubuntu

# setwd("~/Documents/LSHTMproject") # back to project

# install.packages("poLCA")
# install.packages("ggplot")
# install.packages("ggplot2")
# library(plyr)
# library(ggplot2)
# source("../../../Rcode/Raoulmycode.R")
# 
# 
# options(stringsAsFactors=FALSE)
library(haven)
data <- read_dta("ndns_rp_yr1-4a_foodleveldietarydata_uk.dta")
data56 <- read_dta("ndns_rp_yr5-6a_foodleveldietarydata.dta")
# data <- read.csv("ndns_rp_yr1-4a_foodleveldietarydata_uk.csv", header=TRUE, sep=",")
# data56 <- read.csv("h:/icarus/data/ndns_rp_yr5-6a_foodleveldietarydata_viastata.csv", header=TRUE, sep=",")

names(data)[names(data)=="seriali"] <- "id"
names(data56)[names(data56)=="seriali"] <- "id"
 df14d<-data[,c(113,1,2,3,5,6,7,8,9,21,24,55,57,58,59,60,61,62,63,64)]
 
 names(data)[c(113,1,2,3,5,6,7,8,9,21,24,55,57,58,59,60,61,62,63,64)]
 # [1] "id"                 "SurveyYear"             
 # [3] "Age"                     "Sex"                    
 # [5] "DayofWeek"               "DayNo"                  
 # [7] "DiaryDaysCompleted"      "MealTimeDescription"    
 # [9] "MealTime"                "EnergykJ"               
 # [11] "Carbohydrateg"           "Totalsugarsg"           
 # [13] "Starchg"                 "Glucoseg"               
 # [15] "Fructoseg"               "Sucroseg"               
 # [17] "Maltoseg"                "Lactoseg"               
 # [19] "Nonmilkextrinsicsugarsg" "Intrinsicandmilksugarsg"
df56d<-data56[,c(113,1,2,3,5,6,7,8,9,21,24,55,57,58,59,60,61,62,63,64)]
dfs1<-rbind(df14d,df56d)


dfs2<-dfs1[dfs1$Age>=19,]
 rm(data, data56)
dfs2$MealTime_chr <- as.character(dfs2$MealTime)
dfs2$MealTime_hm <- unlist(strsplit(dfs2$MealTime_chr," "))[c(FALSE, TRUE)]


dfs2$MealHourN<-as.numeric(unlist(strsplit(dfs2$MealTime_hm,":"))[c(TRUE, FALSE, FALSE)])
dfs2$MealMinN<-as.numeric(unlist(strsplit(dfs2$MealTime_hm,":"))[c(FALSE, TRUE, FALSE)])
dfs2$MealMinN0<- (60*dfs2$MealHourN)+dfs2$MealMinN


dfs3<-dfs2[order(dfs2$id,dfs2$DayNo,dfs2$MealMinN0),]


# dfs3$MealTimeDescription<-NULL
# dfs3$MealTime<-NULL
# dfs3$MealMinN<-NULL
# dfs3$MealHourN<-NULL

dfs4 <- aggregate(dfs3$EnergykJ, 
                  by=list(id=dfs3$id,
                          SurveyYear=dfs3$SurveyYear,
                          DiaryDaysCompleted=dfs3$DiaryDaysCompleted,
                          Age=dfs3$Age, 
                          Sex=dfs3$Sex,
                          MealMinN0=dfs3$MealMinN0), 
                  FUN=sum, na.rm=TRUE)


names(dfs4)[names(dfs4) == "x"] <- "EnergyKJ"
dfs4$EnergyKJAve<-dfs4$EnergyKJ/dfs4$DiaryDaysCompleted
saveRDS(dfs4, file="dfs4.Rda")
gap<-15
energy_threshold<-210

vecid<-unique(dfs4$id)



saveRDS(dfs4, file="dfs4.Rda")
# newobj <- readRDS(file="bigdf.Rda")

gap<-15
energy_threshold<-210



vecid<-unique(dfs4$id)

counter<-0
dfidmin<-data.frame()
numid<-length(vecid)
old<-Sys.time()
for (k in vecid){
  counter<-counter+1
  if ((counter/100) %% 1 ==0){
    new<-Sys.time()-old
    print(new)
    print(counter)
  }
  dftmp<-dfs4[dfs4$id==k,]
  dftmp<-dftmp[,c(1,6,7)]
  dfmin<-data.frame(min=0:1439)
  dfmin$id<-k
  dfmin$f1<-FALSE
  
  #Create a vector of id's
  #Loop through ids
  #Create a dataframe for each id containing a row.
  #for every minute of the day with two flags f1 and f2. Set f1 and f2 for all 1440 minutes.
  #set f1 TRUE if an EO(>210kj) has been in that minute or with in the next +14 minutes of that minute. 
  #set f2 TRUE if f1 TRUE and f1 false for the prior 15 minutes.
  
  
  for (i in 0:1439){
    if(nrow(dftmp[dftmp$MealMinN0==i,])>=1){
      dfsub<-subset(dftmp,(MealMinN0>=i & MealMinN0 <(i+(gap-1)))) 
      if (nrow(dfsub)>0){
        dfsub2<-ddply(dfsub,~id,summarise,Sum=sum(dfsub$EnergyKJ))
        if (dfsub2$Sum>energy_threshold){
          dfmin$f1[[i+1]]<-TRUE
        }
      }
    }
  }
  dfmin$f2<-FALSE
  for (i in 1:1440){
    if (dfmin$f1[[i]]==TRUE){
      dfmin$f2[[i]]<-TRUE
      if (i!=1) {
        tmp<-min((i-1),gap)
        for (j in 1:tmp){
          if(dfmin$f1[[i-j]]==TRUE){
            dfmin$f2[[i]]<-FALSE
          }
        }
      }
    }
  }
  dfmin$hour<-trunc(dfmin$min/60)
  dfmin$f1<-NULL
  dfmin$min<-NULL
  dfmin<-unique(dfmin[dfmin$f2==TRUE,])
  dfmin$f2<-NULL
  dfidmin<-rbind(dfidmin,dfmin)
}
dfidmin$f2<-2