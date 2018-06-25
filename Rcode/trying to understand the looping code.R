dftmp<-dfs4[dfs4$id==vecid[1],]
dftmp<-dftmp[,c(1,6,7)]
dfmin<-data.frame(min=0:1439)
dfmin$id<-vecid[1]
dfmin$f1<-FALSE

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

# 
# dfsub<-subset(dftmp,(MealMinN0>=900 & MealMinN0 <(900+(gap-1)))) 
# dfsub2<-aggregate(dfsub$EnergyKJAve,by=list(id=dfsub$id),
#                   FUN=sum, na.rm=TRUE)
# library(plyr)
# ddply(dfsub,~id,summarise,Sum=sum(dfsub$EnergyKJ))


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
# dfmin$f1<-NULL
# dfmin$min<-NULL
dfmin<-unique(dfmin[dfmin$f2==TRUE,])
# dfmin$f2<-NULL
dfidmin<-rbind(dfidmin,dfmin)
