
myplot<-function(mylist,mysubtitle) {
  myclasspr<-round(mylist[[2]],3)
  myplottext<-paste0(mylist[[3]]," Observations\n")
  myplottext<-paste0(myplottext,"class pr ",myclasspr[1]," ",myclasspr[2]," ",myclasspr[3],"\n")
  title<-"Probability of an Eating Occasion \n"
  myheading<-paste(title,mysubtitle)
  pr <-mylist[[2]]
  df2<-mylist[[1]]
  nobs<-mylist[[1]]
  ggplot(df2,aes(x=df2$hour)) +
    geom_line(aes(y = df2$pr1, colour = "Class 1")) +
    geom_point(aes(y = df2$pr1, colour = "Class 1"),size=0.4) +
    geom_line(aes(y = df2$pr2, colour = "Class 2")) +
    geom_point(aes(y = df2$pr2, colour = "Class 2"),size=0.4) +
    geom_line(aes(y = df2$pr3, colour = "Class 3")) +
    geom_point(aes(y = df2$pr3, colour = "Class 3"),size=0.4) +
    scale_colour_manual(name="line key", 
                        breaks = c("Class 1", "Class 2", "Class 3"),
                        values = c("red", "green", "blue") ) +
    ggtitle(myheading) +
    xlab('hour') +
    ylab('Probability')+
    annotate("text", label = myplottext, x=7,y = .03, size = 3, colour = "black",hjust = 0) +
    scale_x_continuous(breaks = seq(0,22 ,2))
}


mypoLCAfemale <- function(df1, reps) {
  f <- cbind(h0,h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12,h13,h14,h15,h16,h17,h18,h19,h20,h21,h22,h23)~1
  lc3 <- poLCA(f,df1,nclass=3,graphs = FALSE,nrep=reps)
  probs.start<-poLCA.reorder(lc3$probs.start,order(lc3$P,decreasing = T))
  new.probs.start <- poLCA.reorder(probs.start,c(2,1,3))
  lc3<-poLCA(f,df1,nclass=3,probs.start = new.probs.start)
  
  #View(lc2$probs$h0[.,2])
  hour<-c()
  pr1 <-c()
  pr2 <-c()
  pr3 <-c()
  #probs list of large poLCA object h0..h23 and the pr(2) of class one, two and three
  #I have used indices as I couldn't get names to work in a loop
  for(i in 0:23){
    j<-i+1
    hour<-append(hour,i)
    # nam<-paste("lc3$probs$h",i,sep = "")
    # nam<-paste(nam,"[1,2]",sep="")
    #   o<-as.numeric(get(nam))
    pr1<-append(pr1,lc3[[4]][[j]][1,2]) #class one prob
    pr2<-append(pr2,lc3[[4]][[j]][2,2]) #class two prob
    pr3<-append(pr3,lc3[[4]][[j]][3,2]) #class three prob
  }
  
  dfprdata<-data.frame(hour,pr1,pr2,pr3)
  mylist<-list(dfprdata,lc3$P,lc3$Nobs,lc3$llik,lc3$predclass)
  return (mylist)
}

mypoLCAfemale4 <- function(df1, reps) {
  f <- cbind(h0,h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12,h13,h14,h15,h16,h17,h18,h19,h20,h21,h22,h23)~1
  lc3 <- poLCA(f,df1,nclass=4,graphs = FALSE,maxiter = 10000,tol = 1e-13,nrep=reps)
  probs.start<-poLCA.reorder(lc3$probs.start,order(lc3$P,decreasing = T))
  new.probs.start <- poLCA.reorder(probs.start,c(2,3,1,4))
  lc3<-poLCA(f,df1,nclass=4,maxiter = 10000,probs.start = new.probs.start)
  
  #View(lc2$probs$h0[.,2])
  hour<-c()
  pr1 <-c()
  pr2 <-c()
  pr3 <-c()
  pr4 <-c()
  #probs list of large poLCA object h0..h23 and the pr(2) of class one, two and three
  #I have used indices as I couldn't get names to work in a loop
  for(i in 0:23){
    j<-i+1
    hour<-append(hour,i)
    # nam<-paste("lc3$probs$h",i,sep = "")
    # nam<-paste(nam,"[1,2]",sep="")
    #   o<-as.numeric(get(nam))
    pr1<-append(pr1,lc3[[4]][[j]][1,2]) #class one prob
    pr2<-append(pr2,lc3[[4]][[j]][2,2]) #class two prob
    pr3<-append(pr3,lc3[[4]][[j]][3,2]) #class three prob
    pr4<-append(pr4,lc3[[4]][[j]][4,2]) #class four prob
  }
  
  dfprdata<-data.frame(hour,pr1,pr2,pr3,pr4)
  mylist<-list(dfprdata,lc3$P,lc3$Nobs,lc3$llik,lc3$predclass)
  return (mylist)
}

mypoLCAmale <- function(df1, reps) {
  f <- cbind(h0,h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12,h13,h14,h15,h16,h17,h18,h19,h20,h21,h22,h23)~1
  lc3 <- poLCA(f,df1,nclass=3,maxiter = 10000,tol = 1e-13,graphs = FALSE,nrep=reps)
  probs.start<-poLCA.reorder(lc3$probs.start,order(lc3$P,decreasing = T))
  # new.probs.start <- poLCA.reorder(probs.start,c(1,3,2,4))
  lc3<-poLCA(f,df1,nclass=3,probs.start = probs.start)
  
  #View(lc2$probs$h0[.,2])
  hour<-c()
  pr1 <-c()
  pr2 <-c()
  pr3 <-c()
  #probs list of large poLCA object h0..h23 and the pr(2) of class one, two and three
  #I have used indices as I couldn't get names to work in a loop
  for(i in 0:23){
    j<-i+1
    hour<-append(hour,i)
    # nam<-paste("lc3$probs$h",i,sep = "")
    # nam<-paste(nam,"[1,2]",sep="")
    #   o<-as.numeric(get(nam))
    pr1<-append(pr1,lc3[[4]][[j]][1,2]) #class one prob
    pr2<-append(pr2,lc3[[4]][[j]][2,2]) #class two prob
    pr3<-append(pr3,lc3[[4]][[j]][3,2]) #class three prob
  }
  
  dfprdata<-data.frame(hour,pr1,pr2,pr3)
  mylist<-list(dfprdata,lc3$P,lc3$Nobs,lc3$llik,lc3$predclass)
  return (mylist)
}

mypoLCAmalebuck <- function(df1, reps) {
  f<-NULL
  f <- cbind(h5,h6,h7,h8,h9,h10,h11,h12,h13,h14,h15,h16,h17,h18,h19,h20,h21,h22,h23)~1
  lc3 <- poLCA(f,df1,nclass=3,maxiter = 10000,tol = 1e-13,graphs = FALSE,nrep=reps)
  probs.start<-poLCA.reorder(lc3$probs.start,order(lc3$P,decreasing = T))
  new.probs.start <- poLCA.reorder(probs.start,c(2,1,3))
  lc3<-poLCA(f,df1,nclass=3,probs.start = new.probs.start,maxiter = 10000)
  f<-NULL
  #View(lc2$probs$h0[.,2])
  hour<-c(0,1,2,3,4)
  pr1 <-c(0,0,0,0,0)
  pr2 <-c(0,0,0,0,0)
  pr3 <-c(0,0,0,0,0)
  #probs list of large poLCA object h0..h23 and the pr(2) of class one, two and three
  #I have used indices as I couldn't get names to work in a loop
  for(i in 0:18){
    j<-i+1
    hour<-append(hour,i+5)
    # nam<-paste("lc3$probs$h",i,sep = "")
    # nam<-paste(nam,"[1,2]",sep="")
    #   o<-as.numeric(get(nam))
    pr1<-append(pr1,lc3[[4]][[j]][1,2]) #class one prob
    pr2<-append(pr2,lc3[[4]][[j]][2,2]) #class two prob
    pr3<-append(pr3,lc3[[4]][[j]][3,2]) #class three prob
  }
  
  dfprdata<-data.frame(hour,pr1,pr2,pr3)
  mylist<-list(dfprdata,lc3$P,lc3$Nobs,lc3$llik,lc3$predclass)
  return (mylist)
}

mypoLCAfemalebuck <- function(df1, reps) {
  f<-NULL
  f <- cbind(h5,h6,h7,h8,h9,h10,h11,h12,h13,h14,h15,h16,h17,h18,h19,h20,h21,h22,h23)~1
  lc3 <- poLCA(f,df1,nclass=3,maxiter = 10000,tol = 1e-13,graphs = FALSE,nrep=reps)
  probs.start<-poLCA.reorder(lc3$probs.start,order(lc3$P,decreasing = T))
  new.probs.start <- poLCA.reorder(probs.start,c(3,2,1))
  lc3<-poLCA(f,df1,nclass=3,probs.start = new.probs.start,maxiter = 10000)
  f<-NULL
  #View(lc2$probs$h0[.,2])
  hour<-c(0,1,2,3,4)
  pr1 <-c(0,0,0,0,0)
  pr2 <-c(0,0,0,0,0)
  pr3 <-c(0,0,0,0,0)
  #probs list of large poLCA object h0..h23 and the pr(2) of class one, two and three
  #I have used indices as I couldn't get names to work in a loop
  for(i in 0:18){
    j<-i+1
    hour<-append(hour,i+5)
    # nam<-paste("lc3$probs$h",i,sep = "")
    # nam<-paste(nam,"[1,2]",sep="")
    #   o<-as.numeric(get(nam))
    pr1<-append(pr1,lc3[[4]][[j]][1,2]) #class one prob
    pr2<-append(pr2,lc3[[4]][[j]][2,2]) #class two prob
    pr3<-append(pr3,lc3[[4]][[j]][3,2]) #class three prob
  }
  
  dfprdata<-data.frame(hour,pr1,pr2,pr3)
  mylist<-list(dfprdata,lc3$P,lc3$Nobs,lc3$llik,lc3$predclass)
  return (mylist)
}
mypoLCAfemalebuck <- function(df1, reps) {
  f<-NULL
  f <- cbind(h5,h6,h7,h8,h9,h10,h11,h12,h13,h14,h15,h16,h17,h18,h19,h20,h21,h22,h23)~1
  lc3 <- poLCA(f,df1,nclass=3,maxiter = 10000,tol = 1e-13,graphs = FALSE,nrep=reps)
  probs.start<-poLCA.reorder(lc3$probs.start,order(lc3$P,decreasing = T))
  new.probs.start <- poLCA.reorder(probs.start,c(3,2,1))
  lc3<-poLCA(f,df1,nclass=3,probs.start = new.probs.start,maxiter = 10000)
  f<-NULL
  #View(lc2$probs$h0[.,2])
  hour<-c(0,1,2,3,4)
  pr1 <-c(0,0,0,0,0)
  pr2 <-c(0,0,0,0,0)
  pr3 <-c(0,0,0,0,0)
  #probs list of large poLCA object h0..h23 and the pr(2) of class one, two and three
  #I have used indices as I couldn't get names to work in a loop
  for(i in 0:18){
    j<-i+1
    hour<-append(hour,i+5)
    # nam<-paste("lc3$probs$h",i,sep = "")
    # nam<-paste(nam,"[1,2]",sep="")
    #   o<-as.numeric(get(nam))
    pr1<-append(pr1,lc3[[4]][[j]][1,2]) #class one prob
    pr2<-append(pr2,lc3[[4]][[j]][2,2]) #class two prob
    pr3<-append(pr3,lc3[[4]][[j]][3,2]) #class three prob
  }
  
  dfprdata<-data.frame(hour,pr1,pr2,pr3)
  mylist<-list(dfprdata,lc3$P,lc3$Nobs,lc3$llik,lc3$predclass)
  return (mylist)
}


mypoLCAallbuck <- function(df1, reps) {
  f<-NULL
  f <- cbind(h5,h6,h7,h8,h9,h10,h11,h12,h13,h14,h15,h16,h17,h18,h19,h20,h21,h22,h23)~1
  lc3 <- poLCA(f,df1,nclass=3,maxiter = 10000,tol = 1e-13,graphs = FALSE,nrep=reps)
  probs.start<-poLCA.reorder(lc3$probs.start,order(lc3$P,decreasing = T))
  new.probs.start <- poLCA.reorder(probs.start,c(2,1,3))
  lc3<-poLCA(f,df1,nclass=3,probs.start = new.probs.start,maxiter = 10000)
  f<-NULL
  #View(lc2$probs$h0[.,2])
  hour<-c(0,1,2,3,4)
  pr1 <-c(0,0,0,0,0)
  pr2 <-c(0,0,0,0,0)
  pr3 <-c(0,0,0,0,0)
  #probs list of large poLCA object h0..h23 and the pr(2) of class one, two and three
  #I have used indices as I couldn't get names to work in a loop
  for(i in 0:18){
    j<-i+1
    hour<-append(hour,i+5)
    # nam<-paste("lc3$probs$h",i,sep = "")
    # nam<-paste(nam,"[1,2]",sep="")
    #   o<-as.numeric(get(nam))
    pr1<-append(pr1,lc3[[4]][[j]][1,2]) #class one prob
    pr2<-append(pr2,lc3[[4]][[j]][2,2]) #class two prob
    pr3<-append(pr3,lc3[[4]][[j]][3,2]) #class three prob
  }
  
  dfprdata<-data.frame(hour,pr1,pr2,pr3)
  mylist<-list(dfprdata,lc3$P,lc3$Nobs,lc3$llik,lc3$predclass)
  return (mylist)
}

mypoLCAfemalebuck15 <- function(df1, reps) {
  f<-NULL
  f <- cbind(h5,h6,h7,h8,h9,h10,h11,h12,h13,h14,h15,h16,h17,h18,h19,h20,h21,h22,h23)~1
  lc3 <- poLCA(f,df1,nclass=3,maxiter = 10000,tol = 1e-13,graphs = FALSE,nrep=reps)
  probs.start<-poLCA.reorder(lc3$probs.start,order(lc3$P,decreasing = T))
  new.probs.start <- poLCA.reorder(probs.start,c(2,3,1))
  lc3<-poLCA(f,df1,nclass=3,probs.start = new.probs.start,maxiter = 10000)
  f<-NULL
  #View(lc2$probs$h0[.,2])
  hour<-c(0,1,2,3,4)
  pr1 <-c(0,0,0,0,0)
  pr2 <-c(0,0,0,0,0)
  pr3 <-c(0,0,0,0,0)
  #probs list of large poLCA object h0..h23 and the pr(2) of class one, two and three
  #I have used indices as I couldn't get names to work in a loop
  for(i in 0:18){
    j<-i+1
    hour<-append(hour,i+5)
    # nam<-paste("lc3$probs$h",i,sep = "")
    # nam<-paste(nam,"[1,2]",sep="")
    #   o<-as.numeric(get(nam))
    pr1<-append(pr1,lc3[[4]][[j]][1,2]) #class one prob
    pr2<-append(pr2,lc3[[4]][[j]][2,2]) #class two prob
    pr3<-append(pr3,lc3[[4]][[j]][3,2]) #class three prob
  }
  
  dfprdata<-data.frame(hour,pr1,pr2,pr3)
  mylist<-list(dfprdata,lc3$P,lc3$Nobs,lc3$llik,lc3$predclass)
  return (mylist)
}
mypoLCAfemalebuck60 <- function(df1, reps) {
  f<-NULL
  f <- cbind(h5,h6,h7,h8,h9,h10,h11,h12,h13,h14,h15,h16,h17,h18,h19,h20,h21,h22,h23)~1
  lc3 <- poLCA(f,df1,nclass=3,maxiter = 10000,tol = 1e-13,graphs = FALSE,nrep=reps)
  probs.start<-poLCA.reorder(lc3$probs.start,order(lc3$P,decreasing = T))
  new.probs.start <- poLCA.reorder(probs.start,c(3,2,1))
  lc3<-poLCA(f,df1,nclass=3,probs.start = new.probs.start,maxiter = 10000)
  f<-NULL
  #View(lc2$probs$h0[.,2])
  hour<-c(0,1,2,3,4)
  pr1 <-c(0,0,0,0,0)
  pr2 <-c(0,0,0,0,0)
  pr3 <-c(0,0,0,0,0)
  #probs list of large poLCA object h0..h23 and the pr(2) of class one, two and three
  #I have used indices as I couldn't get names to work in a loop
  for(i in 0:18){
    j<-i+1
    hour<-append(hour,i+5)
    # nam<-paste("lc3$probs$h",i,sep = "")
    # nam<-paste(nam,"[1,2]",sep="")
    #   o<-as.numeric(get(nam))
    pr1<-append(pr1,lc3[[4]][[j]][1,2]) #class one prob
    pr2<-append(pr2,lc3[[4]][[j]][2,2]) #class two prob
    pr3<-append(pr3,lc3[[4]][[j]][3,2]) #class three prob
  }
  
  dfprdata<-data.frame(hour,pr1,pr2,pr3)
  mylist<-list(dfprdata,lc3$P,lc3$Nobs,lc3$llik,lc3$predclass)
  return (mylist)
}


mypoLCAallbuck30 <- function(df1, reps) {
  f<-NULL
  f <- cbind(h5,h6,h7,h8,h9,h10,h11,h12,h13,h14,h15,h16,h17,h18,h19,h20,h21,h22,h23)~1
  lc3 <- poLCA(f,df1,nclass=3,maxiter = 10000,tol = 1e-13,graphs = FALSE,nrep=reps)
  probs.start<-poLCA.reorder(lc3$probs.start,order(lc3$P,decreasing = T))
  new.probs.start <- poLCA.reorder(probs.start,c(2,3,1))
  lc3<-poLCA(f,df1,nclass=3,probs.start = new.probs.start,maxiter = 10000)
  f<-NULL
  #View(lc2$probs$h0[.,2])
  hour<-c(0,1,2,3,4)
  pr1 <-c(0,0,0,0,0)
  pr2 <-c(0,0,0,0,0)
  pr3 <-c(0,0,0,0,0)
  #probs list of large poLCA object h0..h23 and the pr(2) of class one, two and three
  #I have used indices as I couldn't get names to work in a loop
  for(i in 0:18){
    j<-i+1
    hour<-append(hour,i+5)
    # nam<-paste("lc3$probs$h",i,sep = "")
    # nam<-paste(nam,"[1,2]",sep="")
    #   o<-as.numeric(get(nam))
    pr1<-append(pr1,lc3[[4]][[j]][1,2]) #class one prob
    pr2<-append(pr2,lc3[[4]][[j]][2,2]) #class two prob
    pr3<-append(pr3,lc3[[4]][[j]][3,2]) #class three prob
  }
  
  dfprdata<-data.frame(hour,pr1,pr2,pr3)
  mylist<-list(dfprdata,lc3$P,lc3$Nobs,lc3$llik,lc3$predclass)
  return (mylist)
}



mypoLCAallbuck15 <- function(df1, reps) {
  f<-NULL
  f <- cbind(h5,h6,h7,h8,h9,h10,h11,h12,h13,h14,h15,h16,h17,h18,h19,h20,h21,h22,h23)~1
  lc3 <- poLCA(f,df1,nclass=3,maxiter = 10000,tol = 1e-13,graphs = FALSE,nrep=reps)
  probs.start<-poLCA.reorder(lc3$probs.start,order(lc3$P,decreasing = T))
  new.probs.start <- poLCA.reorder(probs.start,c(3,1,2))
  lc3<-poLCA(f,df1,nclass=3,probs.start = new.probs.start,maxiter = 10000)
  f<-NULL
  #View(lc2$probs$h0[.,2])
  hour<-c(0,1,2,3,4)
  pr1 <-c(0,0,0,0,0)
  pr2 <-c(0,0,0,0,0)
  pr3 <-c(0,0,0,0,0)
  #probs list of large poLCA object h0..h23 and the pr(2) of class one, two and three
  #I have used indices as I couldn't get names to work in a loop
  for(i in 0:18){
    j<-i+1
    hour<-append(hour,i+5)
    # nam<-paste("lc3$probs$h",i,sep = "")
    # nam<-paste(nam,"[1,2]",sep="")
    #   o<-as.numeric(get(nam))
    pr1<-append(pr1,lc3[[4]][[j]][1,2]) #class one prob
    pr2<-append(pr2,lc3[[4]][[j]][2,2]) #class two prob
    pr3<-append(pr3,lc3[[4]][[j]][3,2]) #class three prob
  }
  
  dfprdata<-data.frame(hour,pr1,pr2,pr3)
  mylist<-list(dfprdata,lc3$P,lc3$Nobs,lc3$llik,lc3$predclass)
  return (mylist)
}

mypoLCAmalebuck15 <- function(df1, reps) {
  f<-NULL
  f <- cbind(h5,h6,h7,h8,h9,h10,h11,h12,h13,h14,h15,h16,h17,h18,h19,h20,h21,h22,h23)~1
  lc3 <- poLCA(f,df1,nclass=3,maxiter = 10000,tol = 1e-13,graphs = FALSE,nrep=reps)
  probs.start<-poLCA.reorder(lc3$probs.start,order(lc3$P,decreasing = T))
  new.probs.start <- poLCA.reorder(probs.start,c(3,2,1))
  lc3<-poLCA(f,df1,nclass=3,probs.start = new.probs.start,maxiter = 10000)
  f<-NULL
  #View(lc2$probs$h0[.,2])
  hour<-c(0,1,2,3,4)
  pr1 <-c(0,0,0,0,0)
  pr2 <-c(0,0,0,0,0)
  pr3 <-c(0,0,0,0,0)
  #probs list of large poLCA object h0..h23 and the pr(2) of class one, two and three
  #I have used indices as I couldn't get names to work in a loop
  for(i in 0:18){
    j<-i+1
    hour<-append(hour,i+5)
    # nam<-paste("lc3$probs$h",i,sep = "")
    # nam<-paste(nam,"[1,2]",sep="")
    #   o<-as.numeric(get(nam))
    pr1<-append(pr1,lc3[[4]][[j]][1,2]) #class one prob
    pr2<-append(pr2,lc3[[4]][[j]][2,2]) #class two prob
    pr3<-append(pr3,lc3[[4]][[j]][3,2]) #class three prob
  }
  
  dfprdata<-data.frame(hour,pr1,pr2,pr3)
  mylist<-list(dfprdata,lc3$P,lc3$Nobs,lc3$llik,lc3$predclass)
  return (mylist)
}

mypoLCAmalebuck30 <- function(df1, reps) {
  f<-NULL
  f <- cbind(h5,h6,h7,h8,h9,h10,h11,h12,h13,h14,h15,h16,h17,h18,h19,h20,h21,h22,h23)~1
  lc3 <- poLCA(f,df1,nclass=3,maxiter = 10000,tol = 1e-13,graphs = FALSE,nrep=reps)
  probs.start<-poLCA.reorder(lc3$probs.start,order(lc3$P,decreasing = T))
  new.probs.start <- poLCA.reorder(probs.start,c(2,3,1))
  lc3<-poLCA(f,df1,nclass=3,probs.start = new.probs.start,maxiter = 10000)
  f<-NULL
  #View(lc2$probs$h0[.,2])
  hour<-c(0,1,2,3,4)
  pr1 <-c(0,0,0,0,0)
  pr2 <-c(0,0,0,0,0)
  pr3 <-c(0,0,0,0,0)
  #probs list of large poLCA object h0..h23 and the pr(2) of class one, two and three
  #I have used indices as I couldn't get names to work in a loop
  for(i in 0:18){
    j<-i+1
    hour<-append(hour,i+5)
    # nam<-paste("lc3$probs$h",i,sep = "")
    # nam<-paste(nam,"[1,2]",sep="")
    #   o<-as.numeric(get(nam))
    pr1<-append(pr1,lc3[[4]][[j]][1,2]) #class one prob
    pr2<-append(pr2,lc3[[4]][[j]][2,2]) #class two prob
    pr3<-append(pr3,lc3[[4]][[j]][3,2]) #class three prob
  }
  
  dfprdata<-data.frame(hour,pr1,pr2,pr3)
  mylist<-list(dfprdata,lc3$P,lc3$Nobs,lc3$llik,lc3$predclass)
  return (mylist)
}


mypoLCAmale4 <- function(df1, reps) {
  f <- cbind(h0,h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12,h13,h14,h15,h16,h17,h18,h19,h20,h21,h22,h23)~1
  lc3 <- poLCA(f,df1,nclass=4,maxiter = 10000,tol = 1e-13,graphs = FALSE,nrep=reps)
  probs.start<-poLCA.reorder(lc3$probs.start,order(lc3$P,decreasing = T))
  new.probs.start <- poLCA.reorder(probs.start,c(1,3,2,4))
  lc3<-poLCA(f,df1,nclass=4,probs.start = new.probs.start,maxiter = 10000)
  
  #View(lc2$probs$h0[.,2])
  hour<-c()
  pr1 <-c()
  pr2 <-c()
  pr3 <-c()
  pr4 <-c()
  #probs list of large poLCA object h0..h23 and the pr(2) of class one, two and three
  #I have used indices as I couldn't get names to work in a loop
  for(i in 0:23){
    j<-i+1
    hour<-append(hour,i)
    # nam<-paste("lc3$probs$h",i,sep = "")
    # nam<-paste(nam,"[1,2]",sep="")
    #   o<-as.numeric(get(nam))
    pr1<-append(pr1,lc3[[4]][[j]][1,2]) #class one prob
    pr2<-append(pr2,lc3[[4]][[j]][2,2]) #class two prob
    pr3<-append(pr3,lc3[[4]][[j]][3,2]) #class three prob
    pr4<-append(pr4,lc3[[4]][[j]][4,2]) #class four prob
  }
  
  dfprdata<-data.frame(hour,pr1,pr2,pr3,pr4)
  mylist<-list(dfprdata,lc3$P,lc3$Nobs,lc3$llik,lc3$predclass)
  return (mylist)
}



mypoLCAreduce <- function(df1, reps) {
  f2 <- cbind(h6,h7,h8,h9,h10,h11,h12,h13,h14,h15,h16,h17,h18,h19,h20,h21,h22,h23)~1
  lc3 <- poLCA(f2,df1,nclass=3,maxiter = 10000,tol = 1e-13,graphs = FALSE,nrep=reps)
  probs.start<-poLCA.reorder(lc3$probs.start,order(lc3$P,decreasing = T))
  new.probs.start <- poLCA.reorder(probs.start,c(2,3,1))
  lc3<-poLCA(f2,df1,nclass=3,probs.start = new.probs.start)
  
  #View(lc2$probs$h0[.,2])
  hour<-c(0,1,2,3,4,5)
  pr1 <-c(0,0,0,0,0,0)
  pr2 <-c(0,0,0,0,0,0)
  pr3 <-c(0,0,0,0,0,0)
  #probs list of large poLCA object h0..h23 and the pr(2) of class one, two and three
  #I have used indices as I couldn't get names to work in a loop
  # for(i in 0:5){
  #   hour<-append(hour,i)
  #   # nam<-paste("lc3$probs$h",i,sep = "")
  #   # nam<-paste(nam,"[1,2]",sep="")
  #   #   o<-as.numeric(get(nam))
  #   pr1<-append(pr1,0) #class one prob
  #   pr2<-append(pr2,0) #class two prob
  #   pr3<-append(pr3,0) #class three prob
  # }
  
  for(i in 0:17){
    j<-i+1
    hour<-append(hour,i+6)
    # nam<-paste("lc3$probs$h",i,sep = "")
    # nam<-paste(nam,"[1,2]",sep="")
    #   o<-as.numeric(get(nam))
    pr1<-append(pr1,lc3[[4]][[j]][1,2]) #class one prob
    pr2<-append(pr2,lc3[[4]][[j]][2,2]) #class two prob
    pr3<-append(pr3,lc3[[4]][[j]][3,2]) #class three prob
  }
  
  dfprdata<-data.frame(hour,pr1,pr2,pr3)
  mylist<-list(dfprdata,lc3$P,lc3$Nobs,lc3$llik,lc3$predclass)
  return (mylist)
}

mypoLCAreduceMale <- function(df1, reps) {
  f2 <- cbind(h6,h7,h8,h9,h10,h11,h12,h13,h14,h15,h16,h17,h18,h19,h20,h21,h22,h23)~1
  lc3 <- poLCA(f2,df1,nclass=3,maxiter = 10000,tol = 1e-13,graphs = FALSE,nrep=reps)
  probs.start<-poLCA.reorder(lc3$probs.start,order(lc3$P,decreasing = T))
  # new.probs.start <- poLCA.reorder(probs.start,c(1,3,2))
  lc3<-poLCA(f2,df1,nclass=3,probs.start = probs.start,maxiter = 10000)
  
  #View(lc2$probs$h0[.,2])
  hour<-c(0,1,2,3,4,5)
  pr1 <-c(0,0,0,0,0,0)
  pr2 <-c(0,0,0,0,0,0)
  pr3 <-c(0,0,0,0,0,0)
  #probs list of large poLCA object h0..h23 and the pr(2) of class one, two and three
  #I have used indices as I couldn't get names to work in a loop
  # for(i in 0:5){
  #   hour<-append(hour,i)
  #   # nam<-paste("lc3$probs$h",i,sep = "")
  #   # nam<-paste(nam,"[1,2]",sep="")
  #   #   o<-as.numeric(get(nam))
  #   pr1<-append(pr1,0) #class one prob
  #   pr2<-append(pr2,0) #class two prob
  #   pr3<-append(pr3,0) #class three prob
  # }
  
  for(i in 0:17){
    j<-i+1
    hour<-append(hour,i+6)
    # nam<-paste("lc3$probs$h",i,sep = "")
    # nam<-paste(nam,"[1,2]",sep="")
    #   o<-as.numeric(get(nam))
    pr1<-append(pr1,lc3[[4]][[j]][1,2]) #class one prob
    pr2<-append(pr2,lc3[[4]][[j]][2,2]) #class two prob
    pr3<-append(pr3,lc3[[4]][[j]][3,2]) #class three prob
  }
  
  dfprdata<-data.frame(hour,pr1,pr2,pr3)
  mylist<-list(dfprdata,lc3$P,lc3$Nobs,lc3$llik,lc3$predclass)
  return (mylist)
}



myplotreduce<-function(mylist,mysubtitle) {
  myclasspr<-round(mylist[[2]],3)
  myplottext<-paste0(mylist[[3]]," Observations\n")
  myplottext<-paste0(myplottext,"class pr ",myclasspr[1]," ",myclasspr[2]," ",myclasspr[3],"\n")
  title<-"Probability of an Eating Occasion \n"
  myheading<-paste(title,mysubtitle)
  pr <-mylist[[2]]
  df<-mylist[[1]]
  df2<-df[df$hour>=6,]
  nobs<-mylist[[1]]
  ggplot(df2,aes(x=df2$hour)) +
    geom_line(aes(y = df2$pr1, colour = "Class 1")) +
    geom_point(aes(y = df2$pr1, colour = "Class 1"),size=0.4) +
    geom_line(aes(y = df2$pr2, colour = "Class 2")) +
    geom_point(aes(y = df2$pr2, colour = "Class 2"),size=0.4) +
    geom_line(aes(y = df2$pr3, colour = "Class 3")) +
    geom_point(aes(y = df2$pr3, colour = "Class 3"),size=0.4) +
    scale_colour_manual(name="line key", 
                        breaks = c("Class 1", "Class 2", "Class 3"),
                        values = c("red", "green", "blue") ) +
    ggtitle(myheading) +
    xlab('hour') +
    ylab('Probability')+
    annotate("text", label = myplottext, x=7,y = .03, size = 3, colour = "black",hjust = 0) +
    scale_x_continuous(breaks = seq(0,22 ,2))
}

myplotreduce2<-function(mylist,mylist2,mysubtitle) {
  myclasspr<-round(mylist[[2]],3)
  myclasspr2<-round(mylist2[[2]],3)
  myplottext<-paste0(mylist[[3]]," Observations\n")
  myplottext<-paste0(myplottext,"class pr ",myclasspr[1]," ",myclasspr[2]," ",myclasspr[3],"\n")
  myplottext<-paste0(myplottext,"class pr ",myclasspr2[1]," ",myclasspr2[2]," ",myclasspr2[3],"\n")
  title<-"Probability of an Eating Occasion \n"
  myheading<-paste(title,mysubtitle)
  pr <-mylist[[2]]
  df2<-mylist[[1]]
  df3<-mylist2[[1]]
  nobs<-mylist[[1]]
  ggplot(df2,aes(x=df2$hour)) +
    geom_line(aes(y = df2$pr1, colour = "Class 1")) +
    geom_point(aes(y = df2$pr1, colour = "Class 1"),size=0.4) +
    geom_line(aes(y = df2$pr2, colour = "Class 2")) +
    geom_point(aes(y = df2$pr2, colour = "Class 2"),size=0.4) +
    geom_line(aes(y = df2$pr3, colour = "Class 3")) +
    geom_point(aes(y = df2$pr3, colour = "Class 3"),size=0.4) +
    geom_line(aes(y = df3$pr1, colour = "Class 1") ,linetype="dotted") +
    geom_point(aes(y = df3$pr1, colour = "Class 1"),size=0.4) +
    geom_line(aes(y = df3$pr2, colour = "Class 2") ,linetype="dotted") +
    geom_point(aes(y = df3$pr2, colour = "Class 2"),size=0.4) +
    geom_line(aes(y = df3$pr3, colour = "Class 3"),linetype="dotted") +
    geom_point(aes(y = df3$pr3, colour = "Class 3"),size=0.4) +
    geom_vline(xintercept=6,linetype=4) +
    scale_colour_manual(name="line key", 
                        breaks = c("Class 1", "Class 2", "Class 3"),
                        values = c("red", "green", "blue") ) +
    ggtitle(myheading) +
    xlab('hour') +
    ylab('Probability')+
    annotate("text", label = myplottext, x=7,y = .03, size = 3, colour = "black",hjust = 0) +
    scale_x_continuous(breaks = seq(0,22 ,2))
}
myplotbuck<-function(mylist,mylist2,mysubtitle) {
  myclasspr<-round(mylist[[2]],3)
  myclasspr2<-round(mylist2[[2]],3)
  myplottext<-paste0(mylist[[3]]," Observations\n")
  myplottext<-paste0(myplottext,"class pr ",myclasspr[1]," ",myclasspr[2]," ",myclasspr[3],"\n")
  myplottext<-paste0(myplottext,"class pr ",myclasspr2[1]," ",myclasspr2[2]," ",myclasspr2[3],"\n")
  title<-"Probability of an Eating Occasion \n"
  myheading<-paste(title,mysubtitle)
  pr <-mylist[[2]]
  df2<-mylist[[1]]
  df3<-mylist2[[1]]
  nobs<-mylist[[1]]
  df2<-df2[df2$hour>=5,]
  df3<-df3[df3$hour>=5,]
  ggplot(df2,aes(x=df2$hour)) +
    geom_line(aes(y = df2$pr1, colour = "Class 1")) +
    geom_point(aes(y = df2$pr1, colour = "Class 1"),size=0.4) +
    geom_line(aes(y = df2$pr2, colour = "Class 2")) +
    geom_point(aes(y = df2$pr2, colour = "Class 2"),size=0.4) +
    geom_line(aes(y = df2$pr3, colour = "Class 3")) +
    geom_point(aes(y = df2$pr3, colour = "Class 3"),size=0.4) +
    geom_line(aes(y = df3$pr1, colour = "Class 1") ,linetype="dotted") +
    geom_point(aes(y = df3$pr1, colour = "Class 1"),size=0.4) +
    geom_line(aes(y = df3$pr2, colour = "Class 2") ,linetype="dotted") +
    geom_point(aes(y = df3$pr2, colour = "Class 2"),size=0.4) +
    geom_line(aes(y = df3$pr3, colour = "Class 3"),linetype="dotted") +
    geom_point(aes(y = df3$pr3, colour = "Class 3"),size=0.4) +
    scale_colour_manual(name="line key", 
                        breaks = c("Class 1", "Class 2", "Class 3"),
                        values = c("red", "green", "blue") ) +
    ggtitle(myheading) +
    xlab('hour') +
    ylab('Probability')+
    annotate("text", label = myplottext, x=7,y = .03, size = 3, colour = "black",hjust = 0) +
    scale_x_continuous(breaks = seq(5,23 ,2),labels=c("0-5",seq(7,23,2)))
}
myplot19pub<-function(mylist,mysubtitle) {
  myclasspr<-round(mylist[[2]],3)
  # myplottext<-paste0(mylist[[3]]," Observations\n")
  # myplottext<-paste0(myplottext,"class pr ",myclasspr[1]," ",myclasspr[2]," ",myclasspr[3],"\n")
  #  title<-"Probability of an Eating Occasion \n"
  #  myheading<-paste(title,mysubtitle)
  pr <-mylist[[2]]
  df2<-mylist[[1]]
  df2<-df2[df2$hour>=5,]
  nobs<-mylist[[1]]
  ggplot(df2,aes(x=df2$hour)) +
    geom_line(aes(y = df2$pr1, colour = "Class 1")) +
    geom_point(aes(y = df2$pr1, colour = "Class 1"),size=0.4) +
    geom_line(aes(y = df2$pr2, colour = "Class 2")) +
    geom_point(aes(y = df2$pr2, colour = "Class 2"),size=0.4) +
    geom_line(aes(y = df2$pr3, colour = "Class 3")) +
    geom_point(aes(y = df2$pr3, colour = "Class 3"),size=0.4) +
    scale_colour_manual(name="line key",
                        breaks = c("Class 1", "Class 2", "Class 3"),
                        values = c("red", "green", "blue") ) +
    #    ggtitle(myheading) +
    # theme(legend.position="bottom") +
    # theme(legend.box = "horizontal") +
    xlab('Hour of day') +
    ylab('Probability')+
    ylim(-0.01,0.9)+ 
    annotate("text", label=mysubtitle, x=5, y=.8, size=5, hjust = 0) +
    theme(legend.position="none") +
    #    annotate("text", label = myplottext, x=7,y = .03, size = 3, colour = "black",hjust = 0) +
    scale_x_continuous(breaks = seq(5,23 ,2),labels=c("0-5",seq(7,23,2)))
}
myplot19<-function(mylist,mysubtitle) {
  myclasspr<-round(mylist[[2]],3)
  myplottext<-paste0(mylist[[3]]," Observations\n")
  myplottext<-paste0(myplottext,"class pr ",myclasspr[1]," ",myclasspr[2]," ",myclasspr[3],"\n")
  title<-"Probability of an Eating Occasion \n"
  myheading<-paste(title,mysubtitle)
  pr <-mylist[[2]]
  df2<-mylist[[1]]
  df2<-df2[df2$hour>=5,]
  nobs<-mylist[[1]]
  ggplot(df2,aes(x=df2$hour)) +
    geom_line(aes(y = df2$pr1, colour = "Class 1")) +
    geom_point(aes(y = df2$pr1, colour = "Class 1"),size=0.4) +
    geom_line(aes(y = df2$pr2, colour = "Class 2")) +
    geom_point(aes(y = df2$pr2, colour = "Class 2"),size=0.4) +
    geom_line(aes(y = df2$pr3, colour = "Class 3")) +
    geom_point(aes(y = df2$pr3, colour = "Class 3"),size=0.4) +
    scale_colour_manual(name="line key",
                        breaks = c("Class 1", "Class 2", "Class 3"),
                        values = c("red", "green", "blue") ) +
    ggtitle(myheading) +
    #theme(panel.background = element_rect(fill="white",colour="black"),panel.grid.major = element_line(colour = "black")) +
    # theme(legend.position="bottom") +
    # theme(legend.box = "horizontal") +
    xlab('Hour of day') +
    ylab('Probability')+
    ylim(-0.01,.95)+ 
    # annotate("text", label=mysubtitle, x=5, y=.8, size=5, hjust = 0) +
    theme(legend.position="none") +
    annotate("text", label = myplottext, x=7,y = .03, size = 3, colour = "black",hjust = 0) +
    scale_x_continuous(breaks = seq(5,23 ,2),labels=c("0-5",seq(7,23,2)))
}
myplot19conf<-function(mylist,mysubtitle) {
  myclasspr<-round(mylist[[2]],3)
  myplottext<-paste0(mylist[[3]]," Observations\n")
  myplottext<-paste0(myplottext,"class pr ",myclasspr[1]," ",myclasspr[2]," ",myclasspr[3],"\n")
  title<-"Probability of an Eating Occasion \n"
  myheading<-paste(title,mysubtitle)
  pr <-mylist[[2]]
  df2<-mylist[[1]]
  df2<-df2[df2$hour>=5,]
  nobs<-mylist[[1]]
  ggplot(df2,aes(x=df2$hour)) +
    geom_line(aes(y = df2$pr1, colour = "Class 1")) +
    geom_point(aes(y = df2$pr1, colour = "Class 1"),size=0.4) +
    geom_line(aes(y = df2$pr2, colour = "Class 2")) +
    geom_point(aes(y = df2$pr2, colour = "Class 2"),size=0.4) +
    geom_line(aes(y = df2$pr3, colour = "Class 3")) +
    geom_point(aes(y = df2$pr3, colour = "Class 3"),size=0.4) +
    scale_colour_manual(name="line key",
                        breaks = c("Class 1", "Class 2", "Class 3"),
                        values = c("red", "green", "blue") ) +
    theme(panel.background = element_blank(), 
          panel.grid.major = element_line(colour="grey", size = (0.2)), 
          panel.grid.minor = element_line(size = (0.2), colour="grey"),
          axis.line.x = element_line(size=1, colour = "red"),
          axis.line.y = element_line(size=1, colour = "red")
    ) +
    # ggtitle(myheading) +
    #theme(panel.background = element_rect(fill="white",colour="black"),panel.grid.major = element_line(colour = "black")) +
    # theme(legend.position="bottom") +
    # theme(legend.box = "horizontal") +
    xlab('Hour of day') +
    ylab('Probability')+
    ylim(-0.01,0.9)+ 
    annotate("text", label=mysubtitle, x=5, y=.8, size=7, hjust = 0) +
    theme(legend.position="none") +
    # annotate("text", label = myplottext, x=7,y = .03, size = 3, colour = "black",hjust = 0) +
    scale_x_continuous(breaks = seq(5,23 ,2),labels=c("0-5",seq(7,23,2)))
}
myplot19conf<-function(mylist,mysubtitle) {
  myclasspr<-round(mylist[[2]],3)
  myplottext<-paste0(mylist[[3]]," Observations\n")
  myplottext<-paste0(myplottext,"class pr ",myclasspr[1]," ",myclasspr[2]," ",myclasspr[3],"\n")
  title<-"Probability of an Eating Occasion \n"
  myheading<-paste(title,mysubtitle)
  pr <-mylist[[2]]
  df2<-mylist[[1]]
  df2<-df2[df2$hour>=5,]
  nobs<-mylist[[1]]
  ggplot(df2,aes(x=df2$hour)) +
    geom_line(aes(y = df2$pr1, colour = "Class 1")) +
    geom_point(aes(y = df2$pr1, colour = "Class 1"),size=0.4) +
    geom_line(aes(y = df2$pr2, colour = "Class 2")) +
    geom_point(aes(y = df2$pr2, colour = "Class 2"),size=0.4) +
    geom_line(aes(y = df2$pr3, colour = "Class 3")) +
    geom_point(aes(y = df2$pr3, colour = "Class 3"),size=0.4) +
    scale_colour_manual(name="line key",
                        breaks = c("Class 1", "Class 2", "Class 3"),
                        values = c("red", "green", "blue") ) +
    theme(panel.background = element_blank(), 
          panel.grid.major = element_line(colour="grey", size = (0.2)), 
          panel.grid.minor = element_line(size = (0.2), colour="grey"),
          axis.text.x =element_text(size=11),
          axis.text.y =element_text(size=11),
          axis.title.x =element_text(size=15),
          axis.title.y =element_text(size=15),
          axis.line.x = element_line(size=1, colour = "red"),
          axis.line.y = element_line(size=1, colour = "red")
    ) +
    # ggtitle(myheading) +
    #theme(panel.background = element_rect(fill="white",colour="black"),panel.grid.major = element_line(colour = "black")) +
    # theme(legend.position="bottom") +
    # theme(legend.box = "horizontal") +
    xlab('Hour of day') +
    ylab('Probability')+
    ylim(-0.01,0.9)+ 
    annotate("text", label=mysubtitle, x=5, y=.8, size=7, hjust = 0) +
    theme(legend.position="none") +
    # annotate("text", label = myplottext, x=7,y = .03, size = 3, colour = "black",hjust = 0) +
    scale_x_continuous(breaks = seq(5,23 ,2),labels=c("0-5",seq(7,23,2)))
}
