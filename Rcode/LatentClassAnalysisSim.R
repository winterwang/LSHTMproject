alpha = rep(0.25, 4)
## equal
p1 = 0.85
p2 = 0.1
p3 = 0.2
P = matrix(c(
  p1,p2,p2,p2,
  p1,p3,p3,p3,
  p2,p1,p2,p2,
  p3,p1,p3,p3,
  p2,p2,p1,p2,
  p3,p3,p1,p3,
  p2,p2,p2,p1,
  p3,p3,p3,p1), nrow=8, ncol=4, byrow = TRUE)


library(poLCA)

source("Rcode/simLCA.R")
set.seed(37421)
X = simLCA(alpha, P, sampleSize=50, nsim=1)[[1]]
head(X)


f = cbind(Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8) ~ 1
poLCA(f, X, nclass = 3, verbose=FALSE)
