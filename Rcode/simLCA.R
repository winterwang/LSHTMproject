simLCA <- function(alpha, ProbMat, sampleSize, nsim) {
  ## Reformat the probability Matrix to a form required by poLCA.simdata
  probs = vector("list", nrow(ProbMat))
  for (i in 1:nrow(ProbMat)) {
    probs[[i]] = cbind(ProbMat[i,], 1 - ProbMat[i,])
  }

X = poLCA.simdata(N = sampleSize*nsim, probs = probs, P = alpha)$dat
split(X, rep(1:nsim, each=sampleSize))

}