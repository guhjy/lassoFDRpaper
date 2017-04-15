source("R/helpers.R")
# Sample splitting

require(hdi)
n <- 100
p1 <- 6
N <- 100
p <- c(60,600)
K <- c(3,10)
##p <- 500
lam <- exp(seq(log(1.5),log(1.5*.01),len=89))
res <- array(NA, dim=c(N, length(lam), length(p), 4), dimnames=list(1:N, lam, p, c("S1", "S2", "S3", "EF")))
cmp <- array(NA, dim=c(N, length(p), 1, 3), dimnames=list(1:N, p, c("sampleSplit"), c("S1", "S2", "S3")))

pb <- txtProgressBar(1, N, style=3)
for (i in 1:N) {
  for (j in 1:length(p)) {
    ## Generate data; fit
    bb <- numeric(6*K[j])
    bb[(0:5)*K[j]+1] <- c(1,-1,0.5,0.5,-0.5,-0.5)
    D1 <- genData(n, J=6, J0=6, K=K[j], K0=1, rho=0, rho.g=0.5, beta=bb)
    D2 <- genData(n, p[j] - 6*K[j], beta=0)
    Data <- list(y=D1$y, X=cbind(D1$X, D2$X), beta=c(D1$beta, D2$beta), group=c(D1$group, 6+D2$group))
    res <- multi.split(Data$X, Data$y, B=5, ci=FALSE, args.model.selector=list(lambda.min=0.1, nfolds=5), gamma=c(.05, .25, .45, .65, .85))
    q <- fdr.adjust(res$pval.corr)    
    b <- 1*(q < .1)

    truth <- rep("", p[j])
    truth[Data$beta!=0] <- "S1"
    truth[Data$beta==0 & Data$group<=6] <- "S2"
    truth[Data$group > 6] <- "S3"
    cmp[i,j,1,] <- tapply(b, truth, sum)
  }
  setTxtProgressBar(pb, i)
  cmpMS <- cmp
}


# Selective inference

require(selectiveInference)
n <- 100
p1 <- 6
N <- 100
p <- c(60,600)
K <- c(3,10)
##p <- 500
lam <- exp(seq(log(1.5),log(1.5*.01),len=89))
res <- array(NA, dim=c(N, length(lam), length(p), 4), dimnames=list(1:N, lam, p, c("S1", "S2", "S3", "EF")))
cmp <- array(NA, dim=c(N, length(p), 1, 3), dimnames=list(1:N, p, c("covTest"), c("S1", "S2", "S3")))

pb <- txtProgressBar(1, N, style=3)
for (i in 1:N) {
  for (j in 1:length(p)) {
    ## Generate data; fit
    bb <- numeric(6*K[j])
    bb[(0:5)*K[j]+1] <- c(1,-1,0.5,0.5,-0.5,-0.5)
    D1 <- genData(n, J=6, J0=6, K=K[j], K0=1, rho=0, rho.g=0.5, beta=bb)
    D2 <- genData(n, p[j] - 6*K[j], beta=0)
    Data <- list(y=D1$y, X=cbind(D1$X, D2$X), beta=c(D1$beta, D2$beta), group=c(D1$group, 6+D2$group))

    fit <- lar(Data$X, Data$y)
    inf <- larInf(fit, estimateSigma(Data$X, Data$y)$sigma)
    stp <- forwardStop(inf$pv)
    sel <- inf$vars[1:stp]
    b <- numeric(p[j])
    b[sel] <- 1

    truth <- rep("", p[j])
    truth[Data$beta!=0] <- "S1"
    truth[Data$beta==0 & Data$group<=6] <- "S2"
    truth[Data$group > 6] <- "S3"
    cmp[i,j,1,] <- tapply(b, truth, sum)
  }
  setTxtProgressBar(pb, i)
  cmpCT <- cmp
}

save(cmpMS, cmpCT, file="res/Sim3.RData")
