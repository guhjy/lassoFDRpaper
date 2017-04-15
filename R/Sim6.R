source("R/helpers.R")
require(ncvreg)
n <- 100
p1 <- 6
N <- 100
p <- 500
lam <- exp(seq(log(0.8),log(0.8*.1),len=50))
include.perm <- TRUE

if (include.perm) {
  res <- array(NA, dim=c(N, length(lam), 5), dimnames=list(1:N, lam, c("Selected", "True", "Analytic", "PermY", "PermR")))
} else {
  res <- array(NA, dim=c(N, length(lam), 3), dimnames=list(1:N, lam, c("Selected", "True", "Estimated")))
}

pb <- txtProgressBar(1, N, style=3)
for (i in 1:N) {
  ## Generate data; fit
  Data1 <- genData(n, p1, J0=6, rho=0)
  Data2 <- genData(n, p-p1, rho=0.8, beta=0)
  X <- cbind(Data1$X, Data2$X)
  if (include.perm) {
    pmfit.y <- perm.ncvreg(X, Data1$y, penalty="lasso", lambda=lam, permute="outcome")
    pmfit.r <- perm.ncvreg(X, Data1$y, penalty="lasso", lambda=lam, permute="residuals")    
    fit <- pmfit.y$fit
  } else {
    fit <- ncvreg(X, Data1$y, penalty="lasso", lambda=lam)
  }
  S <- apply(fit$beta[-1,]!=0, 2, sum)
  F <- apply(fit$beta[-(1:7),]!=0, 2, sum)
  
  ## Store
  res[i,,1] <- S
  res[i,,2] <- F
  res[i,,3] <- fir(fit)$EF
  if (include.perm) {
    res[i,,4] <- pmfit.y$EF
    res[i,,5] <- pmfit.r$EF    
  }
  setTxtProgressBar(pb, i)
}
save(res, file="res/Sim6.RData")
