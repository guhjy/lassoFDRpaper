SimCT <- function(N=100) {
  n <- 100
  p1 <- 6
  p <- c(60,600)
  K <- c(3,10)
  lam <- exp(seq(log(1.5),log(1.5*.01),len=89))
  cmp <- array(NA, dim=c(N, length(p), 1, 3), dimnames=list(1:N, p, c("covTest"), c("S1", "S2", "S3")))

  pb <- txtProgressBar(1, N, style=3)
  for (i in 1:N) {
    for (j in 1:length(p)) {
      ## Generate data; fit
      bb <- numeric(6*K[j])
      bb[(0:5)*K[j]+1] <- c(1,-1,0.5,0.5,-0.5,-0.5)
      D1 <- genData(n, J=6, J0=6, K=K[j], K0=1, rho=0, rho.g=0.5, beta=bb)
      D2 <- genData(n, p[j] - 6*K[j], beta=0)
      X <- cbind(D1$X, D2$X)
      y <- D1$y
      beta <- c(D1$beta, D2$beta)
      group <- c(D1$group, 6+D2$group)
      fit <- lars(X, y, use.Gram=FALSE)
      l <- ceiling(sqrt(p[j]))
      sig <- sqrt(fit$RSS[l]/(n-fit$df[l]))
      res <- covTest(fit, X, y, sigma.est=sig)
      if (length(which(res$results[,3] < 0.05))) {
        sel <- res$results[1:max(which(res$results[,3] < 0.05)),1]
      } else {
        sel <- NULL
      }
      b <- numeric(p[j])
      b[sel] <- 1

      truth <- rep("", p[j])
      truth[beta!=0] <- "S1"
      truth[beta==0 & group<=6] <- "S2"
      truth[group > 6] <- "S3"
      cmp[i,j,1,] <- tapply(b, truth, sum)
    }
    setTxtProgressBar(pb, i)
  }
  cmp
}
