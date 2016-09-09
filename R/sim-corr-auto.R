Sim4 <- function(include.perm=FALSE, N=ifelse(include.perm, 100, 500)) {
  n <- 100
  p1 <- 6
  p <- 500
  lam <- exp(seq(log(0.8),log(0.8*.1),len=50))

  if (include.perm) {
    res <- array(NA, dim=c(N, length(lam), 5), dimnames=list(1:N, lam, c("Selected", "True", "Analytic", "PermY", "PermR")))
  } else {
    res <- array(NA, dim=c(N, length(lam), 3), dimnames=list(1:N, lam, c("Selected", "True", "Estimated")))
  }

  pb <- txtProgressBar(1, N, style=3)
  for (i in 1:N) {
    ## Generate data; fit
    Data1 <- genData(n, p1, J0=6, rho=0)
    Data2 <- genData(n, p-p1, rho=0.8, beta=0, corr="auto")
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
  res
}
Fig4 <- function(res) {
  df <- array2df(apply(res[,,-1], 2:3, sum), vars=c("lambda", "Type", "Avg"))
  S <- apply(res[,,1], 2, sum)
  df$FIR <- df$Avg/S
  df$FIR[is.na(df$FIR)] <- 0
  df$Avg <- df$Avg/dim(res)[1]
  df$lam <- factor2num(df$lambda)
  df$ll <- log(df$lam)

  xlim <- rev(range(df$ll))
  p1 <- qplot(ll, Avg, data=df, color=Type, geom="line", xlab=expression(log(lambda)), ylab="False inclusions", xlim=xlim) +
    geom_line(size=2) + scale_color_manual(values=pal(2, alpha=0.5)) + theme(legend.position="top")
  p2 <- qplot(ll, FIR, data=df, color=Type, geom="line", xlab=expression(log(lambda)), ylab="False inclusions", xlim=xlim) +
    geom_line(size=2) + scale_color_manual(values=pal(2, alpha=0.5)) + theme(legend.position="top")

  grid.arrange(p1, p2, widths=c(1,1))
}
