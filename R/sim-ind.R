Sim2 <- function(N=100) {
  n <- 100
  p1 <- 6
  p <- c(60,600)
  K <- c(3,10)
  lam <- exp(seq(log(1.5),log(1.5*.01),len=89))
  res <- array(NA, dim=c(N, length(lam), length(p), 4), dimnames=list(1:N, lam, p, c("S1", "S2", "S3", "EF")))
  cmp <- array(NA, dim=c(N, length(p), 4, 3), dimnames=list(1:N, p, c("Univ", "LassoFDR", "LassoBIC", "LassoCV"), c("S1", "S2", "S3")))

  pb <- txtProgressBar(1, N, style=3)
  for (i in 1:N) {
    for (j in 1:length(p)) {
      ## Generate data; fit
      #D1 <- genData(n, J=6, J0=6, K=K[j], K0=1, rho=0, rho.g=0.5, beta=1)
      bb <- numeric(6*K[j])
      bb[(0:5)*K[j]+1] <- c(1,-1,0.5,0.5,-0.5,-0.5)
      D1 <- genData(n, J=6, J0=6, K=K[j], K0=1, rho=0, rho.g=0.5, beta=bb)
      D2 <- genData(n, p[j] - 6*K[j], beta=0)
      Data <- list(y=D1$y, X=cbind(D1$X, D2$X), beta=c(D1$beta, D2$beta), group=c(D1$group, 6+D2$group))
      D1 <- genData(n, J=6, J0=6, K=K[j], K0=1, rho=0, rho.g=0.5, beta=bb)
      D2 <- genData(n, p[j] - 6*K[j], beta=0)
      CVData <- list(y=D1$y, X=cbind(D1$X, D2$X), beta=c(D1$beta, D2$beta), group=c(D1$group, 6+D2$group))
      ##Data <- genData(n, p[j], J0=6, K=1, K0=1, rho=0, rho.g=0.5)
      X <- std(Data$X)
      y <- as.numeric(scale(Data$y, scale=FALSE))
      z <- crossprod(X,y)/n
      fit <- ncvreg(X, y, penalty="lasso", lambda=lam, warn=FALSE)

      S1 <- apply(fit$beta[-1,]!=0 & Data$beta!=0, 2, sum)
      S2 <- apply(fit$beta[-1,]!=0 & Data$beta==0 & Data$group<=6, 2, sum)
      S3 <- apply(fit$beta[-1,]!=0 & Data$group > 6, 2, sum)

      ## Estimate
      R <- y-predict(fit,X)
      sig <- sqrt(apply(R,2,crossprod)/(n-S1-S2-S3))
      ##tau <- sqrt(apply(R,2,crossprod))/n
      Z <- crossprod(X,R)/n+fit$beta[-1,]

      EF <- pmin(p[j]*2*pnorm(-sqrt(n)*fit$lam/sig), S1+S2+S3)
      ##matplot(fit$lam,cbind(FDR0,FDR1,FDR2), lty=1,lwd=3,type="l",col=pal(3))

      res[i,,j,1] <- S1
      res[i,,j,2] <- S2
      res[i,,j,3] <- S3
      res[i,,j,4] <- EF

      ## Select
      yhat <- predict(fit, std(CVData$X))
      lam.cv <- which.min(apply(yhat-as.numeric(scale(CVData$y, scale=FALSE)), 2, crossprod))
      cv <- coef(fit, which=lam.cv)[-1]!=0
      ind <- which((S1+S2+S3)!=0)
      bic.ss <- smooth.spline(-log(lam)[ind], BIC(fit)[ind], spar=0.5)
      d <- diff(bic.ss$y)
      bic.ind <- if (all(d<0)) length(bic.ss$x) else min(which(d>0))-1
      if (bic.ind==0) bic.ind <- 1
      ##plot(bic.ss); abline(v=bic.ss$x[bic.ind])
      bic <- coef(fit, lambda=exp(-bic.ss$x[bic.ind]))[-1]!=0

      univ <- select.univ(X,y,"gaussian")$s
      allowed <- EF/(S1+S2+S3) < 0.1
      allowed[is.na(allowed)] <- FALSE
      fdr.ind <- if (sum(allowed)) max(which(allowed)) else 1
      fdr <- coef(fit, which=fdr.ind)[-1]!=0
      truth <- rep("", p[j])
      truth[Data$beta!=0] <- "S1"
      truth[Data$beta==0 & Data$group<=6] <- "S2"
      truth[Data$group > 6] <- "S3"
      cmp[i,j,1,] <- tapply(univ, truth, sum)
      cmp[i,j,2,] <- tapply(fdr, truth, sum)
      cmp[i,j,3,] <- tapply(bic, truth, sum)
      cmp[i,j,4,] <- tapply(cv, truth, sum)
    }
    setTxtProgressBar(pb, i)
  }
  list(res=res, cmp=cmp)
}
Fig2 <- function(results) {
  errMsg <- "You first need to run Sim2 and pass its results:
  results <- Sim2()
  Fig2(results)"
  if (missing(results)) stop(errMsg)
  res <- results$res
  res[,,1,4] <- res[,,1,4]*42/60
  res[,,2,4] <- res[,,2,4]*540/600
  dimnames(res)[[4]][3:4] <- c("Actual", "Estimated")
  df <- data.frame(array2df(apply(res[,,,3:4],2:4,mean),vars=c("lambda","p","Type", "Avg")))
  df$lambda <- factor2num(df$lambda)
  df$loglambda <- log(df$lambda)
  df$S <- as.numeric(apply(apply(res[,,,1:3], 1:3, sum), 2:3, mean))
  df$FDR <- df$Avg/df$S
  df$FDR[is.na(df$FDR)] <- 0

  p1 <- qplot(loglambda, Avg, data=df, color=Type, geom="line", xlab=expression(log(lambda)), ylab="False inclusions", xlim=c(0.41, -4.2)) +
    geom_line(size=2) + scale_color_manual(values=pal(2, alpha=0.5)) + facet_grid(~p, labeller=label_both) + theme(legend.position="top") + theme(panel.background=element_rect(fill = "gray90"))
  p2 <- qplot(loglambda, FDR, data=df, color=Type, geom="line", xlab=expression(log(lambda)), ylab="FIR", xlim=c(0.41, -4.2)) +
    geom_line(size=2) + scale_color_manual(values=pal(2, alpha=0.5)) + facet_grid(~p, labeller=label_both) + theme(legend.position="none") + theme(panel.background=element_rect(fill = "gray90"))

  grid.arrange(p1, p2, heights=c(1.2, 1))
}
