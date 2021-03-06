source("R/helpers.R")
require(ncvreg)
n <- 100
p1 <- 6
N <- 100
p <- c(60,600)
K <- c(3,10)
##p <- 500
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
    X <- .Call("standardize", Data$X)[[1]]
    y <- as.numeric(scale(Data$y, scale=FALSE))
    z <- crossprod(X,y)/n
    ##plot(runif(50,-0.1,0.1),z,pch=19, xlim=c(-1,1),col=c(rep("red",6),rep("black",44)),ylim=c(-max(abs(z)),max(abs(z))))
    fit <- ncvreg(X, y, penalty="lasso", lambda=lam)
    ##fit <- glmnet(X, y, lambda=lam, family=family[k])
    
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
    yhat <- predict(fit, .Call("standardize", CVData$X)[[1]])
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
  save(res, cmp, file="res/Sim2.RData")
}
