array2df <- function(X,vars=paste("V",1:ncol(df),sep=""))
  {
    df <- cbind(do.call("expand.grid",dimnames(X)),as.numeric(X))
    names(df) <- vars
    return(df)
  }

factor2num <- function(f)
  {
    return(as.numeric(levels(f))[f])
  }

#' Generates a random design matrix and outcome.
#'
#' This function generated a random design matrix \code{X} and
#' response vector \code{y} for use in regression testing/simulation.
#'
#' @param n Sample size
#' @param J Number of groups
#' @param K Number of explanatory variables per group
#' @param beta Vector of regression coefficients in the generating model, or, if a scalar, the value of each nonzero regression coefficient.
#' @param family Generate \code{y} according to linear (\code{"gaussian"})
#' or logistic (\code{"binomial"}) model
#' @param J0 Number of nonzero groups
#' @param K0 Number of nonzero coefficients per group
#' @param SNR Signal to noise ratio
#' @param sig Should the groups be heterogeneous (in beta) or homogeneous
#' @param sig.g Should the coefficients within a group be heterogeneous or homogeneous
#' @param rho Correlation between groups
#' @param rho.g Correlation between parameters within a group
#' 
#' @export
genData <- function(n, J, K=1, beta, family=c("gaussian","binomial"), J0=ceiling(J/2), K0=K, SNR=1, sig = c("homogeneous","heterogeneous"), sig.g = c("homogeneous","heterogeneous"), rho = 0, rho.g = rho, corr=c("exchangeable", "autoregressive")) {
  family <- match.arg(family)
  sig <- match.arg(sig)
  sig.g <- match.arg(sig.g)
  corr <- match.arg(corr)
  
  ## Gen X, S
  if (corr=="exchangeable") {
    X <- genX(n=n, J=J, K=K, rho=rho, rho.g=rho.g)
  } else {
    require(Matrix)
    RHO <- matrix(rho^(0:(J-1)), J, J, byrow=TRUE)
    S <- bandSparse(J, k=0:(J-1), diagonals=RHO, symmetric=TRUE)
    R <- chol(S)
    X <- as.matrix(matrix(rnorm(n*J), n, J) %*% R)
  }
  
  j <- rep(1:J,rep(K,J))
  
  ## Gen beta
  if (missing(beta) || length(beta)==1) {
    k <- rep(1:K,J)
    b <- (j <= J0) * (k <= K0)
    s <- c(1,-1)[1+j%%2] * c(1,-1)[1+k%%2]
    if (missing(beta)) {
      S <- matrix(rho, nrow=J*K, ncol=J*K)
      for (i in 1:J) S[(i-1)*K+1:K,(i-1)*K+1:K] <- rho.g
      diag(S) <- rep(1,J*K)
      if (sig=="heterogeneous") b <- b*j
      if (sig.g=="heterogeneous") b <- b*k
      b <- b*s
      beta <- b*sqrt(SNR)/sqrt(crossprod(b,S)%*%b)
    } else beta <- b*s*beta
  }
  
  ## Gen y
  y <- genY(X%*%beta, family=family, sigma=1)
  return(list(X=X,y=y,beta=beta,family=family,group=j))
}

## rho  : correlation across all explanatory variables
## rho.g: correlation within group (must be at least rho)
genX <- function(n, J, K=1, rho=0, rho.g=rho, corr=corr) {
  a <- sqrt(rho/(1-rho.g))
  b <- sqrt((rho.g-rho)/(1-rho.g))
  Z <- rnorm(n)
  ZZ <- t(matrix(rep(rnorm(n*J), rep(K,n*J)), ncol=n))
  ZZZ <- matrix(rnorm(n*J*K),nrow=n)
  return(matrix(as.numeric(a*Z + b*ZZ + ZZZ),nrow=n)/sqrt(1+a^2+b^2))
}

genY <- function(eta,family=c("gaussian","binomial"),sigma=1) {
  family=match.arg(family)
  n <- length(eta)
  if (family=="gaussian") y <- rnorm(n,mean=eta,sd=sigma)
  else if (family=="binomial")
  {
    pi. <- exp(eta)/(1+exp(eta))
    pi.[eta > log(.9999/.0001)] <- 1
    pi.[eta < log(.0001/.9999)] <- 0
    y <- rbinom(n,1,pi.)
  }
  return(y)
}

loadData <- function(name, envir=parent.frame()) {
  address <- paste0("http://myweb.uiowa.edu/pbreheny/data/", name, ".RData")
  load(url(address), envir=envir)
}

pal <- function(n, alpha=1) {
  if (n==2) {
    val <- hcl(seq(15,375,len=4), l=60, c=150, alpha=alpha)[c(1,3)]
  } else {
    val <- hcl(seq(15,375,len=n+1), l=60, c=150, alpha=alpha)[1:n]
  }
  val
}

toplegend <- function(horiz=TRUE, ...) {
  if (par("oma")[3]==0) {
    x <- mean(par("usr")[1:2])
    yy <- transform.coord(par("usr")[3:4], par("plt")[3:4])
    y  <- mean(c(yy[2],par("usr")[4]))
    legend(x, y, xpd=NA, bty="n", xjust=0.5, yjust=0.5, horiz=horiz, ...)    
  } else {
    g <- par("mfrow")
    xx <- transform.coord(par("usr")[1:2], par("plt")[1:2])
    yy <- transform.coord(par("usr")[3:4], par("plt")[3:4])
    xxx <- transform.coord(xx, c(g[2]-1,g[2])/g[2])
    yyy <- transform.coord(yy, c(g[1]-1,g[1])/g[1])
    yyyy <- transform.coord(yyy, par("omd")[3:4])
    legend(mean(xxx), mean(c(yyy[2],yyyy[2])), xpd=NA, bty="n", xjust=0.5, yjust=0.5, horiz=horiz, ...)
  }
}
rightlegend <- function(...) {
  if (par("oma")[3]==0) {
    y <- mean(par("usr")[3:4])
    xx <- transform.coord(par("usr")[1:2], par("plt")[1:2])
    x <- mean(c(xx[2],par("usr")[2]))
    legend(x, y, xpd=NA, bty="n", xjust=0.5, yjust=0.5, ...)    
  } else {
    g <- par("mfrow")
    xx <- transform.coord(par("usr")[1:2], par("plt")[1:2])
    yy <- transform.coord(par("usr")[3:4], par("plt")[3:4])
    xxx <- transform.coord(xx, c(g[2]-1,g[2])/g[2])
    yyy <- transform.coord(yy, c(g[1]-1,g[1])/g[1])
    yyyy <- transform.coord(yyy, par("omd")[3:4])
    legend(mean(xxx), mean(c(yyy[2],yyyy[2])), xpd=NA, bty="n", xjust=0.5, yjust=0.5, ...)
  }  
}
transform.coord <- function(x,p) {
  ba <- (x[2]-x[1])/(p[2]-p[1])
  a <- x[1]-p[1]*ba
  b <- a + ba
  c(a,b)
}

select.univ <- function(X,y,family,q=.1,test="r")
  {
    n <- nrow(X)
    p <- ncol(X)
    pval <- qval <- numeric(p)
    if (test=="t") for (i in 1:p) pval[i] <- t.test(X[y==1,i],X[y==0,i])$p.value
    if (test=="r") for (i in 1:p) pval[i] <- summary(glm(y~X[,i],family=family))$coef[2,4]
    if (test=="ca") for (i in 1:p) pval[i] <- prop.trend.test(c(sum(X[y==1,i]==0),sum(X[y==1,i]==1),sum(X[y==1,i]==2)),c(sum(X[,i]==0),sum(X[,i]==1),sum(X[,i]==2)))$p.value
    for (i in 1:p) qval[i] <- pval[i]*p/sum(pval<=pval[i])
    return(list(p=pval,q=qval,s=qval<=q))
  }

