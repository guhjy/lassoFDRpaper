Fig6 <- function(penalty="lasso") {
  loadData("bc-tcga")
  fit <- ncvreg(X, y, penalty=penalty)
  f <- fir(fit)
  ll <- log(fit$lambda)

  op <- par(mar=c(5,5,5,0.5), mfrow=c(1,2))
  plot(f, lwd=3, log.l=TRUE, bty="n")
  plot(f, type="EF", lwd=3, log.l=TRUE, bty="n", legend=FALSE)
  toplegend(legend=c("Selected", "E(FI)"), col=pal(2), lwd=3)
  par(op)

  #A <- cbind(f$FIR, n.s)
  #A[A[,1] < .1,]
  #A[A[,1] < .05,]
}
