Fig <- function() {
  loadData("bc-tcga")

  # Lasso -------------------------------------------------------------------

  fit <- ncvreg(X, y, penalty="lasso")
  f <- fir(fit)
  ll <- log(fit$lambda)

  par(mar=c(5,5,5,0.5), mfrow=c(1,2))
  plot(f, lwd=3, log.l=TRUE, bty="n")
  n.s <- predict(fit, type = "nvars")
  ind <- round(seq(1, 100, len=6))
  axis(3, at = ll[ind], labels = n.s[ind], tick = TRUE)
  mtext("Variables selected", cex = 0.8, line = 2)
  plot(f, type="EF", lwd=3, log.l=TRUE, bty="n", legend=FALSE)
  toplegend(legend=c("Selected", "E(FI)"), col=pal(2), lwd=3)

  #A <- cbind(f$FIR, n.s)
  #A[A[,1] < .1,]
  #A[A[,1] < .05,]

  #cvfit <- cv.ncvreg(X, y, penalty="lasso")
  #fir(fit)$FIR[cvfit$min]

  # MCP -------------------------------------------------------------------

#   cvfit <- cv.ncvreg(X, y, penalty="lasso", seed=1)
#   fit <- cvfit$fit
#   cvmcp <- cv.ncvreg(X, y, seed=1)
#   mcp <- cvmcp$fit
#   f1 <- fir(mcp)
#   f2 <- fir(fit)
#   f1$FIR[cvmcp$min]
#   f2$FIR[cvfit$min]
#
#   A <- cbind(f2$FIR, (cvfit$null.dev - cvfit$cve)/cvfit$null.dev, predict(fit, type = "nvars"))
#   A[A[,1] < .05,]

}
