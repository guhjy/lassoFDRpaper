source("R/helpers.R")
loadData("bcTCGA")
require(ncvreg)

# Lasso -------------------------------------------------------------------

fit <- ncvreg(X, y, penalty="lasso")
f <- mfdr(fit)
ll <- log(fit$lambda)

pdf("fig/Fig7.pdf", 7, 3.5)
par(mar=c(5,5,5,0.5), mfrow=c(1,2))
plot(f, lwd=3, log.l=TRUE, bty="n")
plot(f, type="EF", lwd=3, log.l=TRUE, bty="n")
dev.off()

subset(f, mFDR < .1)
subset(f, mFDR < .05)

cvfit <- cv.ncvreg(X, y, penalty="lasso", seed=3)
f[cvfit$min,]

# MCP ---------------------------------------------------------------------

cvmcp <- cv.ncvreg(X, y, seed=3)
mcp <- cvmcp$fit
mfdr(mcp)[cvmcp$min,]


# Sample splitting --------------------------------------------------------

# Uncomment to run; takes about 10 minutes
# require(hdi)
# resMS <- hdi(X, y, method='multi.split', args.model.selector=list(lambda.min=0.1, nfolds=5))


# forwardStop -------------------------------------------------------------

# Uncomment to run; takes a very long time and may crash depending on your RAM
# require(selectiveInference)
# fit <- lar(X, y, maxsteps=100)
# inf <- larInf(fit, estimateSigma(X, y)$sigma)
# stp <- forwardStop(inf$pv)
