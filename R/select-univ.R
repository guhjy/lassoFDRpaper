select.univ <- function(X, y, family, q=.1, test="r") {
  n <- nrow(X)
  p <- ncol(X)
  pval <- qval <- numeric(p)
  if (test=="t") for (i in 1:p) pval[i] <- t.test(X[y==1,i],X[y==0,i])$p.value
  if (test=="r") for (i in 1:p) pval[i] <- summary(glm(y~X[,i],family=family))$coef[2,4]
  if (test=="ca") for (i in 1:p) pval[i] <- prop.trend.test(c(sum(X[y==1,i]==0),sum(X[y==1,i]==1),sum(X[y==1,i]==2)),c(sum(X[,i]==0),sum(X[,i]==1),sum(X[,i]==2)))$p.value
  for (i in 1:p) qval[i] <- pval[i]*p/sum(pval<=pval[i])
  return(list(p=pval,q=qval,s=qval<=q))
}
