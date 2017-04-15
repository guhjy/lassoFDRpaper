Fig6 <- function(res) {
  col <- c(pal(2, alpha=0.5), pal(4, alpha=0.5)[c(2,4)])
  ind <- is.na(res[,,4])
  res[,,4][ind] <- res[,,1][ind]
  df <- array2df(apply(res[,,c(2:5)], 2:3, sum), vars=c("lambda", "Type", "Avg"))
  S <- apply(res[,,1], 2, sum)
  df$FIR <- df$Avg/S
  df$FIR[is.na(df$FIR)] <- 0
  df$Avg <- df$Avg/dim(res)[1]
  df$lam <- factor2num(df$lambda)
  df$ll <- log(df$lam)

  xlim <- rev(range(df$ll))
  qplot(ll, FIR, data=df, color=Type, geom="line", xlab=expression(log(lambda)), ylab="False inclusion rate", xlim=xlim) + geom_line(size=2) + scale_color_manual(values=col)

  #subset(df, ll == ll[14])
  #subset(df, ll == ll[22])

}
