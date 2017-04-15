source("R/helpers.R")
require(ggplot2)
require(gridExtra)

## Read in data
load("res/Sim5.RData")
df <- array2df(apply(res[,,-1], 2:3, sum), vars=c("lambda", "Type", "Avg"))
S <- apply(res[,,1], 2, sum)
df$FIR <- df$Avg/S
df$Avg <- df$Avg/dim(res)[1]
df$lam <- factor2num(df$lambda)
df$ll <- log(df$lam)

# xlim <- rev(range(lam))
# p1 <- qplot(lam, Avg, data=df, color=Type, geom="line", xlab=expression(log(lambda)), ylab="False inclusions", xlim=xlim) +
#   geom_line(size=2) + scale_color_manual(values=pal(2, alpha=0.5))
# p2 <- qplot(lam, FIR, data=df, color=Type, geom="line", xlab=expression(log(lambda)), ylab="False inclusions", xlim=xlim) +
#   geom_line(size=2) + scale_color_manual(values=pal(2, alpha=0.5))
xlim <- rev(range(df$ll))
p1 <- qplot(ll, Avg, data=df, color=Type, geom="line", xlab=expression(log(lambda)), ylab="Noise features selected", xlim=xlim) +
  geom_line(size=2) + scale_color_manual(values=pal(2, alpha=0.5)) + theme(legend.position="top")
p2 <- qplot(ll, FIR, data=df, color=Type, geom="line", xlab=expression(log(lambda)), ylab="mFDR", xlim=xlim) +
  geom_line(size=2) + scale_color_manual(values=pal(2, alpha=0.5)) + theme(legend.position="top")

pdf("fig/Fig5.pdf", 7, 3.5)
grid.arrange(p1, p2, widths=c(1,1))
dev.off()

subset(df, ll == ll[14])
