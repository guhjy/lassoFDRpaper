source("R/helpers.R")
require(ggplot2)
require(gridExtra)
require(abind)

load("res/Sim2.RData")
res[,,1,4] <- res[,,1,4]*42/60
res[,,2,4] <- res[,,2,4]*540/600
dimnames(res)[[4]][3:4] <- c("Actual", "Estimated")
df <- data.frame(array2df(apply(res[,,,3:4],2:4,mean),var=c("lambda","p","Type", "Avg")))
df$lambda <- factor2num(df$lambda)
df$loglambda <- log(df$lambda)
df$S <- as.numeric(apply(apply(res[,,,1:3], 1:3, sum), 2:3, mean))
df$FDR <- df$Avg/df$S

# xlim <- rev(range(df$lambda))
# p1 <- qplot(lambda, Avg, data=df, color=Type, geom="line", xlab=expression(lambda), ylab="False inclusions", xlim=xlim) +
#   geom_line(size=2) + scale_color_manual(values=pal(2, alpha=0.5)) + facet_grid(~p, labeller=label_both) + theme(legend.position="top") + theme(panel.background=element_rect(fill = "gray90"))
# p2 <- qplot(lambda, FDR, data=df, color=Type, geom="line", xlab=expression(lambda), ylab="FIR", xlim=xlim) +
#   geom_line(size=2) + scale_color_manual(values=pal(2, alpha=0.5)) + facet_grid(~p, labeller=label_both) + theme(legend.position="none") + theme(panel.background=element_rect(fill = "gray90"))
p1 <- qplot(loglambda, Avg, data=df, color=Type, geom="line", xlab=expression(log(lambda)), ylab="Noise features selected", xlim=c(0.4, -4.2)) +
  geom_line(size=2) + scale_color_manual(values=pal(2, alpha=0.5)) + facet_grid(~p, labeller=label_both) + theme(legend.position="top") + theme(panel.background=element_rect(fill = "gray90"))
p2 <- qplot(loglambda, FDR, data=df, color=Type, geom="line", xlab=expression(log(lambda)), ylab="mFDR", xlim=c(0.4, -4.2)) +
  geom_line(size=2) + scale_color_manual(values=pal(2, alpha=0.5)) + facet_grid(~p, labeller=label_both) + theme(legend.position="none") + theme(panel.background=element_rect(fill = "gray90"))

pdf("fig/Fig2.pdf", height=7, width=7)
grid.arrange(p1, p2, heights=c(1.2, 1))
dev.off()

subset(df, loglambda == loglambda[20])
