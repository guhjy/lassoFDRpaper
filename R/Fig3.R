source("R/helpers.R")
require(ggplot2)
require(gridExtra)
require(abind)

# Setup
load("res/Sim3.RData")
load("res/Sim3.RData")
cmp <- abind(cmp, cmpCT, cmpMS, along=3)
dimnames(cmp)[[3]] <- c("Univariate", "Lasso (mFDR)", "LassoBIC", "LassoCV", "Selective inference", "Sample splitting")
dimnames(cmp)[[4]] <- c("Causative (A)", "Correlated (B)", "Noise (N)")
df <- array2df(apply(cmp, 2:4, mean, na.rm=TRUE),var=c("p","Method", "Group", "Avg"))

# Subsets / reordering factors
ggdf1 <- subset(df, p==60 & Method != "LassoBIC")
ggdf2 <- subset(df, p==600 & !(Method %in% c("LassoBIC", "LassoCV")))
revlevel <- function(x) factor(x, levels=rev(levels(x)))
ggdf1$Method <- revlevel(relevel(droplevels(ggdf1$Method), 'Lasso (mFDR)'))
ggdf2$Method <- revlevel(relevel(droplevels(ggdf2$Method), 'Lasso (mFDR)'))

# Plot
p1 <- ggplot(ggdf1, aes(Method, Avg, fill=Group)) + geom_bar(stat='identity', position=position_stack(reverse=TRUE)) +
  scale_fill_grey(start=0,end=1) + coord_flip() + theme(panel.background=element_rect(fill = "gray90")) +
  facet_grid(p~., labeller=label_both) + theme(legend.position="top", legend.background=element_rect(fill = "gray90"))
p2 <- ggplot(ggdf2, aes(Method, Avg, fill=Group)) + geom_bar(stat='identity', position=position_stack(reverse=TRUE)) +
  scale_fill_grey(start=0,end=1) + coord_flip() + theme(panel.background=element_rect(fill = "gray90")) +
  facet_grid(p~., labeller=label_both) + theme(legend.position="none")

pdf("fig/Fig3.pdf", 7, 5)
grid.arrange(p1, p2, heights=c(1.5, 1))
dev.off()

# Summaries
T1 <- apply(cmp[,1,,],2:3,mean)
T1[,3]/apply(T1,1,sum)
(T1[,2]+T1[,3])/apply(T1,1,sum)
T2 <- apply(cmp[,2,,],2:3,mean)
T2[,3]/apply(T2,1,sum)
(T2[,2]+T2[,3])/apply(T2,1,sum)
