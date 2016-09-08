Sim <- function() {
  load("~/res/lassoFDR/uncorr/sim1.RData")
  load("~/res/lassoFDR/uncorr/covTest.RData")
  load("~/res/lassoFDR/uncorr/hdi.RData")
  cmp <- abind(cmp, cmpCT, cmpMS, along=3)
  cmp
}
Fig <- function() {
  # Setup
  dimnames(cmp)[[3]][1:2] <- c("Univariate", "LassoFIR")
  dimnames(cmp)[[4]] <- c("Causative (A)", "Correlated (B)", "Noise (C)")
  df <- array2df(apply(cmp, 2:4, mean, na.rm=TRUE),var=c("p","Method", "Group", "Avg"))

  # Subsets / reordering factors
  ggdf1 <- subset(df, p==60 & Method != "LassoBIC")
  ggdf2 <- subset(df, p==600 & !(Method %in% c("LassoBIC", "LassoCV")))
  revlevel <- function(x) factor(x, levels=rev(levels(x)))
  ggdf1$Method <- revlevel(relevel(droplevels(ggdf1$Method), 'LassoFIR'))
  ggdf2$Method <- revlevel(relevel(droplevels(ggdf2$Method), 'LassoFIR'))

  # Plot
  p1 <- ggplot(data=ggdf1) + aes(Method, Avg, fill=Group) + geom_bar(stat='identity') +
    scale_fill_grey(start=0,end=1) + coord_flip() + theme(panel.background=element_rect(fill = "gray90")) +
    facet_grid(p~., labeller=label_both) + theme(legend.position="top", legend.background=element_rect(fill = "gray90"))
  p2 <- ggplot(data=ggdf2) + aes(Method, Avg, fill=Group) + geom_bar(stat='identity') +
    scale_fill_grey(start=0,end=1) + coord_flip() + theme(panel.background=element_rect(fill = "gray90")) +
    facet_grid(p~., labeller=label_both) + theme(legend.position="none")

  grid.arrange(p1, p2, heights=c(1.5, 1))
}
