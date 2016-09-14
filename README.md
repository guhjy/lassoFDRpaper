# Estimating false inclusion rates in penalized regression models
This repository contains the code to reproduce the results in [Estimating false inclusion rates in penalized regression models](http://arxiv.org/pdf/1607.05636v1.pdf), by Patrick Breheny.
The `fir` function, which actually calculates the false inclusion rates, is implemented in [`ncvreg`](https://github.com/pbreheny/ncvreg).

You can install the package via `devtools` using:

```r
library(devtools)
install_github("pbreheny/lassoFDRpaper")
```

Once installed, you can reproduce figures from the paper with `Fig6()` to reproduce Figure 6, and so on.  Many of the figures require a corresponding simulation to be done first, for example:

```r
results <- Sim2()
Fig2(results)
```

See the help files (`?Fig3`, and so on) for more details.
