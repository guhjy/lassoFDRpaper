# Estimating false inclusion rates in penalized regression models
This repository contains the code to reproduce the results in [Estimating false inclusion rates in penalized regression models](http://arxiv.org/pdf/1607.05636v1.pdf), by Patrick Breheny.
The `mfdr` function, which actually calculates the false inclusion rates, is implemented in [`ncvreg`](https://github.com/pbreheny/ncvreg).

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

Note: Due to restrictions on the sharing of human genetic data, I am unable to provide the data necessary to reproduce the analyses in Section 6.3.  All other analyses in the manuscript are covered by this repository.
