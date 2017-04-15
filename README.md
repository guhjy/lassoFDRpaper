# Marginal false discovery rates for penalized regression models

This repository contains the code to reproduce the results in [Marginal false discovery rates for penalized regression models](https://arxiv.org/pdf/1607.05636), by Patrick Breheny.
The `mfdr` function, which actually calculates the marginal false discovery rates, is implemented in [`ncvreg`](https://github.com/pbreheny/ncvreg).

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

Note about N

## Contents

### Functions

* Sim2(), Fig2(): Reproduces figure 2
* Sim3(), Fig3(): Reproduces figure 3
* Fig4(): Reproduces figure 4
* Sim5(), Fig5(): Reproduces figure 5
* Sim6(), Fig6(): Reproduces figure 6

### Vignettes

* CS6.1
