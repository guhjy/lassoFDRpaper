# Marginal false discovery rates for penalized regression models

This repository contains the code to reproduce the results in [Marginal false discovery rates for penalized regression models](https://arxiv.org/pdf/1607.05636), by Patrick Breheny.
The `mfdr` function, which actually calculates the marginal false discovery rates, is implemented in [`ncvreg`](https://github.com/pbreheny/ncvreg).

If you are familiar with the `make` utility, using this repository is straightforward:

```
make Fig4
```

makes Figure 4, and so on (the file `Fig4.pdf` will appear in the `fig/` directory).

If you are not familiar with `make`, Karl Broman has a helpful primer [here](http://kbroman.org/minimal_make), or you can run the R code manually: `Fig2.R` has the code for creating Figure 2, and so on.  Note that many of the figures are based on simulations, so you'll have to generate the output with the code in `Sim2.R` before you can plot it.

If you have any questions, please [contact me](mailto:patrick-breheny@uiowa.edu).

Note: Due to restrictions on the sharing of human genetic data, I am unable to provide the data necessary to reproduce the analyses in Section 6.3.  All other analyses in the manuscript are covered by this repository.
