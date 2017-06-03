all: Fig2 Fig3 Fig4 Fig5 Fig6 Fig7

R_OPTS=--vanilla

Fig2: fig/Fig2.pdf
Fig3: fig/Fig3.pdf
Fig4: fig/Fig4.pdf
Fig5: fig/Fig5.pdf
Fig6: fig/Fig6.pdf
Fig7: fig/Fig7.pdf

res/Sim2.RData: R/Sim2.R
	R CMD BATCH $(R_OPTS) R/Sim2.R R/.Sim2.Rout

res/Sim3.RData: R/Sim3.R
	R CMD BATCH $(R_OPTS) R/Sim3.R R/.Sim3.Rout

res/Sim5.RData: R/Sim5.R
	R CMD BATCH $(R_OPTS) R/Sim5.R R/.Sim5.Rout

res/Sim6.RData: R/Sim6.R
	R CMD BATCH $(R_OPTS) R/Sim6.R R/.Sim6.Rout

fig/Fig2.pdf: R/Fig2.R res/Sim2.RData
	R CMD BATCH $(R_OPTS) R/Fig2.R R/.Fig2.Rout

fig/Fig3.pdf: R/Fig3.R res/Sim2.RData res/Sim3.RData
	R CMD BATCH $(R_OPTS) R/Fig3.R R/.Fig3.Rout

fig/Fig4.pdf: R/Fig4.R
	R CMD BATCH $(R_OPTS) R/Fig4.R R/.Fig4.Rout

fig/Fig5.pdf: R/Fig5.R res/Sim5.RData
	R CMD BATCH $(R_OPTS) R/Fig5.R R/.Fig5.Rout

fig/Fig6.pdf: R/Fig6.R res/Sim6.RData
	R CMD BATCH $(R_OPTS) R/Fig6.R R/.Fig6.Rout

fig/Fig7.pdf: R/Fig7.R
	R CMD BATCH $(R_OPTS) R/Fig7.R R/.Fig7.Rout

clean:
	rm -f fig/*
	rm -f res/*
	rm -f R/*.Rout
