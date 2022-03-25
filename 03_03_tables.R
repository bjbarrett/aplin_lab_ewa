library(xtable)
#all groups no rank
WAICtab <- compare(fit_i , fit_freq, fit_male , fit_adult , fit_roost)
print(xtable(WAICtab, type = "latex"), file = "tables/all_waic_tab.tex") # save to tex
