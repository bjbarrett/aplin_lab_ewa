library(xtable)
#all groups no rank
WAICtab <- compare(fit_i , fit_freq, fit_male , fit_adult , fit_roost , fit_adult_lin , fit_male_lin)
WAICtab
print(xtable(WAICtab, type = "latex"), file = "tables/all_waic_tab_60.tex") # save to tex
