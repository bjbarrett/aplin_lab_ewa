library(xtable)
library(stringi)
library(stringr)

#all groups no rank
WAICtab <- compare(fit_i , fit_freq, fit_roost , fit_adult_lin , fit_male_lin)
WAICtab
print(xtable(WAICtab, type = "latex"), file = "tables/all_waic_tab_60.tex") # save to tex



####parameter outputs
prec_i <- precis(fit_i , pars=c("log_lambda" , "logit_phi", "sigma_i" , "Rho_i") , depth=3)
prec_freq <- precis(fit_freq , pars=c("log_lambda" , "logit_phi" , "logit_gamma" , "log_f" , "sigma_i" , "Rho_i") , depth=3)
prec_roost <- precis(fit_roost , pars=c("log_lambda" , "logit_phi" , "logit_gamma" , "betaq" , "sigma_i" , "Rho_i") , depth=3)
prec_male_lin <- precis(fit_male_lin , pars=c("log_lambda" , "logit_phi" , "logit_gamma" , "sigma_i" , "Rho_i") , depth=3)
prec_adult_lin <- precis(fit_adult_lin , pars=c("log_lambda" , "logit_phi" , "logit_gamma" , "sigma_i" , "Rho_i") , depth=3)
#prec_male <- precis(fit_male , pars=c("log_lambda" , "logit_phi" , "logit_gamma" , "betaq", "sigma_i" , "Rho_i") , depth=3)
#prec_adult <- precis(fit_adult , pars=c("log_lambda" , "logit_phi" , "logit_gamma" , "betaq", "sigma_i" , "Rho_i") , depth=3)

prec_i$row_names <- prec_i@row.names
prec_i <- prec_i[,c("row_names","mean" , "sd" , "5.5%" , "94.5%")]
prec_freq$row_names <- prec_freq@row.names
prec_freq <- prec_freq[,c("row_names","mean" , "sd" , "5.5%" , "94.5%")]
prec_male_lin$row_names <- prec_male_lin@row.names
prec_male_lin <- prec_male_lin[,c("row_names","mean" , "sd" , "5.5%" , "94.5%")]
prec_adult_lin$row_names <- prec_adult_lin@row.names
prec_adult_lin <- prec_adult_lin[,c("row_names","mean" , "sd" , "5.5%" , "94.5%")]
prec_roost$row_names <- prec_roost@row.names
prec_roost <- prec_roost[,c("row_names","mean" , "sd" , "5.5%" , "94.5%")]
#1 is adult, 2 is juv, 3 is unknown
#1 is female, 2 is male, 3 is unknown
prec_roost$row_names <- str_replace_all(prec_roost$row_names , "4", "666") #replace with placeholder for parameter table
##lets merge
precis_list <- list(prec_i[,1:2],prec_freq[,1:2],prec_male_lin[,1:2],prec_adult_lin[,1:2],prec_roost[,1:2])

param_mean_table <- Reduce(function(x,y) merge(x,y,all=TRUE,by="row_names"), precis_list)

colnames(param_mean_table) <- c("Parameter", "Ind. Learn" ,"Freq. Dep" , "Male-bias" ,"Adult-bias","Roost-Bias")
col_mods <-param_mean_table$Parameter
#lets tidy up names
col_mods[1:45] <- stri_replace_all_fixed(col_mods[1:45] , "[1,", "[a,")
col_mods[1:45] <- stri_replace_all_fixed(col_mods[1:45] , "[2,", "[j,")
col_mods[1:45] <- stri_replace_all_fixed(col_mods[1:45] , "[3,", "[u,")
col_mods[1:45] <- stri_replace_all_fixed(col_mods[1:45] , ",1]", ",f]")
col_mods[1:45] <- stri_replace_all_fixed(col_mods[1:45] , ",2]", ",m]")
col_mods[1:45] <- stri_replace_all_fixed(col_mods[1:45] , ",3]", ",u]")

col_mods[69:73] <- c("sigma(log_lambda)" , "sigma(logit_phi)" , "sigma(logit_gamma)" , "sigma(log_f)" , "sigma(beta)") #note the beta badness here

param_mean_table$Parameter <- col_mods
param_mean_table <- param_mean_table[-which(str_detect(param_mean_table$Parameter , "Rho")),] #drop rho
v1 <- param_mean_table[which(str_detect(param_mean_table$Parameter , "lambda\\[")),]
v2 <- param_mean_table[which(str_detect(param_mean_table$Parameter , "phi\\[")),]
v3 <- param_mean_table[which(str_detect(param_mean_table$Parameter , "gamma\\[")),]
v4 <- param_mean_table[which(str_detect(param_mean_table$Parameter , "log_f\\[")),]
v5 <- param_mean_table[which(str_detect(param_mean_table$Parameter , "betaq\\[")),]
v6 <- param_mean_table[which(str_detect(param_mean_table$Parameter , "sigma")),]
param_mean_table_link <- rbind(v1,v2,v3,v4,v5,v6)
param_mean_table2 <- rbind(v1,v2,v3,v4,v5,v6)

#table on real scale
param_mean_table_link[ which(str_detect(param_mean_table_link$Parameter , "lambda\\[")) , 2:6] <- exp(param_mean_table_link[ which(str_detect(param_mean_table_link$Parameter , "lambda\\[")) , 2:6] )
param_mean_table_link[ which(str_detect(param_mean_table_link$Parameter , "phi\\[")) , 2:6] <- logistic(as.matrix(param_mean_table_link[ which(str_detect(param_mean_table_link$Parameter , "phi\\[")) , 2:6] ))
param_mean_table_link[ which(str_detect(param_mean_table_link$Parameter , "gamma\\[")) , 2:6] <- logistic(as.matrix(param_mean_table_link[ which(str_detect(param_mean_table_link$Parameter , "gamma\\[")) , 2:6] ))
param_mean_table_link[ which(str_detect(param_mean_table_link$Parameter , "log_f\\[")) , 2:6] <- exp(param_mean_table_link[ which(str_detect(param_mean_table_link$Parameter , "log_f\\[")) , 2:6] )

param_mean_table_link$Parameter[1:45] <- str_replace_all(param_mean_table_link$Parameter[1:45] , "log_", "")
param_mean_table_link$Parameter[1:45] <- str_replace_all(param_mean_table_link$Parameter[1:45] , "logit_", "")
param_mean_table_link$Parameter[1:45] <- str_replace_all(param_mean_table_link$Parameter[1:45] , "betaq", "beta")

str(param_mean_table_link)
caption_text <- "Posterior mean estimates for all learning parameters and standard deviation of varying effects for five learning models. In the brackets, the first entry indicates age class (j=juveniles, a=adult, u=unknown). The second entry indicates sex class (f=female, m=male, u=unknown). Values of sigma correspond to the link scale."  
print(xtable(param_mean_table_link, type = "latex"), include.rownames=FALSE, file = "tables/all_groups_param_means_real_scale_60s.tex" , size="scriptsize" ) # save to tex

