library(janitor)
d_BA <- read.csv("cockatoo data/EWA_raw_data_BA.csv")
d_BA <- clean_names(d_BA)
unique(d_BA$behav1)
d_BA$choose_red <- ifelse(d_BA$behav1=="R" , 1, 0)
d_BA$choose_blue <- ifelse(d_BA$behav1=="B" , 1, 0)
d_BA$open <- ifelse(d_BA$behav2=="op" , 1, 0)

#plot_raw_data

####run this reight before model in case subsetting occurs
d_BA$subject_index <- as.integer(as.factor(d_BA$subject) )
pch_pal <- c(1,19)
col_pal <- c("red" , "blue")
plot(d_BA$subject_index ~ d_BA$rel_time , col=col_pal[d_BA$choose_blue +1] , pch=pch_pal[d_BA$open + 1] , cex=0.5)
#thing to think about IL
#1_ start with open as payoff
#consider 1/0 for option chose as well in the future
#rank-bias but new individuals show up
#same roost vs. not
# male-bias?
#frequency dependence
#kinship variables

#scans every 10 minutes, social window if they were in most recent scan
#or first time they showed up at feeder since last scan
ps_BA <- read.csv("cockatoo data/presence_solve_BA.csv")

