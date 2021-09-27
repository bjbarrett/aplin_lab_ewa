library(lubridate)

# par(mfrow = c(4, 2))
# par(cex = 0.7)
# par(mar = c(4, 4, 2, .1), oma = c(.1, .1 , .1, .1))
#model <- fit_male
#plot(precis(model , pars=c('lambda' , 'phi' , 'gamma' , 'betaq')) , main=paste(model))

unique(d$session)
unique(d$date)
ds <- d[d$session==1 & d$date==20190815,]
plot(subject_index ~ rel_time , data=ds , col=colz[ds$tech_index] , pch=pchez[ds$open + 1] , cex=0.7)
plot(subject_index ~ rel_time , data=d , col=colz[ds$tech_index] , pch=pchez[d$open + 1] , cex=0.5)
mean(d$choose_blue[d$session==1 & d$date==20190815 & d$rel_time<60,])
mean(d$choose_blue[d$session==1 & d$date==20190815 & d$rel_time < 60*10])
mean(d$choose_blue[d$session==1 & d$date==20190815])
mean(d$choose_blue[d$session==1 & d$date==20190815])

dh <- aggregate(cbind( d$choose_blue , d$choose_red ) , list(date=d$date , session=d$session  ) , mean ) 
dh$date_obj <- ymd(dh$date)
dh <- dh[with(dh, order(date, session)), ]

#plot raw data
plot(1:nrow(dh),dh$V1 , ylim=c(.6,.9) , col="white" , pch=19 , xaxt='n' , ylab="Prob. Choose Red" , xlab='')
lines(1:nrow(dh),dh$V1 , ylim=c(0,1) , col="blue" , pch=19 , type="b")
lines(1:nrow(dh),dh$V2 , ylim=c(0,1) , col="red" , pch=19 , type="b")
axis(1,at=seq(1:nrow(dh)) , labels=paste0("S",dh$session,"; ",dh$date) , hadj=1 , cex.axis=.4 , las=2)
#add in posterior mean preds for each model across sessions
n_list <-  c( "mean_choose_red_ind" , "mean_choose_red_freq" , "mean_choose_red_male" , "mean_choose_red_adult" , "mean_choose_red_roost" , "mean_choose_red_rank")
#mean
di <- aggregate(cbind( d$mean_choose_red_ind , d$mean_choose_red_freq , d$mean_choose_red_male , d$mean_choose_red_adult , d$mean_choose_red_roost , d$mean_choose_red_rank ) , list(date=d$date , session=d$session  ) , mean ) 
di <- di[with(di, order(date, session)), ]

for(i in 1:length(n_list)){
  lines(1:nrow(di),di[,2+i] , ylim=c(0,1) , col=col_strat[i] , pch=1 , type="l")
}
#median
di <- aggregate(cbind( d$mean_choose_red_ind , d$mean_choose_red_freq , d$mean_choose_red_male , d$mean_choose_red_adult , d$mean_choose_red_roost , d$mean_choose_red_rank ) , list(date=d$date , session=d$session  ) , median ) 
di <- di[with(di, order(date, session)), ]

for(i in 1:length(n_list)){
  lines(1:nrow(di),di[,2+i] , ylim=c(0,1) , col=col_strat[i] , pch=1 , type="l" , lty=2)
}

legend("bottomleft" , fill=col_strat , legend=c("IL" , "FDSL" , "MaleBSL" , "AdultBSL" , "RoostBSL" , "RankBSL") , bty='n' , cex=0.5 )

