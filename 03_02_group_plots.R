library(lubridate)

# par(mfrow = c(4, 2))
# par(cex = 0.7)
# par(mar = c(4, 4, 2, .1), oma = c(.1, .1 , .1, .1))
#model <- fit_male
#plot(precis(model , pars=c('lambda' , 'phi' , 'gamma' , 'betaq')) , main=paste(model))

unique(d$session)
unique(d$date)
ds <- d[d$session==1 & d$date==20190815,]
d$group_index <- as.integer(as.factor(d$group))
plot(subject_index ~ rel_time , data=ds , col=colz[ds$tech_index] , pch=pchez[ds$open + 1] , cex=0.7)
for(g in 1:3){
  dg <- d[d$group_index==g,]
  dg$subject_index_group <- as.integer(as.factor(dg$subject))
  plot(subject_index_group ~ rel_time , data=dg , col=colz[dg$tech_index] , pch=pchez[dg$open + 1] , cex=0.5 , main=unique(dg$group))
}
  
for(g in 1:3){
  dg <- d[d$group_index==g,]
  dg$subject_index_group <- as.integer(as.factor(dg$subject))

  dh <- aggregate(cbind( dg$choose_blue , dg$choose_red ) , list(date=dg$date , session=dg$session  ) , mean ) 
  dh$date_obj <- ymd(dh$date)
  dh <- dh[with(dh, order(date, session)), ]
  
  #plot raw data
  plot(1:nrow(dh),dh$V1 , ylim=c(0,1) , col="white" , pch=19 , xaxt='n' , ylab="Prob. Choose Red" , xlab='' , main=min(dg$group))
  axis(1,at=seq(1:nrow(dh)) , labels=paste0("S",dh$session,"; ",dh$date) , hadj=1 , cex.axis=.4 , las=2)
  #add in posterior mean preds for each model across sessions
  #n_list <-  c( "mean_choose_red_ind" , "mean_choose_red_freq" , "mean_choose_red_male" , "mean_choose_red_adult" , "mean_choose_red_roost" , "mean_choose_red_rank")
  #median
  #di <- aggregate(cbind( d$mean_choose_red_ind , d$mean_choose_red_freq , d$mean_choose_red_male , d$mean_choose_red_adult , d$mean_choose_red_roost , d$mean_choose_red_rank ) , list(date=d$date , session=d$session  ) , mean ) 
  di <- aggregate(cbind(  dg$median_choose_red_freq ) , list(date=dg$date , session=dg$session  ) , median ) 
  di <- di[with(di, order(date, session)), ]
  di2 <- aggregate(cbind(  dg$median_choose_red_freq ) , list(date=dg$date , session=dg$session  ) , HPDI , prob=0.80 ) 
  di2 <- di2[with(di2, order(date, session)), ]
  # for(i in 1:length(n_list)){
  #   lines(1:nrow(di),di[,2+i] , ylim=c(0,1) , col=col_strat[i] , pch=1 , type="l")
  # }

  #lets get per individual predictions
  idpred <- aggregate(cbind(  dg$median_choose_red_freq ) , list(date=dg$date , session=dg$session , subject_index_group=dg$subject_index_group  ) , median ) 
  idpred <- idpred[with(idpred, order(date, session,subject_index_group)), ]
  idpred$date_sess_index <- cumsum(!duplicated(idpred[1:2]))
  #idpred <- idpred[with(idpred, order(subject_index_group,date, session)), ]
  #canelo <- palette(rainbow(max(idpred$subject_index_group)))     # six color rainbow
  
  for (id in 1:max(idpred$subject_index_group) ){
    idpred2 <- idpred[idpred$subject_index_group==id,]
    # lines(idpred2$date_sess_index,idpred2[,4] , ylim=c(0,1) , col=col.alpha(canelo[id], alpha=0.9) , pch=19 , type="b" , lty=1 , lw=1 , cex=0.5)
    lines(idpred2$date_sess_index,idpred2[,4] , ylim=c(0,1) , col=col.alpha("darkred", alpha=0.3) , pch=19 , type="b" , lty=1 , lw=1 , cex=0.5)
    }
  #plot the freq model
  lines(1:nrow(dh),dh$V2 , ylim=c(0,1) , col="red" , pch=19 , type="b" , lw=3)
  i <- 1
  lines(1:nrow(di),di[,2+i] , ylim=c(0,1) , col="black" , pch=1 , type="l" , lty=2 , lw=3)
  lines(1:nrow(di2),di2$V1[,1] , ylim=c(0,1) , col="black" , pch=1 , type="l" , lty=3 , lw=2)
  lines(1:nrow(di2),di2$V1[,2] , ylim=c(0,1) , col="black" , pch=1 , type="l" , lty=3 , lw=2)
  
}


dh <- aggregate(cbind( d$choose_blue , d$choose_red ) , list(date=d$date , session=d$session  ) , mean ) 
dh$date_obj <- ymd(dh$date)
dh <- dh[with(dh, order(date, session)), ]

#plot raw data
plot(1:nrow(dh),dh$V1 , ylim=c(0,1) , col="white" , pch=19 , xaxt='n' , ylab="Prob. Choose Red" , xlab='')
lines(1:nrow(dh),dh$V1 , ylim=c(0,1) , col="blue" , pch=19 , type="b")
lines(1:nrow(dh),dh$V2 , ylim=c(0,1) , col="red" , pch=19 , type="b")
#lines(1:nrow(dh2),dh$V2 , ylim=c(0,1) , col="red" , pch=19 , type="b")

axis(1,at=seq(1:nrow(dh)) , labels=paste0("S",dh$session,"; ",dh$date) , hadj=1 , cex.axis=.4 , las=2)
#add in posterior mean preds for each model across sessions
n_list <-  c( "mean_choose_red_ind" , "mean_choose_red_freq" , "mean_choose_red_male" , "mean_choose_red_adult" , "mean_choose_red_roost" , "mean_choose_red_rank")
#median

#di <- aggregate(cbind( d$mean_choose_red_ind , d$mean_choose_red_freq , d$mean_choose_red_male , d$mean_choose_red_adult , d$mean_choose_red_roost , d$mean_choose_red_rank ) , list(date=d$date , session=d$session  ) , mean ) 
di <- aggregate(cbind(  d$median_choose_red_freq ) , list(date=d$date , session=d$session  ) , mean ) 
di <- di[with(di, order(date, session)), ]
di2 <- aggregate(cbind(  d$median_choose_red_freq ) , list(date=d$date , session=d$session  ) , HPDI ) 
di2 <- di2[with(di2, order(date, session)), ]
# for(i in 1:length(n_list)){
#   lines(1:nrow(di),di[,2+i] , ylim=c(0,1) , col=col_strat[i] , pch=1 , type="l")
# }
#plot the freq model
i <- 1
lines(1:nrow(di),di[,2+i] , ylim=c(0,1) , col="darkred" , pch=1 , type="l" , lty=2 , lw=2)
lines(1:nrow(di2),di2$V1[,1] , ylim=c(0,1) , col="darkred" , pch=1 , type="l" , lty=3)
lines(1:nrow(di2),di2$V1[,2] , ylim=c(0,1) , col="darkred" , pch=1 , type="l" , lty=3)


legend("bottomleft" , fill=col_strat , legend=c("IL" , "FDSL" , "MaleBSL" , "AdultBSL" , "RoostBSL" , "RankBSL") , bty='n' , cex=0.5 )

