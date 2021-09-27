library(RColorBrewer)
#individual raw data:
#calculate mean pr at each timestep
d$mean_choose_red_ind <- apply(post_i$PrPreds[,,2] , 2 , mean)
d$mean_choose_red_freq <- apply(post_freq$PrPreds[,,2] , 2 , mean)
d$mean_choose_red_male <- apply(post_male$PrPreds[,,2] , 2 , mean)
d$mean_choose_red_adult <- apply(post_adult$PrPreds[,,2] , 2 , mean)
d$mean_choose_red_roost <- apply(post_roost$PrPreds[,,2] , 2 , mean)
d$mean_choose_red_rank <- apply(post_rank$PrPreds[,,2] , 2 , mean)


#plots conditions
colz <- c( "blue" , "red")
pchez <- c(1,19)
col_strat <- brewer.pal( 6 ,"YlOrRd" )
pdf("individual_preds_choosered_multimodel_60s_slu.pdf")

par(mfrow = c(4, 2))
par(cex = 0.7)
par(mar = c(4, 4, 2, .1), oma = c(.1, .1 , .1, .1))
for(i in 1:max(d$subject_index)){
  data <- d[d$ID_all_index==i,]
  plot(rep(1.03 , nrow(data)) ~ data$bout , ylim=c(0,1.04) , ylab="Prob Choose Red" , xlab="foraging bout" ,
       col=colz[data$tech_index] , pch=pchez[data$open + 1] , cex=.5 )
  abline(h=1)
  lines(data$mean_choose_red_ind ~ data$bout , col=col_strat[1])
  lines(data$mean_choose_red_freq ~ data$bout , col=col_strat[2] )
  lines(data$mean_choose_red_male ~ data$bout , col=col_strat[3] )
  lines(data$mean_choose_red_adult ~ data$bout , col=col_strat[4] )
  lines(data$mean_choose_red_roost ~ data$bout , col=col_strat[5] )
  lines(data$mean_choose_red_rank ~ data$bout , col=col_strat[6] )
  
  title(main = paste0("ID=",unique(data$subject),", Group=",unique(data$group),", Sex=",unique(data$sex_index),", Age=",unique(data$age_index)) , cex.main=1)
  legend("bottomleft" , fill=col_strat , legend=c("IL" , "FDSL" , "MaleBSL" , "AdultBSL" , "RoostBSL" , "RankBSL") , bty='n' , cex=0.5 )
}

dev.off()

#all individuals