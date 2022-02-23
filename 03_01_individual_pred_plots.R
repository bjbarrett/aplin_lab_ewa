library(RColorBrewer)
library(DescTools)#ramp legend

#individual raw data:
#calculate median pr at each timestep
d$median_choose_red_ind <- apply(post_ind$PrPreds[,,2] , 2 , median)
d$median_choose_red_freq <- apply(post_freq$PrPreds[,,2] , 2 ,  median)
d$median_choose_red_male <- apply(post_male$PrPreds[,,2] , 2 ,  median)
d$median_choose_red_adult <- apply(post_adult$PrPreds[,,2] , 2 ,  median)
d$median_choose_red_roost <- apply(post_roost$PrPreds[,,2] , 2 ,  median)
d$median_choose_red_rank <- apply(post_rank$PrPreds[,,2] , 2 ,  median)


#plots conditions
colz <- c( "blue" , "red")
pchez <- c(1,19)
col_strat <- brewer.pal( 6 ,"YlOrRd" )
pdf("individual_preds_choosered_multimodel_60s_slu_all.pdf")

par(mfrow = c(4, 2))
par(cex = 0.7)
par(mar = c(4, 4, 2, .1), oma = c(.1, .1 , .1, .1))
for(i in 1:max(d$subject_index)){
  data <- d[d$subject_index==i,]
  plot(rep(1.03 , nrow(data)) ~ data$bout , ylim=c(0,1.04) , ylab="Post Med Prob Choose Red" , xlab="foraging bout" ,
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
colMain <- rev(colorRampPalette(brewer.pal(11, "RdBu"))(101))# reverse pallette of ramped colors from 0 to 1 by 0.1 so red in 1 and blue is 0

###BA
dd <- d[d$group=="BA",]
dd$subject_group_index <- as.integer(as.factor(dd$subject_index))
plot(dd$subject_group_index ~ dd$time_hh_min , cex=0.5 , pch=19 ,
     col=colMain[round(dd$mean_choose_red_freq*100)] , main="Group BA"   ) 
ColorLegend(x=max(dd$time_hh_min) + 50 ,y=max(dd$subject_group_index), 
            cols=colMain , labels=seq(from=0 , to=1 , by=0.1) , cex=0.8)


plot(dd$rank_index ~ dd$time_hh_min , cex=0.5 , pch=19 ,
     col=colMain[round(dd$mean_choose_red_freq*100)] , main="Group BA"   ) 

### CG
dd <- d[d$group=="CG",]
dd$subject_group_index <- as.integer(as.factor(dd$subject_index))
plot(dd$subject_group_index ~ dd$rel_time , cex=0.5 , pch=19 ,
     col=colMain[round(dd$median_choose_red_freq*100)] , main="Group CG"   ) 
ColorLegend(x=max(dd$rel_time) + 50 ,y=max(dd$subject_group_index), 
            cols=colMain , labels=seq(from=0 , to=1 , by=0.1) , cex=0.8)

plot(dd$rank_index ~ dd$rel_time , cex=0.5 , pch=19 ,
     col=colMain[round(dd$median_choose_red_freq*100)] , main="Group CG"   ) 
ColorLegend(x=max(dd$rel_time) + 50 , y=max(dd$rank_index, na.rm=TRUE), 
            cols=colMain , labels=seq(from=0 , to=1 , by=0.1) , cex=0.8)

#NB
dd <- d[d$group=="NB",]
dd$subject_group_index <- as.integer(as.factor(dd$subject_index))
plot(dd$rank_index ~ dd$rel_time , cex=0.5 , pch=19 ,
     col=colMain[round(dd$median_choose_red_freq*100)] , main="Group NB"   ) 
ColorLegend(x=max(dd$rel_time) + 100 ,y=max(dd$rank_index , na.rm=TRUE), 
            cols=colMain , labels=seq(from=0 , to=1 , by=0.1) , cex=0.8)


##questions for julia
#rel time and my ordering
#age sex
#missing ranks?

##rank by learning parameters
# groups hape switch
# color prefernce in isolation vs socially
# variance of individual probability as a function of rankvar
#var(d$mean_choose_red_freq)

apply(post_freq$fc_i , 2, median)

il_stuff <- d[select.list()]