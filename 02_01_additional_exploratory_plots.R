library(janitor)
library(lubridate)
d_BA_to_clean <- read.csv("C:/Users/jpenndorf/owncloud/EWA/aplin_lab_ewa/cockatoo_data/EWA_raw_data_BA_CG_NB.csv")

d_BA <- d_BA_to_clean[which(d_BA_to_clean$subject !="Ser Onion" & d_BA_to_clean$subject !="corella"),]
#d_BA <- clean_names(d_BA)
unique(d_BA$behav1)
d_BA$choose_red <- ifelse(d_BA$behav1=="R" , 1, 0)
d_BA$choose_blue <- ifelse(d_BA$behav1=="B" , 1, 0)
d_BA$open <- ifelse(d_BA$behav2=="op" , 1, 0)

ILVba <- read.csv("C:/Users/jpenndorf/owncloud/EWA/aplin_lab_ewa/cockatoo_data/ILV_allgroups.csv",row.names = 1)
ILVba <- clean_names(ILVba)
colnames(ILVba)[1] <- "id"
ILVba <- ILVba[which(ILVba$id !="Ser Onion" & ILVba$id !="corella"),]

#plot_raw_data

####run this reight before model in case subsetting occurs
d_BA$subject_index <- as.integer(as.factor(d_BA$subject) )
pch_pal <- c(1,19)
col_pal <- c("red" , "blue")
plot(d_BA$subject_index ~ d_BA$rel.time , col=col_pal[d_BA$choose_blue +1] , pch=pch_pal[d_BA$open + 1] , cex=0.5)



ILV <- ILVba[order(ILVba$id),] #order by ID_actor 

## separating by group for plotting
dba <- d_BA[which(d_BA$group=="BA"),]
dba$rank <- ILV$rank_ba[match(dba$subject,ILV$id)]
plot(dba$rank ~ dba$rel.time , col=col_pal[dba$choose_blue +1] , pch=pch_pal[dba$open + 1] , cex=0.5)

dnb <- d_BA[which(d_BA$group=="NB"),]
dnb$rank <- ILV$rank_nb[match(dnb$subject,ILV$id)]
plot(dnb$rank ~ dnb$rel.time , col=col_pal[dnb$choose_blue +1] , pch=pch_pal[dnb$open + 1] , cex=0.5)

dcg <- d_BA[which(d_BA$group=="CG"),]
dcg$rank <- ILV$rank_cg[match(dcg$subject,ILV$id)]
plot(dcg$rank ~ dcg$rel.time , col=col_pal[dcg$choose_blue +1] , pch=pch_pal[dcg$open + 1] , cex=0.5)



## plotting by date
d0818 <- d_BA[which(d_BA$date=="20190823"),]
d0818_nb <- d0818[which(d0818$group=="NB"),]
d0818_nb <- d0818_nb[order(d0818_nb$rel.time),] 

for (i in 2:nrow(d0818_nb)) {
  d0818_nb$same[i] <- d0818_nb$behav1[i] ==d0818_nb$behav1[i-1]
}

d0818_nb$rank <- ILV$rank_nb[match(d0818_nb$subject,ILV$id)]

plot(d0818_nb$rank ~ d0818_nb$rel.time, col=col_pal[d0818_nb$choose_blue +1] , pch=pch_pal[d0818_nb$same + 1] , cex=0.5)

d0818_nb$behav <- d0818_nb$behav1=="R"
plot(d0818_nb$behav ~ d0818_nb$rel.time, col=col_pal[d0818_nb$choose_blue +1] , pch=pch_pal[d0818_nb$same + 1] , cex=0.5)
