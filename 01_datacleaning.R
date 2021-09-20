library(janitor)
library(lubridate)
d_BA <- read.csv("C:/Users/jpenndorf/ownCloud/old ewa/aplin_lab_ewa/cockatoo data/EWA_raw_data_BA.csv")
d_BA <- clean_names(d_BA)
unique(d_BA$behav1)
d_BA$choose_red <- ifelse(d_BA$behav1=="R" , 1, 0)
d_BA$choose_blue <- ifelse(d_BA$behav1=="B" , 1, 0)
d_BA$open <- ifelse(d_BA$behav2=="op" , 1, 0)

ILVba <- read.csv("C:/Users/jpenndorf/ownCloud/old ewa/aplin_lab_ewa/cockatoo data/ILV_BA.csv",row.names = 1)
ILVba <- clean_names(ILVba)

roosts <- read.csv("C:/Users/jpenndorf/ownCloud/EWA/aplin_lab_ewa/cockatoo_data/roosts.csv",row.names=1)
roosts <- clean_names(roosts)

ILVba$roost <- roosts$roost[match(ILVba$id,roosts$id)]

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
ps_BAcomplete <- read.csv("C:/Users/jpenndorf/ownCloud/EWA/BA_presence_solves.csv")
ps_BA <- ps_BAcomplete[,11:197]

nrow(ps_BA)
nrow(d_BA)
#just use social information associated with 1 in ps_BA
d_BA$n_obs_blue <- 0
d_BA$n_obs_red <- 0

# subsetting to only individuals that soled at least once
d_BA[d_BA$subject=="118",3]<-"X118"
d_BA[d_BA$subject=="136",3]<-"X136"
ILVba[ILVba$id=="118",1] <- "X118"
ILVba[ILVba$id=="136",1] <- "X136"


#unique.id <- unique(d_BA$subject)
#present.BA.cut <- present.BA[,colnames(present.BA) %in%unique.id]

#for each solve
## define time window (previous solve or previous scan)
### count number of solves red / blue within window
freq_red <- matrix(0,nrow=nrow(d_BA),ncol=ncol(ps_BA))##sum values where we tally up ones
freq_blue <- matrix(0,nrow=nrow(d_BA),ncol=ncol(ps_BA))
ILV <- ILVba[order(ILVba$id),] #order by ID_actor 

for( i in 1:nrow(d_BA) ){ # creates 2 matrices, specifying if individual was present at red (freq_red) or blue (freq_blue) solve
  for (j in 1:ncol(ps_BA)){
    if (d_BA$subject[i] == colnames(ps_BA)[j]){     
      freq_red[i,j] <- as.numeric(d_BA[i,12])*as.numeric(ps_BA[i,j])
      freq_blue[i,j] <- as.numeric(d_BA[i,13])*as.numeric(ps_BA[i,j])
        }	
    }
}

win_width <- 1*60 #social info memory window in seconds (num_min*60secs)

d_BA$obs_index <- seq(1:nrow(d_BA)) #unique sequential value to each row after ordering dataframe by timestamp
ILV$ID_all_index <- as.integer(as.factor(ILV$id))
d_BA$ID_all_index <- ILV$ID_all_index[match(d_BA$subject, ILV$id)]


for (nobs in 1:nrow(d_BA)){
  zz <- min(d_BA$obs_index[as.numeric(as.duration(d_BA$rel_time[nobs] - d_BA$rel_time)) <= win_width ]) #what is minimal value or earliest observation that occured within the window width
  
  d_BA$n_obs_blue[nobs] <- sum( freq_blue[ zz : (d_BA$obs_index[nobs] - 1) , d_BA$ID_all_index[nobs] ] )
  d_BA$n_obs_red[nobs] <- sum( freq_red[ zz : (d_BA$obs_index[nobs] - 1) , d_BA$ID_all_index[nobs]] ) 

  
}

d_BA2 <- d_BA[which(d_BA$subject!=""),]

#get rid of NaNs and make value zero so it does not affect behavior
d_BA2$n_obs_blue[is.nan(d_BA2$n_obs_blue)] <- 0
d_BA2$n_obs_red[is.nan(d_BA2$n_obs_red)] <- 0



d_BA2$forg_bout <- rep(0,nrow(d_BA2))

d <-d_BA2[with(d_BA2, order(subject, forg_bout)),]


d_BA2$sex_index <- ILVba$sex[match(d_BA2$subject,ILVba$id)]
d_BA2$age_index <- ILVba$age[match(d_BA2$subject,ILVba$id)]

d_BA2[is.na(d_BA2$sex_index),21] <- 0
d_BA2[is.na(d_BA2$age_index),22] <- 0

for (i in 1:nrow(d_BA2)) {
  if (d_BA2$sex_index[i]=="F"){
    d_BA2$sex_index[i] <- 0
  }
  if (d_BA2$sex_index[i]=="M"){
    d_BA2$sex_index[i] <- 1
  }
}
for (i in 1:nrow(d_BA2)) {
  if (d_BA2$age_index[i]=="J"){
    d_BA2$age_index[i] <- 2
  }
  if (d_BA2$age_index[i]=="A"){
    d_BA2$age_index[i] <- 3
  }
}

#### male solves witnessed
### count number of solves red / blue done by males within window
freq_male_red <- matrix(0,nrow=nrow(d_BA2),ncol=ncol(ps_BA))##sum values where we tally up ones
freq_male_blue <- matrix(0,nrow=nrow(d_BA2),ncol=ncol(ps_BA))
ILV <- ILVba[order(ILVba$id),] #order by ID_actor 

for( i in 1:nrow(d_BA2) ){ # creates 2 matrices, specifying if individual was present at red (freq_red) or blue (freq_blue) solve
  for (j in 1:ncol(ps_BA)){
    if (d_BA2$subject[i] == colnames(ps_BA)[j] ){     
      freq_male_red[i,j] <- as.numeric(d_BA2$choose_red[i])*as.numeric(ps_BA[i,j])*as.numeric(d_BA2$sex_index[i])
      freq_male_blue[i,j] <- as.numeric(d_BA2$choose_blue[i])*as.numeric(ps_BA[i,j])*as.numeric(d_BA2$sex_index[i])
    }	
  }
}


for (nobs in 1:nrow(d_BA2)){
  zz <- min(d_BA2$obs_index[as.numeric(as.duration(d_BA2$rel_time[nobs] - d_BA2$rel_time)) <= win_width ]) #what is minimal value or earliest observation of males that occured within the window width
  
  d_BA2$s_male_red[nobs] <- sum( freq_male_red[ zz : (d_BA2$obs_index[nobs] - 1) , d_BA2$ID_all_index[nobs] ] )
  d_BA2$s_male_blue[nobs] <- sum( freq_male_blue[ zz : (d_BA2$obs_index[nobs] - 1) , d_BA2$ID_all_index[nobs] ]) 
  
  
}

#### adult solves witnessed
### count number of solves red / blue done by adults within window
freq_adult_red <- matrix(0,nrow=nrow(d_BA2),ncol=ncol(ps_BA)) ##sum values where we tally up ones
freq_adult_blue <- matrix(0,nrow=nrow(d_BA2),ncol=ncol(ps_BA))

for( i in 1:nrow(d_BA2) ){ # creates 2 matrices, specifying if individual was present at red (freq_red) or blue (freq_blue) solve
  for (j in 1:ncol(ps_BA)){
    if (d_BA2$subject[i] == colnames(ps_BA)[j] ){     
      freq_adult_red[i,j] <- as.numeric(d_BA2$choose_red[i])*as.numeric(ps_BA[i,j])*as.numeric(d_BA2$age_index[i])
      freq_adult_blue[i,j] <- as.numeric(d_BA2$choose_blue[i])*as.numeric(ps_BA[i,j])*as.numeric(d_BA2$age_index[i])
    }	
  }
}


for (nobs in 1:nrow(d_BA2)){
  zz <- min(d_BA2$obs_index[as.numeric(as.duration(d_BA2$rel_time[nobs] - d_BA2$rel_time)) <= win_width ]) #what is minimal value or earliest observation of males that occured within the window width
  
  d_BA2$s_adult_red[nobs] <- sum( freq_adult_red[ zz : (d_BA2$obs_index[nobs] - 1) , d_BA2$ID_all_index[nobs] ] )
  d_BA2$s_adult_blue[nobs] <- sum( freq_adult_blue[ zz : (d_BA2$obs_index[nobs] - 1) , d_BA2$ID_all_index[nobs] ]) 
  
  
}

#### solves of higher ranking individuals  witnessed
### count number of solves red / blue done by higher ranking individuals within window
freq_highrank_red <- matrix(0,nrow=nrow(d_BA2),ncol=ncol(ps_BA)) ##sum values where we tally up ones
freq_highrank_blue <- matrix(0,nrow=nrow(d_BA2),ncol=ncol(ps_BA))

d_BA2$rank_index <- ILV$rank[match(d_BA2$subject,ILV$id)]

rank_matrix <- matrix(NA,nrow=1,ncol=ncol(ps_BA))
colnames(rank_matrix)<- colnames(ps_BA)

for (i in 1:nrow(ILV)) {
  for (j in 1:ncol(rank_matrix)) {
    if (ILV$id[i]==colnames(rank_matrix)[j]) {
      rank_matrix[1,j] <- ILV$rank[i]
    }
  }
}

higher_rank <- matrix(NA,nrow=nrow(d_BA2),ncol=ncol(rank_matrix))

for(i in 1:nrow(d_BA2)) {
  for (j in 1:ncol(higher_rank)) {
    higher_rank[i,j] <- rank_matrix[1,j]>d_BA2$rank_index[i] #whether solving individual is higher ranked (1 highest rank)
  }
}
higher_rank[is.na(higher_rank)] <- FALSE #replacing NAs introduced by unknown ranks
# binary matrix: TRUE= observer is higher ranked than attending individual
# FALSE: attending individual higher ranked than individual

for( i in 1:nrow(d_BA2)){ # creates 2 matrices, specifying if individual was present at red (freq_red) or blue (freq_blue) solve
  for (j in 1:ncol(ps_BA)){
    if (d_BA2$subject[i] == colnames(ps_BA)[j]){     
      freq_highrank_red[i,j] <- d_BA2$choose_red[i]*higher_rank[i,j]*ps_BA[i,j]
      freq_highrank_blue[i,j] <- d_BA2$choose_blue[i]*ps_BA[i,j]*higher_rank[i,j]
    }	
  }
}


#### solves of individuals from same roost witnessed
### count number of solves red / blue done by individuals of same roost within window
freq_roost_red <- matrix(0,nrow=nrow(d_BA2),ncol=ncol(ps_BA)) ##sum values where we tally up ones
freq_roost_blue <- matrix(0,nrow=nrow(d_BA2),ncol=ncol(ps_BA))

d_BA2$roost <- ILV$roost[match(d_BA2$subject,ILV$id)]

roost.matrix <- matrix(0,nrow=1,ncol=ncol(ps_BA))
colnames(roost.matrix)<- colnames(ps_BA)

for (i in 1:nrow(ILV)) {
  for (j in 1:ncol(roost.matrix)) {
    if (ILV$id[i]==colnames(roost.matrix)[j]) {
      roost.matrix[1,j] <- ILV$roost[i]
    }
  }
}

roost_similarity <- matrix(0,nrow=nrow(d_BA2),ncol=ncol(roost.matrix))

for(i in 1:nrow(d_BA2)) {
  for (j in 1:ncol(roost.matrix)) {
    roost_similarity[i,j] <- roost.matrix[1,j]==d_BA2$roost[i]
  }
}

roost_similarity[is.na(roost_similarity)] <- FALSE #NAs due to individuals of unknown roosting location
## roost similarity: binary, whether observing individual is from same roost than solver
## does not take into account whether they are solving at their roost, or just visiting this particular roost for a day



for( i in 1:nrow(d_BA2) ){ # creates 2 matrices, specifying if individual was present at red (freq_red) or blue (freq_blue) solve
  for (j in 1:ncol(ps_BA)){
    if (d_BA2$subject[i] == colnames(ps_BA)[j]){     
      freq_roost_red[i,j] <- as.numeric(d_BA2$choose_red[i])*as.numeric(ps_BA[i,j])*as.numeric(roost_similarity[i,j])
      freq_roost_blue[i,j] <- as.numeric(d_BA2$choose_blue[i])*as.numeric(ps_BA[i,j])*as.numeric(roost_similarity[i,j])
    }	
  }
}

for (nobs in 1:nrow(d_BA2)){
  zz <- min(d_BA2$obs_index[as.numeric(as.duration(d_BA2$rel_time[nobs] - d_BA2$rel_time)) <= win_width ]) #what is minimal value or earliest observation of males that occured within the window width
  
  d_BA2$s_roost_red[nobs] <- sum( freq_roost_red[ zz : (d_BA2$obs_index[nobs] - 1) , d_BA2$ID_all_index[nobs] ] )
  d_BA2$s_roost_blue[nobs] <- sum( freq_roost_blue[ zz : (d_BA2$obs_index[nobs] - 1) , d_BA2$ID_all_index[nobs] ]) 

}

write.csv(d_BA2,'C:/Users/jpenndorf/ownCloud/EWA/aplin_lab_ewa/cockatoo_data/BA_Almonds_cockatoo_60s.csv')

