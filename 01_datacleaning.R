library(janitor)
library(lubridate)
library(beepr)
library(readr)

#d_BA_to_clean <- read.csv("C:/Users/jpenndorf/owncloud/EWA/aplin_lab_ewa/EWA_raw_data_BA_CG_NB_MA_BG.csv")
d_BA_to_clean <- read.csv("cockatoo_data/EWA_raw_data_BA_CG_NB_MA_BG.csv")

d_BA_to_clean$subject[d_BA_to_clean$subject=="Msmu_V_BA"]<- "Msmu_BA"
d_BA_to_clean$subject[d_BA_to_clean$subject=="Msmush_V_BA"]<- "Msmu_BA"

d_BA <- d_BA_to_clean[which(d_BA_to_clean$subject !="Ser Onion" & d_BA_to_clean$subject !="corella"),]
d_BA <-  d_BA[which( d_BA$subject!=""),]

d_BA <- d_BA[order(d_BA$date,d_BA$time_hh_min),]

#d_BA <- clean_names(d_BA)
unique(d_BA$behav1)
d_BA$choose_red <- ifelse(d_BA$behav1=="R" , 1, 0)
d_BA$choose_blue <- ifelse(d_BA$behav1=="B" , 1, 0)
d_BA$open <- ifelse(d_BA$behav2=="op" , 1, 0)

#ILVba <- read.csv("C:/Users/jpenndorf/owncloud/EWA/aplin_lab_ewa/cockatoo_data/ILV_allgroups20220310.csv",row.names = 1)
ILVba <- read.csv("cockatoo_data/ILV_allgroups20220310.csv",row.names = 1)

ILVba <- clean_names(ILVba)
ILVba$tutor_red <- 0
ILVba$tutor_blue <- 0

#add index for tutor to seed attraction score
ILVba$tutor_red[ILVba$id=="X11"] <- 1
ILVba$tutor_red[ILVba$id=="BNV_H_CG"] <- 1
ILVba$tutor_blue[ILVba$id=="BPO_V_BA"] <- 1
ILVba$tutor_blue[ILVba$id=="MVT_V_BA"] <- 1
#plot_raw_data

####run this right before model in case subsetting occurs
d_BA$subject_index <- as.integer(as.factor(d_BA$subject) )
pch_pal <- c(1,19)
col_pal <- c("red" , "blue")
plot(d_BA$subject_index ~ d_BA$rel.time , col=col_pal[d_BA$choose_blue +1] , pch=pch_pal[d_BA$open + 1] , cex=0.5)
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
colnames(d_BA)
ps_BA <- d_BA[,11:541]
ps_BA <- ifelse( ps_BA==TRUE , 1 , 0)
nrow(ps_BA)
nrow(d_BA)
#just use social information associated with 1 in ps_BA
d_BA$n_obs_blue <- 0
d_BA$n_obs_red <- 0


#unique.id <- unique(d_BA$subject)
#present.BA.cut <- present.BA[,colnames(present.BA) %in%unique.id]

#for each solve
## define time window (previous solve or previous scan)
### count number of solves red / blue within window
freq_red <- matrix(0,nrow=nrow(d_BA),ncol=ncol(ps_BA))##sum values where we tally up ones
freq_blue <- matrix(0,nrow=nrow(d_BA),ncol=ncol(ps_BA))
ILV <- ILVba[order(ILVba$id),] #order by ID_actor

sort(unique(d_BA$subject)) %in% sort(unique(colnames(ps_BA)))#check to make sure all focals are in colnames

for( i in 1:nrow(d_BA) ){ # creates 2 matrices, specifying if individual was present at red (freq_red) or blue (freq_blue) solve
  for (j in 1:ncol(ps_BA)){
    if (d_BA$subject[i] == colnames(ps_BA)[j]){     
      freq_red[i,j] <- as.numeric(d_BA$choose_red[i])*as.numeric(ps_BA[i,j])
      freq_blue[i,j] <- as.numeric(d_BA$choose_blue[i])*as.numeric(ps_BA[i,j])
        }	
    }
}

beep(3)
win_width <- 0.5*60 #social info memory window in seconds (num_min*60secs)
d_BA$obs_index <- seq(1:nrow(d_BA)) #unique sequential value to each row after ordering dataframe by timestamp
ILV$ID_all_index <- as.integer(as.factor(ILV$id)) #is this still needed?
d_BA$ID_all_index <- ILV$ID_all_index[match(d_BA$subject, ILV$id)]
d_BA$date_time <- d_BA$date*10000 #what is d_BA$time_hh_mon ##ASK JULIA
d_BA$date_time <- d_BA$date_time + d_BA$time_hh_min #what is d_BA$time_hh_mon ##ASK JULIA

d_BA$zz <- NA
for (nobs in 1:nrow(d_BA)){
  d_BA$zz <- zz <- min(d_BA$obs_index[as.numeric(as.duration(d_BA$date_time[nobs] - d_BA$date_time)) <= win_width ]) #what is minimal value or earliest observation that occured within the window width
  # d_BA$n_obs_blue[nobs] <- sum( freq_blue[ zz : (d_BA$obs_index[nobs] - 1) , d_BA$ID_all_index[nobs] ] ) # we need to get these indives to match
  # d_BA$n_obs_red[nobs] <- sum( freq_red[ zz : (d_BA$obs_index[nobs] - 1) , d_BA$ID_all_index[nobs]] ) 
  d_BA$n_obs_blue[nobs] <- sum( freq_blue[ zz : (d_BA$obs_index[nobs] - 1) , which(colnames(ps_BA)==d_BA$subject[nobs]) ] ) # we need to get these indives to match
  d_BA$n_obs_red[nobs] <- sum( freq_red[ zz : (d_BA$obs_index[nobs] - 1) , which(colnames(ps_BA)==d_BA$subject[nobs]) ] )
}

#get rid of NaNs and make value zero so it does not affect behavior
d_BA2 <- d_BA

d_BA2$n_obs_blue[is.nan(d_BA2$n_obs_blue)] <- 0
d_BA2$n_obs_red[is.nan(d_BA2$n_obs_red)] <- 0

d_BA2$forg_bout <- rep(0,nrow(d_BA2))
d <-d_BA2[with(d_BA2, order(subject, forg_bout)),]

d_BA2$sex_index <- ILVba$sex[match(d_BA2$subject,ILVba$id)]
d_BA2$age_index <- ILVba$ag[match(d_BA2$subject,ILVba$id)]

d_BA2$sex_index[is.na(d_BA2$sex_index)] <- 0
d_BA2$age_index[is.na(d_BA2$age_index)] <- 0

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
    d_BA2$age_index[i] <- 0
  }
  if (d_BA2$age_index[i]=="A"){
    d_BA2$age_index[i] <- 1
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
  zz <- min(d_BA2$obs_index[as.numeric(as.duration(d_BA2$date_time[nobs] - d_BA2$date_time)) <= win_width ]) #what is minimal value or earliest observation of males that occured within the window width
  
  d_BA2$s_male_red[nobs] <- sum( freq_male_red[ zz : (d_BA2$obs_index[nobs] - 1) , which(colnames(ps_BA)==d_BA$subject[nobs]) ] )
  d_BA2$s_male_blue[nobs] <- sum( freq_male_blue[ zz : (d_BA2$obs_index[nobs] - 1) , which(colnames(ps_BA)==d_BA$subject[nobs]) ]) 
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
  zz <- min(d_BA2$obs_index[as.numeric(as.duration(d_BA2$date_time[nobs] - d_BA2$date_time)) <= win_width ]) #what is minimal value or earliest observation of males that occured within the window width
  d_BA2$s_adult_red[nobs] <- sum( freq_adult_red[ zz : (d_BA2$obs_index[nobs] - 1) , which(colnames(ps_BA)==d_BA$subject[nobs]) ] )
  d_BA2$s_adult_blue[nobs] <- sum( freq_adult_blue[ zz : (d_BA2$obs_index[nobs] - 1) , which(colnames(ps_BA)==d_BA$subject[nobs]) ]) 
}
beep(2)
# 
# #### solves of higher ranking individuals  witnessed
# ### count number of solves red / blue done by higher ranking individuals within window
# freq_highrank_red <- matrix(0,nrow=nrow(d_BA2),ncol=ncol(ps_BA)) ##sum values where we tally up ones
# freq_highrank_blue <- matrix(0,nrow=nrow(d_BA2),ncol=ncol(ps_BA))
# 
# d_BA2$rank_index <- NA
# 
# for (i in 1:nrow(d_BA2)) { # rank calculated at each site
#   for (j in 1:nrow(ILV)) {
#     if (d_BA2$group[i]=="BA" & d_BA2$subject[i]==ILV$id[j]) {
#       d_BA2$rank_index[i] <- ILV$rank_ba[j]
#     }
#     if (d_BA2$group[i]=="CG" & d_BA2$subject[i]==ILV$id[j]) {
#       d_BA2$rank_index[i] <- ILV$rank_cg[j]
#     }
#     if (d_BA2$group[i]=="NB" & d_BA2$subject[i]==ILV$id[j]) {
#       d_BA2$rank_index[i] <- ILV$rank_nb[j]
#     }
# 
#   }
# }
# 
# ##ranks at other sites
# d_BA2$roost <- ILV$roost[match(d_BA2$subject,ILV$id)]
# 
# rank.matrix <- matrix(NA,nrow=nrow(d_BA2),ncol=ncol(ps_BA))
# colnames(rank.matrix)<- colnames(ps_BA)
# 
# for (i in 1:nrow(rank.matrix)) {
#   for (j in 1:ncol(rank.matrix)) {
#     for (k in 1:nrow(ILV)) {
#       if (d_BA2$group[i]=="BA" & colnames(rank.matrix)[j]==ILV$id[k]) {
#         rank.matrix[i,j] <- ILV$rank_ba[k]
#       }
#       if (d_BA2$group[i]=="CG" & colnames(rank.matrix)[j]==ILV$id[k]) {
#         rank.matrix[i,j] <- ILV$rank_cg[k]
#       }
#       if (d_BA2$group[i]=="NB" & colnames(rank.matrix)[j]==ILV$id[k]) {
#         rank.matrix[i,j] <- ILV$rank_nb[k]
#       }
#     }
#   }
# }
# 
# rank.matrix[is.na(rank.matrix)] <- 0
# rank_similarity <- matrix(0,nrow=nrow(rank.matrix),ncol=ncol(rank.matrix))
# 
# for(i in 1:nrow(rank.matrix)) {
#   for (j in 1:ncol(rank.matrix)) {
#     rank_similarity[i,j] <- rank.matrix[i,j]>d_BA2$rank_index[i] 
#   }
# }
# 
# rank_similarity[is.na(rank_similarity)] <- 0 #NAs due to individuals of unknown rank
# 
# for( i in 1:nrow(d_BA2) ){ # creates 2 matrices, specifying if individual was present at red (freq_red) or blue (freq_blue) solve
#   for (j in 1:ncol(ps_BA)){
#     if ( (rank_similarity[i,j]==1))    {     
#       freq_highrank_red[i,j] <- as.numeric(d_BA2$choose_red[i])*as.numeric(ps_BA[i,j])
#       freq_highrank_blue[i,j] <- as.numeric(d_BA2$choose_blue[i])*as.numeric(ps_BA[i,j])
#     }	
#     else { freq_highrank_red[i,j] <- 0
#       freq_highrank_blue[i,j] <- 0
#     }	
#   }
# }
# 
# for (nobs in 1:nrow(d_BA2)){
#   zz <- min(d_BA2$obs_index[as.numeric(as.duration(d_BA2$date_time[nobs] - d_BA2$date_time)) <= win_width ]) #what is minimal value or earliest observation of males that occured within the window width
#   
#   d_BA2$s_highrank_red[nobs] <- sum( freq_highrank_red[ zz : (d_BA2$obs_index[nobs] - 1) , which(colnames(ps_BA)==d_BA$subject[nobs]) ] )
#   d_BA2$s_highrank_blue[nobs] <- sum( freq_highrank_blue[ zz : (d_BA2$obs_index[nobs] - 1) , which(colnames(ps_BA)==d_BA$subject[nobs]) ]) 
#   
# }
# beep(2)
# 
#### solves of individuals from same roost witnessed
### count number of solves red / blue done by individuals of same roost within window
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
  zz <- min(d_BA2$obs_index[as.numeric(as.duration(d_BA2$date_time[nobs] - d_BA2$date_time)) <= win_width ]) #what is minimal value or earliest observation of males that occured within the window width
  
  d_BA2$s_roost_red[nobs] <- sum( freq_roost_red[ zz : (d_BA2$obs_index[nobs] - 1) , which(colnames(ps_BA)==d_BA$subject[nobs]) ] )
  d_BA2$s_roost_blue[nobs] <- sum( freq_roost_blue[ zz : (d_BA2$obs_index[nobs] - 1) , which(colnames(ps_BA)==d_BA$subject[nobs]) ]) 
  
}
beep(2)
df <- d_BA2
df[ , c(11:541)] <- list(NULL)
colnames(df)
str(df)
write.csv(df,'ALL_ROOSTS_Almonds_cockatoo_30s.csv')
