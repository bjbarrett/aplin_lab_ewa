###freq plots
fc_med <- apply(post_freq$fc_i , 2 , median)
fc_mean <- apply(post_freq$fc_i , 2 , mean)
as_acq <- d[,c("subject_index","age_index","sex_index")]
as_acq <- cbind(as_acq[-which(duplicated(as_acq)),] , fc_med , fc_mean)
colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

sex_pal <- colorBlindBlack8[2:4]
age_pal <- colorBlindBlack8[6:8]
pdf(file = "age_sex_acc_curve_60s.pdf", width = 8, height = 8) # The height of the plot in inches

N1<- seq(from=0 , to=100 , by=1)
N2<- 100-N1
FreqN1B4 <- N1/(N1+N2)
FreqN1After <- rep (0,100)

par(mfrow = c(2, 2))
par(cex = 0.8)
par(mar = c(4, 4, 4, 1), oma = c(.1, .1, .1, .1))
par(xpd=FALSE)

which(as_acq$age_index==1)
plot(FreqN1B4,FreqN1B4 , ylim=c(0,1) , xlim=c(0,1) , ylab="frequency of trait after social learning" , xlab="frequency of trait before social learning",type="n" , bty="n" , cex.lab=1 , main="a. adults")
for(i in which(as_acq$age_index==1) ){
  f <- as_acq$fc_mean[i]
  FreqN1After <- N1^f/(N1^f+N2^f)
  lines( FreqN1B4,FreqN1After,  col=col.alpha( sex_pal[as_acq$sex_index[i]] ,  alpha=0.25  )  , lwd=1)
}
abline(a=0 , b=1 , lty=2)
legend("bottomright", inset=c(0,0), legend=c("female","male","unknown"), fill=sex_pal, title="Sex", cex=0.7 , bty='n')


plot(FreqN1B4,FreqN1B4 , ylim=c(0,1) , xlim=c(0,1) , ylab="frequency of trait after social learning" , xlab="frequency of trait before social learning",type="n" , bty="n" , cex.lab=1 , main="b. juveniles")
for(i in which(as_acq$age_index==2) ){
  f <- as_acq$fc_mean[i]
  FreqN1After <- N1^f/(N1^f+N2^f)
  lines( FreqN1B4,FreqN1After,  col=col.alpha( sex_pal[as_acq$sex_index[i]] ,  alpha=0.25  )  , lwd=1)
}
abline(a=0 , b=1 , lty=2)

legend("bottomright", inset=c(0,0), legend=c("female","male","unknown"), fill=sex_pal, title="Sex", cex=0.7 , bty='n')


plot(FreqN1B4,FreqN1B4 , ylim=c(0,1) , xlim=c(0,1) , ylab="frequency of trait after social learning" , xlab="frequency of trait before social learning",type="n" , bty="n" , cex.lab=1 , main="c. females")
for(i in which(as_acq$sex_index==1) ){
  f <- as_acq$fc_mean[i]
  FreqN1After <- N1^f/(N1^f+N2^f)
  lines( FreqN1B4,FreqN1After,  col=col.alpha( age_pal[as_acq$age_index[i]] ,  alpha=0.25  )  , lwd=1)
}
abline(a=0 , b=1 , lty=2)
legend("bottomright", inset=c(0,0), legend=c("adult","juvenile","unknown"), fill=age_pal, title="Age",cex=0.7 , bty='n')

plot(FreqN1B4,FreqN1B4 , ylim=c(0,1) , xlim=c(0,1) , ylab="frequency of trait after social learning" , xlab="frequency of trait before social learning",type="n" , bty="n" , cex.lab=1 , main="d. males")
for(i in which(as_acq$sex_index==2) ){
  f <- as_acq$fc_mean[i]
  FreqN1After <- N1^f/(N1^f+N2^f)
  lines( FreqN1B4,FreqN1After,  col=col.alpha( age_pal[as_acq$age_index[i]] ,  alpha=0.25  )  , lwd=1)
}
abline(a=0 , b=1 , lty=2)
legend("bottomright", inset=c(0,0), legend=c("adult","juvenile","unknown"), fill=age_pal, title="Age",cex=0.7 , bty='n')

dev.off()
