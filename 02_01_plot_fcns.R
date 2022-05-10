library(RColorBrewer)
#lambda
DensLambda <- function(x , prior=TRUE , individual=FALSE , group=FALSE , age_sex=TRUE , unknown=TRUE){
  dens(exp(x$log_lambda) , ylim=c(0,2) , xlim=c(0,6) , adj=.9 , col=col.alpha("white", 0.01) , yaxt='n' ) # all lambdas
  if(age_sex==TRUE){
    u <- ifelse(unknown==TRUE , 3 , 2 )
    for(sex in 1:u){
      for(age in 1:u){
        if(sex==1) colz=brewer.pal(6,"Reds")[c(2,4,6)]
        if(sex==2) colz=brewer.pal(6,"Blues")[c(2,4,6)]
        if(sex==3) colz=brewer.pal(6,"Purples")[c(2,4,6)]

        dens(exp(x$log_lambda[,age,sex]) , add=TRUE , adj=.9 , col=colz[age] , lw=2)
        shade( density(exp(x$log_lambda[,age,sex])) , PCI(exp(x$log_lambda[,age,sex]) , prob=0.9999) , col=col.alpha(colz[age], 0.4) )
        }
    }
    if(unknown==TRUE) legend("topright" , c("AF","JF","UF","AM","JM","UM","AU","JU","UU"),fill=c(brewer.pal(6,"Reds")[c(2,4,6)], brewer.pal(6,"Blues")[c(2,4,6)] , brewer.pal(6,"Purples")[c(2,4,6)]) , bty='n')
    if(unknown==FALSE) legend("topright" , c("AF","JF","AM","JM"),fill=c(brewer.pal(6,"Reds")[c(2,4)], brewer.pal(6,"Blues")[c(2,4)]) , bty='n')
  }
  
  if(prior==TRUE){
  dens( exp(rnorm(4000, mean = 1, sd = 0.6)) , add=TRUE , lty=2 , col="black",adj=1) #prior
  }
  
  if(individual==TRUE){
     for(i in 1:dim(x$lambda_i)[2]){
       dens(x$lambda_i[,i] , add=TRUE , col=col.alpha("black", 0.15) , adj=0.8)
     }
  }
  
 if(group==TRUE){
  colz3 <- brewer.pal(dim(x$G)[2] , "Dark2")
  for(i in 1:dim(x$G)[2]){
    dens(exp(x$G[,i,1] + x$log_lambda ), add=TRUE , col=colz3[i] , adj=0.8)
    shade( density(exp(x$G[,i,1] + x$log_lambda )) , PCI(exp(x$G[,i,1] + x$log_lambda ) , prob=0.9999) , col=col.alpha(colz3[i], 0.4) )
  }
  legend("topright", c("BA","BG","CG","MA","NB") ,fill=colz3 , bty='n')
  #legend("topright" , c("AF","JF","AM","JM"),fill=c("salmon4","salmon2","royalblue4","royalblue2" ) , bty='n')
  
 }
  
}

DensPhi <- function(x, prior=TRUE , individual=FALSE , group=FALSE , age_sex=TRUE , unknown=TRUE ){
  dens(logistic(x$logit_phi) , ylim=c(0,20) , xlim=c(0,0.8) , adj=.9 , col="white", yaxt='n' ) 
if(age_sex==TRUE){
  u <- ifelse(unknown==TRUE , 3 , 2 )
  for(sex in 1:u){
    for(age in 1:u){
      if(sex==1) colz=brewer.pal(6,"Reds")[c(2,4,6)]
      if(sex==2) colz=brewer.pal(6,"Blues")[c(2,4,6)]
      if(sex==3) colz=brewer.pal(6,"Purples")[c(2,4,6)]
      dens(logistic(x$logit_phi[,age,sex]) , add=TRUE , adj=.9 , col=colz[age] , lw=2)
      shade( density(logistic(x$logit_phi[,age,sex])) , PCI(logistic(x$logit_phi[,age,sex]) , prob=0.9999) , col=col.alpha(colz[age], 0.4) )
    }
  }
  if(unknown==TRUE) legend("topright" , c("AF","JF","UF","AM","JM","UM","AU","JU","UU"),fill=c(brewer.pal(6,"Reds")[c(2,4,6)], brewer.pal(6,"Blues")[c(2,4,6)] , brewer.pal(6,"Purples")[c(2,4,6)]) , bty='n')
  if(unknown==FALSE) legend("topright" , c("AF","JF","AM","JM"),fill=c(brewer.pal(6,"Reds")[c(2,4)], brewer.pal(6,"Blues")[c(2,4)]) , bty='n')
}  

  if(prior==TRUE){
    dens( logistic(rnorm(4000, mean = 0, sd = 1)) , add=TRUE , lty=2 , col="black",adj=1) #prior
  }
  if(individual==TRUE){
    for(i in 1:dim(x$phi_i)[2]){
      dens(x$phi_i[,i] , add=TRUE , col=col.alpha("black", 0.05) , adj=0.8)
    }
  }
  if(group==TRUE){
    colz3 <- brewer.pal(dim(x$G)[2] , "Dark2")
    for(i in 1:dim(x$G)[2]){
      dens(logistic(x$G[,i,2] + x$logit_phi ), add=TRUE , col=colz3[i] , adj=0.8)
      shade( density(logistic(x$G[,i,2] + x$logit_phi )) , PCI(exp(x$G[,i,2] + x$logit_phi) , prob=0.9999) , col=col.alpha(colz3[i], 0.4) )
    }
    legend("topright", c("BA","BG","CG","MA","NB") ,fill=colz3 , bty='n')
  }
}

DensGamma <- function(x, prior=TRUE , individual=FALSE , group=FALSE , age_sex=TRUE , unknown=TRUE  ){
  dens(logistic(x$logit_gamma) , ylim=c(0,12) , xlim=c(0,0.8) , adj=.9 , col="white", yaxt='n' )
  if(age_sex==TRUE){
    u <- ifelse(unknown==TRUE , 3 , 2 )
    for(sex in 1:u){
      for(age in 1:u){
        if(sex==1) colz=brewer.pal(6,"Reds")[c(2,4,6)]
        if(sex==2) colz=brewer.pal(6,"Blues")[c(2,4,6)]
        if(sex==3) colz=brewer.pal(6,"Purples")[c(2,4,6)]
        dens(logistic(x$logit_gamma[,age,sex]) , add=TRUE , adj=.9 , col=colz[age] , lw=2)
        shade( density(logistic(x$logit_gamma[,age,sex])) , PCI(logistic(x$logit_gamma[,age,sex]) , prob=0.9999) , col=col.alpha(colz[age], 0.5) )
      }
    }
    if(unknown==TRUE) legend("topright" , c("AF","JF","UF","AM","JM","UM","AU","JU","UU"),fill=c(brewer.pal(6,"Reds")[c(2,4,6)], brewer.pal(6,"Blues")[c(2,4,6)] , brewer.pal(6,"Purples")[c(2,4,6)]) , bty='n')
    if(unknown==FALSE) legend("topright" , c("AF","JF","AM","JM"),fill=c(brewer.pal(6,"Reds")[c(2,4)], brewer.pal(6,"Blues")[c(2,4)]) , bty='n')
  }
  
    if(prior==TRUE){
      dens( logistic(rnorm(4000, mean = 0, sd = 1)) , add=TRUE , lty=2 , col="black",adj=1) #prior
    }
  
  if(individual==TRUE){
    for(i in 1:dim(x$gamma_i)[2]){
      dens(x$gamma_i[,i] , add=TRUE , col=col.alpha("black", 0.05) , adj=0.8)
    }
  }
  
  if(group==TRUE){
    colz3 <- brewer.pal(dim(x$G)[2] , "Dark2")
    for(i in 1:dim(x$G)[2]){
        dens(logistic(x$G[,i,3] + x$logit_gamma ) , add=TRUE , col=colz3[i] , adj=0.8)
        shade( density( logistic(x$G[,i,3] + x$logit_gamma )  ) , PCI( logistic(x$G[,i,3] + x$logit_gamma ) , prob=0.9999) , col=col.alpha(colz3[i], 0.5) )
      }
  }
}

DensFc <- function(x, prior=TRUE , individual=FALSE , group=FALSE , age_sex=TRUE , unknown=TRUE  ){
  dens(exp(x$log_f) , ylim=c(0,1.5) , xlim=c(0,5) , adj=.9 , col="white", yaxt='n' )
  if(age_sex==TRUE){
    u <- ifelse(unknown==TRUE , 3 , 2 )
    for(sex in 1:u){
      for(age in 1:u){
        if(sex==1) colz=brewer.pal(6,"Reds")[c(2,4,6)]
        if(sex==2) colz=brewer.pal(6,"Blues")[c(2,4,6)]
        if(sex==3) colz=brewer.pal(6,"Purples")[c(2,4,6)]
        dens(exp(x$log_f[,age,sex]) , add=TRUE , adj=.9 , col=colz[age] , lw=2)
        shade( density(exp(x$log_f[,age,sex])) , PCI(exp(x$log_f[,age,sex]) , prob=0.9999) , col=col.alpha(colz[age], 0.5) )
      }
    }
    if(unknown==TRUE) legend("topright" , c("AF","JF","UF","AM","JM","UM","AU","JU","UU"),fill=c(brewer.pal(6,"Reds")[c(2,4,6)], brewer.pal(6,"Blues")[c(2,4,6)] , brewer.pal(6,"Purples")[c(2,4,6)]) , bty='n')
    if(unknown==FALSE) legend("topright" , c("AF","JF","AM","JM"),fill=c(brewer.pal(6,"Reds")[c(2,4)], brewer.pal(6,"Blues")[c(2,4)]) , bty='n')    
  }
  if(prior==TRUE){
    dens( exp(rnorm(4000, mean = 0, sd = 0.7)) , add=TRUE , lty=2 , col="black",adj=.9) #prior
  }
  if(individual==TRUE){
    for(i in 1:dim(x$fc_i)[2]){
      dens(x$fc_i[,i] , add=TRUE , col=col.alpha("black", 0.05) , adj=0.8)
    }    
  }
  # for(i in 1:dim(x$G)[2]){
  #   dens(exp(x$G[,i,4] + x$log_f ), add=TRUE , col=col.alpha("cyan", 0.25) , adj=0.8)
  # }
}

DensBetaq <- function(x , prior=TRUE , individual=FALSE , group=FALSE , age_sex=TRUE , unknown=TRUE){
  dens(x$betaq , ylim=c(0,0.75) , xlim=c(-4,4) , adj=.9 , col="white", yaxt='n' ) 
  if(age_sex==TRUE){
    u <- ifelse(unknown==TRUE , 3 , 2 )
    for(sex in 1:u){
      for(age in 1:u){
        if(sex==1) colz=brewer.pal(6,"Reds")[c(2,4,6)]
        if(sex==2) colz=brewer.pal(6,"Blues")[c(2,4,6)]
        if(sex==3) colz=brewer.pal(6,"Purples")[c(2,4,6)]
        dens(x$betaq[,age,sex] , add=TRUE , adj=.9 , col=colz[age] , lw=2)
        shade( density(x$betaq[,age,sex]) , PCI(x$betaq[,age,sex] , prob=0.9999) , col=col.alpha(colz[age], 0.5) )
      }
    }
    if(unknown==TRUE) legend("topright" , c("AF","JF","UF","AM","JM","UM","AU","JU","UU"),fill=c(brewer.pal(6,"Reds")[c(2,4,6)], brewer.pal(6,"Blues")[c(2,4,6)] , brewer.pal(6,"Purples")[c(2,4,6)]) , bty='n')
    if(unknown==FALSE) legend("topright" , c("AF","JF","AM","JM"),fill=c(brewer.pal(6,"Reds")[c(2,4)], brewer.pal(6,"Blues")[c(2,4)]) , bty='n')    
    }
  if(prior==TRUE){
    dens( rnorm(4000, mean = 0, sd = 0.8) , add=TRUE , lty=2 , col="black",adj=1) #prior
  }
  if(individual==TRUE){
    for(i in 1:dim(x$betaq_i)[2]){
      dens(x$betaq_i[,i] , add=TRUE , col=col.alpha("black", 0.05) , adj=0.8)
    }
  }
}

DensSigma <- function(x , maintext="sigma_blah"){
  colz2 <- brewer.pal(ncol(x) , "Dark2")
  dens(x[,1] , col=colz2[1] , xlim=c(0,10) , adj=0.9 , main=maintext) # get on this
  for(i in 2:ncol(x) ) {dens(x[,i] , col=colz2[i], add=TRUE , adj=0.9)}
  dens(rexp(nrow(x), rate = 1), add=TRUE , lty=3 , adj=0.9)
  legend("topright" , c("log_lambda" , "logit_phi" , "logit_gamma" , "log_f") , fill=colz2 , bty="n")
}

PlotRenameAgeSex <- function(x){
  library(stringi)
  x <- stri_replace_all_fixed(x , "[1,", "[a,")
  x <- stri_replace_all_fixed(x , "[2,", "[j,")
  x <- stri_replace_all_fixed(x , "[3,", "[u,")
  x <- stri_replace_all_fixed(x , ",1]", ",f]")
  x <- stri_replace_all_fixed(x , ",2]", ",m]")
  x <- stri_replace_all_fixed(x , ",3]", ",u]")
}