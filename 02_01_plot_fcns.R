library(RColorBrewer)
#lambda
DensLambda <- function(x , prior=TRUE , individual=FALSE , group=FALSE , age_sex=TRUE){
  dens(exp(x$log_lambda) , ylim=c(0,1.75) , xlim=c(0,8) , adj=.9 , col=col.alpha("white", 0.01) , main="lambda") # all lambdas
  if(age_sex==TRUE){
    for(sex in 1:2){
      for(age in 1:2){
        if(sex==1) colz=c("salmon4","salmon2" )
        if(sex==2) colz=c("royalblue4","royalblue2" )
        dens(exp(x$log_lambda[,age,sex]) , add=TRUE , adj=.9 , col=colz[age] , lw=2)
        shade( density(exp(x$log_lambda[,age,sex])) , PCI(exp(x$log_lambda[,age,sex]) , prob=0.9999) , col=col.alpha(colz[age], 0.5) )
        }
    }
    legend("topright" , c("AF","JF","AM","JM"),fill=c("salmon4","salmon2","royalblue4","royalblue2" ) , bty='n')
  }
  
  if(prior==TRUE){
  dens( exp(rnorm(50000, mean = 1, sd = 0.6)) , add=TRUE , lty=2 , col="black",adj=1) #prior
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
    shade( density(exp(x$G[,i,1] + x$log_lambda )) , PCI(exp(x$G[,i,1] + x$log_lambda ) , prob=0.9999) , col=col.alpha(colz3[i], 0.5) )
    
  }
  legend("topright", c("BA","BG","CG","MA","NB") ,fill=colz3 , bty='n')
  #legend("topright" , c("AF","JF","AM","JM"),fill=c("salmon4","salmon2","royalblue4","royalblue2" ) , bty='n')
  
 }
  
}

DensPhi <- function(x, prior=TRUE , individual=FALSE , group=FALSE , age_sex=TRUE){
  dens(logistic(x$logit_phi) , ylim=c(0,20) , xlim=c(0,1) , adj=.9 , col="white", main="phi") # all lambdas
if(age_sex==TRUE){
  for(sex in 1:2){
    for(age in 1:2){
      if(sex==1) colz=c("salmon4","salmon2" )
      if(sex==2) colz=c("royalblue4","royalblue2" )
      dens(logistic(x$logit_phi[,age,sex]) , add=TRUE , adj=.9 , col=colz[age] , lw=2)
      shade( density(logistic(x$logit_phi[,age,sex])) , PCI(logistic(x$logit_phi[,age,sex]) , prob=0.9999) , col=col.alpha(colz[age], 0.5) )
    }
  }
  legend("topright" , c("AF","JF","AM","JM"),fill=c("salmon4","salmon2","royalblue4","royalblue2" ) , bty='n')
  
}

  if(prior==TRUE){
    dens( logistic(rnorm(50000, mean = 0, sd = 1)) , add=TRUE , lty=2 , col="black",adj=1) #prior
  }
  if(individual==TRUE){
    for(i in 1:dim(x$phi_i)[2]){
      dens(x$phi_i[,i] , add=TRUE , col=col.alpha("black", 0.05) , adj=0.8)
    }
  }
}

DensGamma <- function(x, prior=TRUE , individual=FALSE , group=FALSE , age_sex=TRUE){
  dens(logistic(x$logit_gamma) , ylim=c(0,8) , xlim=c(0,1) , adj=.9 , col="white", main="gamma") # all lambdas
  for(sex in 1:2){
    for(age in 1:2){
      if(sex==1) colz=c("salmon4","salmon2" )
      if(sex==2) colz=c("royalblue4","royalblue2" )
      dens(logistic(x$logit_gamma[,age,sex]) , add=TRUE , adj=.9 , col=colz[age] , lw=2)
      shade( density(logistic(x$logit_gamma[,age,sex])) , PCI(logistic(x$logit_gamma[,age,sex]) , prob=0.9999) , col=col.alpha(colz[age], 0.5) )
    }
  }
  legend("topright" , c("AF","JF","AM","JM"),fill=c("salmon4","salmon2","royalblue4","royalblue2" ) , bty='n')
  dens( logistic(rnorm(50000, mean = 0, sd = 1)) , add=TRUE , lty=2 , col="black",adj=1) #prior
  
  for(i in 1:dim(x$gamma_i)[2]){
    dens(x$gamma_i[,i] , add=TRUE , col=col.alpha("black", 0.05) , adj=0.8)
  }
  for(i in 1:dim(x$G)[2]){
    dens( logistic(x$G[,i,3] + x$logit_gamma ), add=TRUE , col=col.alpha("cyan", 0.25) , adj=0.8)
  }
}

DensFc <- function(x, prior=TRUE , individual=FALSE , group=FALSE , age_sex=TRUE){
  dens(exp(x$log_f) , ylim=c(0,1.75) , xlim=c(0,5) , adj=.9 , col="white" ,main="f") # all lambdas
  if(age_sex==TRUE){
    for(sex in 1:2){
      for(age in 1:2){
        if(sex==1) colz=c("salmon4","salmon2" )
        if(sex==2) colz=c("royalblue4","royalblue2" )
        dens(exp(x$log_f[,age,sex]) , add=TRUE , adj=.9 , col=colz[age] , lw=2)
        shade( density(exp(x$log_f[,age,sex])) , PCI(exp(x$log_f[,age,sex]) , prob=0.9999) , col=col.alpha(colz[age], 0.5) )
      }
    }
  }
  for(sex in 1:2){
    for(age in 1:2){
      if(sex==1) colz=c("salmon4","salmon2" )
      if(sex==2) colz=c("royalblue4","royalblue2" )
      dens(exp(x$log_f[,age,sex]) , add=TRUE , adj=.9 , col=colz[age] , lw=2)
      shade( density(exp(x$log_f[,age,sex])) , PCI(exp(x$log_f[,age,sex]) , prob=0.9999) , col=col.alpha(colz[age], 0.5) )
    }
  }
  legend("topright" , c("AF","JF","AM","JM"),fill=c("salmon4","salmon2","royalblue4","royalblue2" ) , bty='n')
  if(prior==TRUE){
    dens( exp(rnorm(50000, mean = 0, sd = 1)) , add=TRUE , lty=2 , col="black",adj=1) #prior
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

DensBetaq <- function(x){
  dens(x$betaq , ylim=c(0,1.75) , xlim=c(0,5) , adj=.9 , col="white" ,main="beta") # all lambdas
  for(sex in 1:2){
    for(age in 1:2){
      if(sex==1) colz=c("salmon4","salmon2" )
      if(sex==2) colz=c("royalblue4","royalblue2" )
      dens(x$betaq[,age,sex] , add=TRUE , adj=.9 , col=colz[age] , lw=2)
      shade( density(x$betaq[,age,sex]) , PCI(x$betaq[,age,sex] , prob=0.9999) , col=col.alpha(colz[age], 0.5) )
    }
  }
  legend("topright" , c("AF","JF","AM","JM"),fill=c("salmon4","salmon2","royalblue4","royalblue2" ) , bty='n')
  dens( rnorm(50000, mean = 0, sd = 0.8) , add=TRUE , lty=2 , col="black",adj=1) #prior
  
  for(i in 1:dim(x$fc_i)[2]){
    dens(x$betaq_i[,i] , add=TRUE , col=col.alpha("black", 0.05) , adj=0.8)
  }
  
  # for(i in 1:dim(x$G)[2]){
  #   dens(exp(x$G[,i,4] + x$log_f ), add=TRUE , col=col.alpha("cyan", 0.25) , adj=0.8)
  # }
}

DensSigma <- function(x){
  colz2 <- brewer.pal(ncol(x) , "Dark2")
  dens(x[,1] , col=colz2[1] , xlim=c(0,4) , adj=0.9 , main="sigma_blah") # get on this
  for(i in 2:ncol(x) ) {dens(x[,i] , col=colz2[i], add=TRUE , adj=0.9)}
  dens(rexp(nrow(x), rate = 1), add=TRUE , lty=3 , adj=1)
  legend("topright" , c("log_lambda" , "logit_phi" , "logit_gamma" , "log_f") , fill=colz2)
}