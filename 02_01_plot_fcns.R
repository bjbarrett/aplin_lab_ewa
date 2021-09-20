#lambda
DensLambda <- function(x){
  dens(x$lambda , col="white" , xlim=c(0,12) , main="lambda" )
  shade( density(x$lambda) , PCI(x$lambda , prob=0.9999) , col=col.alpha("slateblue", 0.5) )
  dens( exp(rnorm(50000, mean = 1, sd = 0.6)) , add=TRUE , lty=2 , col="black") #prior
  for(i in 1:dim(x$lambda_i)[2]){
    dens(x$lambda_i[,i] , add=TRUE , col=col.alpha("slateblue", 0.2))
  }
}

DensPhi <- function(x){
  dens(x$phi , col="white" , xlim=c(0,1) , main="phi" )
  shade( density(x$phi) , PCI(x$phi , prob=0.9999) , col=col.alpha("orange", 0.5) )
  dens( logistic(rnorm(50000, mean = 0, sd = 1)) , add=TRUE , lty=2 , col="black") #prior
  for(i in 1:dim(x$lambda_i)[2]){
    dens(x$phi_i[,i] , add=TRUE , col=col.alpha("orange", 0.2))
  }
}

DensGamma <- function(x){
  dens(x$gamma , col="white" , xlim=c(0,1) , main="gamma" )
  shade( density(x$gamma) , PCI(x$gamma , prob=0.9999) , col=col.alpha("green", 0.5) )
  dens( logistic(rnorm(50000, mean = 0, sd = 1)) , add=TRUE , lty=2 , col="black") #prior
  for(i in 1:dim(x$gamma_i)[2]){
    dens(x$gamma_i[,i] , add=TRUE , col=col.alpha("green", 0.2))
  }
}

DensFc <- function(x){
  dens(x$lambda , col="white" , xlim=c(0,12) , main="f_c" )
  shade( density(x$fc) , PCI(x$fc , prob=0.9999) , col=col.alpha("red", 0.5) )
  dens( exp(rnorm(50000, mean = 0, sd = 0.6)) , add=TRUE , lty=2 , col="black") #prior
  for(i in 1:dim(x$fc_i)[2]){
    dens(x$fc_i[,i] , add=TRUE , col=col.alpha("red", 0.2))
  }
}

DensBetaq <- function(x){
  dens(x$betaq , col="white" , xlim=c(0,12) , main="betaq" )
  shade( density(x$betaq) , PCI(x$betaq , prob=0.9999) , col=col.alpha("violet", 0.5) )
  dens( rnorm(50000, mean = 0, sd = 1) , add=TRUE , lty=2 , col="black") #prior
  for(i in 1:dim(x$betaq_i)[2]){
    dens(x$betaq_i[,i] , add=TRUE , col=col.alpha("violet", 0.2))
  }
}