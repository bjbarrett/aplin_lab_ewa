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
