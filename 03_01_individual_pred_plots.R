#individual learning <- 
post <- extract(fit_i)
x<- apply(post$PrPreds[,,1] , 2 , mean)
str(x)
points(x[1:100], nrow(x[1:100]) , cex=.5 , )
