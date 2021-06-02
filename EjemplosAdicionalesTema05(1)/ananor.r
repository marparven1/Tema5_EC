
ananor<-function(x)	
{
 par(mfrow=c(2,2))
 n<-length(x)
 qqnorm(x,main=paste("Graf. Normal de Prob. n=",n))
 qqline(x)
 boxplot(x,main="")
 hist(x,col="red",xlim=c(min(x),max(x)),br=30,prob=TRUE)
 curve(dnorm(x,mean(x),sd(x)),min(x),max(x),1000,col="blue",
 add=TRUE,lwd=2)
 plot(ecdf(x), do.points=FALSE, verticals=TRUE)
 curve(pnorm(x,mean(x),sd(x)),min(x),max(x),1000,col="blue",
 add=TRUE,lwd=2)
 par(mfrow=c(1,1))
 shapiro.test(x)
}
