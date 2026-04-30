x<-seq(0,50,by=0.01)
y<-pnorm((2.32625-1.95996)/x)
plot(x,y,type="l",xlab="Replication noise",ylab="Reproducible probability")
abline(h=0.5,col="Red")
legend("topright",
       legend=c("s=1","p=0.5"),
       col=c("red","black"),
       lty=c(1,1))