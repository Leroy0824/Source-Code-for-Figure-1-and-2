x<-seq(0.1,50,by=0.1)
alpha1<-function(n,x=x){
  pnorm((2.32635/(x*n))-(1.95996/n),mean=0,sd=1)
}
y1<-alpha1(1,x)
plot(x,y1,type="l",col="red",ylim=c(0,1), 
     ylab ="Reproducible probability",
     xlab="standard error")
y2<-alpha1(sqrt(2),x)
lines(x,y2,col="orange")
y3<-alpha1(sqrt(3),x)
lines(x,y3,col="green")
y4<-alpha1(2,x)
lines(x,y4,col="darkgreen")
y10<-alpha1(sqrt(10),x)
lines(x,y10,col="blue")
y100<-alpha1(10,x)
lines(x,y100,col="purple")
legend("topright", 
       legend=c("k=1",expression(k==sqrt(2)),
                expression(k==sqrt(3)),"k=2",
                expression(k==sqrt(10)),"k=10"), 
       col=c("red","orange","green","darkgreen",
             "blue","purple"), 
       lty=c(1,1,1,1,1,1))