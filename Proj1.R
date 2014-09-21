exp <- 40
lamda <- 0.2
m<-numeric()
for(i in 1:1000){
   a<-rexp(exp,lamda) 
   m<-c(m,mean(a))
}

ms<-mean(a)
print(paste("Theoritical Mean =",1/lamda,"while the simulated mean =",ms))
vs<-var(a)
print(paste("Theoritical Variance =",1/(lamda*lamda),
            "while the simulated variance =",vs))

png(filename = "plot1.png")
hist(m, col="blue",main="Histogram of means of 1000 simulations",
     xlab="Mean of 40 exponential with lamda = 0.2")
abline(v=1/lamda,col="red",lwd=2)
abline(v=ms,col="green",lwd=2)
legend("topright",col=c("Red","Green"),
       legend=c("Theoritical Mean","Simulated Mean"),lwd=1,bty="n")
dev.off()
