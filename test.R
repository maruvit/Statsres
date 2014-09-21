set.seed <- 21
lamda <- 0.2
no.sim <- 1000
no.samp<-40
simu<-matrix(rexp(no.sim*no.samp, rate = lamda),no.sim,no.samp)
row.means <- rowMeans(simu)

hist(row.means, breaks=50, prob=TRUE,main="Distribution of averages of samples,
from exponential with lambda=0.2",xlab="Mean")

# density of the averages of samples
lines(density(row.means))
# theoretical center of distribution
abline(v=1/lamda, col="red")
# theoretical density of the averages of samples
x <- seq(min(row.means), max(row.means), length=100)
y <- dnorm(x, mean=1/lamda, sd=(1/lamda/sqrt(no.samp)))
lines(x, y, pch=20, col="red", lty=2)
# add legend
legend('topright', c("simulation", "theoretical"), lty=c(1,2), 
       col=c("black", "red"))

qqnorm(row.means); qqline(row.means)

lamda_vals <- seq(4, 6, by=0.01)
coverage <- sapply(lamda_vals, function(lamb) {
    mu_hats <- rowMeans(matrix(rexp(no.samp*no.sim, rate=0.2),
                               no.sim, no.samp))
    ll <- mu_hats - qnorm(0.975) * sqrt(1/lamda**2/no.samp)
    ul <- mu_hats + qnorm(0.975) * sqrt(1/lamda**2/no.samp)
    mean(ll < lamb & ul > lamb)
})

library(ggplot2)
qplot(lamda_vals, coverage) + geom_hline(yintercept=0.95)
