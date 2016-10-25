
x = rep(1,10)
var(x)

x[1] = 100

var(x)
sd(x)

sqrt(var(x))

sd1 = rnorm(n=10000,mean=50,sd=1)
sd2 = rnorm(n=10000,mean=50,sd=2)
sd4 = rnorm(n=10000,mean=50,sd=4)


hist(sd1,ceiling(max(sd1) - min(sd1)/10))
hist(sd2,ceiling(max(sd2) - min(sd2)/10))
hist(sd4,ceiling(max(sd4) - min(sd4)/10))

boxplot(x=cbind(sd1,sd2,sd4))

