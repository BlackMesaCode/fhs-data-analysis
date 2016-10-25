y1 = seq(1:10)
y2 = seq(1:10)

x = seq(1:10)

var(y1,y2)
plot = plot(x=x, y=y1, pch=3)
plot = plot+points(y2, pch=2)
plot

#reversing the vectors
y1 = rev(y1)

#plotting y1 and y2 as points
plot(x =x,y=y1, pch=3)+points(y2, pch=2)
cov(y1,y2)


y1 = rep(1,10)
y2 = rep(1,10)
plot(x =x,y=y1, pch=3)+points(y2, pch=2)
cov(y1,y2)

#creating Vector and filling it with random numbers
y1=sample(101,100,T)
y2=sample(101,100,T)

plot(x=c(1:length(y1)),y=y1, pch=3)+points(y2, pch=2)
cov(y1,y2)
