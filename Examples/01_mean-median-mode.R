#MEAN

#creating Vector and filling it with random numbers

#Help Function
?sample
data=sample(x=101,size=100,T)

#Display Data
data
View(data)
?iris
View(iris)


#Compute the mean for data

#Definition for-Loop
for(i in 1:n){
  
}

sum = 0

#iterating through the vector's values and summing them up
for(i in 1:length(data)){
  sum = sum +data[i]
}

#computing the mean
mean = sum / length(data)
mean

mean(data)


#Median vs Mean
data=sample(101,100,T)

#creates a seqeuenc of entries from 1 to lenght of data
seq(1:length(data))

#Plots the data as points
plot(seq(1:length(data)), data)
?plot

#shows a summary of the data
summary(data)


#What happens with mean & median if we manipulate the data?
data[1] = max(data)*2
data
plot(seq(1:length(data)), data)
summary(data)

data[1] = max(data)*10
plot(seq(1:length(data)), data)
summary(data)

data[1] = max(data)*100
plot(seq(1:length(data)), data)
summary(data)

#Mean vs. Median 2
anscombe

?anscombe
xVal = c(1:length(anscombe$x1))
par(mfrow=c(2,2))
colnames(anscombe)
anscombe[1]
anscombe["x1"]
anscombe$x1

?pch

#col->color; pch->symbol; cex->size
plot(xVal, anscombe$x1, col="blue", pch=16,cex=2)
plot(xVal, anscombe$x2, col="blue", pch=16,cex=2)
plot(xVal, anscombe$x3, col="blue", pch=16,cex=2)
plot(xVal, anscombe$x4, col="blue", pch=16,cex=2)


summary(anscombe$x1)
summary(anscombe$x2)
summary(anscombe$x3)
summary(anscombe$x4)


#Mode
mode(data) #doesn't work.. mmhh...



#Quantiles
?quantile

data = seq(1:10)
data

quantile(data,0.25)

quantile(data,0.5)
median(data)

quantile(data,0.25)
