symmetricData = rnorm(10000,50,5)
rightSkewedData = rbeta(10000,5,2)
leftSkewedData = rbeta(10000,2,5)

#transforming the dat
map = function(x, newMin, newMax){
  x = x -min(x)
  x = x / max(x)
  x = x * (newMax-newMin)
  x + newMin
}
symmetricData = map(symmetricData,0,100)
rightSkewedData = map(rightSkewedData,0,100)
leftSkewedData = map(leftSkewedData,0,100)


#plotting histograms
hist(symmetricData,20)
hist(rightSkewedData,20)
hist(leftSkewedData,20)

summary(symmetricData)
summary(leftSkewedData)
summary(rightSkewedData)

#plotting boxplot
#cbind: binds column-wise
boxplot(x = cbind(symmetricData,rightSkewedData,leftSkewedData))
