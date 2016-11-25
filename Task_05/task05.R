source("./Task_04/task04.R")

# now lets futher evaluate our linear models:


##################### linear model #1  #####################


# leverage / hatvalues 
linear_model_1.investigation$levereage = hatvalues(linear_model_1)

h.mean.1 = mean(linear_model_1.investigation$levereage) # calculating regular mean of all leverages
h.mean.2 = length(linear_model_1$coefficients) / nrow(dataset)
h.mean.1-h.mean.2  # not quite 0 ... not sure why ...

linear_model_1.investigation$levereage[which(linear_model_1.investigation$rs > 2.0)]  # leverage values whose rs values > 2.0
linear_model_1.investigation$levereage[linear_model_1.investigation$levereage > 2*h.mean.2] # 3 values > threshold
linear_model_1.investigation$levereage[linear_model_1.investigation$levereage > 3*h.mean.2] # 0 values > threshold


# cooks distance
linear_model_1.investigation$cook.distance = cooks.distance(linear_model_1)
linear_model_1.investigation$cook.distance[which(linear_model_1.investigation$cook.distance > 0.5)] # 1 value > threshold

View(linear_model_1.investigation)


# standardized residuals 
# i already compared those in task 04


##################### linear model #2  #####################

# leverage / hatvalues 
linear_model_2.investigation$levereage = hatvalues(linear_model_2)

h.mean.1 = mean(linear_model_2.investigation$levereage) # calculating regular mean of all leverages
h.mean.2 = length(linear_model_2$coefficients) / nrow(dataset)
h.mean.1-h.mean.2  # not quite 0 ... not sure why ...

linear_model_2.investigation$levereage[which(linear_model_2.investigation$rs > 2.0)]  # leverage values whose rs values > 2.0
linear_model_2.investigation$levereage[linear_model_2.investigation$levereage > 2*h.mean.2] # 2 values > threshold
linear_model_2.investigation$levereage[linear_model_2.investigation$levereage > 3*h.mean.2] # 0 values > threshold


# cooks distance
linear_model_2.investigation$cook.distance = cooks.distance(linear_model_2)
linear_model_2.investigation$cook.distance[which(linear_model_2.investigation$cook.distance > 0.5)] # 0 value > threshold

View(linear_model_2.investigation)


# standardized residuals 
# i already compared those in task 04


##################### linear model #3  #####################

# leverage / hatvalues 
linear_model_3.investigation$levereage = hatvalues(linear_model_3)

h.mean.1 = mean(linear_model_3.investigation$levereage) # calculating regular mean of all leverages
h.mean.2 = length(linear_model_3$coefficients) / nrow(dataset)
h.mean.1-h.mean.2  # not quite 0 ... not sure why ...

linear_model_3.investigation$levereage[which(linear_model_3.investigation$rs > 2.0)]  # leverage values whose rs values > 2.0
linear_model_3.investigation$levereage[linear_model_3.investigation$levereage > 2*h.mean.2] # 0 values > threshold
linear_model_3.investigation$levereage[linear_model_3.investigation$levereage > 3*h.mean.2] # 0 values > threshold


# cooks distance
linear_model_3.investigation$cook.distance = cooks.distance(linear_model_3)
linear_model_3.investigation$cook.distance[which(linear_model_3.investigation$cook.distance > 0.5)] # 1 value > threshold

View(linear_model_3.investigation)


# standardized residuals 
# i already compared those in task 04


if ("car" %in% rownames(installed.packages()) == T) {
  print("car already installed")
} else {
  install.packages("car")
  library(car)
}


##################### Multicollinearity analyzation  #####################

vif(linear_model_1)
vif(linear_model_2)
vif(linear_model_3)

View(cor(dataset))

##################### Heteroscedasticity  #####################

dev.off()

plot(linear_model_1)
plot(linear_model_2)
plot(linear_model_3)





# the pdf file describing Heteroscedasticity and Multicollinearity resides also in the Task_05 folder within git