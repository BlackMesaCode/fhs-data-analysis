data_folder =  "./data"

# dependand variable
gdp_path = file.path(data_folder, "gdp.csv")
gdp_path
# independand variables
co2_path = file.path(data_folder, "co2.csv")
electricity_path = file.path(data_folder, "electricity.csv")
energy_path = file.path(data_folder, "energy.csv")
gnp_path = file.path(data_folder, "gnp.csv")
greenhousegas_path = file.path(data_folder, "greenhousegas.csv")
hightechexports_path = file.path(data_folder, "hightechexports.csv")
internetusers_path = file.path(data_folder, "internetusers.csv")


# get datasets
gdp_dataset = read.csv2(file=gdp_path, head=TRUE,sep=",",dec = ".", skip = 4)

co2_dataset = read.csv2(file=co2_path, head=TRUE,sep=",",dec = ".", skip = 4)
electricity_dataset = read.csv2(file=electricity_path, head=TRUE,sep=",",dec = ".", skip = 4)
energy_dataset = read.csv2(file=energy_path, head=TRUE,sep=",",dec = ".", skip = 4)
gnp_dataset = read.csv2(file=gnp_path, head=TRUE,sep=",",dec = ".", skip = 4)
greenhousegas_dataset = read.csv2(file=greenhousegas_path, head=TRUE,sep=",",dec = ".", skip = 4)
hightechexports_dataset = read.csv2(file=hightechexports_path, head=TRUE,sep=",",dec = ".", skip = 4)
internetusers_dataset = read.csv2(file=internetusers_path, head=TRUE,sep=",",dec = ".", skip = 4, )



# select country germany and only those columns that do not contain NA values

#MT: Sie hätten sich ws leichter getan, wenn Sie zuerst die Daten kombiniert hätten und dann mit complete.cases jene gefiltert hätten für die es in allen Spalten Daten gibt
gdp_germany = gdp_dataset[gdp_dataset$Country.Name == "Germany",]
gdp_germany = gdp_germany[,sapply(gdp_germany, function(x) {is.double(x) && !is.na(x)})]

co2_germany = co2_dataset[co2_dataset$Country.Name == "Germany", ]
co2_germany = co2_germany[,sapply(co2_dataset, function(x) {is.double(x) && !is.na(x)})]

electricity_germany = electricity_dataset[electricity_dataset$Country.Name == "Germany",]
electricity_germany = electricity_germany[,sapply(electricity_germany, function(x) {is.double(x) && !is.na(x)})]

energy_germany = energy_dataset[energy_dataset$Country.Name == "Germany",]
energy_germany = energy_germany[,sapply(energy_germany, function(x) {is.double(x) && !is.na(x)})]

gnp_germany = gnp_dataset[gnp_dataset$Country.Name == "Germany",]
gnp_germany = gnp_germany[,sapply(gnp_germany, function(x) {is.double(x) && !is.na(x)})]

greenhousegas_germany = greenhousegas_dataset[greenhousegas_dataset$Country.Name == "Germany",]
greenhousegas_germany = greenhousegas_germany[,sapply(greenhousegas_germany, function(x) {is.double(x) && !is.na(x)})]

hightechexports_germany = hightechexports_dataset[hightechexports_dataset$Country.Name == "Germany",]
hightechexports_germany = hightechexports_germany[,sapply(hightechexports_germany, function(x) {is.double(x) && !is.na(x)})]

internetusers_germany = internetusers_dataset[internetusers_dataset$Country.Name == "Germany",]
internetusers_germany = internetusers_germany[,sapply(internetusers_germany, function(x) {is.double(x) && !is.na(x)})]



# get the common columns of all datasets
common_cols = Reduce(intersect, list(colnames(gdp_germany), colnames(co2_germany), colnames(electricity_germany), 
                       colnames(energy_germany), colnames(gnp_germany), colnames(greenhousegas_germany), 
                       colnames(hightechexports_germany), colnames(internetusers_germany)))
                     


# selecting only the common columns
#MT: Hier hätten Sie merge() anwenden können
gdp_germany = gdp_germany[, common_cols]
co2_germany = co2_germany[, common_cols]
electricity_germany = electricity_germany[, common_cols]
energy_germany = energy_germany [, common_cols]
gnp_germany = gnp_germany[, common_cols]
greenhousegas_germany = greenhousegas_germany[, common_cols]
hightechexports_germany = hightechexports_germany[, common_cols]
internetusers_germany = internetusers_germany[, common_cols]

# merging into one datasets
dataset = rbind(gdp_germany, co2_germany, electricity_germany, energy_germany, gnp_germany, 
                greenhousegas_germany, hightechexports_germany, internetusers_germany)

#MT: hier wäre ein Sanity-Check noch gut gewesen nachdem Sie hier recht aufwendig die Daten zusammengeführt haben

# transpose (transposing causes the dataset to be converted to a matrix, so we have to convert it back to a dataframe afterwards)
dataset = t(dataset)
dataset = as.data.frame(dataset)

# get row and column naming right
colnames(dataset) = c("gdp", "co2", "electricity", "energy", "gnp", "greenhousegas", "hightechexports", "internetusers")
new_row_names = as.numeric(sapply(rownames(dataset), function(x) { substr(x,2,nchar(x)) }))
rownames(dataset) = new_row_names

# lets have a look at our final, beautifully handcraftet dataset :-)
# View(dataset)

# getting the final vectors for further processing

# MT: Sie könen auch imt  data$gdp auf die spalte zugreifen
# MT: Wofür benötigen Sie die Vektoren? Für die Formual in lm() sind sie nicht nötig, hier werden einfach die colnames() herangezogen
gdp = dataset[,"gdp"] 
co2 = dataset[,"co2"]
electricity = dataset[,"electricity"]
energy = dataset[,"energy"]
gnp = dataset[,"gnp"]
greenhousegas = dataset[,"greenhousegas"]
hightechexports = dataset[,"hightechexports"]
internetusers = dataset[,"internetusers"]

# last 3 rows
dataset_tail = tail(dataset, 3)

# all rows, except the last 3
dataset_head = head(dataset, nrow(dataset)-3)



##################### linear model #1  ##################### 

# our linear model gets to see all the data MINUS the last three years (for which we want to do the prediction)
linear_model_1 = lm(gdp ~ co2 + energy + greenhousegas, data=dataset_head)

predicted_values_1 = predict(linear_model_1, data.frame(co2=dataset$co2, 
                                                      energy=dataset$energy,
                                                      greenhousegas=dataset$greenhousegas))

# drawing all of our datapoints
plot(x=rownames(dataset),y=dataset$gdp,pch=16, col="grey")

# drawing the predicted values using the linear model
points(rownames(dataset), y = predicted_values_1, pch=4, col="red")

#drawing the residuals
segments(as.numeric(rownames(dataset), 3), 
         dataset$gdp, 
         as.numeric(rownames(dataset), 3), 
         predicted_values_1, col="red")


##################### linear model #2  ##################### 

dev.off()

linear_model_2 = lm(gdp ~ gnp + electricity, data=as.data.frame(dataset_head))

#MT: predict mathched die spalten anhand der colnames(); wäre nicht nötig gwesen hier ein Subset der Daten zu verwenden
predicted_values_2 = predict(linear_model_2, data.frame(gnp=dataset$gnp, 
                                                        electricity=dataset$electricity))

plot(x=rownames(dataset),y=dataset$gdp,pch=16, col="grey")

points(x=rownames(dataset), y = predicted_values_2, pch=4, col="blue")

segments(as.numeric(rownames(dataset)), 
         dataset$gdp, 
         as.numeric(rownames(dataset)), 
         predicted_values_2, col="blue")


##################### linear model #3  #####################
dev.off()

linear_model_3 = lm(gdp ~ hightechexports + internetusers, data=as.data.frame(dataset_head))

predicted_values_3 = predict(linear_model_3, data.frame(hightechexports=dataset$hightechexports, 
                                                        internetusers=dataset$internetusers))

plot(x=rownames(dataset),y=dataset$gdp,pch=16, col="grey")

points(x=rownames(dataset), y = predicted_values_3, pch=4, col="magenta")

segments(as.numeric(rownames(dataset)), 
         dataset$gdp, 
         as.numeric(rownames(dataset)), 
         predicted_values_3, col="magenta")



##################### comparing model 1-3 by their residuals #####################


# analyzing the residuals
summary(linear_model_1)
summary(linear_model_2)
summary(linear_model_3)

# Residuals of linear_model_1:
#Min         1Q         Median         3Q        Max 
#-5.510e+11 -1.777e+11 -3.209e+10  7.305e+10  5.906e+11

# as we see, the summaries of our residuals gives us only absolute and very domain specific values, 
# which makes it hard to compare them with other models
# comparison is easier if we divide the residuals by their standard deviation. 
# so lets have a look at the standardized residuals:

# creating a investigation dataframe for linear_model_1
linear_model_1.investigation = data.frame(r = residuals(linear_model_1))
linear_model_1.investigation = cbind(linear_model_1.investigation, rs = rstandard(linear_model_1))

median(linear_model_1.investigation$rs)
max(linear_model_1.investigation$rs)
min(linear_model_1.investigation$rs)


# creating a investigation dataframe for linear_model_2
linear_model_2.investigation = data.frame(r = residuals(linear_model_2))
linear_model_2.investigation = cbind(linear_model_2.investigation, rs = rstandard(linear_model_2))

median(linear_model_2.investigation$rs)
max(linear_model_2.investigation$rs)
min(linear_model_2.investigation$rs)


# creating a investigation dataframe for linear_model_3
linear_model_3.investigation = data.frame(r = residuals(linear_model_3))
linear_model_3.investigation = cbind(linear_model_3.investigation, rs = rstandard(linear_model_3))

median(linear_model_3.investigation$rs)
max(linear_model_3.investigation$rs)
min(linear_model_3.investigation$rs)

#MT: hier haben Sie vergessen die Variablen umzubennen
# comparing the medians with each other
#absolute_rs_medians = abs(c(median(linear_model_1.rs), median(linear_model_2.rs), median(linear_model_3.rs)))
#absolute_rs_medians
#min(absolute_rs_medians) # linear_model_2 has the lowest median of standardized residuals

# comparing the medians with each other
absolute_rs_medians = abs(c(median(linear_model_1.investigation$rs), median(linear_model_2.investigation$rs), median(linear_model_3.investigation$rs)))
absolute_rs_medians
min(absolute_rs_medians) # linear_model_2 has the lowest median of standardized residuals




#standardized residuals greater than 2 for linear_model_1
linear_model_1.investigation$rs.greater.2 = abs(linear_model_1.investigation$rs) > 2
sum(linear_model_1.investigation$rs.greater.2) / nrow(linear_model_1.investigation) # 15% of our data has an rs > 2

#standardized residuals greater than 2.5 for linear_model_1
linear_model_1.investigation$rs.greater.2.5 = abs(linear_model_1.investigation$rs) > 2.5
sum(linear_model_1.investigation$rs.greater.2.5) / nrow(linear_model_1.investigation) # 0% of our data has an rs > 2.5
View(linear_model_1.investigation)




#standardized residuals greater than 2 for linear_model_2
linear_model_2.investigation$rs.greater.2 = abs(linear_model_2.investigation$rs) > 2
sum(linear_model_2.investigation$rs.greater.2) / nrow(linear_model_2.investigation) # 0% of our data has an rs > 2

#standardized residuals greater than 2.5 for linear_model_2
linear_model_2.investigation$rs.greater.2.5 = abs(linear_model_2.investigation$rs) > 2.5
sum(linear_model_2.investigation$rs.greater.2.5) / nrow(linear_model_2.investigation) # 0% of our data has an rs > 2.5
View(linear_model_2.investigation)




#standardized residuals greater than 2 for linear_model_3
linear_model_3.investigation$rs.greater.2 = abs(linear_model_3.investigation$rs) > 2
sum(linear_model_3.investigation$rs.greater.2) / nrow(linear_model_3.investigation) # 5% of our data has an rs > 2

#standardized residuals greater than 2.5 for linear_model_3
linear_model_3.investigation$rs.greater.2.5 = abs(linear_model_3.investigation$rs) > 2.5
sum(linear_model_3.investigation$rs.greater.2.5) / nrow(linear_model_3.investigation) # 0% of our data has an rs > 2.5
View(linear_model_3.investigation)

#MT: feine und aufwendige Evaluierung, sehr gut!


# linear_model_2 seems the most accurate by just looking at the residuals
# it has the lowest median of the standardized residuals, which means it has the lowest error
# moreover it has 0% standardizes residuals that are over 2.0 compared to model 1 (which had 15%) and model 3 (which had 5%)

# linear_model_2 examines the impacts of gnp and electricity(independand variables) to the gdp (dependand variable)
# we can observe a good correlation between gnp and gdp (due to their relative similiar definition)
# moreover we can observe a good correlation beween electricity and gdp (the more value a country is producing the more electricity it needs for its industry ..) 
# even though we have a strong correlation between those values we cant conduct a causal relationship. 
# our gdp doesnt start to grow magically as soon as we increase the electricity consumption. 
# we might just waste it on powering tvs and microwaves ... which surely wont increase our gdp ...


# in task 5 we will further evaluate the characteristics of our linear models

