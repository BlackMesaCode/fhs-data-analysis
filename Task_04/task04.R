data_folder =  "./Task_04/data/"

# dependand variable
gdp_path = file.path(data_folder, "gdp.csv")

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

# transpose (transposing causes the dataset to be converted to a matrix, so we have to convert it back to a dataframe afterwards)
dataset = t(dataset)
dataset = as.data.frame(dataset)

# get row and column naming right
colnames(dataset) = c("gdp", "co2", "electricity", "energy", "gnp", "greenhousegas", "hightechexports", "internetusers")
new_row_names = as.numeric(sapply(rownames(dataset), function(x) { substr(x,2,nchar(x)) }))
rownames(dataset) = new_row_names

# getting the final vectors for further processing
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
linear_model_1 = lm(gdp ~ co2 + greenhousegas + gnp, data=dataset_head)

predicted_values_1 = predict(linear_model_1, data.frame(co2=dataset_tail$co2, 
                                                      greenhousegas=dataset_tail$greenhousegas, 
                                                      gnp=dataset_tail$gnp))

# drawing all of our datapoints
plot(x=rownames(dataset),y=dataset$gdp,pch=16, col="grey")

# drawing the predicted values for the last three years using the linear model
points(x=tail(rownames(dataset), 3), y = predicted_values_1, pch=4, col="red")

#drawing the residuals
segments(as.numeric(tail(rownames(dataset), 3)), 
         dataset_tail$gdp, 
         as.numeric(tail(rownames(dataset), 3)), 
         predicted_values_1, col="red")


linear_model_1



##################### linear model #2  ##################### 

linear_model_2 = lm(gdp ~ energy + electricity, data=as.data.frame(dataset_head))

predicted_values_2 = predict(linear_model_2, data.frame(energy=dataset_tail$energy, 
                                                        electricity=dataset_tail$electricity))

plot(x=rownames(dataset),y=dataset$gdp,pch=16, col="grey")

points(x=tail(rownames(dataset), 3), y = predicted_values_2, pch=4, col="blue")

segments(as.numeric(tail(rownames(dataset), 3)), 
         dataset_tail$gdp, 
         as.numeric(tail(rownames(dataset), 3)), 
         predicted_values_2, col="red")




##################### linear model #3  #####################

linear_model_3 = lm(gdp ~ hightechexports + internetusers, data=as.data.frame(dataset_head))

predicted_values_3 = predict(linear_model_3, data.frame(hightechexports=dataset_tail$hightechexports, 
                                                        internetusers=dataset_tail$internetusers))

plot(x=rownames(dataset),y=dataset$gdp,pch=16, col="grey")

points(x=tail(rownames(dataset), 3), y = predicted_values_3, pch=4, col="magenta")

segments(as.numeric(tail(rownames(dataset), 3)), 
         dataset_tail$gdp, 
         as.numeric(tail(rownames(dataset), 3)), 
         predicted_values_3, col="red")


