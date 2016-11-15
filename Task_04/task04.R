data_folder =  "./Task_04/data/"

# DEPENDAND VARIABLE
gdp_path = file.path(data_folder, "gdp.csv")

# INDEPENDAND VARIABLES
co2_path = file.path(data_folder, "co2.csv")
electricity_path = file.path(data_folder, "electricity.csv")
energy_path = file.path(data_folder, "energy.csv")
gnp_path = file.path(data_folder, "gnp.csv")
greenhousegas_path = file.path(data_folder, "greenhousegas.csv")
hightechexports_path = file.path(data_folder, "hightechexports.csv")
internetusers_path = file.path(data_folder, "internetusers.csv")



gdp_dataset = read.csv2(file=gdp_path, head=TRUE,sep=",",dec = ".", skip = 4)

co2_dataset = read.csv2(file=co2_path, head=TRUE,sep=",",dec = ".", skip = 4)
electricity_dataset = read.csv2(file=electricity_path, head=TRUE,sep=",",dec = ".", skip = 4)
energy_dataset = read.csv2(file=energy_path, head=TRUE,sep=",",dec = ".", skip = 4)
gnp_dataset = read.csv2(file=gnp_path, head=TRUE,sep=",",dec = ".", skip = 4)
greenhousegas_dataset = read.csv2(file=greenhousegas_path, head=TRUE,sep=",",dec = ".", skip = 4)
hightechexports_dataset = read.csv2(file=hightechexports_path, head=TRUE,sep=",",dec = ".", skip = 4)
internetusers_dataset = read.csv2(file=internetusers_path, head=TRUE,sep=",",dec = ".", skip = 4)


fossil_dataset = read.csv2(file=fossil_path, head=TRUE,sep=",",dec = ".", skip = 4)

# lets look at the data sets:
str(renew_dataset)
str(fossil_dataset)

# select a country and for that country only the columns that dont contain NA values
renew_germany = renew_dataset[renew_dataset$Country.Name == "Germany", colSums(!is.na(renew_dataset)) > 0] # we could also select the row using subset()
fossil_germany = fossil_dataset[fossil_dataset$Country.Name == "Germany", colSums(!is.na(fossil_dataset)) > 0]

# get the common columns of both datasets
common_cols = intersect(colnames(renew_germany), colnames(fossil_germany))
renew_common = renew_germany[, common_cols]
fossil_common = fossil_germany[, common_cols]

# skip the first four info columns and only select the years
fossil_consumption = fossil_common[,-(1:4)]
renew_consumption = renew_common[,-(1:4)]

# merging the datasets
dataset = rbind(fossil_consumption, renew_consumption)

# transpose
dataset = t(dataset)

# get the naming right
colnames(dataset) = c("fossil_consumption", "renew_consumption")

# transpose transformed the dataframe to a numeric data matrix, which is fine, as our datatypes are homogeneous by now
class(dataset)
mode(dataset)

# getting the vectors for further processing
fossil_consumption = dataset[,"fossil_consumption"]
renew_consumption = dataset[,"renew_consumption"]


#MT: SanitCheck: sehr gut!
# comparing with results from built-in function
summary(fossil_consumption)
summary(renew_consumption)


cov(fossil_consumption, renew_consumption) # covariance is -7.7  -> high values of x correspond to low values of y

cor(fossil_consumption, renew_consumption) # correlation is -0.94 -> so there is a strong negative linear correlation between fossil and renewable energy consumption


plot = plot(fossil_consumption, renew_consumption,pch=2) # reneable energy consumption increased while fossile energy consumption decreased

