#original -start

#MT: kurz & schmerzlos - einwandfrei

# data analysis of germanys fossil energy consumption vs renewable energy consumption over time

#MT: / sollte auch in Windows funktionieren
# dataset = read.csv2(file="./Task_02/data/merged.csv",head=TRUE,sep=";",dec = ",", colClasses = c(year="numeric", fossil_consumption="numeric", renew_consumption = "numeric"))

#original -end




# revision -start

# okay now lets try to read the raw csv files from worldbank instead of an excel prepared csv 

data_folder =  "./Task_03/data/"
renew_path = file.path(data_folder, "API_EG.FEC.RNEW.ZS_DS2_en_csv_v2.csv")
fossil_path = file.path(data_folder, "API_EG.USE.COMM.FO.ZS_DS2_en_csv_v2.csv")


renew_dataset = read.csv2(file=renew_path, head=TRUE,sep=",",dec = ".", skip = 4) # skip the first 4 lines of garbage
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


# results further below are still the same as they were with task02 - so i guess we can trash excel now :)

# as stated in the task description we should also examine if we could make use of factors():  
# as for my dataset, factors arent really applicaple as we are only examine the cardinal scaled data

# country codes and names are getting initliazed as factors (by read.csv()) but all we do is just selecting that one country row, 
# we are interested in - we might as well import the country names as characters - no difference in that scenario


# revision -end






#original -start


#  Der Median einer Auflistung von Zahlenwerten ist der Wert, der an der mittleren (zentralen) Stelle steht, 
#  wenn man die Werte der Größe nach sortiert.
my_median = function(data) {
  
  sorted_data = sort(data)
  data_size = length(sorted_data)
  
  if (data_size %% 2 == 0) # even
  {
    center_left = data_size / 2
    center_right = (data_size / 2) + 1
    return((sorted_data[center_left] + sorted_data[center_right]) / 2)
  }
  else { # odd
    center = (data_size + 1) / 2
    return(sorted_data[center])
  }
  
}

my_median(fossil_consumption) # -> 83.60503
my_median(renew_consumption)     # -> 3.881178

#MT: SanitCheck: sehr gut!
# comparing with results from built-in function
summary(fossil_consumption)
summary(renew_consumption)
# looking good :-)


cov(fossil_consumption, renew_consumption) # covariance is -7.7  -> high values of x correspond to low values of y

cor(fossil_consumption, renew_consumption) # correlation is -0.94 -> so there is a strong negative linear correlation between fossil and renewable energy consumption


plot = plot(fossil_consumption, renew_consumption,pch=2) # reneable energy consumption increased while fossile energy consumption decreased

#original -end
