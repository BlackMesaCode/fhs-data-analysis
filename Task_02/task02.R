#MT: kurz & schmerzlos - einwandfrei

# data analysis of germanys fossil energy consumption vs renewable energy consumption over time

#MT: / sollte auch in Windows funktionieren
dataset = read.csv2(file="./Task_02/data/merged.csv",head=TRUE,sep=";",dec = ",",
                    colClasses = c(year="numeric", fossil_consumption="numeric", renew_consumption = "numeric"))

# lets look at the data set:
str(dataset)

# column 1: years

# column 3: fossil energy consumption per capita
fossil_consumption = dataset[["fossil_consumption"]]

# column 4: renewable energy consumption per capita
renew_consumption = dataset[["renew_consumption"]]


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

