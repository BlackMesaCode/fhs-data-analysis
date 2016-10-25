dataset = read.csv2(file=".\\Task_02\\data\\Merged GDP and Birth Rate.csv",head=TRUE,sep=";",dec = ",",
                    colClasses = c(country_name="character", country_code="character", gdp_per_capita = "numeric", birth_rate = "numeric"))

# lets look at the data set:
str(dataset)

# row: set of data for each country

# column 3: GDP per Capita
gdp_per_capita = dataset[["gdp_per_capita"]]

# column 4: Birth Rate
birth_rate = dataset[["birth_rate"]]


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

my_median(gdp_per_capita) # -> 5484.067
my_median(birth_rate)     # -> 19.918

# comparing with results from built-in function
summary(gdp_per_capita)
summary(birth_rate)
# looking good :-)



# assumption: the higher the GDP per Capita in a country, the lower the birth rate
# lets see:

cov(gdp_per_capita, birth_rate) # covariance is -109056.6  -> high values of x correspond to low values of y

cor(gdp_per_capita, birth_rate) # correlation is -0.53 -> so there is a moderate negative linear correlation between gdp and birth_rate


plot = plot(gdp_per_capita, birth_rate,pch=2) # we can see that countries with low gdp tend to have a higher birthrate - values drop almost exponentially


