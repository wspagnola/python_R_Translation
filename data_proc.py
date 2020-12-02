

# Note: Are these modules only needed for R markdown files?
#import reticulate
#use_condaenv()


import pandas as pd
from pandas_datareader import data as pdr
import datetime



#### Yahoo Stock Time Series 2020  #####


# From https://pypi.org/project/pandas-datareader/
  
  
start = datetime.datetime(2020, 1, 1)
end = datetime.datetime(2020, 10, 1)


# Note:Should save this data as CSV !!!!
yahoo = pdr.get_data_yahoo('iex', start, end)


yahoo.info()


start = datetime.datetime(2020, 1, 1)
end = datetime.datetime(2020, 10, 1)

yahoo = pdr.get_data_yahoo('iex', start, end)

# Note write.csv from R is to_csv() in Python

## Format df.to_csv(path)
yahoo.to_csv('data/yahoo_timeseries_2020.csv')


