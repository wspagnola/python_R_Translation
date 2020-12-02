


library(tidyverse)





#### World Bank GDP Data Wide ####

# Downloaded from https://databank.worldbank.org/indicator/NY.GDP.PCAP.CD/1ff4a498/Popular-Indicators#
# On October 22, 20202
 

#Load in data
## Note: read_csv allows you to specify ".." as missing
## Note: read_csv correctly identifies year columns as double; read.csv() coerced them to character because of '..'

gdp_raw <- read_csv('data/raw/worldbank_gdp_raw.csv', na = c("", ".."))
gdp_raw %>%  glimpse

# Change all column names to lowercase and replace all intermediate blank spaces with underscores
names(gdp_raw) <- names(gdp_raw) %>%  tolower %>%  str_replace(' ', '_')


# nNote: last two rows contain metadata
meta_data_rows <- gdp_raw[is.na(gdp_raw$country_code) & !is.na(gdp_raw$series_name) , ]
meta_data_rows[, 1:4]

# Remove rows with blank country codes

gdp_raw <- gdp_raw[!is.na(gdp_raw$country_code) , ]
gdp_raw %>% head

# Make sure these columns all refer to the same indicator (i.e. GDP)
gdp_raw$series_name %>%  table()
gdp_raw$series_code %>%  table()
gdp_wide <-  gdp_raw[, 3:ncol(gdp_raw)] # Remove "Series" variables

# Clean the year columns
names(gdp_wide) <- names(gdp_wide) %>%  str_remove('\\d+_\\[') %>%  str_remove('\\]$')
names(gdp_wide)

head(gdp_wide)
str(gdp_wide)
#View(gdp_wide)

write.csv(gdp_wide, 'data/gdp_wide.csv', row.names = FALSE)

# Use pivot wider to convert year data rows into two columns:  year/GDP
gdp_long <- gdp_wide %>%  
    pivot_longer(cols =starts_with('yr'), names_to = 'year', values_to = 'GDP_per_capita') %>% 
    mutate(year = as.numeric(str_sub(year, start = 3)) )


gdp_long %>% View

# Linear SCale
gdp_long %>% 
  filter(country_code %in% c('IRL','CHN', 'USA', 'GDR', 'ITA', 'DEU', 'ISR', 'PHL')) %>% 
  ggplot(aes(x=year, y=GDP_per_capita, group = country_code, color = country_code)) +
  geom_line() +
  geom_point()


# Log Scale
gdp_long %>% 
  filter(country_code %in% c('IRL','CHN', 'USA', 'GDR', 'ITA', 'DEU', 'ISR', 'PHL')) %>% 
  ggplot(aes(x=year, y=GDP_per_capita, group = country_code, color = country_code)) +
  geom_line() +
  geom_point() +
  scale_y_log10()



#### Datasets Package in Base R #### 

datasets::ChickWeight %>%  head()


datasets::cars %>%  head()

datasets::uspop %>%  head()


datasets::attitude %>%  head()

datasets::attenu %>%  head()


datasets::beaver1 %>%  slice(1:20)

datasets::beaver2 %>%  slice(1:20)



datasets::BJsales


datasets::euro %>%  slice(1:30)

datasets::InsectSprays %>%  slice(1:30)

datasets::LakeHuron %>%  slice(1:30)


datasets::PlantGrowth %>%  slice(1:20)

datasets::occupationalStatus %>%  slice(1:10)


datasets::faithful %>%  slice(1:10)

datasets::Titanic %>%  as.data.frame
