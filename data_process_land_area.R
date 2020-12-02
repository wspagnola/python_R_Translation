



####

#### Test merge

library(MEMSS)
library(tidyr)
library(dplyr)



#### Election Data ####


#MIT Election Data and Science Lab, 2018, "County Presidential Election Returns 2000-2016", 
#https://doi.org/10.7910/DVN/VOQCHQ, Harvard Dataverse, V6, UNF:6:ZZe1xuZ5H2l4NUiSRcRf8Q== [fileUNF]
# Downloaded on October 22, 2020

#https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ/HEIJCQ&version=6.0
#https://electionlab.mit.edu/data


elect_raw <- read.csv('data/countypres_2000-2016.csv')

elect_2016_long <- elect_raw[elect_raw$year  == 2016 , ]

elect_2016_long$voteprop <- elect_2016_long$candidatevotes / elect_2016_long$totalvotes

elect_2016_long %>% 
    select(- office, -version) %>%
    head()

# Note can use refactor instead
elect_2016_long[is.na(elect_2016_long$party) ,]$party <- "other"#

filter_other <- elect_2016_long$party != 'other'
elect_2016_long[filter_other, ]$party <- substr(elect_2016_long[filter_other , ]$party, 1, 3)

elect_2016_long %>% 
  select(- office, -version) %>%
  head()

elect_2016 <- elect_2016_long %>% 
                  select(-candidate,  -office, -version) %>% 
                  pivot_wider(names_from = party, values_from = c(candidatevotes, voteprop)) 
                
                


#### Census Data: Population, Urbanicity ####

library(tidycensus)
#Note: need to get API key

county_pop_2016_raw <- get_estimates(geography = 'county', product = 'population', year = 2016)
head(county_pop_2016_raw)

names(county_pop_2016_raw)
unique(county_pop_2016_raw$variable)

county_density_2016_raw<- county_pop_2016_raw[county_pop_2016_raw$variable == 'DENSITY' ,]


county_density_2016_raw$county <- vapply(county_density_2016_raw$NAME, 
                                     function(x)gsub("\\,\\s.+", "", x), 
                                     FUN.VALUE = character(1), 
                                     USE.NAMES = FALSE)

county_density_2016_raw$state <- vapply(county_density_2016_raw$NAME, 
                                    function(x) gsub ("^.+\\,\\s", "", x),
                                    FUN.VALUE = character(1),
                                    USE.NAMES = FALSE)



county_density_2016 <- county_density_2016_raw %>%  select(-NAME, -variable)
names(county_density_2016)[2] <- 'density'
names(county_density_2016)

county_density_2016 %>% head()



#### Merge data

library(ggplot2)

names(elect_2016)
merged_df <- merge(elect_2016, county_density_2016, by.x = 'FIPS', by.y = 'GEOID')
merged_df$log10_density <-  log10(merged_df$density)
merged_df$log_density <-  log(merged_df$density)

merged_df %>% slice(1:100) %>%  View()



names(merged_df)

# linear fit
merged_df %>% 
  ggplot(aes(x = log_density, y = voteprop_dem)) +
  geom_point() +
  geom_smooth(method = 'lm', formula =y ~ x)

merged_df %>% 
  ggplot(aes(x = log_density, y = voteprop_dem)) +
  geom_point() +
  geom_smooth(method = 'lm', formula =y ~ poly(x, 2))

merged_df %>% 
  ggplot(aes(x = log_density, y = voteprop_dem)) +
  geom_point() +
  geom_smooth(method = 'lm', formula =y ~ poly(x, 2))
?geom_smooth
?lm
fit <- lm(voteprop_dem ~ log(density), data = merged_df)
summary(fit)


fit_quad<- lm(voteprop_dem ~ poly(log_density, 2), data = merged_df)
summary(fit_quad)


#### Rat Pup Data ####

# Good Dataset for linear mixed models
rat <- MEMSS::RatPupWeight
names(rat)
head(rat)


# Note: Treatment administered at Liter level
# Note: would need to control for Liter and liter size
# Note: Liter/liter  size may be associated with both Sex and Weight
rat_mean_wt <- with(rat, aggregate(weight, by=list(sex, Treatment), mean  ))

names(rat_mean_wt) <- c('Sex', 'Treatment', 'Weight')


df <- rat_mean_wt %>% pivot_wider(id_cols = Sex, names_from =Treatment, values_from = Weight)

df %>% 
  dplyr::select(Sex, High, Low, Control) %>% 
  mutate(High_effect = High - Control,
         Low_effect = Low - Control) %>% 
  mutate_if(is.numeric,~ round(.x, 2)) %>%   View

?mutate_if
#### Penguins ####


library(palmerpenguins)

pen <- palmerpenguins::penguins

#### Bacteria ####

library(MASS)
bact <- data(bacteria)




# Note: Drop variable, NAME columns

#### Test merge

county_pop_2018[county_pop_2018$NAME == 'Crawford County, Missouri' ,]
county_area_2010[county_area_2010$County == 'Crawford County', ]
head(county_pop_2018)
head(county_area_2010)
names(county_pop_2018)
names(county_area_2010)
merged <- base::merge(county_pop_2018, county_area_2010, by.x='GEOID', by.y = 'FIPS', all = TRUE)
View(merged)
