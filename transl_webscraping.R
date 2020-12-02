


library(rvest)
library(dplyr)
library(magrittr)
library(stringr)
library(RCurl)
library(httr)



clean_table <- function(x) {
  
  
  row_idx <-  x[, 1] %>% str_detect('Nickname|Motto|Anthem')
  
  # Extract nickname, motto, anthem
  first_rows <- x[row_idx, 1] %>% 
                    str_remove_all('\\(s\\)') %>% 
                    str_remove_all("\"") %>% 
                    str_split(':', n = 2) %>% 
                    lapply( function(x) str_trim(x,side = 'left')) %>% 
                    unlist %>% 
                    matrix( ncol = 2, byrow = TRUE) %>% 
                    as.data.frame

  
  df_2 <- x[8:nrow(x), ]
  
  names(first_rows) <- c('col_name', 'value')
  names(df_2) <- c('col_name', 'value')
  df <- rbind(first_rows, df_2)
  
  # Remove Messy timezone rows
  tz_rows <-  df[, 2] %>%  str_which('UTC')
  df <- df[-tz_rows ,]
  
  # Remove footnotes
  df[, 2] <- df[, 2] %>%  str_remove_all('\\[\\d\\]')
  
  # Transposed data frame
  df_tr <- df[, 2] %>% t %>%  as.data.frame
  
  # Remove leading bullets
  # Remove specific place name for elevation
  # Replace spaces with under scores
  # Correct inconsistence with Official Language(s) columns
  # Remove foot notes
  
    df[, 1] <- df[, 1] %>% 
    str_remove('^\\W\\s')  %>% 
    str_replace('elevation.*$', 'elevation') %>% 
    str_replace('Capital.*$', 'Capital') %>% 
    str_replace('Official languages', 'Official language') %>% 
    str_remove_all('\\[\\d\\]') %>% 
    str_trim('both') %>% 
    str_replace('\\s', '_')
    
    # Add Area suffix to Total to distinguish Total Area from Total Population
    df <- df %>% 
            mutate(col_name = if_else(str_detect(value, '\\ssq\\smi'), 
                                        paste( col_name, 'Area', sep = '_'),
                                        col_name)
    )
  
    names(df_tr) <- df[, 1]
  
    return(df_tr) 
}

#### Create Urls ####

states <- datasets::state.name %>% str_replace('\\s', '_')

# Georgia shares its name with a country
states[10] <- paste0(states[10], '_(U.S._state)')


# New York State and Washington State share names with US cities
fix_states <- c(32, 47)
states[fix_states] <- paste0(states[fix_states], '_(state)')

urls <- paste0('https://en.wikipedia.org/wiki/', states)


#### Check if Urls exist ###


#?url.exists
# url.exists(url = urls[1], .header = TRUE)
#httr::GET(urls)

#### Scrape ###fil
docs <- urls %>% 
          lapply(read_html) 

tabs <- docs %>% 
            lapply(function(x)
              html_node(x, xpath = "//table[@class='infobox geography vcard']")) %>% 
            lapply(html_table)

check_dims <- tabs %>% sapply(dim) %>%  t

tabs[[1]] %>%  head(n=8) %>%  View
tabs[[3]] %>%  View 


tabs[[1]][4:6, 1] %>% 
  str_remove_all('\\(s\\)') %>% 
  str_remove_all("\"") %>% 
  str_split(':', n = 2) %>% 
  lapply( function(x) str_trim(x,side = 'left')) %>% 
  unlist %>% 
  matrix( ncol = 2, byrow = TRUE) %>% 
  as.data.frame



tabs[[30]][4:6,]

tabs[29] %>%  
  lapply(clean_table) %>% 
  bind_rows %>%  View
  
  View 
  bind_rows %>% View()
  # select(starts_with(c('Total', 'Density'))) %>% 
  # View()




df_list <- tabs%>%  lapply(clean_table)
        

# Drop heading columns and sparse columns
# Also drop country column since we know all states  are in US
state_df <- df_list %>%  
              bind_rows  %>% 
              select(-Area, 
                     -Dimensions, 
                     -Senate_President,
                     -Time_zones,
                     -Government,
                     -Language,
                     - Country, 
                    -starts_with('Population'),
                    -starts_with('Approximately'),
                    -starts_with('Secretary_of'),
                    -contains('citation_needed')) %>% 
              rename(Total_Population = Total)

state_df$state <- datasets::state.name
state_df <- state_df %>% 
              select(state, everything())

# Need to convewrt to numeric 
state_df$Total_Population %>% 
  str_remove_all('\\[\\d\\]') %>%
  str_remove_all(',') %>% 
  as.numeric
#names(state_df)

state_df %>%  View

state_df$Government
#### Next Steps ####

# 1) Split metric/English units
# 2) Convert to Numeric
# 3) Get motto and Anthem
# 4) Remove foot notes from values
# 5) Let Largest City = Capital if capital is largest city
#  6) Possibly split part identification from each Office
# 7) Count House delegation by party ID
# 8) Split admit_rank from admit_date
# 9) Alabama is missing population density?
#### Other ####

# state_df <- state_df %>% 
#               select(starts_with('Capital.')) %>% 
#               head()



#### One State ####

x <- read_html('https://en.wikipedia.org/wiki/Alabama')
infobox
x %>%  str
states[15]
x$doc[1]
html_text(x)

?html_node

tab <- 

?str_remove
tab[3:nrow(tab), 1] %>% 
?html_nodes
  str_remove('^\\W\\s') %>% 
  str_replace('elevation\\s\\*+', 'elevation')

tab[, 2]


tab %>%  dim
tab[,1]
tab[4:6, 2]


tab

tab
tab %>%  class

tab %>%  t %>%  View

?html_table
infobox geography vcard