library(readxl)
library(tidyr)
library(dplyr)
library(writexl)
library(stringr)
library(tidycensus)


current_year = format(Sys.Date(), "%Y") #define current year
years = seq.int(2019, current_year, by = 1) #list all years from 2019 to present
data = data.frame() #create empty dataframe to dump data into


#for every year in the list, run the census API call and add it to the empty dataframe. 
# key = (get your own free API key from https://api.census.gov/data/key_signup.html)

for(i in years){
  df = get_acs(geography = "tract", table = "B01001", cache_table = TRUE, year = i, state = 53, county = c(33, 61, 53)) #call API
  df$Year = i #add year column
  try((data = rbind(data, df)), silent = TRUE) #add to larger dataframe. ignore years where data doesn't yet exist. 
}


rm(df) #remove df, which was just used to construct final dataframe. 




#access list of variables in words not codes, and clean up names.
var_list <- load_variables(2020, "acs5", cache = TRUE) %>% 
  select(-concept) %>% 
  mutate(label = str_remove(label, "Estimate!!")) %>% 
  mutate(label = str_remove(label, "Total:!!")) %>% 
  mutate(label = str_remove(label, "!!"))


#join that list of variables to the original dataframe.
data = data %>% 
  left_join(var_list, by = c("variable" = "name")) %>%  #join based on column "variable" in data and "name" in var_list
  select(-variable) %>% 
  janitor::clean_names() 


data = select(data, -c(moe, geography))

data = data %>% mutate(label = str_replace(label, ":", " "))


#match exact column names of what's imported to Power BI already, to avoid breaking queries
data = rename(data, Year = year, GEOID = geoid, Tract = name)


#pivot wider 
data = data %>% 
  pivot_wider(names_from = label, values_from = estimate)

data = rename(data, `Total:` = `Total `, `Male total` = `Male `,`Female total`= `Female `)