# 1.4 Task 4 - Halloween Candy Data

library(tidyverse)
library(readxl)
library(janitor)
library(stringr)

# First analysis of the three files.

### First of all we are going to start reading the three excel files and investigating a few starting properties.

candy_2015 <- read_excel("raw_data/boing-boing-candy-2015.xlsx")
candy_2016 <- read_excel("raw_data/boing-boing-candy-2016.xlsx")
candy_2017 <- read_excel("raw_data/boing-boing-candy-2017.xlsx")

names(candy_2015)
names(candy_2016)
names(candy_2017)

#view(candy_2015)


glimpse(candy_2015)

###The column 'Timestamp' has a data format. All the other  columns are either character or logical. The dimension is 5630 rows and 124 columns.

### At a very first look it appears clear that the column's names are very confusing, being them sentences instead of clear names.

glimpse(candy_2016)

### In the 2016 file, seems that the situation is almost the same and the dimension here is 1259 rows and 123 columns.

glimpse(candy_2017)

### In the 2017 file, the types of the columns are either character or double. The columns names are very confusing as well, even if it seems they have a just a bit more similar structures. It seems that there are a lot of NAs in this file.


# candy_2015 file.

### We start giving to the first three columns simpler and more identifiable names. I will not use the 'clean_name' function because it would remove the '[' ']' around the names of the candies that could be useful later. Furthermore, I would change the names in a shorter and simpler format.




candy_2015 <- candy_2015 %>% 
  rename("age" = "How old are you?", "timestamp" = "Timestamp", "going_trick" = "Are you going actually going trick or treating yourself?")


candy_2015 <- candy_2015 %>% 
  mutate(gender = "unknown", .after = age) %>% 
  mutate(gender = na_if(gender, "unknown"))



candy_2015 <- candy_2015 %>% 
  mutate(country = "unknown") %>% 
  mutate(country = na_if(country, "unknown"))




  
### I change the format of the age columns in integer, in such a way that, character values that wouldn't mean nothing will be considered as NAs.
### I also assume not to consider as a valid value, ages greater than 100 years old.


candy_2015 <- candy_2015 %>% 
  mutate(age = as.integer(age)) %>% 
  mutate(age = if_else(age >= 100, na_if(age, age), age))

### I am interested only in the year, so I extract it from the timestamp table.


candy_2015 <- candy_2015 %>% 
  mutate(year = as.numeric(format(timestamp,'%Y')))


### I ma going to apply a pivot_longer function for comparing the ratings for each different candy. 

candy_2015 <- candy_2015 %>% 
  pivot_longer(cols = starts_with("["),
               names_to = "candies",
               values_to = "appreciation_rating")


candy_2015 <- candy_2015 %>% 
  mutate(candies = str_remove(candies, "\\[")) %>% 
  mutate(candies = str_remove(candies, "\\]")) 
  


### I only select the columns I am interested in.

candy_2015 <- candy_2015 %>% 
  select(year, age, gender, going_trick, candies, appreciation_rating, country)

### I check the NAs but I decide, for the aim of our questions, not to do anything for those. The same holds for the other tables.


candy_2015 %>% 
  summarise(across(.col = everything(), .fns = ~sum(is.na(.x))))
### There are NAs in the columns 'age', 'appreciation_rating'.


## Finally, I write the new cleaned data file into a separate folder.

write_csv(candy_2015, "clean_data/clean_candies-2015.csv")






# candy_2016

### First of all, I will change the names of some significant columns, making them equal to the names of the same kind columns in the other dataframes.

 candy_2016 <- candy_2016 %>% 
  rename("timestamp" = "Timestamp", "gender" = "Your gender:", "going_trick" = "Are you going actually going trick or treating yourself?", "age" = "How old are you?", "country" = "Which country do you live in?", "state" = "Which state, province, county do you live in?")

 
### I am going to change the data type in the "age" column converting it to integer! This will create a few NAs but, it's ok because everything there that cannot be converted in an integer is not a significant value. Again I will not consider  ages greater than 100.
 
 candy_2016 <- candy_2016 %>% 
   mutate(age = as.integer(age)) %>% 
   mutate(age = if_else(age >= 100, na_if(age, age), age))
 
 candy_2016 <- candy_2016 %>% 
   mutate(gender = na_if(gender, "I'd rather not say")) %>% 
   mutate(gender = na_if(gender, "Other"))
 
 
 candy_2016 <- candy_2016 %>% 
   mutate(year = as.numeric(format(timestamp,'%Y')))

 candy_2016 <- candy_2016 %>% 
   select(year, age, gender, country, going_trick, starts_with("["))
 
 ## Let's analyse the 'country' column:
 
 
 ### First of all I would like to convert all the countries in lower case, so that I will have less cases to deal with.
 
 candy_2016 <- candy_2016 %>% 
   mutate(country = str_to_lower(country))
 
 ### Now I want to change any different denomination of a same country with a unique one.
 
 ### Let's have a look of the different denominations
 
candy_2016 %>%
distinct(country)

### Let's start replacing numbers with NAs 

candy_2016 <- candy_2016 %>% 
  mutate(country = if_else(str_detect(country, "[1-9]"), "number", country)) %>% 
  mutate(country = na_if(country, "number"))

### USA

candy_2016 <- candy_2016 %>% 
  mutate(country = if_else(str_detect(country, "^u[s. ]"), "USA", country)) %>% 
  mutate(country = if_else(str_detect(country, "rica$"), "USA", country)) %>% 
  mutate(country = if_else(str_detect(country, "the united states"), "USA", country)) %>% 
  mutate(country = if_else(str_detect(country, "usa"), "USA", country))

### UK

candy_2016 <- candy_2016 %>% 
  mutate(country = if_else(country %in% c("uk", "england", "united kingdom"), "UK", country))  

### Netherlands
candy_2016 <- candy_2016 %>% 
  mutate(country = if_else(str_detect(country, "the netherlands"), "netherlands", country))

### Cascadia

candy_2016 <- candy_2016 %>% 
  mutate(country = if_else(str_detect(country, "cascadia"), "cascadia", country))



### Other invalid names

candy_2016 <- candy_2016 %>% 
  mutate(country = if_else(str_detect(country, "^[a-z] "), "invalid", country)) %>% 
  mutate(country = if_else(str_detect(country, "[y{2}']"), "invalid", country)) %>% 
  mutate(country = if_else(str_detect(country, "one"), "invalid", country)) %>% 
 mutate(country = if_else(country %in% c("somewhere", "eua", "see above"), "invalid", country)) %>% 
  mutate(country = na_if(country, "invalid"))

### Finally, a part from "USA" and "UK" that I want to maintain in upper case, I change the format of all the other Countries in Title format.

candy_2016 <- candy_2016 %>% 
     mutate(country = case_when(
       country == "USA" ~country,
       country == "UK" ~country,
       TRUE           ~str_to_title(country)
     )) 

### pivot_longer

candy_2016 <- candy_2016 %>% 
  pivot_longer(cols = starts_with("["),
               names_to = "candies",
               values_to = "appreciation_rating")

candy_2016 <- candy_2016 %>% 
  mutate(candies = str_remove(candies, "\\[")) %>% 
  mutate(candies = str_remove(candies, "\\]"))

### Checking of the NAs.


candy_2016 %>% 
  summarise(across(.col = everything(), .fns = ~sum(is.na(.x))))

### It seems there are NAs in the columns 'age', 'gender', 'country' 'state' appreciation_rental'


## I write the cleaned file into the new folder.

write_csv(candy_2016, "clean_data/clean_candies_2016.csv")




# candy_2017
 
 
 
 candy_2017 <- candy_2017 %>% 
   rename("going_trick" = starts_with("Q1:"), "age" = starts_with("Q3"), "country" = starts_with("Q4"), "state" = starts_with("Q5"), "gender" = starts_with("Q2"))
 
 
###candy_2017 doesn't have a 'year' column, so I create one for it. 
 
 candy_2017 <- candy_2017 %>% 
   mutate(year = 2017, .before = age)
 
 
 
 candy_2017 <- candy_2017 %>% 
   mutate(age = as.integer(age)) %>% 
   mutate(age = if_else(age >= 100, na_if(age, age), age))
 
 
 candy_2017 <- candy_2017 %>% 
   mutate(gender = na_if(gender, "I'd rather not say")) %>% 
   mutate(gender = na_if(gender, "Other"))
 

 
 candy_2017 <- candy_2017 %>% 
   select(year, age, gender, country, going_trick, starts_with("Q6"))
 
 
 
 candy_2017 <- candy_2017 %>% 
   pivot_longer(cols = starts_with("Q6"),
                names_to = "candies",
                values_to = "appreciation_rating")
 
 candy_2017 <- candy_2017 %>% 
   mutate(candies = str_remove(candies, "Q6")) %>% 
   mutate(candies = str_remove(candies, "\\|")) 
 
 
 ### Let's analyse the 'country' column.


 candy_2017 <- candy_2017 %>% 
   mutate(country = str_to_lower(country))




### We are going to convert numbers in NAs

candy_2017 <- candy_2017 %>% 
  mutate(country = if_else(str_detect(country, "[1-9]"), "number", country)) %>% 
  mutate(country = na_if(country, "number"))


candy_2017 <- candy_2017 %>% 
  mutate(country = if_else(str_detect(country, "^u[s. ]"), "USA", country)) %>% 
  mutate(country = if_else(str_detect(country, "rica$"), "USA", country)) %>% 
  mutate(country = if_else(str_detect(country, "^u[sn.]"), "USA", country)) %>% 
  mutate(country = if_else(str_detect(country, "the united states"), "USA", country))

### UK

candy_2017 <- candy_2017 %>% 
  mutate(country = if_else(country %in% c("uk", "england", "united kingdom"), "UK", country))  

### Netherlands
candy_2017 <- candy_2017 %>% 
  mutate(country = if_else(str_detect(country, "the netherlands"), "netherlands", country))


### Other invalid names

candy_2017 <- candy_2017 %>% 
  mutate(country = if_else(str_detect(country, "^[a-z]$"), "invalid", country)) %>% 
  mutate(country = if_else(str_detect(country, "[.{2}`]"), "invalid", country)) %>% 
  mutate(country = if_else(country %in% c("earth", "insanity lately", "can", "i don't know anymore", "fear and loathing", "narnia", "uae", "canae", "ud"), "invalid", country)) %>% 
  mutate(country = na_if(country, "invalid"))




candy_2017 <- candy_2017 %>% 
  mutate(country = case_when(
    country == "USA" ~country,
    country == "UK" ~country,
    TRUE           ~str_to_title(country)
  )) 



candy_2017 %>% 
  summarise(across(.col = everything(), .fns = ~sum(is.na(.x))))

###There are NAs in the columns 'age', 'gender', 'country', 'state', going_trick' and 'appreciation_rental'

## Finally, I write the cleaned file into the new folder.


write_csv(candy_2017, "clean_data/clean_candies_2017.csv")




















