# In this file we are going to start reading the raw data files. Then, we will have a look of how the data appear in order to have an idea of which kind of operations we should do for cleaning them.

### First of all we read the data files and assign them to variables. In order to work with data files, we need to call the 'tidyverse' library.

library(tidyverse)
library(stringr)


cakes_ingredients <- read_csv("raw_data/cake-ingredients-1961.csv")

### I want to have a look of how this first data file appear and keep it open in a separate window.

view(cakes_ingredients)


#### As a first consideration I see that the names of the columns are not clean: the first column ('Cake') starts with a capitol letter, so it is not in snake-case. For what concerns the other columns, they are named only with two capital letters, that presumably represent the initials of the ingredients.
#### At first glance it is also evident that there are many NAs in the data table.

#### Let's start capturing some information about the dataframe.

dim(cakes_ingredients) #### 18 rows and 35 columns

names(cakes_ingredients)

####[1] "Cake" "AE"   "BM"   "BP"   "BR"   "BS"   "CA"   "CC"   "CE"   "CI"   "CS"   "CT"   "DC"   "EG"   "EY"   "EW"   "FR"  
####[18] "GN"   "HC"   "LJ"   "LR"   "MK"   "NG"   "NS"   "RM"   "SA"   "SC"   "SG"   "SR"   "SS"   "ST"   "VE"   "WR"   "YT"  
####[35] "ZH" 

glimpse(cakes_ingredients)

#### All the columns are of type 'double', with the exceptions of the first column 'Cake' which is of type character and the third column 'BM' which is logical.

is.na(cakes_ingredients) ### it seems that there are NAs in all the columns.

## First of all, I would like to change the name of the first column putting it in snake-case and overwrite over 'cake_ingredients'. I will not change the other column's name for now, because they represent a possible foreign key with the other table.


cakes_ingredients <- cakes_ingredients %>% 
  rename("cake" = "Cake")


### Let's have a look also to the other file!

cake_ingredient_code <- read_csv("raw_data/cake_ingredient_code.csv")

view(cake_ingredient_code)

#### It seems that the column's names in the 'cake_ingredients' folder represent exactly the code of the ingredients. The table 'cake_ingredient_code', contains the column 'code' which contains the code of the ingredients, the column 'ingredient' which contains their names and the column 'measure' which specify the unit of measurement that is used for dosing them.

### Let us assume I wanted to change the format of my data-file using a pivot_longer function. Then, all the values that indicate the doses of each ingredient would became of type double. There is only one column anìmong the ingredients that is of type logical. Let's check if there are significant values there. 

cakes_ingredients %>% 
  filter(!(is.na(BM))) 

#### It seems that all the values in the 'BM' column are NAs, so we can easily convert it in double format without altering any information.

### I use a pivot_longer for organize the date in a cleaner format and I filter the rows ignoring NAs, assuming that if there is an NA, then means that that cake doesn't need that specific ingredient.


cakes_ingredients <- cakes_ingredients %>% 
  pivot_longer(cols = -cake, names_to = "code", values_to = "dose") %>% 
  filter(!(is.na(dose)))


### Now we would like to change the code of the ingredients with their exact name. For doing so, we should join the 'cake_ingredients' table with 'cake_ingredient_code'.


cakes_ingredients_join <- inner_join(cakes_ingredients, cake_ingredient_code, by = "code")

#### I change the order of the columns 

cakes_ingredients_join <- cakes_ingredients_join[, c("cake", "ingredient", "code", "dose", "measure")]

#### Let's check if there are other NAs

cakes_ingredients_join %>% 
    summarise(na_cake = sum(is.na(cake)),
              na_ingredient = sum(is.na(ingredient)),
              na_code = sum(is.na(code)),
              na_dose = sum(is.na(dose)),
              na_measure = sum(is.na(measure))
    )

#### There are only 4 NAs in the measure column, let's see where they are.

cakes_ingredients_join %>% 
  filter(is.na(measure))

cakes_ingredients_join %>% 
  filter(ingredient == "Sour cream cup")

#### It seems that these NAs correspond exactly to the ingredient 'Sour cream'. I don't think it would be wise to drop by these lines, because it is enough to set the measure 'cup' for this specific ingredient.

#cakes_ingredients_join <- 
cakes_ingredients_join <- cakes_ingredients_join %>% 
  mutate(measure = coalesce(measure, "cup")) %>% 
  mutate(ingredient = recode(ingredient, "Sour cream cup" = "Sour Cream"))

cakes_ingredients_join <- cakes_ingredients_join %>% 
  mutate(ingredient = str_to_lower(ingredient))




## Once all the cleaning operations have been completed, I am going to write the cleaned data into a new folder.


write_csv(cakes_ingredients_join, "clean_data/cakes.csv")







