
library(tidyverse)
library(data.table)
library(inspectdf)
library(shiny)

# The data ----
injuries <- fread("C:/Users/User/Desktop/R/Week_4/Day_1/injuries.csv")
products <- fread("C:/Users/User/Desktop/R/Week_4/Day_1/products.csv")
population <- fread("C:/Users/User/Desktop/R/Week_4/Day_1/population.csv")

# EDA ----
injuries %>% inspect_na()
products %>% inspect_na()
population %>% inspect_na()

# Look at the product associated with the most injuries
injuries$prod_code %>% 
  as.factor() %>% 
  table() %>% 
  as.data.frame() %>% 
  arrange(desc(Freq)) %>% 
  .[1,1] -> pc

products$title[products$prod_code == pc]

stair_step <- injuries %>% filter(prod_code == 1842)


# App de olacaq 4 dene analiz ----

#1 ci analiz
stair_step %>% count(diag, sort = T) 

#2 ci analiz
stair_step %>% count(body_part, sort = T) 

#3 cu analiz
stair_step %>% count(location, sort = T) 

#4 cu analiz
stair_step %>% 
  count(age, sex) %>% 
  merge(population, by = c("age", "sex"), all.x = T) %>% 
  mutate(rate = n / population * 10000) %>% 
  ggplot(aes(age, rate, colour = sex)) + 
  geom_line() + 
  labs(y = "Injuries per 10,000 people")



# UI ----



# Server ----
