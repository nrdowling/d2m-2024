library(tidyverse)
library(readxl)


##############################################
############# WEEK 2 CLASS 2 #################
##############################################

# Defining a simple function
add.two <- function(x) {return(x + 2)}

# The names don't matter
# Assign it to any name and call the arguments whatever you want
sdfkjsdf <- function(kgguw) {return(kgguw + 2)}

# Slightly more complicated, including other functions in a function
add.two2 <- function(x) {return(paste(as.character(x), "two"))}

# What counts as "returning"?

fnc_paste_return <- function(argument1, argument2) {
  return(paste(argument1, argument2))
}

fnc_paste <- function(argument1, argument2) {
  paste(argument1, argument2)
}

fnc_paste_store <- function(argument1, argument2) {
  myvalue <- paste(argument1, argument2)
}

fnc_paste_return("hello", "world")
fnc_paste("hello", "world")
fnc_paste_store("hello", "world")


#### See groceries-demo.R for all the grocer's partner conditional/functions shenanigans! ####


for (i in c(2,4,8,16)) {print(i)}

for (i in c(1,2,3,4)) {print(2^i)}

timestwo <- function(number) {x <- number*2
print(x) }


i <- 1
while(i <= 4) {
  print(2^i)
  i <- i + 1}

# Hello world demo

print("Hello world!")

print(paste("Hello", "world!")) # Default separator is a space

print(paste("Hello", "world!", sep = "-")) # Make it anything

print(paste0("Hello", "world!")) # Or nothing

print(paste0("Hello", " ", "world", "!")) # Break it up with more than two arguments





##############################################
############# WEEK 3 CLASS 1 #################
##############################################

## From last week's homework, making a hello_world function

# Define a function that gives you as many positive greetings as you want; assign to object hello_world()

# hello_world() takes arguments name (character) and n_greet (number of greetings requested)
hello_world <- function(name, n_greet) {
  # set some possible greetings, character variables that may reference the name argument
  greetings <- c(paste0("Hello ", name, "!"), # paste0 will concatenate strings and string-formatted objects without a separator
                 "How's your day going?",
                 paste0("Wow, ", name, ", looking good today."),
                 paste0(name, ", that outfit looks great on you."),
                 paste0(name, "! I'm so glad you're here!"),
                 "OMG hey!", # if I don't want to reference a stored variable/object, I don't have to concatenate anything
                 paste0("sup ", name, "?"))
  # start the counter off at 1
  i <- 1
  # but before using the counter, check to see if we don't want any greetings at all
  if (n_greet == 0) {
    return(paste0("I have nothing to say to you, ", name, ".")) # return something other than a greeting, but still reference the name argument
  } else {
    while(i <= n_greet) { # is i less than or equal to the number of greetings I asked for?
      print(sample(greetings, 1)) # if yes, print out a random greeting from my list (if no, while loop ends)
      i <- i + 1 # add 1 to the counter because we delivered 1 greeting and restart the while loop
    }
  }
} # end hello_world function define



## Today in-class: Data read-in & column types

# Read in adult-data.csv with no optional arguments defined
adult.data <- read_csv("data/adult-data.csv")

# Examining structure shows that readr guess all columns were either numeric or character
str(adult.data)

# But we want to treat many of these as factor (categorical) variables, so we specify those with col_types
adult.data2 <- read_csv("data/adult-data.csv",
                        col_types = cols(
                          workclass = col_factor(), # use either the col_*() functions
                          education = "f" # or the compact string representations
                        ))
str(adult.data2)

# Alternatively, we can use those shortcut compact strings to specify how to import all columns quickly
# but it can be hard to keep track when you have a lot of columns
adult.data3 <- read_csv("data/adult-data.csv",
                        col_types = "ifdfifcfff???f?") # the "?" tells R to just guess for that col
str(adult.data3)

## M&M data

mmdata <- read_excel("data/MM data.xlsx", skip = 1)



## dplyr functions

# filter()
filter(mmdata, Weight >= 50)
filter(mmdata, Green == 7, Blue == 7)

# select()
select(mmdata, Bag, Weight)
select(mmdata, Bag, Yellow, Red)
select(mmdata, Red, Yellow, Bag)
select(mmdata, -Bag)
select(mmdata, starts_with("B"))
select(mmdata, bag_id = Bag, Red, Blue, wght = Weight)
select(mmdata, red = Red, bag_id = Bag)
rename(mmdata, red = Red, bag_id = Bag)

# arrange() 
arrange(mmdata, Weight)
mmdata %>% 
  arrange(Blue, -Red)

# put it together: filter+select+rename+arrange
mmdata %>% 
  filter(Weight >= 50) %>% 
  select(-Bag) %>% 
  rename(Tangerine = Orange, Lemon = Yellow) %>% 
  arrange(Tangerine, -Lemon)

# mutate()
mutate(mmdata, Candy = "M&M")
mutate(mmdata, Christmas = Red + Green)
mutate(mmdata,
       Primary = Red + Blue + Yellow,
       Avg_Primary_Weight = Weight/Primary)
mutate(mmdata,
       Weight = Weight*.035, #convert grams to ounces, replacing values in Weight column
       Weight_Gr = Weight/.035, # convert back...just to be confusing I guess
       Total = Red + Green + Blue + Orange + Yellow + Brown,
       Avg_Weight_Oz = Weight/Total)

#### messing with the data a bit to use in next examples
mmdata2 <- mmdata %>%  
  mutate(Whose = case_when(Bag < 11 ~ "Natalie",
                           Bag < 21 ~ "Grace",
                           TRUE ~ "Jenny"),
         Big_Green = ifelse(Green > 7, TRUE, FALSE))
####

# group_by() & summarize()
summarize(mmdata2, mean_wgt = mean(Weight))
group_by(mmdata2, Whose) %>% summarize(whose_mean_wgt = mean(Weight))
mmdata2 %>% 
  group_by(Whose, Big_Green) %>% 
  summarize(count_big_green_bags = n()) # the n() function counts rows without taking any arguments
mmdata2 %>% 
  group_by(Whose, Big_Green) %>% 
  summarize(Sum_BG_Green = sum(Green), Sum_BG_Red = sum(Red), Sum_BG_Blue = sum(Blue)) %>% 
  # We can regroup by just one condition to then summarize on summary values
  # although why we would want the mean per person of the sum of each color Big Green and not Big Green is beyond me
  group_by(Whose) %>% 
  summarize(why_green = mean(Sum_BG_Green), why_red = mean(Sum_BG_Red), why_blue = (mean(Sum_BG_Blue)))

# ONE GIANT PIPE!
# Does Natalie or Grace have the Citrus-y-est, Primary-est, and Christmas-y collections of M&Ms?
# This is an absurd pipe for example purposes only
# Please never actually construct something that does something so confusing and pointless

mmdata %>%  
  mutate(Whose = case_when(Bag < 11 ~ "Natalie",
                           Bag < 21 ~ "Grace",
                           TRUE ~ "Jenny")) %>% 
  filter(Whose != "Jenny") %>% 
  select(-Bag) %>% 
  rowwise() %>% # This makes it easier to sum up many columns in the next line, but you could skip it if you wrote out each column individually: Red + Yellow + Green + ...
  mutate(Total = sum(c_across(Red:Brown))) %>% 
  rename(Tangerine = Orange, Lemon = Yellow, Grapefruit = Red, Lime = Green) %>% 
  mutate(Christmas = Blue + Lime,
         Primary = Grapefruit + Blue + Lemon,
         Citrus = Tangerine + Lemon + Grapefruit + Lime) %>% 
  group_by(Whose) %>% 
  summarize(Total = sum(Total), sum_Christmas = sum(Christmas), sum_Primary = sum(Primary), sum_Citrus = sum(Citrus)) %>% 
  mutate(pct_Christmas = sum_Christmas/Total, pct_Primary = sum_Primary/Total, pct_Citrus = sum_Citrus/Total) %>% 
  select(1:2, Christmas = pct_Christmas, Primary = pct_Primary, Citrus = pct_Citrus) %>% 
  arrange(Citrus)