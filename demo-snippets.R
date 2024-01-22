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

##############################################
############# WEEK 3 CLASS 2 #################
##############################################

# read in the mmdata from above if it's not loaded!

## TIDYR##

# pivot mmdata (originally wide) to long
mmdata.long <- mmdata %>%
  pivot_longer(cols = c("Red", "Green", "Blue", "Orange", "Yellow", "Brown"),
               names_to = "Color",
               values_to = "Number")


# pivot long mmdata to wide; back to where it started
mmdata.wide <- mmdata.long %>% 
  pivot_wider(names_from = "Color", values_from = "Number") %>% 
  # Relocate works like select by reordering columns, but doesn't drop anything
  # Kind of like how rename works like select by renaming but not dropping
  relocate(Weight,.after = last_col())

mmdata.wide == mmdata

# example tbl for tidyr functions

# ?Q?: Is this wide or long?
glasses <- tibble(
  condition = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
  participant = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6),
  score = runif(12),
  notes = c("glasses_none", "glasses_none", "none_none", "none_none",
            "none_incomplete", "none_incomplete", "none_none", "none_none",
            "glasses_none", "glasses_none", "glasses_late", "glasses_late")
)

# Separate "notes" column into two columns by recognizing "_" separator
glasses.sep <- glasses %>%
  separate(notes, c("vision_correction", "other_notes"), sep = "_")

# separate() has been superseded into separate_wider_position() & separate_wider_position()

# *_position() will split based on integer values for each new column and for the separator
glasses.sep.pos <- glasses %>% 
  separate_wider_position(notes, c(vision_correction = 4, 1, other_notes = 4), too_many = "drop")

# *_delim() will split based on a given separator, like separate()
# but has slightly different syntax
glasses.sep.delim <- glasses %>% 
  separate_wider_delim(notes, delim = "_", names = c("vision_correction", "other_notes"))

# Unite the 2 columns back to 1 with a new separator (use ; not _)
glasses.unite <- glasses.sep %>%
  unite("semicolon_notes", "vision_correction":"other_notes", sep = ";")


## Missing data

# ?Q? What's actually missing in this missingdata tibble?
missingdata <- tibble(
  condition = c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2),
  participant = c(1, NA, NA, NA, 2, NA, NA, NA, 3, NA, NA),
  trial = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 3, 4),
  score = c(0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1),
)

fixeddata <- missingdata %>%
  # participant id is only indicated for the first observation for each participant, fill it in for the rest
  fill(participant) %>% 
  # p3 did not complete trial 2, but we should still see evidence of the (incomplete) observation
  complete(condition, trial) %>%
  replace_na(list(participant = 3)) %>% # a HACK (will not generalize/will break)
  arrange(condition, participant, trial) # just to make it pretty

## Alternatively...

# use crossing() to create a tibble of all possible (i.e. expected) participant x trial observations 
# note that crossing() is a wrapper for expand_grid(), which is a helper for expand(), meaning you can replicate this just with expand() but this is more convenient
all.ptcp.trial.combos <- crossing(
  participant = unique(missingdata$participant),
  trial = c(1:4)) %>%
  filter(!is.na(participant)) 

## now fix the missing data by...
fixeddata2 <- missingdata %>%
  # using the same fill() fnct to fix participant col.
  fill(participant) %>%
  # join with the expanded tibble of expected combos
  full_join(all.ptcp.trial.combos) %>%
  # arrange(participant, trial) %>% # superstitious but harmless
  # the expected observation for p3, trial2 has been created and is missing values for score (appropriately) and condition, but we know what the condition should be and use fill() as we did above 
  fill(condition)

## STRINGR ##

# str... _detect, _starts, _ends
# returns logical

# the string you're searching within comes first
# the pattern you're searching for within that string comes second
str_detect("pear", "apple")
str_detect("green apple", "apple")
str_detect("apple", "green apple")
str_detect("apple", "green apple", negate=T) # negate = T : is this pattern NOT detected?
str_detect("Green Apple", "apple")
str_detect("Green Apple", regex("apple", ignore_case = T)) # wrap your pattern in regex() for more options like case insensitivity

str_starts("green apple", "apple")
str_ends("green apple", "apple")
str_ends("green apple ", "apple")

str_count("green apple", "apple")
str_count("green apple", "e")
str_count("green apple", "ee")
str_count("green apple", "EE")
str_count("green apple", regex("EE", ignore_case = T))
str_count("green apple", "ee|e") # "ee|e" is a regex looking for *either* ee or e; note that the 2 es in green count as an "ee" and not as 2 "e"s!

# Use str_ functions to manipulate tibbles
mmdata.long %>% 
  mutate(has_e = str_detect(Color, "e"),
         count_e = str_count(Color, "e"))

## Subsetting & Length

fruit <- c("apple", "pear", "banana")
str_sub(fruit, 1,3) # what are the characters from element 1 to element 3?
str_sub(fruit, 1,3) <- "FRUIT!" # take out those elements and replace those substrings with something else

bread <- c("rye", "baguette", "wonder", "sourdough")

str_subset(bread, "e")
str_subset("bread", "e")
str_subset("brEad", "e")

str_length(bread)
str_length("bread")

str_pad(bread, 10)
str_pad(bread, 10, side="both", pad = "-")
str_trunc(bread, 4)
str_trunc(bread, 4, side = "left", ellipsis="---")

str_trim(" bread   ")
str_trim(str_pad(bread, 10))

## Mutate, join, and split

str_sub(fruit, 1,3) <- "FRUIT!" # take out those elements and replace those substrings with something else

str_replace(bread, "e", "E")
str_replace_all(bread, "e", "E")

str_remove(bread, "e")
str_remove_all(bread, "e")

str_to_upper(bread)
str_to_upper(bread[2])

str_glue("rye", "wonder")
str_c("rye", "wonder", sep=", ")
str_flatten(bread)
str_flatten(bread, collapse = " + ")

## FORCATS ## 

factor(bread)
factor(bread, ordered=T)
factor(bread, levels = c("rye", "baguette", "wonder", "sourdough"), ordered=T)

bread.fct <- factor(bread)

levels(bread)
levels(bread.fct)
levels(factor(bread)) 

levels(bread) <- c("rye", "baguette", "wonder", "sourdough")

bread

mmdata.fct <- mmdata.long %>% 
  mutate(Color = factor(Color))

levels(mmdata.fct$Color)

mmdata.fct <- mmdata.fct %>% 
  mutate(Color.Fav = factor(Color, levels = c("Green", "Orange", "Blue", "Red", "Yellow", "Brown"), ordered = T))

str(mmdata.fct$Color.Fav)

## fct_relevel: reorder levels by shifting one or more to a specific location relative to the others
levels(mmdata.fct$Color.Fav)
fct_relevel(mmdata.fct$Color.Fav, "Blue")
f <- factor(c("a", "b", "c", "d"), levels = c("b", "c", "d", "a"))
fct_relevel(f)
fct_relevel(f, "a")
fct_relevel(f, "b", "a")
fct_relevel(f, "a", after = 2) # Move to the third position
fct_relevel(f, "a", after = Inf) # Relevel to the end (or any no. of levels)
fct_relevel(f, "a", after = 3) # Relevel to the end (of 4 levels)


mmdata.fct <- mmdata.fct %>% 
  mutate(Color.Fav2 = factor(Color.Fav, levels = c("Red", "Yellow", "Brown", "Green", "Orange", "Blue"), ordered = F))

mmdata.fct$Color.Fav2

## fct_infreq: reorder levels by descending frequency 
# useful for making easy-to-read plots
fct_infreq(mmdata.fct$Color.Fav)

## fct_rev: reverse the current order of levels
# handy when you want a plot legend to read top to bottom but the plot itself to render the opposite
# which is a thing that happens a lot it's just hard to explain exactly why
fct_rev(mmdata.fct$Color.Fav)

## fct_recode: recode (ie rename/relabel) levels
x <- factor(c("apple", "bear", "banana", "dear"))
fct_recode(x, fruit = "apple", fruit = "banana")
fct_recode(x, fruit = "apple", fruit = "bananana")
fct_recode(x, NULL = "apple", fruit = "banana") # If you name the level NULL it will be removed


## fct_collapse: combine multiple levels into new levels
gss_cat$partyid # examine this column from a pre-loaded example dataset
fct_count(gss_cat$partyid) # look at counts for each factor level in the column
partyid2 <- fct_collapse(gss_cat$partyid,
                         missing = c("No answer", "Don't know"),
                         other = "Other party",
                         rep = c("Strong republican", "Not str republican"),
                         ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                         dem = c("Not str democrat", "Strong democrat")
)
fct_count(partyid2) # look at counts for each new, collapsed factor level


## fct_other: replace some levels with "other"
x <- factor(rep(LETTERS[1:9], times = c(40, 10, 5, 27, 1, 1, 1, 1, 1)))
fct_other(x, keep = c("A", "B")) %>% # comment out the pipes (and the lines it pipes into) to look at the factor itself
  fct_count()
fct_other(x, drop = c("A", "B")) %>% # ditto
  fct_count()


## fct_drop: drop unused levels
f <- factor(c("a", "b"), levels = c("a", "b", "c"))
f
fct_drop(f)

# Set only to restrict which levels to drop
fct_drop(f, only = "a")
fct_drop(f, only = "c")


## fct_expand: add levels
f <- factor(sample(letters[1:3], 20, replace = TRUE))
f
fct_expand(f, "d", "e", "f")
fct_expand(f, letters[1:6])
fct_expand(f, "Z", after = 0)


##############################################
############# WEEK 4 CLASS 1 #################
##############################################

# Bind rows

df1 <- tibble(x = 1:2, y = letters[1:2])
df2 <- tibble(x = 4:5, z = 1:2)
bind_rows(df1, df2)
#rbind(df1, df2) #throw error, column structure doesn't match
#bind_rows allows for different column structures and will add NAs

# Bind columns

df1 <- tibble(x = 1:3) # 3 rows
df2 <- tibble(y = 3:1) # 3 rows
df3 <- tibble(z=1:5) # 5 rows
bind_cols(df1, df2)
bind_cols(df1, df3) #throw error, different number of rows
cbind(df1, df2)
cbind(df1, df3) #throw error, different number of rows
#neither function works with different number of rows

