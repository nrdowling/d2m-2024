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


##############################################
############# WEEK 5 CLASS 2 #################
##############################################

### EXCEPT NOT REALLY because we did a workshop instead on 5.2...

## DISTRIBUTIONS ##

theme_set(theme_minimal())

## 1-Var Counts ##

# Dot plots

# To get the best idea of an actual distribution you can use a dot plot
# where each dot is literally an observation

# It works best when you have a small-ish sample
diamonds %>% 
  slice_sample(n=100) %>% 
  ggplot(aes(x=depth)) +
  geom_dotplot()


# But it's not practical for large datasets
# diamonds has >50,000 observations
diamonds %>% 
  ggplot(aes(x=depth)) +
  geom_dotplot()

# Also not that the "count" y-axis is entirely uninformative
# Each dot is an observation, the dots themselves are the count

## Histograms

# Histograms are much more common and pretty easy to read

diamonds %>% 
  ggplot(aes(x=depth)) +
  geom_histogram()

# The range of your x-axis values will determine 
# how wide your bins should be
# Trial and error is usually a good way to decide on bin width
# if you don't have a strong theoretical reason for something specific
diamonds %>% 
  ggplot(aes(x=depth)) +
  geom_histogram(binwidth = 1)

diamonds %>% 
  ggplot(aes(x=depth)) +
  geom_histogram(binwidth = 10)


# Compare using the same binwidth for 2 different x variables
# with very different ranges
diamonds %>% 
  ggplot(aes(x=depth)) +
  geom_histogram(binwidth = 50)

diamonds %>% 
  ggplot(aes(x=price)) +
  geom_histogram(binwidth = 50)

# As an alternative to binwidth you can just set
# the number of bins, which is less sensitive to variable range
diamonds %>% 
  ggplot(aes(x=depth)) +
  geom_histogram(bins = 50)

diamonds %>% 
  ggplot(aes(x=price)) +
  geom_histogram(bins = 50)


# We can use histograms to compare across groups

# But it's hard to read
diamonds %>% 
  ggplot(aes(x=depth, fill=cut)) +
  geom_histogram(bins = 50)

# Under the hood, geom_histogram is a version of geom_bar
# so we can adjust the position argument in the same ways
# By default (like above) it's using position="stack", same as geom_bar would

# position fill will show us proportional distributions by bin
# which...may be informative?
diamonds %>% 
  ggplot(aes(x=depth, fill=cut)) +
  geom_histogram(bins = 50, position = "fill")

# Or even dodge them to look like a "regular" bar plot
# Which is...less useful
diamonds %>% 
  ggplot(aes(x=depth, fill=cut)) +
  geom_histogram(bins = 50, position="dodge")

# Arguably the most informative would be if they were overlayed on top of one another
# using position="identity" and adding transparency (alpha)
diamonds %>% 
  ggplot(aes(x=depth, fill=cut)) +
  geom_histogram(bins = 50, position="identity", alpha=.5)

# That kind of works, but this would be clearer with a density plot

# Without any grouping, you can see it's kind of a "smoothed" histogram
diamonds %>% 
  ggplot(aes(x=depth)) +
  geom_histogram(aes(depth, after_stat(density))) + #this is basically scaling the axis to match the density plot
  geom_density(color="red")

# Adding groups in is easier to read than the histogram
diamonds %>% 
  ggplot(aes(x=depth, fill=cut, color=cut)) +
  geom_density(alpha=.5)

## 2-Var Count

# The histogram and density plots with groups are actually 2-var count distributions
# We can display 2 vars just on the axes without additional grouping

diamonds %>% 
  ggplot(aes(x=cut, y=depth)) +
  geom_violin()

# Overlaying dot plots onto violin plots can be useful...
diamonds %>% 
  slice_sample(n=100) %>% 
  ggplot(aes(x=cut, y=depth)) +
  geom_violin() +
  geom_dotplot(dotsize=.5, binaxis = "y", stackdir = "center")
# run this one a couple times to see how much distribution error
# a random sample can introduce!


# ...but still not with a giant sample
diamonds %>% 
  ggplot(aes(x=cut, y=depth)) +
  geom_violin() +
  geom_dotplot(dotsize = .1, binaxis = "y", stackdir = "center")


# Usually the best way to represent summary/descriptive stats
# across groups is with a boxplot (aka box and whiskers)
# Where the "box" is bounded at the 1st and 3rd quartiles
# (ie the middle 50% of the data) with the dividing line at the median (NOT the mean!)
# From documentation: 
# The upper whisker extends from the hinge to the largest value 
# no further than 1.5 * IQR from the hinge (where IQR is the inter-quartile range, 
# or distance between the first and third quartiles). 
# The lower whisker extends from the hinge to the smallest value 
# at most 1.5 * IQR of the hinge. Data beyond the end of the whiskers 
# are called "outlying" points and are plotted individually.

diamonds %>% 
  filter(color %in% c("D", "E", "F")) %>% 
  ggplot(aes(y=carat, x="")) +
  geom_boxplot(color="darkblue") #+
# geom_jitter(alpha=.5, color="darkgray") # optionally add individual observation points

diamonds %>% 
  filter(color %in% c("D", "E", "F")) %>% 
  ggplot(aes(y=carat, x=cut)) +
  geom_boxplot(color="darkblue") #+
#geom_jitter(alpha=.5, color="darkgray")# optionally add individual observation points

# Group by a second discrete variable with the fill and/or color aes()
# Note that the additional grouping variable here is "color" which makes 
# for weird things like "fill=color" and "color=color"...
diamonds %>% 
  filter(color %in% c("D", "E", "F")) %>% 
  ggplot(aes(y=carat, x=cut, fill=color)) + 
  geom_boxplot()

diamonds %>% 
  filter(color %in% c("D", "E", "F")) %>% 
  ggplot(aes(y=carat, x=cut, color=color)) + 
  geom_boxplot()

# In theory use any kind of mapping aes() for this, but in practice don't
diamonds %>% 
  filter(color %in% c("D", "E", "F")) %>% 
  ggplot(aes(y=carat, x=cut, alpha=color)) +
  geom_boxplot()

diamonds %>% 
  filter(color %in% c("D", "E", "F")) %>% 
  ggplot(aes(y=carat, x=cut, linewidth=color)) +
  geom_boxplot()


# Means and bar charts...

# It's very common to represent means across groups with bar/col plots:

diamonds %>% 
  # You have to group & summarize the data first, geom_bar/col won't calculate these for you
  group_by(cut) %>% 
  summarise(mean_price = mean(price)) %>% 
  ggplot() +
  geom_col(aes(x=cut, y=mean_price))

# You can add error bars by summarizing those too

diamonds %>% 
  group_by(cut) %>% 
  summarise(mean_price = mean(price),
            sd_price = sd(price),
            N = sum(!is.na(price)),
            se_price = sd_price / sqrt(N)) %>% 
  ggplot(aes(x=cut, y=mean_price)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_price - se_price, ymax = mean_price + se_price), width=.2)

# HOWEVER! it's generally not recommended to represent means in this way
# The visual message a bar chart communicates is that everything within the bar is included
# and everything above the bar is not
# That implies (visually) that the distribution goes all the way to 0 and stops at the y-axis max
# which could be very far from reality

# If we add on the actual data points you can see that's very much not the case here

diamonds %>% 
  group_by(cut) %>% 
  summarise(mean_price = mean(price),
            sd_price = sd(price),
            N = sum(!is.na(price)),
            se_price = sd_price / sqrt(N)) %>% 
  ggplot(aes(x=cut, y=mean_price)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_price - se_price, ymax = mean_price + se_price), width=.2) +
  geom_jitter(data=diamonds, aes(x=cut, y=price), alpha=.1, color="red")

# and even more so here!!
diamonds %>% 
  group_by(color) %>% 
  summarise(mean_depth = mean(depth),
            sd_depth = sd(depth),
            N = sum(!is.na(depth)),
            se_depth = sd_depth / sqrt(N)) %>% 
  ggplot(aes(x=color, y=mean_depth)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_depth - se_depth, ymax = mean_depth + se_depth), width=.2) +
  geom_jitter(data=diamonds, aes(x=color, y=depth), alpha=.1, color="red")


# Using plots that are intended to represent distributions (boxplots, violins) 
# communicate that info more effectively

diamonds %>% 
  ggplot(aes(x=cut, y=price)) +
  geom_violin()

diamonds %>% 
  ggplot(aes(x=color, y=depth)) +
  geom_boxplot()

# BUT BAR CHARTS ARE VERY USEFUL FOR OTHER THINGS!
# they really intuitively convey SUMS and PROPORTIONS

# Remember how histograms were just bar charts under the hood?
# We can use bar/column charts to compare counts across groups too

# use geom_bar() more like a distribution across groups
# it will count how many observations there are per group
# which means it only takes one discrete axis variable, an x OR a y
diamonds %>% 
  ggplot(aes(x=cut)) +
  geom_bar()

# use geom_col() when the y axis value exists as a variable in your data
# this is the same as geom_bar with the "identity" stat
diamonds %>%
  group_by(cut) %>% 
  summarize(mean_price = mean(price)) %>% 
  ggplot(aes(x=cut, y=mean_price)) +
  geom_col()

# this is the same as geom_bar with the "identity" stat
diamonds %>%
  group_by(cut) %>% 
  summarize(mean_price = mean(price)) %>% 
  ggplot(aes(x=cut, y=mean_price)) +
  geom_bar(stat = "identity")


# Use bar plots to compare counts across groups

# geom_bar "stacks" by default, so that the total height of the bar
# is the total number of observations for that group on the x-axis

diamonds %>% 
  ggplot(aes(x=cut, fill=clarity)) +
  #geom_bar(position = "stack") +
  geom_bar()

# use position = "dodge" for bars next to each other in groups
diamonds %>% 
  ggplot(aes(x=cut, fill=clarity)) +
  geom_bar(position = "dodge")

# this can look weird if you don't have observations in each 
# unique combination of groups
diamonds %>% 
  filter(!(cut=="Good" & !(clarity %in% c("VS1", "VVS1"))),
         !(cut=="Premium" & str_starts(clarity, "S"))) %>% 
  ggplot(aes(x=cut, fill=clarity)) +
  geom_bar(position = "dodge")

# position_dodge2 with preserve = "single" will keep bars the same width
# rather than stretching them, but still won't "hold space" for empty bars
diamonds %>% 
  filter(!(cut=="Good" & !(clarity %in% c("VS1", "VVS1"))),
         !(cut=="Premium" & str_starts(clarity, "S"))) %>% 
  ggplot(aes(x=cut, fill=clarity)) +
  geom_bar(position = position_dodge2(preserve = "single"))

# Use a bar chart with position = "fill" to show proportional/percentage data
# you could just break up one group this way 
# (prop distribution of a discrete variable)

diamonds %>% 
  ggplot() +
  geom_bar(position = "fill", aes(x="", fill=clarity)) # geom_bar still requires an x aes, so use an empty string

# But it's more useful when you have multiple groups
diamonds %>% 
  ggplot(aes(x=cut, fill=clarity)) +
  geom_bar(position = "fill")

# A filled bar chart is an alternative to a pie chart, 
# which are generally discouraged for...reasons
# But if you wanted to make a pie chart, you do so
# by wrapping a filled bar plot into a circle with the coord_polar() layer
diamonds %>% 
  ggplot(aes(x="", fill=clarity)) +
  geom_bar() +
  coord_polar(theta = "y")

# Note: it's very trendy to hate on pie charts, and it's kinda valid
# Basically, the problem is that people have a much harder time perceiving
# relative size of angles than relative size of area
# and since pie charts depend on angle to mentally "calculate" area,
# we're less accurate reading them, and the more levels there are
# the worse we are. Labeling can also be a challenge. And you should
# never use one for ordered factors because the order breaks down at
# the connection point.
# BUT when you are trying to communicate a simple point about a clear
# proportional difference with just 2 or 3 levels in one factor,
# they are actually pretty intuitive and effective
# https://depictdatastudio.com/when-pie-charts-are-okay-seriously-guidelines-for-using-pie-and-donut-charts/

# Here is the #1 best use case for a pie chart:
df <- data.frame(
  variable = c("does not resemble", "resembles"),
  value = c(20, 80)
)
ggplot(df, aes(x = "", y = value, fill = variable)) +
  geom_col(width = 1) +
  scale_fill_manual(values = c("red", "yellow")) +
  coord_polar("y", start = pi / 3) +
  labs(title = "Pac man")


## 2 continuous variables ##

# let's get a smaller dataset to plot faster...

diamonds.smaller <- diamonds %>% 
  slice_sample(n=1000)

# The most common way to look for a relationship between 2 continuous variables
# is with a scatter plot: geom_point

diamonds.smaller %>% 
  ggplot(aes(x=carat, y=price)) +
  geom_point()

# A super common problem with point plots is that densely distributed data will
# overlap significantly and make interpretation difficult
# You can solve that by adjusting how the points look

# Make them smaller
diamonds.smaller %>% 
  ggplot(aes(x=carat, y=price)) +
  geom_point(size=.01)

# Make them semi-transparent
diamonds.smaller %>% 
  ggplot(aes(x=carat, y=price)) +
  geom_point(alpha=.1)

# Or you can adjust where the points are with a "jitter" 
# which randomly moves each point a tiny bit on both axes

diamonds.smaller %>% 
  ggplot(aes(x=carat, y=price)) +
  geom_jitter()

# Or combine them!
diamonds.smaller %>% 
  ggplot(aes(x=carat, y=price)) +
  geom_jitter(size=.1, alpha=.2)

# In this case changing size or transparency seems to be more useful
# than the jitter, but you should always try both to decide what works
# best with your data

# Draw a relationship with geom_smooth


diamonds.smaller %>% 
  ggplot(aes(x=carat, y=price)) +
  geom_jitter(size=.1, alpha=.2) +
  geom_smooth() # by default it uses lowess smoothing and shows a standard error ribbon

diamonds.smaller %>% 
  ggplot(aes(x=carat, y=price)) +
  geom_jitter(size=.1, alpha=.2) +
  geom_smooth(method="lm", se=FALSE) # more often we want a linear model to show a regression or correlation effect, we can also hide the error bar


# Line plots

# Use line plots to communicate *change*
# usually change over time, but change over anything ordered can work
# As a general rule, you don't want to draw a line between anything does doesn't
# have a clear common property, e.g., always the same subjects
# Only use lines when you can imaging a data point traveling along the line
# and that traveling is plausible, like subjects moving through time points
# NEVER use lines to connect data across unordered categorical variables

# BAD example:
# the 2 continuous variable example from above, but with line instead of scatter/smooth
# it works, but what is this crazy jagged line telling you that the geom_smooth didn't
# tell you much more effectively? 
diamonds.smaller %>% 
  ggplot(aes(x=carat, y=price)) +
  geom_line(stat="count")


# But for change over time, lines are great

storms %>% 
  filter(name=="Amy") %>%
  # create a single variable for date&time that can be treated as continuous
  mutate(datetime = ymd_hms(paste0(year, "-", month, "-", day, " ", hour, ":00:00"))) %>% 
  select(datetime, lat, long, wind, pressure) %>% 
  ggplot(aes(x=datetime, y=pressure)) +
  geom_line()


# There are other ways of thinking about "traveling" along lines
# like literally across space -- here longitude
storms %>% 
  filter(name=="Amy") %>% 
  ggplot(aes(x=long, y=pressure)) +
  geom_line()

# or even more across space, just tracking latitude and longitude
storms %>% 
  filter(name=="Amy") %>% 
  ggplot(aes(x=long, y=lat)) +
  geom_line(aes(color=name))

# But remember that this is just connecting along the x-axis
# It works well for time because time moves in one direction
# What's going on here? Why do Amy and Caroline look normal
# but Blanche and Doris are jagged and weird?
storms %>% 
  filter(name %in% c("Amy", "Blanche", "Caroline", "Doris")) %>% 
  ggplot(aes(x=long, y=lat)) +
  geom_line(aes(color=name))


##############################################
############# WEEK 6 CLASS 1 #################
##############################################

## FACETING ##

# The diamonds dataset has >50,000 observations
# So filter to a random selection of 1000 observations
# for processing speed purposes
g.diamond <- diamonds %>% 
  slice_sample(n=1000) %>% 
  # create a base plot with the aes() we'll always want to use
  ggplot(aes(x=carat, y=price)) 


## Wrap examples

# Without any faceting, use color aes() to group by clarity
g.diamond +
  geom_point(aes(color=clarity)) +
  geom_smooth(method="lm", se=F, aes(color=clarity)) 

# Use facets to create panels for clarity
# By default guesses to wrap across 3 rows & 3 cols
g.diamond +
  geom_point() +
  geom_smooth(method="lm", se=F) +
  facet_wrap(vars(clarity))

# Use facets to create panels for clarity
# Specify to wrap across 2 rows
g.diamond +
  geom_point() +
  geom_smooth(method="lm", se=F) +
  facet_wrap(vars(clarity), nrow=2)

# Use facets to create panels for each combination of clarity and cut
# By default it wraps 37(!) panels across 7 columns
## ?Q?: Why isn't it 40?
g.diamond +
  geom_point() +
  geom_smooth(method="lm", se=F) +
  facet_wrap(vars(clarity, cut))

## Grid examples

# Without any faceting, use color aes() to group by clarity and linetype by cut
g.diamond +
  geom_point(aes(color=clarity), size=.2) +
  geom_smooth(method="lm", se=F, aes(color=clarity, linetype=cut))

# Use facets to create panels for each combination of clarity & cut
# Use vars() syntax to set clarity as rows and cut as columns
g.diamond +
  geom_point(size=.2) +
  geom_smooth(method="lm", se=F) +
  facet_grid(vars(clarity), vars(cut))

# Use facets to create panels for each combination of clarity & cut
# Use formula syntax to set cut as rows and clarity as columns
g.diamond +
  geom_point(size=.2) +
  geom_smooth(method="lm", se=F) +
  facet_grid(cut ~ clarity)

g.diamond +
  geom_point(size=.2) +
  geom_smooth(method="lm", se=F) +
  facet_grid(. ~ clarity)


## LABELS ##

g.diamond +
  geom_point(aes(color=cut), size=.2) +
  geom_smooth(method="lm", se=F, aes(color=cut)) +
  # add example labels
  labs(	title = "My title",
        subtitle = "My subtitle",
        x = "X Axis Variable",
        y = "Y Axis Variable",
        color = "Color Grouped Variable",
        caption = "A note at the bottom"
  )

g.diamond +
  geom_point(aes(color=cut), size=.2) +
  geom_smooth(method="lm", se=F, aes(color=cut)) +
  # add example labels
  labs(	title = "Diamond prices",
        subtitle = "By carat and cut",
        x = "Carats",
        y = "Price (USD)",
        color = "Diamond cut style",
        caption = "This is a random sample of 1000 diamonds."
  )

labeled.diamond <- g.diamond +
  geom_point(aes(color=cut), size=.2) +
  geom_smooth(method="lm", se=F, aes(color=cut)) +
  # add example labels
  labs(	title = "Diamond prices",
        subtitle = "By carat and cut",
        x = "Carats",
        y = "Price (USD)",
        color = "Diamond cut style",
        caption = "This is a random sample of 1000 diamonds."
  )

labeled.diamond +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(	fill = 'aquamarine'),
        panel.grid.major = element_line(linetype = 'dashed', color="#FF8C00"),
        legend.title = element_blank())


# Create your own theme layer by defining it as a function
# Once defined, you can set it as default with set_theme()
# Or use it as a layer on any ggplot object

blue_theme <- function() {
  theme(
    # add border 1)
    panel.border = element_rect(colour = "blue", fill = NA, linetype = 2),
    # color background 2)
    panel.background = element_rect(fill = "aliceblue"),
    # modify grid 3)
    panel.grid.major.x = element_line(colour = "steelblue", linetype = 3, size = 0.5),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y =  element_line(colour = "steelblue", linetype = 3, size = 0.5),
    panel.grid.minor.y = element_blank(),
    # modify text, axis and colour 4) and 5)
    axis.text = element_text(colour = "steelblue", face = "italic", family = "Times New Roman"),
    axis.title = element_text(colour = "steelblue", family = "Times New Roman"),
    axis.ticks = element_line(colour = "steelblue"),
    # legend at the bottom 6)
    legend.position = "bottom"
  )
}

# use it on the labeled diamonds plot

labeled.diamond + blue_theme()
#ggsave("images/plots/themes-custom.png", width = 6, height = 4)

