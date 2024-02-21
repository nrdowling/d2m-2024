library(tidyverse)

## Create your goal tibble to replicate

# # Run this line to see what your end product should look like
# sw.wrangled.goal <- read_csv("data/sw-wrangled.csv") %>% 
#   mutate(across(c(hair, gender, species, homeworld), factor)) # this is a quick-and-dirty fix to account for odd importing behavior
# 
# # View in console
# sw.wrangled.goal 
# 
# # Examine the structure of the df and take note of data types
# # Look closely at factors (you may need another function to do so) to see their levels
# str(sw.wrangled.goal) 
# 
# 
# 
# ## Use the built-in starwars dataset to replicate the tibble above in a tbl called sw.wrangled
# # If you get stuck, use comments to "hold space" for where you know code needs to go to achieve a goal you're not sure how to execute
# sw.wranged <- starwars # %>% ...
# 
# 
# 
# ## Check that your sw.wrangled df is identical to the goal df
# # Use any returned information about mismatches to adjust your code as needed
# all.equal(sw.wrangled.goal, sw.wrangled.goal)


sw.wrangled <- starwars %>% 
  # Select only needed columns & rename height (to height_cm) and hair_color (to hair)
  select(name, height_cm = height, mass, hair = hair_color, gender, species, homeworld) %>% 
  # Filter out any rows where height data is missing
  filter(!is.na(height_cm)) %>% 
  # Break names into two columns (first_name, last_name); use first space " " as delimiter
  ## too_many="merge": if more than one delim (space) is found, merge everything after the space into the second column
  ## too_few="align_start": if less than one delim is found, treat the name like a first name and make last_name NA
  ## notice where in the column order the new columns appear vs if you did this with a mutate 
  separate_wider_delim(name, delim=" ", names = c("first_name", "last_name"), too_many="merge", too_few = "align_start") %>% 
  # Change categorical variables (but currently character) to factors
  mutate(
    ## for the 2 detected levels of gender (feminine, masculine) relabel (i.e., rename/replace those values) with f & m
    gender = factor(gender, levels = c("feminine", "masculine"), labels = c("f", "m")),
    ## convert character values in species to all upper case before creating factor levels
    species = factor(str_to_upper(species)),
    homeworld = factor(homeworld)) %>% 
  # create a second height column by converting cm to inches
  mutate(height_in = height_cm*.3937) %>% 
  # where there is no value in hair, use value "bald"
  mutate(hair = factor(replace_na(hair, "bald"))) %>% 
  # create a logical variable that returns true if "brown" is anywhere in the string value for hair 
  mutate(brown_hair = str_detect(hair, "brown")) %>% 
  # create an initials column by concatenating the first characters of the first and last name
  ## str_sub(colname, 1, 1) -- the '1,1' bit means the first character to include is the one in position 1
  ## and the last is in position 1 (so just the first character)
  mutate(initials = paste0(str_sub(first_name, 1, 1), str_sub(last_name, 1, 1))) %>% 
  # move the new height_in column to be immediately left of the height_cm column 
  relocate(height_in, .before = height_cm) %>% 
  # move the new initials column to be immediately right of the last_name column
  relocate(initials, .after = last_name) %>% 
  # sort by last_name and then (when last_name matches) by first_name 
  arrange(last_name, first_name)

#write_csv(sw.wrangled, "data/sw-wrangled.csv")

# 
# sw.wrangled <- read_csv("data/sw-wrangled.csv") %>% 
#   mutate(across(c(hair, gender, species, homeworld), factor))

## ASSIGNMENT (11): RECREATE THESE PLOTS (BASIC) ##

ggplot(sw.wrangled) +
  geom_histogram(aes(x = height_cm), binwidth = 10)

sw.wrangled %>% 
  mutate(sorted_hair = fct_infreq(hair)) %>% 
  ggplot() +
  geom_bar(aes(x = sorted_hair))

sw.wrangled %>% 
  filter(mass < 300) %>% 
  ggplot() +
  geom_point(aes(x = height_in, y = mass), shape = "triangle")


## ASSIGNMENT (12): RECREATE THESE PLOTS (INTERMEDIATE) ##

sw.wrangled %>% 
  filter(mass < 300) %>% 
  ggplot() +
  geom_boxplot(aes(x=fct_infreq(hair), y=mass, fill=fct_infreq(hair))) +
  geom_point(aes(x=fct_infreq(hair), y=mass)) +
  labs(x="Hair color(s)",
       y="Mass (kg)",
       fill="Colorful hair")

sw.wrangled %>% 
  mutate(brown_hair = ifelse(brown_hair, "Has brown hair", "No brown hair")) %>% 
  ggplot(aes(mass, height_in)) +
  geom_point() +
  geom_smooth(method="lm") +
  scale_x_continuous(limits = c(-200, 200)) +
  scale_y_continuous(limits = c(-4, 200), breaks = c(-4, 20, 23, 80, 100)) +
  facet_grid(cols = vars(brown_hair)) +
  theme_minimal() +
  labs(title = "Mass vs. height by brown-hair-havingness",
       subtitle = "A critically important analysis")

sw.wrangled %>% 
  mutate(species_first_letter = fct_rev(factor(str_sub(species, 1, 1)))) %>%
  filter(!is.na(species_first_letter)) %>% 
  ggplot(aes(x=species_first_letter, fill=gender)) +
  geom_bar() +
  coord_flip() +
  labs(caption = "A clear male human bias") +
  theme_classic()


## ASSIGNMENT (13): RECREATE THIS PLOT (ADVANCED) ##

library(ggsci)

## Base plot
sw.wrangled %>%
  ggplot(aes(x=height_cm, y=mass, color = gender)) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(vars(gender))

sw.wrangled %>% 
  mutate(`Gender Presentation` = case_when(gender == "m" ~ "Male",
                                           gender == "f" ~ "Female",
                                           TRUE ~ "Other")) %>% 
  ggplot(aes(x=height_cm, y=mass, color = `Gender Presentation`)) +
  geom_point(alpha = .5) +
  geom_smooth(method="lm", fill="#ccccff") +
  facet_wrap(vars(`Gender Presentation`), scales = "free_y") +
  scale_color_uchicago() +
  scale_x_continuous(n.breaks = 8) +
  labs(
    title = "Height and weight across gender presentation",
    subtitle = 'A cautionary tale in misleading "free" axis scales & bad design choices',
    caption = "Color hint: use the ggsci package!",
    x = "Height (cm)",
    y = "Mass (kg)"
  ) +
  theme_bw() +
  theme(
    text = element_text(family="Comic Sans MS"),
    strip.text = element_text(hjust = 0, color = "white", family = "Courier"),
    strip.background = element_rect(fill = "darkgreen"),
    axis.text.x = element_text(angle = 45, vjust = .5, hjust = .5), 
    axis.text.y = element_text(face = "bold.italic", hjust = 0),
    legend.title = element_text(family = "Brush Script MT", size = 20),
    legend.position = "bottom",
    legend.background = element_rect(fill="#ccccff"),
    plot.caption = element_text(angle = 180, hjust = 0, color = "red"),
    panel.background = element_rect(fill = "#ffeeee"),
    panel.grid.major.y = element_line(color = "lightgray", linewidth = 1, linetype = 4),
    panel.grid.major.x = element_line(color = "white", linewidth = .5, linetype = 2),
    panel.grid.minor = element_blank()
  )
