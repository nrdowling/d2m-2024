### FUNCTIONS ###

### clean_meetup() ##

# selects and cleans meetup location responses based on the kind of meetup partner

# Location groups
# Because responses to the location questions are open-ended, we end up with some situations where we *might* choose to simplify into a single response:
# 1) responses that reference single locations represented in multiple ways (e.g., the bean, bean, cloudgate; reg, the reg, regenstein library)
# 2) responses that *may* reference single locations in multiple ways (e.g., the reg, the library; my room, the dorms, Burton Judson)
# 3) responses that represent different but closely related locations (e.g., the reg, mansueto; a coffee shop, Hallowed Grounds, the Div)
# These are reasonable, non-mutually exclusive groupings that we can choose to use by specifying them in the case_when() in the clean_meetup() function

bean <- "\\bbean|cloudgate"
thereg <- "\\breg"
park <- "m.*park|g.*park"
library <- "reg|library|crerar|mansueto|eckardt"
dorm <- "north|south|\\bMax|burton|dorm|residential|room|island|i\\shouse" # also included in residence
cafe <- "coffee|cafe|dollop|plein|pret|starbucks|amo\\b|Ex\\sL|hallowed" # also included in dining
residence <- "home|house|dorm|Burton|apartment|they\\slive|north|south|i\\shouse|\\bapt\\b|room|island|max|residen"
dining <- "coffee|cafe|dollop|plein|\\bbart|shop|restaurant|Ex\\sL|pret|starbucks|baker|dining|\\bmed|amo\\b|hallowed|cathey"
uc_other <- "quad|UChicago|ratner|hutch|harper|reynold|bookstore|logan|pond|rock.*?er" # "rock.*er" catches misspellings of Rockefeller
chi_other <- "hare\\b|\\blake\\b|navy|magnificent|tower|\\bart\\b|loop|station"
recent <- "\\blast\\b|usual|before|frequent"
kbg <- "kelly|beecher|kbg" # You might want to define groups that could be useful later, even if you're not currently used in clean_meetup() 

# Clean any of the meetup location response columns, collapse as directed, and filter out clear errors
# colErr : name of corresponding boolean column that indicates whether response has been manually marked as an error
clean_meetup <- function(data, colName, colErr="") {
    
    data$x <- data[[colName]] ## ?Q?: Why is this line of code necessary? *IS* it necessary? What is it really doing, and what are some other ways you could accomplish the same thing? Even further, why does this specific syntax work but other very similar syntax does not? 
    y <- data # This is NOT necessary, but explicitly creating a df here can help avoid silly human error
    if (colErr != "") {
        y$err <- y[[colErr]]
        y <- filter(y, err == FALSE) # This will filter out errors if they have been manually recognized
    } else {y$err = FALSE} # If we didn't specify an error column, we create one. This will let us detect more errors programmatically later if we want
    y <- y %>%
        select(x, colName, err) %>%
        drop_na() %>%
        mutate(x = str_replace_all(x, "[[:punct:]]","")) %>% 
        # These location replacements are applicable to all the meetup types (strangers, students, friends)
        # And can be further grouped in a later step if needed
        # %ilike% is a case-insensitive convenience/wrapper function is from the data.table package
        # ?Q?: How would you alternatively write this function without the data.table package?
        # ?Q?: The ordering of this case_when matters! Why?
        mutate(x = case_when(x %ilike% "quad" ~ "The Quad",
                             x %ilike% "\\bpret" ~ "Pret a Manger",
                             x %ilike% "park" ~ "Millennium/Grant Park",
                             x %ilike% "sears|willis" ~ "Sears/Willis Tower",
                             x %ilike% "reynolds" ~ "Reynold's Club",
                             #x %ilike% "\\bart\\b" ~ "Art Institute",
                             x %ilike% "\\bex\\b" ~ "Ex Libris",
                             x %ilike% dining ~ "Other cafe/dining",
                             x %ilike% bean ~ "The Bean",
                             x %ilike% thereg ~ "The Reg",
                             x %ilike% library ~ "Other library",
                             x %ilike% dorm ~ "Dorms",
                             x %ilike% residence ~ "Other residence",
                             x %ilike% recent ~ "Recent meeting location",
                             x %ilike% uc_other ~ "Other UChicago",
                             x %ilike% chi_other ~ "Other Chicago landmark",
                             TRUE ~ "Other"))
    
    # ?Q?: How could you continue this function to specify some groupings that you'd want dependent on
    # whether the meetup is UChicago-specific? Maybe for strangers you want to name more Chicago
    # landmarks (e.g., Navy Pier) and collapse all cafe/dining into a single category. Maybe for UChicago
    # students & friends you want to name more libraries or specify dorms but collapse all off-campus
    # landmarks. Maybe there's a group that makes sense for either students or friends but not both...etc.
    # With an added argument you could also change things just for specific respondent groups (like adding in
    # the `kbg` grouping for the psychology-grad-student/lab-meeting testers)
    # if (colName %ilike% c("friend", "student")) {
    #     # uchicago specific cleaning 
    # } else if (...) {...}
    if (colName %ilike% "str|student") {
        y <- y %>%
            # ?Q?: What's the point of including these filters?
            mutate(err = case_when(x == "Recent meeting location" ~ TRUE,
                                  (x %ilike% "residence|Dorm") & (.data[[colName]] %ilike% "their") ~ TRUE,
                                   TRUE ~ err))
    }
    y %>% 
        filter(err == FALSE) %>% 
        mutate(x = fct_infreq(factor(x))) %>% # Sorting factor levels by frequency (desc.) will make plots and tables easier
        arrange(x) %>% 
        rename_with(~ paste0(as.character(colName), "_clean"), x)

}

# ?Q?: The clean_meetup() function has a lot that can go wrong! Can you add in some checks along the way that give the user informative error messages?




### plot_meetup() ###

# Create pie chart of responses for meetup location questions
# levDisplay : how many levels labels to display in legend; display n most frequent response levels
# show_n : if TRUE, include subtitle with number of responses
# title, caption : adds title/caption to labs() if specified

plot_meetup <- function(data, colName, levDisplay=5, show_n = TRUE, title="", caption="") {
    display_levels <- levels(data[[1]])[1:levDisplay] # clean_meetup() has already sorted by frequency
    
    p <- ggplot(filter(data,!is.na(.data[[colName]])), aes(x="", fill=.data[[colName]])) +
        geom_bar(width=1) +
        coord_polar("y", start=0) +
        theme_void() + # theme_void() will hide radials but will also hide anything theme elements defined before it, so don't put it at the end!
        #theme(legend.position = "none") +
        scale_fill_discrete(breaks=display_levels,
                            type=colors_100[1:length(levels(data[[colName]]))]) +
        geom_label(stat="count", aes(label=after_stat(count), group=fct_infreq(.data[[colName]])), 
                   position = position_stack(vjust = .5),
                   show.legend = F,
                   fill="white") +
        labs(fill = paste0("Top ", levDisplay, " Meetup Locations"))
    if (show_n == TRUE) {
        p <- p + labs(subtitle = paste0(nrow(filter(data,!is.na(.data[[colName]]))), " total responses"))
    } 
    if (title != "") {
        p <- p + labs(title = title)
    }
    if (caption != "") {
        p <- p + labs(caption = caption)
    }
    print(p)
}


### kable_responses() ###
# Create table/kable of responses
# Not limited to meetup location responses

kbl_responses <- function(data, colName) {
    
    data$Response <- data[[colName]]
    
    data %>% 
        group_by(Response) %>% 
        summarize(N = n(), 
                  Percent = round((N/nrow(data)*100), 2)) %>% 
        arrange(desc(Percent)) %>% 
        kbl()
}

    