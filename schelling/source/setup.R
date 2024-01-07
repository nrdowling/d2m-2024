### PREPARE RESPONSE DATA ###

## MAKE HUMAN-FRIENDLY ##

# Sensible column names
# The resulting google sheet from a google form uses the full text of each question as the column name, which is impractical for analysis.

responses <- responses_raw

colnames(responses) <- c('timestamp', 'str_where', 'str_when', 'student_where', 'friend_where', 'coin_same', 'coin_different', 'number_0to10', 'number_selection', 'number_big', 'uc_status', 'demos', 'language', 'str_error','student_error','friend_error')

responses <- responses %>% 
    mutate(across(ends_with("error"), replace_na, FALSE))

list_cols <- which(sapply(responses,class)=="list")
responses[,list_cols] <- sapply(responses[,list_cols], as.character)


## ACCOUNT FOR DIFFERENT RESPONDENT GROUPS ##

# Assign participants to groups based on timestamps of their responses
# This survey has been used to collect data and give examples across multiple years and in multiple settings/audiences. 
# We might want to account for differences across time (e.g. mid-COVID responses could be odd; dorms & establishments open and close over the years; seasons in Chicago may impact indoor/outdoor meeting choices)
# We might want to account for differences across audiences (e.g. students in a 200 person audience might feel more anonymous and be more candid or be paying less attention; housing and on-campus time are different between undergrad and grad students)

# UPDATE REGULARLY!!
# April/May 2020: Undergraduates in a remote quarter of the large lecture class "Mind" (this was the 1st quarter of remote learning so all students had at least 2 quarters of experience being on campus)
# February 2nd 2023: Attendees at the SGM lab meeting; primarily psychology graduate students and postdocs
# February 7th-9th 2023: Undergrads in Mind; most responded in-person on 2/7 but some watched the lecture and responded in the following few days 
# February 2023 besides the dates above: "testers" who all had experience living and attending/working at UChicago but were otherwise diverse









### MISC & AESTHETIC

# "Random" contrasting colors generated with http://phrogz.net/css/distinct-colors.html
# We need to communicate that there is a lot of variation in responses. We'd like to be able to clearly discern what the most common responses are (ie map color to factor level) but it's not important to be able to visually map most colors to their labels. 
# Sites like this one ^^ can generate n colors and interweave their order to meet these needs. ggplot can pick colors on its own, but chances are that with this many color values it will be pretty ugly. Feeding in a pre-set, pre-sorted list of hex colors can make the chaos a little easier to look at for everyone *and* you can strategically create palattes that are (relatively) colorblind friendly.

colors_string <- "#ff9e9c, #00c28b, #ed88db, #bd3f35, #008862, #ff00c1, #eb6758, #00eeb3, #9b3781, #ff9e8a, #004638, #ffa2dc, #be3e22, #30debd, #ff009f, #ed6745, #006c5a, #d76cae, #ff9d77, #00a58c, #b80071, #cac6bf, #00c3be, #841759, #b2aa9b, #006d6a, #6a003a, #615e58, #004647, #9a0052, #494640, #00f0f4, #ff0085, #33302b, #00a7af, #cd0067, #d4c5a3, #00d2e7, #b14276, #8a8374, #006e7a, #ff006d, #675d48, #008da2, #fa86b8, #92825a, #004756, #bf004c, #37301c, #003256, #9f0036, #c2a964, #23496f, #6e0026, #514629, #3e6088, #ff004d, #98823e, #6d85a8, #e1698c, #3f2f00, #42536c, #a83352, #6e5d25, #263140, #ffa0b8, #554500, #a9c9f6, #c30030, #007900, #bcc7db, #700018, #75dd7a, #7e848e, #ff0035, #003e07, #595f68, #a1001d, #005111, #424750, #c4001c, #00a32d, #c5c6c8, #ff8494, #006b1e, #838485, #ac313a, #00ed78, #464748, #710005, #00c166, #303032, #a20004, #006c39, #ff5bfa, #e96869, #009858, #610057, #004629, #7c1b70"

# The site didn't really give us comma-separated values, it gave us a string. We have to make it a vector.
colors_100 <- str_split_1(colors_string, ", ")


# Quick labels for in-text reference to specific values

str_times <- as.character(filter(responses, !is.na(str_when))$str_when)

on_the_hour <- sum(grepl("*:00:00", str_times))

on_the_thirty <- sum(grepl("*:30:00", str_times))

on_anything_else <- sum(grepl("*:00:00|*:30:00", str_times)==FALSE)

most_pop_time <- as.POSIXct(levels(fct_infreq(str_times))[1])

love_connections <- sum(most_pop_time == str_times)

lonely_hearts <- ifelse(on_anything_else ==1, "person", "people")

