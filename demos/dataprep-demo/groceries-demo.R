
## FUNCTIONS AND CONDITIONALS ##

### The programmer's groceries ###

# A programmer’s partner says: 
# "Please go to the store and buy a carton of milk and if they have eggs, get six."
# The programmer returns with 6 cartons of milk. 
# The partner sees this and exclaims ‘Why the heck did you buy six cartons of milk?’ 
# The programmer replies "They had eggs."

#### The programmer's initial logic ####

# Let's start simple: 
# hard-defined eggs variable, conditional defines number of milk purchased,
# result pasted into terminal
# *not* a function

eggs <- T # Whether there were eggs in the store -- we can change this manually at this point

n.milk <- ifelse(eggs == TRUE, yes = 6, no = 1)

paste0("Here's the milk. I bought ", n.milk, ".")


# Now write a function that does the same thing:
# Instead of pre-defining eggs as TRUE, accept eggs as an argument that can change
# each time we run the function
# Note that defining a function does not return anything to the console
# It should add "shop" the this list of defined functions in our environment

shop <- function(eggs) { # do they have eggs? T/F
  if (eggs == TRUE) { # If there are eggs
    n.milk <- 6 # Get 6 cartons of milk
  } else { # If there are not eggs
    n.milk <- 1 # Get 1 carton of milk
  }
  paste0("Here's the milk. I bought ", n.milk, ".")
} 

shop(TRUE) # Run the function by setting the one and only argument (eggs) to either TRUE or FALSE

n.milk ## Is this what you expected after running the shop function?????

#### The second attempt ####
# Our logic should care whether the store has milk, not just eggs.
# Can't buy milk if they're out, no matter how many eggs are in stock!

# A programmer’s partner says: ‘Please go to the store and buy a carton of milk *if they have any* and if they have eggs, get six.’

# Before we make a function, we write some code that works with pre-defined arguments

milk <- TRUE # Whether there was milk in the store
eggs <- TRUE # Whether there were eggs in the store

if (milk == TRUE && eggs == TRUE) { # If there’s milk AND eggs
  n.milk <- 6 # Get 6 cartons of milk
} else if (milk == FALSE && eggs == TRUE) { # If just eggs
  n.milk <- 0 # Get 0 cartons of milk
} else if (milk == TRUE && eggs == FALSE) { # If just milk
  n.milk <- 1 # Get 1 carton of milk
} else { # If any other condition
  n.milk <- 0 # Get 0 cartons of milk
}

n.milk

# Does this work as expected? If so, we can define a function.
# In "real" code we would wrap this function definition around the existing conditional
# we just wrote rather than copying and pasting
# For demo purposes we'll leave it as a simple conditional above and a function below
# But notice how it's egg-sactly the same code!!
# ...sorry about that one

shop2 <- function(milk, eggs) { # do they have milk? eggs?
 
  if (milk == TRUE && eggs == TRUE) { # If there’s milk AND eggs
    n.milk <- 6 # Get 6 cartons of milk
  } else if (milk == FALSE && eggs == TRUE) { # If just eggs
    n.milk <- 0 # Get 0 cartons of milk
  } else if (milk == TRUE && eggs == FALSE) { # If just milk
    n.milk <- 1 # Get 1 carton of milk
  } else { # If any other condition
    n.milk <- 0 # Get 0 cartons of milk
  }
  
  # If I want to see anything in the console when I run the function
  # then I need to tell that to happen WITHIN the function
  # So after the whole conditional has evaluated, I include a return() or paste()

  return(n.milk) 
  
}

# Now what happens?

# Case 1: The store has milk but not eggs:
shop2(FALSE, FALSE)

# The programmer returns with 1 carton of milk.  
# The partner says, "You didn't get any eggs?" 
# The programmer replies, "They didn't have any. Hold on, you wanted eggs?" 
# The partner shrugs and says, "Doesn't matter."

# Case 2: The store has both milk and eggs
shop2(TRUE, TRUE)

n.milk 
# ?Q?: ^^^ n.milk here in the global enviornment isn't dependent on anything hat happened
# within the shop2, but there IS a way to make it work that way -- figure out how!

# The programmer returns with 6 cartons of milk. 
# The partner sees this and exclaims "Why the heck did you buy six cartons of milk?" 
# The programmer replies "They also had eggs." 
# "So where are the eggs then?" 
# The programmer exclaims, "Hold on, you wanted eggs?"

#### The third attempt ####

# In some cases we got the expected output, but not always
# We need to specify when and how the programmer should buy eggs, not just milk

# A programmer’s partner says: ‘Please go to the store and buy a carton of milk if they have any and if they have eggs, get six eggs.’

# We're good at functions now, so we'll construct the conditional within a function definition
# instead of the copy/paste we did for attempt 2

shop3 <- function(milk, eggs) {
  if (milk == TRUE && eggs == TRUE) { # If there’s milk AND eggs
    n.milk <- 1 # Get 1 carton of milk
    n.eggs <- 6 # Get 6 eggs
  } else if (milk == FALSE && eggs == TRUE) { # If just eggs
    n.milk <- 0 # Get 0 cartons of milk
    n.eggs <- 6 # Get 6 eggs
  } else if (milk == TRUE && eggs == FALSE) { # If just milk
    n.milk <- 1 # Get 1 carton of milk
    n.eggs <- 0 # Get 0 eggs
  } else { # If any other condition
    n.milk <- 0 # Get 0 cartons of milk
    n.eggs <- 0 # Get 0 eggs
  }
  paste0(n.milk, " milk and ", n.eggs, " eggs")
}

# Case 1: The store has milk and eggs.
# This is one case that didn't behave as expected in attempt 2
# because the programmer only came home with milk and no eggs

shop3(TRUE, FALSE)

# The programmer returns with 1 carton of milk and 6 eggs. 
# The partner sees this and exclaims, "Let's make omelets!" 

# ?Q?: What happens in cases 2-4 (see slides), with different combinations of 
# the store having milk and eggs?

# This function works, but it's unweildy

# ?Q?: Revise the conditional (or start from scratch!) to bring home 
# the right groceries without so much redundancy.

