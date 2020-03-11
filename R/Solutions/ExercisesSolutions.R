# Libraries ---------------------------------------------------------------

library(dplyr)
library(here)
library(readr)
library(tidyr)
library(ggplot2)
library(forcats)

# Accessing Files ---------------------------------------------------------

# Set a working directory and load a .csv file
setwd("C:/Users/jr2ef/Documents/ProjectDirectory/Output/")

data <- read_csv("ratings.csv")
# This works just fine on my machine - what about yours?

# myScript.R is in the /ProjectDirectory folder. Why won't this work?
source("myScript.R")


# File Structure ----------------------------------------------------------


# When the data is downloaded from the database, it's an "Output" 
myData <- read_csv("Output/rawData.csv")

# But someone else might place the same file in a "Data" folder.
yourData <- read_csv("Data/rawData.csv")

# Standardizing the file structure for every analysis is helpful!

# The following function helps create folders for a project if they don't yet
# exist.

createStructure <- function(...) {
  
  exists <- c()
  
  created <- c()
  
  l <- list(...)
  
  if (length(l) == 0) {
    l <- list("Data","Markdown","Output", "Reports","SQL","R", "Snippets","Assets")
  }
  
  for (i in l) {
    if (!dir.exists(i)) {
      dir.create(i)
      created <- c(created,i)
    } else {
      exists <- c(exists,i)
    }
  }
  
  if (length(exists) > 0) {
    message(sprintf("Folder(s) already exist for: %s",paste(exists,
                                                            collapse = ", ")))
  }
  
  if (length(created) > 0) {
    message(sprintf("Created folder(s) for: %s",paste(created, collapse = ", ")))
    
  }
}

# Rather than copying and pasting the function into the console, I've placed it
# in an *R package*, and shared it interally. Package creation is beyond the
# scope of this workshop, but for those interested, here are a few
# introductions:
# *  https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/ 
# *  https://tinyheero.github.io/jekyll/update/2015/07/26/making-your-first-R-package.html
# *  The deep dive: http://r-pkgs.had.co.nz/




# The here package --------------------------------------------------------

# The here package is one solution to locating file paths. It looks for short
# cuts to identify which directories are "home" without the hard-coded paths of
# `setwd()`. Typically, this is an .Rproj file (a great reminder to use projects
# in Rstudio!) or version control files (like .git or .svn)


# The following magically reads in the data from the "Data" folder.
basicReport <- read_csv(here("Data","basicReport.csv"))


# Sampling ----------------------------------------------------------------

# Obtaining a random sample can be useful when developing models. In fact, some
# models use a random starting point as the first step in the algorithm.


# Find 5 Randomly generated samples from a uniform distribution between 1 and 0

# Run the following and compare to the output below.
runif(5,min = 0, max = 1)
# [1] 0.89469325 0.11537113 0.04477002 0.79655626 0.17113289



# While the randomness is the point, it's certainly not reproducible. To better
# illustrate this point, let's try and create 5 clusters of donors based on the
# numeric variables in the "basicReport" data set.

df <- basicReport %>% 
  # Only select the numeric variables.
  select_if(is.numeric) %>% 
  # Remove missing values for the capacity range and last gift.
  filter_at(vars(low_egc,high_egc,last_gift_amount),
            all_vars(!is.na(.))) %>% 
  # Replace missing annual giving totals with 0.
  mutate_all(tidyr::replace_na,0)


# Create 5 clusters using the K-means algorithm
km.out <- kmeans(df,5)

# Repeat the process
km.out2 <- kmeans(df,5)

# Comparing the results of the two models, we can see that the two models do not
# yeild the same results.
table(km.out$cluster,km.out2$cluster)

#     1   2   3   4   5
# 1   9   0   0   0   0
# 2   0   0  30  72   0
# 3   0   6   0   0   0
# 4   0   0   0   0 115
# 5   0   0  12   0   0

# K Means uses a random start point, so the results will always provide a
# *local* optimum solution, which isn't always the *global* or overall best
# result.

# To get around this issue, we can use the `set.seed()` function.


set.seed(2020)
km.out3 <- kmeans(df,5)
set.seed(2020)
km.out4 <- kmeans(df,5)

# With the seed set before building each model, the results are identical!
table(km.out3$cluster,km.out4$cluster)

#     1   2   3   4   5
# 1  42   0   0   0   0
# 2   0   9   0   0   0
# 3   0   0 121   0   0
# 4   0   0   0  69   0
# 5   0   0   0   0   3

# This is also true when running other random features. The following will
# produce the same results when you run the code!

set.seed(323)
runif(5,min = 0, max = 1)
# [1] 0.2969238 0.9105509 0.8877946 0.2445804 0.7742418


# Parsing a Date-String ---------------------------------------------------

# This scenario is based on one found with ADVANCE data, wherein some dates (in
# this case, birth dates), asre stored as strings.

# Load some synthetic constituent data
donorBio <- read_csv(here("Data","donorBio.csv"))

# Select just the ID and BIRTH_DT fields, for simplicity
dates <- donorBio %>% 
  select(ID,BIRTH_DT)

head(dates)
# Glancing at the data, BIRTH_DT is an 8-character string: YYYYMMDD.

# Try converting the string to a date
datesConverted <- dates %>% 
  mutate(date = as.Date(BIRTH_DT))

# That didn't work. We need to specify the format. The
datesConverted <- dates %>% 
  mutate(date = as.Date(BIRTH_DT,
                        format = "%Y%m%d"))

# We can then calculate an approximate age.
datesConverted <- datesConverted %>% 
  # Subtracting date objects shows the difference in days as a string, which is
  # converted and rounded.
  mutate(age = round(as.numeric((Sys.Date() - date)/365)))

# Looks good!
head(datesConverted)

# ...or Does it?
tail(datesConverted)

# How can we work around this?

# One option is to extract the year from the string.
getYear <- function(dateString) {
  # Get the first four characters
  year <- substr(dateString,1,4)
  
  # Find those with no data, i.e. "00000000"
  noYear <- year == "0000"
  # ...and set them to NA
  year[noYear] <- NA
  
  # Return the results as a number.
  as.numeric(year)
}

# Note that this is a simple, monotonic function. It doesn't attempt to do too
# much, making it easy to troubleshoot.

withAge <- dates %>% 
  mutate(year = getYear(BIRTH_DT),
         approxAge = as.numeric(format(Sys.Date(),"%Y")) - year)

# How are things looking now?
tail(withAge)

# A More sophisticated option can synthesize an actual date object, setting
# missing months to January, and missing days to the first of the month.

approxDate <- function(date){
  
  year <- substr(date,1,4)
  month <- substr(date,5,6)
  day <- substr(date,7,8)
  
  noYear <- year == "0000"
  
  noMonth <- month == "00" | is.na(month)
  
  month[noMonth] <- "01"
  
  noDay <- day == "00" | is.na(day)
  
  day[noDay] <- "01"
  
  d <- as.Date(paste(year,month,day,sep="-"))
  
  d[noYear] <- NA
  
  d
}

# Reshaping Data ----------------------------------------------------------


# Let's look at some basic giving data

giving <- read_csv("Data/currentGiving.csv")

head(giving)

# The dollar amounts are formatted like currency - convert them to a number.
giving <- giving %>% 
  mutate(CurrFYGiving = parse_number(CurrFYGiving))

# Combine the giving data with the donor's biographic data, using `ID` as the
# key.
givingData <- donorBio %>% 
  select(ID, SCHOOL) %>% 
  left_join(giving, by = "ID")

# Suppose you were asked to find The total amount of giving for the current
# fiscal year,along with the number of alumni and alumni donors.
FY20Total <- givingData %>% 
  # Exclude non-alumni, who are missing `SCHOOL`
  filter(!is.na(SCHOOL)) %>% 
  summarize(total = sum(CurrFYGiving),
            alums = n(),
            donors = sum(CurrFYGiving>0))

# And then plot a bar graph of donors vs. non Donors
FY20Total %>% 
  mutate(nonDonors = alums - donors) %>% 
  select(donors,nonDonors) %>% 
  gather(type, count) %>% 
  mutate(segment = "All Alumni") %>% 
  # Create a stacked bar chart with donors on the bottom.
  ggplot(aes(x= segment, y = count, fill = fct_rev(type))) +
  geom_bar(postition = "stack", stat = "identity") +
  labs(title = "Alumni Participation in FY20",
       fill = NULL)

# Suppose the Med school wants to focus on their alumni ONLY. You could rewrite
# the above:
FY20Total <- givingData %>% 
  # Exclude non-alumni, who are missing `SCHOOL`
  filter(SCHOOL == "Medicine") %>% 
  summarize(total = sum(CurrFYGiving),
            alums = n(),
            donors = sum(CurrFYGiving>0))

# Easy enough, but you'd also have to change the Titles by hand!

FY20Total %>% 
  mutate(nonDonors = alums - donors) %>% 
  select(donors,nonDonors) %>% 
  gather(type, count) %>% 
  mutate(segment = "School of Medicine") %>% 
  # Create a stacked bar chart with donors on the bottom.
  ggplot(aes(x= segment, y = count, fill = fct_rev(type))) +
  geom_bar(stat = "identity") +
  labs(title = "School of Medicine Alumni Participation in FY20",
       fill = NULL)

# How would you approach it for other schools more reproducibly?

# One option: use a variable for the School. This is especially helpful if you use the school 

school <- "Medicine"

FY20 <- givingData %>% 
  group_by(SCHOOL) %>% 
  summarize(total = sum(CurrFYGiving),
            alums = n(),
            donors = sum(CurrFYGiving>0))

FY20 %>% 
  filter(SCHOOL == school) %>% 
  mutate(nonDonors = alums - donors) %>% 
  select(donors,nonDonors) %>% 
  gather(type, count) %>% 
  mutate(segment = paste("School of ",school)) %>% 
  # Create a stacked bar chart with donors on the bottom.
  ggplot(aes(x= segment, y = count, fill = fct_rev(type))) +
  geom_bar(stat = "identity") +
  labs(title = paste("School of",school,
                     "Alumni Participation in FY20"),
       fill = NULL)

# Change it!

school <- "Engineering"

FY20 %>% 
  filter(SCHOOL == school) %>% 
  mutate(nonDonors = alums - donors) %>% 
  select(donors,nonDonors) %>% 
  gather(type, count) %>% 
  mutate(segment = paste("School of ",school)) %>% 
  # Create a stacked bar chart with donors on the bottom.
  ggplot(aes(x= segment, y = count, fill = fct_rev(type))) +
  geom_bar(stat = "identity") +
  labs(title = paste("School of",school,
                     "Alumni Participation in FY20"),
       fill = NULL)


# More Data ---------------------------------------------------------------

# A new request comes in, asking for a similar summary, but over the course of the past three fiscal years. A colleague sends you the following data.

threeYr <- read_csv("Data/threeYrGiving.csv")

head(threeYr)

# How does this connect to the existing data? What about the formatting?

# One option would be to reshape the original data to make it a year:

reshaped <- givingData %>% 
  mutate(year = 2020) %>% 
  rename(giving = CurrFYGiving)

reshaped <- reshaped %>% 
  select(ID, SCHOOL) %>% 
  left_join(threeYr, by = "ID") %>% 
  bind_rows(reshaped) %>% 
  filter(!is.na(year))

# Another option would be to reshape the new data from long (each ID/year
# combination on a new line) to wide (one row per ID)

threeYrGiving <- threeYr %>% 
  mutate(year = paste0("FY",year)) %>% 
  spread(year, giving,
         fill = 0)

head(threeYrGiving)

# Then combine with the givingData.
threeYear <- givingData %>% 
  # Make the column name consistent
  rename(FY2020 = CurrFYGiving) %>%
  # Left joins keep all the IDs from givingData - the new data only includes
  # those who've donated!
  left_join(threeYrGiving, by = "ID") %>% 
  # Fill '18 and '19 missing values with 0.
  mutate_if(is.numeric, replace_na,0)

head(threeYear)

# Reshape again, this time to long, so that both `SCHOOL` and `year` can be
# variables.
threeYear_reshape <- threeYear %>% 
  gather(year, giving, -c(ID, SCHOOL)) %>% 
  # Extract the year from the "FYxxxx" strings.
  mutate(year = parse_number(year))

head(threeYear_reshape)

three_YearTotals <- threeYear_reshape %>% 
  group_by(SCHOOL,year) %>%
  summarize(total = sum(giving),
            alums = n_distinct(ID),
            donors = sum(giving>0))

# Set a variable for the year
yr <- 2020

# And filter with *two* variables.
three_YearTotals %>% 
  filter(SCHOOL == school,
         year <= yr)


# Explore on your own -----------------------------------------------------

# When outlining a project, keeping the work in a self-contained document (like
# a jupyter notebook or Rmarkdown) can help combine the code and prose and work
# through an analysis. I've included the custom format that I like to use as a
# template - see "Markdown/skeleton.Rmd"

# Start exploring the data on your own, and ask questions that might interest
# you.

# More giving data that joins with givingData
otherGiving <- read_csv("data/otherGiving.csv")

# Sample contact reports
contactReports <- read_csv(here("Data/contactReports.csv"))

