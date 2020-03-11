# Libraries ---------------------------------------------------------------

# If you're missing any of the packages, see the README at
# https://github.com/jrogol/OverDRIVE2020

# These packages are all part of the `tidyverse`
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(forcats)

library(here)

# Accessing Files ---------------------------------------------------------

# Set a working directory and load a .csv file
setwd("C:/Users/jr2ef/Documents/ProjectDirectory/Output/")

data <- read_csv("ratings.csv")

#### QUESTION: Why won't this work on your machine? ####

# myScript.R is in the /ProjectDirectory folder.
source("myScript.R")

#### QUESTION: What's the result? ####


# File Structure ----------------------------------------------------------


# When the data is downloaded from the database, it's an "Output" 
myData <- read_csv("Output/rawData.csv")

# But someone else might place the same file in a "Data" folder.
yourData <- read_csv("Data/rawData.csv")


#### QUESTION: How can this be addressed? ####


# The here package --------------------------------------------------------

# The here package is one solution to locating file paths. It looks for short
# cuts to identify which directories are "home" without the hard-coded paths of
# `setwd()`. Typically, this is an .Rproj file (a great reminder to use projects
# in Rstudio!) or version control files (like .git or .svn).

# As an added benefit, it also autocompletes in a smart way!


# The following magically reads in the data from the "Data" folder.
basicReport <- read_csv(here("Data","basicReport.csv"))


# Sampling ----------------------------------------------------------------

# Obtaining a random sample can be useful when developing models. In fact, some
# models use a random starting point as the first step in the algorithm.


#### ACTIVITY: Run the following and compare your results with the output below ####

# Find 5 Randomly generated samples from a uniform distribution between 1 and 0
runif(5,min = 0, max = 1)
# [OUTPUT] 0.89469325 0.11537113 0.04477002 0.79655626 0.17113289



#### ACTIVITY: Cluster Alumni based on data in basicReport ####

# For simplicity's sake, we'll use only numeric variables.

df <- basicReport %>% 
  # Only select the numeric variables.
  select_if(is.numeric) %>% 
  # Remove missing values for the capacity range and last gift.
  filter_at(vars(low_egc,high_egc,last_gift_amount),
            all_vars(!is.na(.))) %>% 
  # Replace missing annual giving totals with 0.
  mutate_all(replace_na,0)


# Create 5 clusters using the K-means algorithm
km.out <- kmeans(df,5)

# Repeat the process
km.out2 <- kmeans(df,5)

#### ACTIVITY: Compare the results of the two models ####

# Do the clusters align between the two results?

# Creates a cross-tab table of the two models' clusters.
table(km.out$cluster,km.out2$cluster)


#### ACTIVITY: Re-run the models, setting the seed. ####

# Set the seed below to the same number.
set.seed()
km.out3 <- kmeans(df,5)
set.seed()
km.out4 <- kmeans(df,5)

# Compare the results, as before.
table(km.out3$cluster,km.out4$cluster)


# This is also true when running other random features. The following will
# produce the same results when you run the code!

#### ACTIVITY: Run the followign code and compare the output. ####

# Obtain five random samples from a uniform distribution between 1 and 0.
set.seed(323)
runif(5,min = 0, max = 1)
# [OUTPUT] 0.2969238 0.9105509 0.8877946 0.2445804 0.7742418


# Parsing a Date-String ---------------------------------------------------

# This scenario is based on one found with ADVANCE data, wherein some dates (in
# this case, birth dates), asre stored as strings.

# Load some synthetic constituent data
donorBio <- read_csv(here("Data","donorBio.csv"))

# Select just the ID and BIRTH_DT fields, for simplicity
dates <- donorBio %>% 
  select(ID,BIRTH_DT)

#### QUESTION: How do the dates appear to be stored? ####

head(dates)

#### ACTIVITY: Try converting `BIRTH_DT` to a date class. ####

datesConverted <- dates %>% 
  mutate(date = as.Date(BIRTH_DT))

# We need to specify the format.
datesConverted <- dates %>% 
  mutate(date = as.Date(BIRTH_DT,
                        format = ""))

# We can then calculate an approximate age.
datesConverted <- datesConverted %>% 
  # Subtracting date objects shows the difference in days as a string, which is
  # converted and rounded.
  mutate(age = round(as.numeric((Sys.Date() - date)/365)))

#### ACTIVITY: Check the results. Are they as expected? ####

# Looks good!
head(datesConverted)

# ...or Does it?
tail(datesConverted)

#### QUESTION: How to we work around this? ####


# Reshaping Data ----------------------------------------------------------


# Let's look at some basic giving data

giving <- read_csv("Data/currentGiving.csv")

#### QUESTION: How is the data formatted? ####

head(giving)

#### ACTIVITY: Convert the giving to appropriate class  ####

# Replace `newFunction` as you see fit.
giving <- giving %>% 
  mutate(CurrFYGiving = newFunction(CurrFYGiving))

# Combine the giving data with the donor's biographic data.
givingData <- donorBio %>% 
  select(ID, SCHOOL) %>% 
  left_join(giving, by = "")

#### ACTIVITY: Summarize the year ####

# Suppose you were asked to find The total amount of giving for the current
# fiscal year,along with the number of alumni and alumni donors.

# Hint: Non-Alumni will be missing `SCHOOL`

FY20Total <- givingData %>% 
  # Exclude non-alumni, who are missing `SCHOOL`
  filter() %>% 
  summarize(total = ,
            alums = ,
            donors = )

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

#### ACTIVITY: Repeat the above, but for School of Medicine Alumni ONLY ####

# Suppose the Med school wants to focus on their alumni ONLY. You could rewrite
# the above:

FY20Total <- givingData %>% 
  # Exclude non-alumni, who are missing `SCHOOL`
  filter(SCHOOL == "Medicine") %>% 
  summarize(total = ,
            alums = ,
            donors = )

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
#### QUESTION: How would you approach it for other schools more reproducibly? ####



#### ACTIVITY: Try Changing the school (e.g. "Engineering") ####



#### ACTIVITY: More Data ####

# A new request comes in, asking for a similar summary, but over the course of
# the past three fiscal years. A colleague sends you the following data.

threeYr <- read_csv("Data/threeYrGiving.csv")

#### QUESTION: How does this connect to the existing data? What about the formatting? ####





# Explore on your own -----------------------------------------------------

# When outlining a project, keeping the work in a self-contained document (like
# a jupyter notebook or Rmarkdown) can help combine the code and prose and work
# through an analysis. I've included the custom format that I like to use as a
# template - see "Markdown/skeleton.Rmd"

# Start exploring the data on your own, and ask questions that might interest
# you.  Consider the "inflection points" - those variables which may change from
# one request to another when performing similar analyses.

# More giving data that joins with givingData
otherGiving <- read_csv("data/otherGiving.csv")

# Sample contact reports
contactReports <- read_csv(here("Data/contactReports.csv"))

