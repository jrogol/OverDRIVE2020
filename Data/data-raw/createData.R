library(dplyr)
library(readr)
library(readxl)



# Donor Data --------------------------------------------------------------

donorData <- read_csv(here::here("Data/data-raw/data_science_for_fundraising_donor_data.csv"))

# Donor data consists of one row per ID.

#### Split the data into Biographic and Giving Information

donorData_bio <- donorData %>% 
  select(-contains("FY"))


donorData_giving <- donorData %>% 
  select(ID,contains("FY"))


# Create YYYYMMDD date field

YYYYMMDD <- function(date){
  # Use an early return if the Date is NA
  if (is.na(date)){
    return("00000000")
  }
  
  year <- lubridate::year(date)
  month <-  lubridate::month(date)
  day <-  lubridate::day(date)
  
  c(year,month, day) %>% 
    purrr::map(stringr::str_pad,2,pad = "0") %>% 
    paste0(collapse = "")
}

donorData_bio <- donorData_bio %>% 
  mutate(BIRTH_DATE = purrr::map_chr(BIRTH_DATE,YYYYMMDD))

#### 10% of the rows will have a year only

set.seed(20200323)

yearOnly <- donorData_bio %>% 
  select(ID,BIRTH_DATE) %>% 
  filter(BIRTH_DATE != "00000000") %>% 
  sample_frac(.1) %>% 
  rename(BIRTH_DT = BIRTH_DATE) %>% 
  mutate(BIRTH_DT = gsub("\\d{4}$","0000",BIRTH_DT))

donorData_bio <- donorData_bio %>% 
  left_join(yearOnly, by = "ID") %>% 
  mutate(BIRTH_DT = coalesce(BIRTH_DT,BIRTH_DATE)) %>% 
  select(-BIRTH_DATE)

#### Add School and clean up Degree level
donorData_bio <- donorData_bio %>% 
  mutate(SCHOOL = case_when(DEGREE_LEVEL == "UB" ~ "Arts and Sciences",
                                  DEGREE_LEVEL == "GM" ~ "Graduate Arts and Sciences",
                                  DEGREE_LEVEL == "GP" ~ "Business",
                                  DEGREE_LEVEL == "GD" ~ "Law",
                                  DEGREE_LEVEL == "GC" ~ "Medicine",
                                  DEGREE_LEVEL == "UC" ~ "Nursing",
                                  DEGREE_LEVEL == "UG" ~ "Data Science",
                                  ALUMNUS_IND == "Y" ~ "Engineering",
                                  TRUE ~ NA_character_),
         DEGREE_LEVEL = case_when(grepl("G",DEGREE_LEVEL) ~ "G",
                                  grepl("U",DEGREE_LEVEL) ~ "U",
                                  TRUE ~ DEGREE_LEVEL))

write_csv(donorData_bio,here::here("Data/donorBio.csv"))

#### Split Giving into Current and Past giving

currentVars <- c("ID","CurrFYGiving")

# Parse the number from all giving numbers, save for the current FY
donorData_giving <- donorData_giving %>% 
  mutate_at(vars(-!!currentVars),parse_number)

# Create Data for current FY
donorData_current <- donorData_giving %>% 
  select(!!currentVars) %>% 
  write_csv(here::here("Data/currentGiving.csv"))

# create a data set of the last two FY, which will be used to create a three year analysis.
# Data only contains non-zero values.

donorData_threeYear <- donorData_giving %>% 
  select(ID, PrevFYGiving, contains("4")) %>% 
  tidyr::gather(year,giving,-ID) %>% 
  mutate(year = case_when(!grepl("\\d",year) ~ lubridate::year(Sys.Date()) - 1,
                          TRUE ~ lubridate::year(Sys.Date()) - 2 )) %>% 
  filter(giving > 0) %>% 
  write_csv(here::here("Data/threeYrGiving.csv"))


# Remaining Years for longer window.
donorData_otherGiving <- donorData_giving %>% 
  select(ID,matches("[1-3]")) %>% 
  write_csv(here::here("Data/otherGiving.csv"))





# Contact Reports ---------------------------------------------------------

contactReports <- read_csv(here::here("Data/data-raw/data_science_for_fundraising_contact_reports.csv"))

#### Add a report ID
contactReports_clean <- contactReports %>% 
  janitor::clean_names("small_camel") %>% 
  arrange(date) %>% 
  mutate(reportID = 100 + row_number()) %>% 
  write_csv(here::here("Data/contactReports.csv"))

# Clean Lasala Data -------------------------------------------------------

donorData2 <- read_excel(here::here("Data/data-raw/lasala_das_2018.xlsx"),
           sheet = "Sample Data Set")

donorData2 %>% 
  janitor::clean_names() %>% 
  write_csv(here::here("Data/basicReport.csv"))
