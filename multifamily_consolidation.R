
# Explore data for multifamily home project -------------------------------

library(here)
library(socratadata)
library(dplyr)
library(tidyr)
library(lubridate)

here::i_am("code/multifamily_consolidation.r")


# Pull NYC Open Data ------------------------------------------------------

# set up for api
soc_api_key_id = "your api key here"
dataset_query <- soc_discover(domains = "data.cityofnewyork.us",
                              query = "occupancy")


# certificate of occupancy
dob_occupancy <- soc_read(url = "https://data.cityofnewyork.us/d/bs8b-p36w", 
                 page_size = 5000)
soc_metadata(dob_occupancy)


# pluto
pluto <- soc_read(url = "https://data.cityofnewyork.us/d/64uk-42ks",
                  query = soc_query(select = "BBL, zonedist1, assessland, assesstot, exempttot, ownername"),
                  page_size = 10000)



# Identify consolidation properties ---------------------------------------

# query consolidation certifications
consolidations <- dob_occupancy %>%
  
  ## clean up data format
  mutate(
    pr_dwelling_unit = as.numeric(pr_dwelling_unit), 
    ex_dwelling_unit = as.numeric(ex_dwelling_unit)
    ) %>%  
  
  ## remove erroneous entries
  filter(c_o_issue_date <= Sys.Date()) %>%
  
  ## find the consolidation pattern
  filter(
    pr_dwelling_unit > 0 &
      ex_dwelling_unit > 0 &
      pr_dwelling_unit < ex_dwelling_unit & 
      job_type == "A1" & 
      application_status_raw == "Issued",
      issue_type == "Final"
  )

# add zoning, ownership, assessment
consolidations <- inner_join(
  x = consolidations, 
  y = pluto %>%
    mutate(bbl = as.character(BBL)), 
  by = "bbl")

# find only single family consolidations
singlefam <- consolidations %>%
  filter(pr_dwelling_unit == 1)



# Save results ------------------------------------------------------------
write.csv(x = consolidations %>%
            select(-contains(c("location", "@"))),
          file = here("consolidations.csv"), na = "", row.names = FALSE)

