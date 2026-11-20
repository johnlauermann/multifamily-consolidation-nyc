
# Change in rent stabilized buildings -------------------------------------

library(tidyverse)
library(here)
library(readxl)
library(stringr)

i_am("code/rent_stabilization.r")


# Load and clean data --------------------------------------------------------
# Data are archival records, mostly by request from the City. They required manual processing before use.
# Contact research team to discuss replication data. 

# rent regulated buildings
data_2020 <- read_excel(here("rent_stabilization_data/2020 DHCR Citywide.xlsx"))
data_2022 <- read_excel(here("rent_stabilization_data/2022 DHCR Citywide.xlsx"))
data_2023 <- read_excel(here("rent_stabilization_data/2023 DHCR Citywide.xlsx"))
data_2024 <- read_excel(here("rent_stabilization_data/2024 DHCR Citywide.xlsx"))
data_2025 <- read.csv(here("rent_stabilization_data/2025-DHCR-Citywide.csv"))

# add BBL identifiers
frames <- mget(ls(pattern = "^data_"))

frames <- map(frames, ~.x %>%
                mutate(
                  borough = case_when(
                    COUNTY == 60 ~ 2,
                    COUNTY == 61 ~ 3,
                    COUNTY == 62 ~ 1,
                    COUNTY == 63 ~ 4,
                    COUNTY == 64 ~ 5, 
                    .default = NA_integer_),
                  BBL = paste0(
                    borough, 
                    str_pad(BLOCK, width = 5, side = "left", pad = "0"), 
                    str_pad(LOT, width = 4, side = "left", pad = "0"))
                )
)

list2env(frames, envir = .GlobalEnv)

# property records
# obtained from NYC Planning archives: https://s-media.nyc.gov/agencies/dcp/assets/files/zip/data-tools/bytes/pluto/nyc_pluto_20v8_arc_csv.zip
pluto_2020 <- read.csv(here("data/pluto_20v8.csv")) %>%
  mutate(BBL = as.character(bbl))



# Identify change in building lists ---------------------------------------

# find records on past but not present lists
loss_20to25 <- anti_join(data_2020, data_2025, by = "BBL")
loss_22to25 <- anti_join(data_2022, data_2025, by = "BBL")
loss_23to25 <- anti_join(data_2023, data_2025, by = "BBL")
loss_24to25 <- anti_join(data_2024, data_2025, by = "BBL")

# identify unique buildings
loss <- rbind(loss_20to25, loss_22to25, loss_23to25, loss_24to25) %>%
  distinct(BBL, .keep_all = TRUE)

# join to property records
loss <- inner_join(x = loss, 
                   y = pluto_2020 %>%
                     select(BBL, ownername, unitstotal, unitsres, assesstot, latitude, longitude),
                   by = "BBL")
                   


# Save results ------------------------------------------------------------
write.csv(x = loss, 
          file = here("loss_rent_regulated.csv"), 
          na = "", row.names = FALSE)

          