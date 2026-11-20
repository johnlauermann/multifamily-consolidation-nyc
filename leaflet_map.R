
# Bundle data into a web map ----------------------------------------------


library(here)
library(tidyverse)
library(stringr)
library(sf)
library(leaflet)
library(htmlwidgets)

i_am("code/leaflet_map.r")


# Load data ---------------------------------------------------------------
# load & update
consolidations <- read.csv(here("consolidations.csv")) %>%
  filter(pr_dwelling_unit == 1) %>%
  mutate(
    owner_type = case_when(
      str_detect(ownername, "LLC") ~ "LLC",
      str_detect(ownername, "TRUST") ~ "Trust", 
      .default = "Individual"))

loss <- read.csv(here("loss_rent_regulated.csv")) %>%
  filter(unitsres >= 1) %>%
  mutate(
    owner_type = case_when(
      str_detect(ownername, "LLC") ~ "LLC",
      str_detect(ownername, "TRUST") ~ "Trust", 
      .default = "Individual"))


# convert to spatial
consolidations <- st_as_sf(x = consolidations,
                           coords = c("longitude", "latitude")) %>%
  st_set_crs(4326)

loss <- st_as_sf(x = loss %>% filter(!is.na(latitude)),
                 coords = c("longitude", "latitude")) %>%
  st_set_crs(4326)


# Map results -------------------------------------------------------------

# set up map elements
title_box <- "
  <div style='
    background: lightgray; 
    padding: 12px; 
    border-radius: 8px; 
    box-shadow: 0 0 15px rgba(0,0,0,0.2);
    font-family: Arial, sans-serif;
    max-width: 250px;
  '>
    <h3 style='margin: 0 0 6px 0; color: #333;'>Data Exploration</h3>
    <p style='margin: 0; font-size: 13px; color: #666; line-height: 1.2;'>
      This is an exploratory visualization housing data in NYC. 
      Toggle the layers on and off with the check boxes. 
      Click on a point to view information about it. 
    </p>
  </div>
  "

# build map
map <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = loss, 
                   color = "green", 
                   radius = 2, 
                   fillOpacity = .25, 
                   popup = ~paste("Address: ", BLDGNO1, STREET1, STSUFX1, CITY, "NY",
                                  "<br>",
                                  "Was owned by:", owner_type,
                                  "<br>",
                                  "Residential units:", unitsres,  
                                  "<br>",
                                  "Primary tax status:", STATUS1,
                                  "<br>",
                                  "Secondary tax status:", STATUS2, STATUS3),
                   group = "Lost Rent Regulated Buildings, 2020-2025") %>%
  addCircleMarkers(data = consolidations,
                   color = "blue",
                   radius = 2, 
                   fillOpacity = .25,
                   popup = ~paste("Address: ", house_number, street_name, borough, "NY", postcode, 
                                  "<br>", 
                                  "Owned by:", owner_type, 
                                  "<br>",
                                  "Before consolidation:", ex_dwelling_unit, "units",
                                  "<br>",
                                  "After consolidation:", pr_dwelling_unit, "units", 
                                  "<br>",
                                  "Consolidation certified:", c_o_issue_date), 
                   group = "Home Consolidations, 2012-2026") %>%
  addControl(html = title_box, 
             position = "topright", 
  ) %>%
  addLayersControl(
    overlayGroups = c("Home Consolidations, 2012-2026",
                      "Lost Rent Regulated Buildings, 2020-2025"),
    options = layersControlOptions(collapsed = FALSE)
  )
map


# Save results ------------------------------------------------------------
saveWidget(widget = map, 
           file = "index.html", 
           selfcontained = FALSE)
