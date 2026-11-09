

# Explore data for multifamily home project -------------------------------


library(here)
library(socratadata)
library(dplyr)
library(tidyr)
library(lubridate)
library(sf)
library(leaflet)
library(htmlwidgets)

here::i_am("multifamily_consolidation.r")


# Pull NYC Open Data ------------------------------------------------------

# set up for api
soc_api_key_id = "your key here"
dataset_query <- soc_discover(domains = "data.cityofnewyork.us",
                              query = "")


# certificate of occupancy
dob_occupancy <- soc_read(url = "https://data.cityofnewyork.us/d/bs8b-p36w", 
                 page_size = 10000)
soc_metadata(dob_occupancy)


# pluto
pluto <- soc_read(url = "https://data.cityofnewyork.us/d/64uk-42ks",
                  query = soc_query(select = "BBL, zonedist1, assessland, assesstot, exempttot, ownername"),
                  page_size = 10000)
dob_occupancy <- read.csv(here("DOB_Certificate_Of_Occupancy_20261106.csv"))
pluto <- read.csv(here("Primary_Land_Use_Tax_Lot_Output_(PLUTO)_20261106.csv"))



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



# Redlining data ----------------------------------------------------------

bronx <- read_sf("https://dsl.richmond.edu/panorama/redlining/static/citiesData/NYBronx1938/geojson.json")
brooklyn <- read_sf("https://dsl.richmond.edu/panorama/redlining/static/citiesData/NYBrooklyn1938/geojson.json")
manhattan <- read_sf("https://dsl.richmond.edu/panorama/redlining/static/citiesData/NYManhattan1937/geojson.json")
queens <- read_sf("https://dsl.richmond.edu/panorama/redlining/static/citiesData/NYQueens1938/geojson.json")
staten <- read_sf("https://dsl.richmond.edu/panorama/redlining/static/citiesData/NYStatenIsland1940/geojson.json")

redlining <- bind_rows(bronx, brooklyn, manhattan, queens, staten)



# Map results -------------------------------------------------------------

# convert to spatial
singlefam <- st_as_sf(x = singlefam, 
                      coords = c("longitude", "latitude")) %>%
  st_set_crs(4326)

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
      Click on an object to view a information about it. 
    </p>
  </div>
  "

# build map
map <- leaflet(data = singlefam) %>%
  addTiles() %>%
  addCircleMarkers(color = "blue",
                   radius = 2, 
                   fillOpacity = .25,
                   popup = ~paste("Address: ", house_number, street_name, borough, "NY", postcode, 
                                  "<br>", 
                                  "Owned by:", ownername, 
                                  "<br>",
                                  "Before consolidation:", ex_dwelling_unit, "units",
                                  "<br>",
                                  "After consolidation:", pr_dwelling_unit, "units", 
                                  "<br>",
                                  "Consolidation certified:", c_o_issue_date), 
                   group = "Home Consolidation") %>%
  addPolygons(data = redlining,
              weight = 1,
              color = "white",
              fillColor = ~fill, 
              fillOpacity = .5,
              popup = ~paste("In the 1930s and 40s, this area was classified as",
                             grade, 
                             "grade by the HOLC."),
              group = "Redlining Data") %>%
  addControl(html = title_box, 
             position = "topright", 
             ) %>%
  addLayersControl(
    overlayGroups = c("Home Consolidation","Redlining Data"),
    options = layersControlOptions(collapsed = FALSE)
  )
map


# Save results ------------------------------------------------------------
saveWidget(widget = map, 
           file = "index.html", 
           selfcontained = FALSE)

write.csv(x = consolidations, file = "consolidations.csv", na = "", row.names = FALSE)

