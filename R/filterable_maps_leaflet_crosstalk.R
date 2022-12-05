# Created: 2022-12-05
# Author: Alex Zajichek
# Topic: Filterable maps with leaflet and crosstalk
# Objective: Make an interactive map of Wisconsin hospitals and the ability to filter based on penalties in the FY23 Hospital Readmission Reduction Program
# Dataset(s): 
# - Penalty amounts: https://www.cms.gov/medicare/acute-inpatient-pps/fy-2023-ipps-final-rule-home-page
# - Hospital Info: https://data.cms.gov/provider-data/dataset/xubh-q36u

# Load packages
require(tidyverse)
require(readxl)
require(tigris)
require(sf)
require(maps)
require(crosstalk)
require(leaflet)
require(htmltools)
require(shiny)

## 1) Import penalty amounts

# Set the file name
hrrp_zip <- "https://www.cms.gov/files/zip/fy-2023-ipps-final-rule-hrrp-supplemental-file.zip"

# Create a temporary file (Credits: rpubs.com/otienodominic/398952)
temp_file <- tempfile()

# Download into the temporary file
download.file(hrrp_zip, temp_file)

# Name of file needed within zip
hrrp_file <- "FY2023_Final_Rule_Supplemental_File.xlsx"

# Unzip, and place the file in the current working directory
unzip(temp_file, hrrp_file, exdir = ".")

# Import the data file into a data frame
hrrp_results <-
  readxl::read_xlsx(
    path = hrrp_file,
    sheet = "FR FY 2023",
    skip = 1,
    na = c("", " ", ".", "-", "NA", "N/A")
  )

# Delete the downloaded file
file.remove(hrrp_file)
unlink(temp_file)

## 2. Import hospital information file 

# Set path to file (Copied link from "Download full dataset")
hosp_file <- "https://data.cms.gov/provider-data/sites/default/files/resources/092256becd267d9eeccf73bf7d16c46b_1666224317/Hospital_General_Information.csv"

# Import the file
hosp_info <-
  read_csv(
    file = hosp_file
  )

## 3. Gather the centroids of WI zip codes (for plotting individual hospitals)

# Get set of zip codes for Wisconsin
zips_wi <- zctas(year = 2010, state = "WI")

# Gather centroids of corrdinates for each zip
zips_wi_centroids <-
  zips_wi %>%
  
  # Get the centroid
  st_centroid() %>%
  
  # Pluck the coordinates
  st_coordinates() %>%
  
  # Make a tibble
  as_tibble() %>%
  
  # Add identifying column
  add_column(
    Zip = zips_wi$ZCTA5CE10
  ) %>%
  
  # Rename columns
  rename(
    lon = X,
    lat = Y
  )

## 4. Build the mapping data
hrrp_map_data <-
  hosp_info %>%
  
  # Filter to Wisconsin hospitals
  filter(State == "WI") %>%
  
  # Keep a few pieces of information
  select(
    FacilityID = `Facility ID`,
    FacilityName = `Facility Name`,
    Zip = `ZIP Code`
  ) %>%
  
  # Join to get the centroid for the hospital's zip code (plotting coordinates)
  inner_join(
    y = zips_wi_centroids,
    by = "Zip"
  ) %>%
  
  # Add random jitter to coordinates (so hospitals w/ same zip don't overlap)
  mutate(
    across(
      c(lat,lon),
      jitter,
      amount = 0.05
    )
  ) %>%
  
  # Join to get HRRP program results
  inner_join(
    y = 
      hrrp_results %>%
      
      # Make cleaner levels for penalty indicators
      mutate(
        across(
          starts_with("Penalty indicator"),
          ~
            case_when(
              .x == "Y" ~ "Yes",
              .x == "N" ~ "No",
              TRUE ~ NA_character_
            )
        )
      ) %>%
      
      # Keep a few columns
      select(
        FacilityID = `Hospital CCN`,
        PeerGroup = `Peer group assignment`,
        DualProportion = `Dual proportion`,
        Penalty = `Payment reduction percentage`,
        starts_with("Penalty indicator")
      ) %>%
      
      # Remove the prefix from the cohort columns
      rename_with(
        ~str_remove(.x, "^Penalty indicator for "),
        starts_with("Penalty indicator")
      ),
    by = "FacilityID"
  ) %>% 
  
  # Join to get list of penalized cohorts (for labeling)
  left_join(
    y = 
      hrrp_results %>%
      
      # Send down the rows
      pivot_longer(
        cols = starts_with("Penalty indicator"),
        names_prefix = "Penalty indicator for "
      ) %>%
      
      # Filter to penalty cohorts
      filter(
        value == "Y"
      ) %>%
      
      # For each hospital
      group_by(`Hospital CCN`) %>%
      
      # Concatenate the list of penalty cohorts
      summarise(
        PenalizedCohortCount = n(),
        PenalizedCohorts = paste(sort(unique(name)), collapse = ", "),
        .groups = "drop"
      ) %>%
      
      # Rename the column
      rename(
        FacilityID = `Hospital CCN`
      ),
    by = "FacilityID"
  ) %>%
  
  # Fill in non-penalized hospitals
  mutate(
    PenalizedCohortCount = coalesce(PenalizedCohortCount, 0),
    PenalizedCohorts = coalesce(PenalizedCohorts, "")
  ) 

## 5. Gather map components

# WI State outline
state_outline <-
  maps::map(
    database = "state",
    regions = "wisconsin",
    fill = TRUE,
    plot = FALSE
  )

# WI County outlines
county_outlines <- 
  counties(cb = TRUE) %>%
  filter(
    STATE_NAME == "Wisconsin"
  )

## 6. Build the base map (static components)

# Make a color pallete
pal <- 
  colorNumeric(
    palette = "RdYlGn",
    domain = -1*seq(0,3,.1)
  )

# WI outline and counties
base_map <-
  leaflet(height = "500px") %>%
  
  # Set the default view/zoom level
  setView(
    lng = -89.619018,
    lat = 44.597088,
    zoom = 7
  ) %>%
  
  # Add geographic tiles
  addTiles() %>%
  
  # Add WI state outline
  addPolygons(
    data = state_outline,
    fillColor = "gray",
    stroke = FALSE
  ) %>%
  
  # Add county outlines
  addPolygons(
    data = county_outlines,
    color = "black",
    fillColor = "#ff59f7",
    weight = 1,
    opacity = .5,
    fillOpacity = .35,
    highlightOptions = 
      highlightOptions(
        color = "black",
        weight = 3,
        bringToFront = FALSE
      ),
    label = ~NAME
  ) 

## 7. Create sharable dataset between filter and map objects
sd <- SharedData$new(data = hrrp_map_data) # From crosstalk

## 8. Filter components (from crosstalk)

# Header
ui_header <- h3("Hospital Readmissions Reduction Program (HRRP) FY 2023 - Wisconsin")

# Payment penalty slider filter
ui_filter_penalty <-
  filter_slider(
    id = "penalty",
    label = "Payment Reduction",
    sharedData = sd,
    column = ~Penalty,
    step = 0.1,
    min = 0,
    max = 3,
    post = "%"
  )

# Cohort filter sub-header
ui_subheader <- h4("Cohort-specific penalty", style = "font-style:italic;")

# Cohort filters
ui_filters_cohort <-
  list(
    AMI =
      filter_select(
        id = "ami",
        label = "AMI",
        sharedData = sd,
        group = ~AMI
      ),
    CABG = 
      filter_select(
        id = "CABG",
        label = "CABG",
        sharedData = sd,
        group = ~CABG
      ),
    COPD = 
      filter_select(
        id = "COPD",
        label = "COPD",
        sharedData = sd,
        group = ~COPD
      ),
    HF = 
      filter_select(
        id = "HF",
        label = "HF",
        sharedData = sd,
        group = ~HF
      ),
    `THA/TKA` =
      filter_select(
        id = "THA/TKA",
        label = "THA/TKA",
        sharedData = sd,
        group = ~`THA/TKA`
      )
  )

## 9. Put all elements together in a single UI container

# Put everything in 'bscols'
bscols(
  
  # Use shiny functions to make UI rows/columns
  fluidRow(
    
    # Everything falls under one column
    column(
      
      # Text header and penalty slider across the whole page
      ui_header,
      ui_filter_penalty,
      
      # Split the page between cohort filters and the map
      fluidRow(
        
        # Stack the cohort filters on left side
        column(
          ui_subheader,
          ui_filters_cohort$AMI,
          ui_filters_cohort$CABG,
          ui_filters_cohort$COPD,
          ui_filters_cohort$HF,
          ui_filters_cohort$`THA/TKA`,
          width = 4
        ),
        
        # Put map object on right side
        column(
          base_map %>% # <-- base map object created above
            
            # Add individiaul hospitals as a map layer
            addCircleMarkers(
              data = sd, # SharedData object created above (from crosstalk)
              lng = ~lon, 
              lat = ~lat,
              label = ~paste0(FacilityName, " (click for info)"),
              popup = 
                ~paste0(
                  "Hospital: ", FacilityName, 
                  "<br>Zip Code: ", Zip,
                  "<br>Peer Group: ", PeerGroup,
                  "<br>Dual-Eligibility: ", round(DualProportion*100, 2), "%",
                  "<br>Penalties: ", PenalizedCohorts,
                  "<br>Payment Reduction: ", Penalty, "%"
                ),
              color = ~pal(-1*Penalty),
              radius = ~log(5000*Penalty+.1),
              fillOpacity = .75
            ),
          width = 8
        )
      ),
      width = 12
    )
  )
)