# Created: 2024-03-07
# Author: Alex Zajichek
# Project: Example shiny app for readmission risk pool
# Description: Creates global objects

# Load packages
library(shiny)
library(tidyverse)
library(leaflet)
library(plotly)
library(tigris)
library(sf)

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

### Generating fake data for app demonstration

# Assume there are this many patients in the risk pool
n_patients <- 500

# Create some fake patients
set.seed(4411332)
risk_pool <-
    tibble(
        PatientID = 1:n_patients,
        DaysSinceDischarge = sample(0:30, n_patients, replace = TRUE),
        ClinicalRisk = 1 / (1 + exp(-rnorm(n_patients, mean = -3)))
    ) |>
    
    # Join to get all zip codes
    cross_join(
        y = zips_wi_centroids
    ) |>
    
    # Randomly sample 1 zip code per patient
    slice_sample(
        n = 1,
        by = PatientID
    )

# Generate a fake time series of readmission rates
current_time <- Sys.time()
current_date <- as.Date(current_time)
rate_over_time <- 
    tibble(
        Date = seq(current_date - lubridate::years(1), current_date, 365.25/12),
        ReadmissionRate = 1 / (1 + exp(-rnorm(length(Date), mean = -3, sd = .5))),
        ExpectedPenalty = runif(length(Date), min = 0, max = 100)
    ) |>
    mutate(
        ExpectedPenalty = 
            case_when(
                ReadmissionRate < mean(ReadmissionRate) ~ 0,
                TRUE ~ ExpectedPenalty
            )
    )

# Make a color pallette
pal <- 
    colorNumeric(
        palette = "RdYlGn",
        domain = -1*unique(risk_pool$ClinicalRisk)
    )