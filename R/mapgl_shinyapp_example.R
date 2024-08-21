### GLOBAL

# Load packages
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(tidyverse)
library(tidycensus)
library(mapgl)
library(plotly)

# Set codes for a couple variables of interest (use tidycensus::load_variables())
income <- "B19013_001"
age <- "B01002_001"

# Extract a data set to use
dat <- 
  get_acs(
    geography = "tract",
    variables = 
      c(
        income, # Median income
        age # Median age
      ),
    state = "WI",
    county = "Marathon",
    year = 2022,
    geometry = TRUE
  )


# Make a plot
rel_plot <-
  dat |>
  
  # Convert to simple tibble
  as_tibble() |>
  
  # Make names
  mutate(
    Tract = str_remove(NAME, ";.+$"),
    Label = 
      case_when(
        variable == age ~ "Age",
        TRUE ~ "Income"
      )
  ) |>
  
  # Send over the columns
  pivot_wider(
    names_from = Label,
    values_from = estimate,
    id_cols = Tract
  ) |>
  
  # Reorder
  arrange(
    Age,
    Income
  ) |>
  
  # Make a plot
  plot_ly(
    x = ~Age,
    y = ~Income,
    size = 2,
    line = list(width = 1, color = "black"),
    type = "scatter",
    mode = "lines+markers",
    text = 
      ~paste0(
        Tract,
        "<br>Median Age: ", round(Age), " years",
        "<br>Median Income: $", round(Income)
      ),
    hovertemplate = "%{text}"
  ) |>
  layout(
    xaxis = list(title = "Median Age (years)"),
    yaxis = list(title = "Median Income ($)", tickformat = "$")
  )

#### UI

# Make the page
ui <-
  fluidPage(
    
    # Set the title/theme
    titlePanel("Marathon County ACS 2022"),
    theme = shinytheme("cosmo"),
    
    # Typical layout
    sidebarLayout(
      sidebarPanel(
        
        # Select which quantity to display on map
        selectInput(
          inputId = "displayed_var",
          label = "Display variable on map",
          choices = 
            c(
              `Median age` = age,
              `Median income` = income
            )
        ),
        
        # Show a scatter plot
        plotlyOutput(outputId = "rel_plot")
      ),
      mainPanel(
        
        # Make the map output
        maplibreOutput(outputId = "map_display")
        
      )
    )
  )

### SERVER

server <-
  function(input, output, session) {
    
    # Show the relationship plot
    output$rel_plot <- renderPlotly({rel_plot})
    
    # Get the mapping data
    map_dat <- 
      reactive({
        
        dat |> 
          
          # Filter to selected variable
          filter(variable == input$displayed_var) |>
          
          # Make the popup text
          mutate(
            Info = paste0(str_remove(NAME, ";.+$"), "<br>", get_title(), ": ", round(estimate))
          )
        
      })
    
    # Generate a title
    get_title <- 
      reactive({
        
        if(input$displayed_var == age) {
          
          "Median age (years)"
          
        } else {
          
          "Median income ($)"
          
        }
        
      })
    
    # Create the map based on the selected variable
    output$map_display <- 
      renderMaplibre({
        
        maplibre() |>
          
          # Focus the mapping area
          fit_bounds(map_dat()) |>
          
          # Fill with the data values
          add_fill_layer(
            id = "mc_acs",
            source = map_dat(),
            fill_color = 
              interpolate(
                column = "estimate",
                values = range(map_dat()$estimate, na.rm = TRUE),
                stops = c("#f2d37c", "#08519c"),
                na_color = "gray"
              ),
            fill_opacity = 0.50,
            popup = "Info"
          ) |>
          add_legend(
            legend_title = get_title(),
            values = range(map_dat()$estimate, na.rm = TRUE),
            colors = c("#f2d37c", "#08519c")
          )
        
      })
    
  }
