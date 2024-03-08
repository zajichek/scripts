# Created: 2024-03-07
# Author: Alex Zajichek
# Project: Example shiny app for readmission risk pool
# Description: The user interface

# Make a standard UI
fluidPage(
    
    # Make a custom title/logo + timestamp
    titlePanel(
        title = 
            div(
                div(paste0("As of: ", current_time), style = "text-align: right; font-size: 12px; font-style: italic;"),
                div(
                    img(
                        src = "yourlogo.png", # <-- Your image goes here
                        alt = 'img', width = '40', height = '40'
                    ), 
                    "Readmission Risk Pool", 
                    style = "font-family: 'Baskerville', serif; font-size: 30px; font-weight: bold; color: #36302ds"
                )
            )
    ),
    
    # Make a sidebar/main panel layout
    sidebarLayout(
        
        # Sidebar containing filters, etc.
        sidebarPanel(
            width = 5,
            
            # Tally's of risk pool
            HTML("<h3 style = 'text-align: center;'><u>Risk Pool Summary</u></h3>"),
            htmlOutput(outputId = "risk_pool_patients"),
            htmlOutput(outputId = "risk_pool_discharge_risk"),
            
            # Risk pool distribution
            plotlyOutput(outputId = "bar"),
            
            # Filter the days
            sliderInput(
                inputId = "days_in_risk_pool",
                label = "Days in risk pool",
                min = 0,
                max = 30,
                value = c(0, 30),
                step = 1
            )
        ),
        
        # Main panel with map
        mainPanel(
            width = 7,
            
            # Row of metrics
            fluidRow(
                column(
                    plotlyOutput(outputId = "current_rate", height = "150px"),
                    width = 6
                ),
                column(
                    plotlyOutput(outputId = "expected_penalty", height = "150px"),
                    width = 6
                )
            ),
            
            # The map
            leafletOutput(
                outputId = "map",
                height = 600
            )
            
        )
        
    )
)
