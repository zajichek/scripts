# Created: 2024-03-07
# Author: Alex Zajichek
# Project: Example shiny app for readmission risk pool
# Description: The server

shinyServer(function(input, output, session) {
    
    # Filter to the selected risk pool
    current_risk_pool <- 
        reactive({
            risk_pool |>
                
                # Apply filters
                filter(
                    DaysSinceDischarge >= min(input$days_in_risk_pool),
                    DaysSinceDischarge <= max(input$days_in_risk_pool)
                )
        })
    
    # Tallies
    output$risk_pool_patients <- renderUI({HTML("<h4 style = 'text-align: center'>Patients: <span style = 'color: green'>", nrow(current_risk_pool()) ,"</span></h6>")})
    output$risk_pool_discharge_risk <- 
        renderUI({
            
            # Compute the expected readmission count/rate (based on discharge risk)
            exp_readmits <- ceiling(sum(current_risk_pool()$ClinicalRisk))
            exp_rate <- round(100 * exp_readmits / nrow(current_risk_pool()), 1)
            
            HTML(paste0("<h4 style = 'text-align: center'>Expected Readmissions As Of <span style = 'font-size: 14px;'><br>Discharge &rarr; </span> <span style = 'color: red; font-size: 14px;'>", exp_readmits, " (", exp_rate ,"%)</span><span style = 'font-size: 14px;'><br> Now &rarr; X (X%) </span> </h6>"))
        })
    
    # Risk pool distribution
    output$bar <-
        renderPlotly({
            
            current_risk_pool() |>
                
                # Summarize the risk pool volume by days
                summarize(
                    Patients = n(),
                    ExpectedReadmissionsClinical = sum(ClinicalRisk),
                    .by = DaysSinceDischarge
                ) |>
                
                # Arrange the data
                arrange(DaysSinceDischarge) |>
                
                # Make the plot
                plot_ly(
                    x = ~DaysSinceDischarge,
                    y = ~Patients,
                    type = "bar",
                    name = "Patients",
                    hovertemplate = "%{y:.0f}",
                    marker = list(color = "rgb(158,202,225)", line = list(color = "#211a75", width = 1.5))
                ) |>
                
                # Add trace of expected readmissions (based on CLINICAL risk)
                add_trace(
                    y = ~ExpectedReadmissionsClinical,
                    type = "scatter",
                    mode = "lines+markers",
                    yaxis = "y2",
                    name = "Clinical risk",
                    text = 
                        ~paste0(
                            "<br>\tCount: ", ceiling(ExpectedReadmissionsClinical),
                            "<br>\tRate: ", round(100*ExpectedReadmissionsClinical/Patients,1), "%"
                        ),
                    hovertemplate = "%{text}",
                    marker = list(color = "#d6bf49", size = 8),
                    line = list(color = "#d6bf49", width = 3.5)
                ) |>
                
                # Format axes
                layout(
                    showlegend = FALSE,
                    xaxis = list(title = "Days in risk pool"),
                    yaxis = list(title = "Patient Count"),
                    yaxis2 = list(title = "Expected Readmissions", side = "right", overlaying = "y", zeroline = FALSE),
                    hovermode = "x unified",
                    plot_bgcolor = "#ecf0f5",
                    paper_bgcolor = "#ecf0f5"
                )
            
        })
    
    # Plot the current readmission rate over time
    output$current_rate <-
        renderPlotly({
            
            rate_over_time |>
                
                # Make a plot
                plot_ly(
                    x = ~Date,
                    y = ~ReadmissionRate,
                    type = "scatter",
                    mode = "lines+markers"
                ) |>
                layout(
                    title = "Overall Readmission Rate",
                    xaxis = list(title = ""),
                    yaxis = list(title = "", tickformat = ".1%")
                )
            
        })
    
    # Plot the expected penalty amount over time
    output$expected_penalty <-
        renderPlotly({
            
            rate_over_time |>
                
                # Make a plot
                plot_ly(
                    x = ~Date,
                    y = ~ExpectedPenalty * 1000,
                    type = "scatter",
                    mode = "lines+markers",
                    color = I("darkgreen")
                ) |>
                layout(
                    title = "Expected Penalty ($)",
                    xaxis = list(title = ""),
                    yaxis = list(title = "", tickformat = "$")
                )
            
        })
    
    # Risk pool map
    output$map <- renderLeaflet({base_map})
    observe({
        leafletProxy("map") |>
            clearMarkers() |>
            
            # Add the current patients selected
            addCircleMarkers(
                data = current_risk_pool(),
                layerId = ~PatientID,
                lng = ~lon,
                lat = ~lat,
                radius = ~log(ClinicalRisk * 400),
                label = ~paste0("Patient ID: ", PatientID, " (click for info)"),
                popup = 
                    ~paste0(
                        "Patient ID: ", PatientID,
                        "<br>Discharge readmission risk: ", round(ClinicalRisk*100,1), "%",
                        "<br>Days since discharge: ", DaysSinceDischarge,
                        "<br>Days left in risk pool: ", 30 - DaysSinceDischarge
                    ),
                color = ~pal(-1*ClinicalRisk),
                fillOpacity = .75
            )
    })
    
    
})