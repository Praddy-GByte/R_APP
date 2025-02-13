# app.R

# Load necessary libraries
library(shiny)
library(leaflet)
library(plotly)
library(DT)
library(shinydashboard)
library(dplyr)

# Define UI for the application
ui <- dashboardPage(
    dashboardHeader(title = "Colorado River Basin Dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Drought Conditions", tabName = "drought", icon = icon("tint")),
            menuItem("Short-Term Projections", tabName = "short_term", icon = icon("chart-line")),
            menuItem("Long-Term Projections", tabName = "long_term", icon = icon("chart-area")),
            menuItem("VIC/CRSS Model Outputs", tabName = "model_outputs", icon = icon("water")),
            menuItem("Infrastructure Asset Framework", tabName = "infrastructure", icon = icon("building"))
        )
    ),
    dashboardBody(
        tabItems(
            # Drought Conditions Tab
            tabItem(
                tabName = "drought",
                fluidRow(
                    box(
                        title = "Drought Tier Conditions", status = "primary", solidHeader = TRUE,
                        plotlyOutput("drought_tier_plot")
                    ),
                    box(
                        title = "Reservoir Levels", status = "primary", solidHeader = TRUE,
                        plotlyOutput("reservoir_levels_plot")
                    )
                ),
                fluidRow(
                    box(
                        title = "Drought Severity Map", status = "primary", solidHeader = TRUE,
                        leafletOutput("drought_severity_map")
                    )
                )
            ),

            # Short-Term Projections Tab
            tabItem(
                tabName = "short_term",
                fluidRow(
                    box(
                        title = "Short-Term Water Supply Forecast", status = "primary", solidHeader = TRUE,
                        plotlyOutput("short_term_forecast_plot")
                    ),
                    box(
                        title = "Seasonal Climate Forecasts", status = "primary", solidHeader = TRUE,
                        plotlyOutput("seasonal_forecast_plot")
                    )
                )
            ),

            # Long-Term Projections Tab
            tabItem(
                tabName = "long_term",
                fluidRow(
                    box(
                        title = "Long-Term Water Supply Projections", status = "primary", solidHeader = TRUE,
                        plotlyOutput("long_term_projection_plot")
                    ),
                    box(
                        title = "Climate Change Scenarios", status = "primary", solidHeader = TRUE,
                        plotlyOutput("climate_scenarios_plot")
                    )
                )
            ),

            # VIC/CRSS Model Outputs Tab
            tabItem(
                tabName = "model_outputs",
                fluidRow(
                    box(
                        title = "VIC Model Outputs", status = "primary", solidHeader = TRUE,
                        plotlyOutput("vic_output_plot")
                    ),
                    box(
                        title = "CRSS Model Outputs", status = "primary", solidHeader = TRUE,
                        plotlyOutput("crss_output_plot")
                    )
                ),
                fluidRow(
                    box(
                        title = "Model Comparison", status = "primary", solidHeader = TRUE,
                        plotlyOutput("model_comparison_plot")
                    )
                )
            ),

            # Infrastructure Asset Framework Tab
            tabItem(
                tabName = "infrastructure",
                fluidRow(
                    box(
                        title = "Biophysical Assets", status = "primary", solidHeader = TRUE,
                        plotlyOutput("biophysical_assets_plot")
                    ),
                    box(
                        title = "Sociopolitical Assets", status = "primary", solidHeader = TRUE,
                        plotlyOutput("sociopolitical_assets_plot")
                    )
                ),
                fluidRow(
                    box(
                        title = "Asset Scores Over Time", status = "primary", solidHeader = TRUE,
                        plotlyOutput("asset_scores_plot")
                    )
                )
            )
        )
    )
)

# Define server logic required to draw plots and maps
server <- function(input, output) {
    # Drought Conditions Plots and Map
    output$drought_tier_plot <- renderPlotly({
        # Example plot for Drought Tier Conditions
        plot_ly(x = ~ c("Tier 1", "Tier 2", "Tier 3"), y = ~ c(500, 300, 100), type = "bar", name = "Water Allocation (kAF)") %>%
            layout(title = "Drought Tier Conditions", xaxis = list(title = "Drought Tier"), yaxis = list(title = "Water Allocation (kAF)"))
    })

    output$reservoir_levels_plot <- renderPlotly({
        # Example plot for Reservoir Levels
        plot_ly(x = ~ 1:12, y = ~ rnorm(12, 1000, 200), type = "scatter", mode = "lines", name = "Reservoir Levels") %>%
            layout(title = "Reservoir Levels Over Time", xaxis = list(title = "Month"), yaxis = list(title = "Reservoir Level (kAF)"))
    })

    output$drought_severity_map <- renderLeaflet({
        # Example map for Drought Severity
        leaflet() %>%
            addTiles() %>%
            setView(lng = -112.0740, lat = 33.4484, zoom = 6) %>%
            addPolygons(data = drought_polygons, color = ~ drought_palette(drought_severity), fillOpacity = 0.7)
    })

    # Short-Term Projections Plots
    output$short_term_forecast_plot <- renderPlotly({
        # Example plot for Short-Term Water Supply Forecast
        plot_ly(x = ~ 1:12, y = ~ rnorm(12, 500, 100), type = "scatter", mode = "lines", name = "Water Supply Forecast") %>%
            layout(title = "Short-Term Water Supply Forecast", xaxis = list(title = "Month"), yaxis = list(title = "Water Supply (kAF)"))
    })

    output$seasonal_forecast_plot <- renderPlotly({
        # Example plot for Seasonal Climate Forecasts
        plot_ly(x = ~ 1:12, y = ~ rnorm(12, 50, 10), type = "scatter", mode = "lines", name = "Precipitation Forecast") %>%
            layout(title = "Seasonal Climate Forecasts", xaxis = list(title = "Month"), yaxis = list(title = "Precipitation (mm)"))
    })

    # Long-Term Projections Plots
    output$long_term_projection_plot <- renderPlotly({
        # Example plot for Long-Term Water Supply Projections
        plot_ly(x = ~ 2022:2099, y = ~ rnorm(78, 1000, 200), type = "scatter", mode = "lines", name = "Water Supply Projection") %>%
            layout(title = "Long-Term Water Supply Projections", xaxis = list(title = "Year"), yaxis = list(title = "Water Supply (kAF)"))
    })

    output$climate_scenarios_plot <- renderPlotly({
        # Example plot for Climate Change Scenarios
        plot_ly(x = ~ 2022:2099, y = ~ rnorm(78, 2, 0.5), type = "scatter", mode = "lines", name = "Temperature Projection") %>%
            layout(title = "Climate Change Scenarios", xaxis = list(title = "Year"), yaxis = list(title = "Temperature Change (°C)"))
    })

    # VIC/CRSS Model Outputs Plots
    output$vic_output_plot <- renderPlotly({
        # Example plot for VIC Model Outputs
        plot_ly(x = ~ 1:12, y = ~ rnorm(12, 500, 100), type = "scatter", mode = "lines", name = "VIC Model Output") %>%
            layout(title = "VIC Model Outputs", xaxis = list(title = "Month"), yaxis = list(title = "Streamflow (kAF)"))
    })

    output$crss_output_plot <- renderPlotly({
        # Example plot for CRSS Model Outputs
        plot_ly(x = ~ 1:12, y = ~ rnorm(12, 500, 100), type = "scatter", mode = "lines", name = "CRSS Model Output") %>%
            layout(title = "CRSS Model Outputs", xaxis = list(title = "Month"), yaxis = list(title = "Streamflow (kAF)"))
    })

    output$model_comparison_plot <- renderPlotly({
        # Example plot for Model Comparison
        plot_ly(x = ~ 1:12, y = ~ rnorm(12, 500, 100), type = "scatter", mode = "lines", name = "VIC vs CRSS") %>%
            layout(title = "Model Comparison", xaxis = list(title = "Month"), yaxis = list(title = "Streamflow (kAF)"))
    })

    # Infrastructure Asset Framework Plots
    output$biophysical_assets_plot <- renderPlotly({
        # Example plot for Biophysical Assets
        plot_ly(x = ~ 1:10, y = ~ rnorm(10), type = "scatter", mode = "lines", name = "Biophysical Assets") %>%
            layout(title = "Biophysical Assets Over Time", xaxis = list(title = "Time"), yaxis = list(title = "Asset Score"))
    })

    output$sociopolitical_assets_plot <- renderPlotly({
        # Example plot for Sociopolitical Assets
        plot_ly(x = ~ 1:10, y = ~ rnorm(10), type = "scatter", mode = "lines", name = "Sociopolitical Assets") %>%
            layout(title = "Sociopolitical Assets Over Time", xaxis = list(title = "Time"), yaxis = list(title = "Asset Score"))
    })

    output$asset_scores_plot <- renderPlotly({
        # Example plot for Asset Scores Over Time
        plot_ly(x = ~ 1:10, y = ~ rnorm(10), type = "scatter", mode = "lines", name = "Asset Scores") %>%
            layout(title = "Asset Scores Over Time", xaxis = list(title = "Time"), yaxis = list(title = "Score"))
    })
}

# Run the application
shinyApp(ui = ui, server = server)
