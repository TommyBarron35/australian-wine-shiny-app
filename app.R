library(shiny)
library(tidyverse)
library(fpp3)
library(gt)
library(lubridate)
library(shinyWidgets)
library(here)

# -------------------------
# Load and prepare data
# -------------------------
aus_wine <- read_csv(
    ("data/AustralianWines.csv"), na = "*",
    col_types = cols(Rose = col_number()),
    show_col_types = FALSE
) |> 
    fill(Rose, .direction = "down") |> 
    mutate(Month = mdy(str_replace(Month, '-', '-01-')) |> yearmonth())

aus_wine_long <- aus_wine |>
    pivot_longer(
        cols = -c(Month),
        names_to = "Varietal",
        values_to = "Sales"
    )

# Deduplicate and convert to tsibble
wine_tsibble <- aus_wine_long |>
    group_by(Month, Varietal) |>
    summarise(Sales = sum(Sales), .groups = "drop") |>
    as_tsibble(index = Month, key = Varietal)

# Convert Month to Date for sliderInput
min_date <- as.Date(min(wine_tsibble$Month))
max_date <- as.Date(max(wine_tsibble$Month))
default_train_end <- as.Date("1993-12-01")

# -------------------------
# UI
# -------------------------
ui <- fluidPage(
    titlePanel("Australian Wine Data Analysis"),
    
    sidebarLayout(
        sidebarPanel(
            pickerInput(
                inputId = "wine_select",
                label = "Select Wine Varietals",
                choices = c("Rose", "Sweet white", "Red", "Fortified", "Dry white", "sparkling"),
                selected = c("Rose", "Red"),   # default two wines
                multiple = TRUE,
                options = list(`actions-box` = TRUE)
            ),
            
            # ⭐ NEW TWO-SIDED DATE SLIDER ⭐
            sliderInput(
                inputId = "date_range",
                label = "Select Date Range (for visualization)",
                min = min_date,
                max = max_date,
                value = c(as.Date("1989-01-01"), max_date),
                timeFormat = "%Y-%m"
            ),
            
            numericInput(
                inputId = "forecast_horizon",
                label = "Forecast Horizon (months)",
                value = 12,
                min = 1,
                max = 60
            ),
            
            checkboxGroupInput(
                inputId = "models_select",
                label = "Select Models to Display",
                choices = c("TSLM", "ETS", "ARIMA"),
                selected = c("TSLM", "ETS", "ARIMA")
            ),
            
            sliderInput(
                inputId = "train_end",
                label = "Training Data End Month:",
                min = min_date,
                max = max_date,
                value = default_train_end,
                timeFormat = "%Y-%m"
            ),
            
            checkboxInput(
                inputId = "free_y",
                label = "Use Free Y-axis Scales in Graphs",
                value = TRUE
            )
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Overview",
                         plotOutput("overviewPlot")
                ),
                
                tabPanel("Modeling and Forecasts",
                         uiOutput("model_warning"),
                         plotOutput("forecastPlot"),
                         h4("Metrics for Training Data"),
                         gt_output("trainAcc"),
                         h4("Forecast Accuracy"),
                         gt_output("forecastTable")
                ),
                
                tabPanel("About",
                         h4("About this App"),
                         p("This Shiny app allows visualization, modeling, and forecasting of Australian wine sales data."),
                         p("Select wine varietals, adjust the date range, training interval, and forecast horizon to explore trends and predictions.")
                )
            )
        )
    )
)

# -------------------------
# Server
# -------------------------
server <- function(input, output, session) {
    
    # Filtered data for visualization
    filtered_data <- reactive({
        req(input$wine_select, input$date_range)
        wine_tsibble |>
            filter(
                Varietal %in% input$wine_select,
                Month >= yearmonth(input$date_range[1]),
                Month <= yearmonth(input$date_range[2])
            )
    })
    
    # Training data
    train_data <- reactive({
        req(input$wine_select, input$train_end)
        train_end_ym <- yearmonth(input$train_end)
        wine_tsibble |>
            filter(
                Varietal %in% input$wine_select,
                Month <= train_end_ym
            )
    })
    
    # Warning if no training data
    output$model_warning <- renderUI({
        if(nrow(train_data()) == 0) {
            tags$div(
                style = "color:red; font-weight:bold;",
                "Warning: No data available for modeling. Adjust your selections."
            )
        }
    })
    
    # Fit models
    wine_models <- reactive({
        df <- train_data()
        req(df)
        if(nrow(df) == 0) return(NULL)
        
        df |>
            model(
                TSLM = TSLM(Sales ~ trend() + season()),
                ETS = ETS(Sales),
                ARIMA = ARIMA(Sales)
            )
    })
    
    # Forecasts
    wine_forecast <- reactive({
        req(wine_models())
        wine_models() |> forecast(h = paste(input$forecast_horizon, "months"))
    })
    
    # -------------------------
    # Overview plot
    # -------------------------
    output$overviewPlot <- renderPlot({
        req(filtered_data())
        
        scales_opt <- ifelse(input$free_y, "free_y", "fixed")
        
        filtered_data() |>
            autoplot(Sales) +
            labs(y = "Sales", title = "Wine Sales Overview") +
            facet_wrap(~Varietal, ncol = 3, scales = scales_opt) +
            theme_minimal()
    })
    
    # -------------------------
    # Forecast plot
    # -------------------------
    output$forecastPlot <- renderPlot({
        req(train_data(), wine_forecast())
        
        scales_opt <- ifelse(input$free_y, "free_y", "fixed")
        
        fc <- wine_forecast() |> filter(.model %in% input$models_select)
        actual <- train_data()
        
        # Apply visualization date filter
        start <- yearmonth(input$date_range[1])
        end <- yearmonth(input$date_range[2])
        actual <- actual |> filter(Month >= start, Month <= end)
        fc <- fc |> filter(Month >= start, Month <= end)
        
        if(nrow(fc) == 0 || nrow(actual) == 0) return(NULL)
        
        autoplot(actual, Sales) +
            autolayer(fc, .mean, series = ".model") +
            labs(title = "Forecasts with Confidence Intervals", y = "Sales") +
            facet_wrap(~Varietal, ncol = 3, scales = scales_opt) +
            theme_minimal()
    })
    
    # -------------------------
    # Training accuracy table
    # -------------------------
    output$trainAcc <- render_gt({
        req(wine_models())
        models <- wine_models()
        if(is.null(models)) return(gt())
        
        models |>
            accuracy() |>
            filter(Varietal %in% input$wine_select) |>
            select(Varietal, .model, RMSE, MAE, MAPE) |>
            arrange(RMSE) |>
            gt() |>
            fmt_number(decimals = 2)
    })
    
    # -------------------------
    # Forecast accuracy table
    # -------------------------
    output$forecastTable <- render_gt({
        req(wine_forecast())
        fc <- wine_forecast()
        
        fc |>
            as_tibble() |>
            filter(Varietal %in% input$wine_select) |>
            select(Varietal, .model, Month, .mean) |>
            pivot_wider(names_from = Month, values_from = .mean) |>
            gt() |>
            fmt_number(decimals = 0)
    })
}

# -------------------------
# Run the app
# -------------------------
shinyApp(ui, server)
