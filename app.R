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
default_train_end <- as.Date("1993-12-01") # default training data end

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
                selected = c("Rose", "Red"),   # default to two wines
                multiple = TRUE,
                options = list(`actions-box` = TRUE)
            ),
            
            dateRangeInput(
                inputId = "date_range",
                label = "Select Date Range (for visualization)",
                start = as.Date("1989-01-01"),
                end = max_date,
                min = min_date,
                max = max_date
            ),
            
            numericInput(
                inputId = "forecast_horizon",
                label = "Forecast Horizon (months)",
                value = 12,
                min = 1,
                max = 60
            ),
            
            # Checkbox to select models
            checkboxGroupInput(
                inputId = "models_select",
                label = "Select Models to Display",
                choices = c("TSLM", "ETS", "ARIMA"),
                selected = c("TSLM", "ETS", "ARIMA")
            ),
            
            # Slider to select training interval
            sliderInput(
                inputId = "train_end",
                label = "Training Data End Month:",
                min = min_date,
                max = max_date,
                value = default_train_end,
                timeFormat = "%Y-%m"
            ),
            
            # Checkbox for free/fixed y-axis
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
    
    # Reactive filtered dataset for visualization only
    filtered_data <- reactive({
        req(input$wine_select, input$date_range)
        wine_tsibble |>
            filter(
                Varietal %in% input$wine_select,
                Month >= yearmonth(input$date_range[1]),
                Month <= yearmonth(input$date_range[2])
            )
    })
    
    # Reactive training data based on slider
    train_data <- reactive({
        req(input$wine_select, input$train_end)
        train_end_ym <- yearmonth(input$train_end) # convert Date to yearmonth
        wine_tsibble |>
            filter(
                Varietal %in% input$wine_select,
                Month <= train_end_ym
            )
    })
    
    # Warning if training data is empty
    output$model_warning <- renderUI({
        if(nrow(train_data()) == 0) {
            tags$div(
                style = "color:red; font-weight:bold;",
                "Warning: No data available for modeling. Please check your wine selection or training interval."
            )
        }
    })
    
    # Reactive model fitting
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
    
    # Reactive forecast
    wine_forecast <- reactive({
        req(wine_models())
        if(is.null(wine_models())) return(NULL)
        
        wine_models() |>
            forecast(h = paste(input$forecast_horizon, "months"))
    })
    
    # -------------------------
    # Overview plot
    # -------------------------
    output$overviewPlot <- renderPlot({
        req(filtered_data())
        
        scales_option <- ifelse(input$free_y, "free_y", "fixed")
        
        filtered_data() |>
            autoplot(Sales) +
            labs(y = "Sales", title = "Wine Sales Overview") +
            facet_wrap(~Varietal, ncol = 3, scales = scales_option) +
            theme_minimal()
    })
    
    # -------------------------
    # Forecast plot with autoplot + model selection
    # -------------------------
    output$forecastPlot <- renderPlot({
        req(train_data(), wine_forecast())
        
        scales_option <- ifelse(input$free_y, "free_y", "fixed")
        
        fc <- wine_forecast() |> filter(Varietal %in% input$wine_select)
        actual <- train_data() |> filter(Varietal %in% input$wine_select)
        
        if(nrow(fc) == 0 || nrow(actual) == 0) return(NULL)
        
        # Filter forecasts by selected models
        fc <- fc |> filter(.model %in% input$models_select)
        
        # Filter to visualization date range
        start_date <- yearmonth(input$date_range[1])
        end_date <- yearmonth(input$date_range[2])
        
        actual <- actual |> filter(Month >= start_date, Month <= end_date)
        fc <- fc |> filter(Month >= start_date, Month <= end_date)
        
        # Autoplot with confidence intervals
        autoplot(actual, Sales) +
            autolayer(fc, .mean, series = ".model") +
            labs(title = "Forecasts vs Actuals with Confidence Intervals", y = "Sales") +
            facet_wrap(~Varietal, ncol = 3, scales = scales_option) +
            theme_minimal()
    })
    
    # -------------------------
    # Training accuracy table
    # -------------------------
    output$trainAcc <- render_gt({
        req(wine_models())
        models <- wine_models()
        if(is.null(models) || nrow(train_data()) == 0) return(gt())
        
        models |>
            accuracy() |>
            filter(Varietal %in% input$wine_select) |>
            select(Varietal, .model, RMSE, MAE, MAPE) |>
            arrange(RMSE) |>
            gt() |>
            fmt_number(decimals = 2)
    })
    
    # -------------------------
    # Forecast table
    # -------------------------
    output$forecastTable <- render_gt({
        req(wine_forecast())
        fc <- wine_forecast()
        if(nrow(fc) == 0) return(gt())
        
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
