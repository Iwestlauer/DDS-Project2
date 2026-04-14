library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(readr)
library(DT)

# ---------------------------
# Load data
# ---------------------------
df_final <- read_csv("df_rshiny.csv", show_col_types = FALSE)

# Load data dictionary
data_dict <- readLines("data_description.txt", warn = FALSE)

# Optional: convert some variables to factors for cleaner display
df_final <- df_final %>%
  mutate(
    payment_type = as.factor(payment_type),
    month = as.factor(month),
    tourist_season = factor(tourist_season, levels = c(0, 1), labels = c("No", "Yes")),
    source_file = as.factor(source_file),
    pickup_weekday_name = as.factor(pickup_weekday_name),
    dropoff_weekday_name = as.factor(dropoff_weekday_name)
  )

# ---------------------------
# Prepare choices
# ---------------------------
numeric_vars <- names(df_final)[sapply(df_final, is.numeric)]
cat_vars <- names(df_final)[sapply(df_final, function(x) is.character(x) || is.factor(x))]

months <- sort(unique(df_final$month))
weekdays_pickup <- sort(unique(df_final$pickup_weekday_name))
source_files <- sort(unique(df_final$source_file))

# ---------------------------
# UI
# ---------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Trip Data EDA"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Scatterplot", tabName = "scatter"),
      menuItem("Barchart", tabName = "bar"),
      menuItem("Data Viewer", tabName = "data"),
      menuItem("Data Dictionary", tabName = "dictionary")
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # Scatterplot tab
      tabItem(
        tabName = "scatter",
        fluidRow(
          box(
            width = 12, status = "primary",
            title = "Scatterplot Explorer",
            plotlyOutput("scatter_plot", height = "600px")
          )
        ),
        fluidRow(
          box(
            width = 4, title = "Variables",
            selectInput("x_var", "X Variable", choices = numeric_vars, selected = "trip_distance"),
            selectInput("y_var", "Y Variable", choices = numeric_vars, selected = "tip_amount"),
            selectInput("color_var", "Color by", choices = c("None" = "none", cat_vars), selected = "pickup_weekday_name")
          ),
          box(
            width = 4, title = "Filters",
            selectInput(
              "month_filter", "Month",
              choices = months,
              selected = months,
              multiple = TRUE
            ),
            selectInput(
              "weekday_filter", "Pickup Weekday",
              choices = weekdays_pickup,
              selected = weekdays_pickup,
              multiple = TRUE
            ),
            selectInput(
              "source_filter", "Source File",
              choices = source_files,
              selected = source_files,
              multiple = TRUE
            )
          ),
          box(
            width = 4, title = "Options",
            sliderInput("alpha", "Point opacity", 0.1, 1, 0.6, step = 0.1),
            checkboxInput("log_scale", "Log scale (both axes)", FALSE),
            checkboxInput("add_smooth", "Add trend line", FALSE)
          )
        )
      ),
      
      # Barchart tab
      tabItem(
        tabName = "bar",
        fluidRow(
          box(
            width = 12, status = "primary",
            title = "Barchart",
            plotlyOutput("bar_plot", height = "500px")
          )
        ),
        fluidRow(
          box(
            width = 6, title = "Settings",
            selectInput("bar_x", "Category (X)", choices = cat_vars, selected = "pickup_weekday_name"),
            selectInput("bar_fill", "Fill by", choices = c("None" = "none", cat_vars), selected = "payment_type"),
            checkboxInput("bar_percent", "Show as percentage", FALSE)
          ),
          box(
            width = 6, title = "Options",
            selectInput("bar_theme", "Theme", choices = c("Minimal", "Classic", "Light")),
            checkboxInput("bar_flip", "Flip coordinates", FALSE)
          )
        )
      ),
      
      # Data viewer tab
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            width = 12, status = "primary",
            title = "Raw Trip Data",
            DT::dataTableOutput("data_table", height = "600px")
          )
        )
      ),
      
      # Data dictionary tab
      tabItem(
        tabName = "dictionary",
        fluidRow(
          box(
            width = 12, status = "primary",
            title = "Data Dictionary",
            verbatimTextOutput("data_dict", placeholder = FALSE)
          )
        )
      )
    )
  )
)

# ---------------------------
# Server
# ---------------------------
server <- function(input, output, session) {
  
  # Reactive filtered data for scatterplot
  filtered_data <- reactive({
    req(input$month_filter, input$weekday_filter, input$source_filter)
    
    df_final %>%
      filter(
        month %in% input$month_filter,
        pickup_weekday_name %in% input$weekday_filter,
        source_file %in% input$source_filter
      )
  })
  
  # Scatterplot
  output$scatter_plot <- renderPlotly({
    req(input$x_var, input$y_var)
    
    data <- filtered_data() %>%
      filter(
        !is.na(.data[[input$x_var]]),
        !is.na(.data[[input$y_var]])
      )
    
    if (input$log_scale) {
      data <- data %>%
        filter(
          .data[[input$x_var]] > 0,
          .data[[input$y_var]] > 0
        )
    }
    
    p <- ggplot(data, aes(x = .data[[input$x_var]], y = .data[[input$y_var]])) +
      geom_point(alpha = input$alpha, size = 2) +
      theme_minimal() +
      labs(
        title = paste(input$y_var, "vs", input$x_var),
        x = input$x_var,
        y = input$y_var
      )
    
    if (input$color_var != "none") {
      p <- p +
        aes(color = .data[[input$color_var]]) +
        labs(color = input$color_var)
    }
    
    if (input$log_scale) {
      p <- p + scale_x_log10() + scale_y_log10()
    }
    
    if (input$add_smooth) {
      p <- p + geom_smooth(method = "lm", se = FALSE)
    }
    
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  # Barchart
  output$bar_plot <- renderPlotly({
    req(input$bar_x)
    
    if (input$bar_fill == "none") {
      data_sum <- df_final %>%
        count(.data[[input$bar_x]], name = "n") %>%
        arrange(desc(n))
      
      p <- ggplot(data_sum, aes(x = reorder(.data[[input$bar_x]], n), y = n)) +
        geom_col(fill = "steelblue")
    } else {
      data_sum <- df_final %>%
        count(.data[[input$bar_x]], .data[[input$bar_fill]], name = "n")
      
      totals <- data_sum %>%
        group_by(.data[[input$bar_x]]) %>%
        summarize(total = sum(n), .groups = "drop") %>%
        arrange(desc(total))
      
      data_sum <- data_sum %>%
        mutate(!!input$bar_x := factor(.data[[input$bar_x]], levels = totals[[input$bar_x]]))
      
      p <- ggplot(
        data_sum,
        aes(
          x = .data[[input$bar_x]],
          y = n,
          fill = .data[[input$bar_fill]]
        )
      ) +
        geom_col(position = if (input$bar_percent) "fill" else "stack")
    }
    
    p <- p +
      labs(
        title = paste("Distribution of", input$bar_x),
        x = input$bar_x,
        y = if (input$bar_percent) "Proportion" else "Count"
      )
    
    if (input$bar_theme == "Classic") {
      p <- p + theme_classic()
    } else if (input$bar_theme == "Light") {
      p <- p + theme_light()
    } else {
      p <- p + theme_minimal()
    }
    
    if (input$bar_flip) {
      p <- p + coord_flip()
    }
    
    ggplotly(p)
  })
  
  # Data viewer
  output$data_table <- DT::renderDataTable({
    df_final
  },
  options = list(
    pageLength = 15,
    scrollX = TRUE,
    searching = TRUE,
    lengthMenu = c(10, 25, 50, 100, nrow(df_final))
  ),
  rownames = FALSE)
  
  # Data dictionary
  output$data_dict <- renderPrint({
    cat(paste(data_dict, collapse = "\n"))
  })
}

shinyApp(ui, server)