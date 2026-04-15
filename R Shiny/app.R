library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(readr)
library(DT)

source("rag_V2.R", local = TRUE)
rag_store <- initialize_rag_v2(force_rebuild = TRUE)

# ---------------------------
# Load data
# ---------------------------
df_final <- read_csv("df_rshiny.csv", show_col_types = FALSE)

# Load data dictionary
data_dict <- readLines("data_description.txt", warn = FALSE)

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
      menuItem("Data Dictionary", tabName = "dictionary"),
      menuItem("Ask the Analysis", tabName = "rag", icon = icon("comments"))
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
            selectInput("month_filter", "Month", choices = months, selected = months, multiple = TRUE),
            selectInput("weekday_filter", "Pickup Weekday", choices = weekdays_pickup, selected = weekdays_pickup, multiple = TRUE),
            selectInput("source_filter", "Source File", choices = source_files, selected = source_files, multiple = TRUE)
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
      ),
      
      # RAG tab
      tabItem(
        tabName = "rag",
        fluidRow(
          box(
            width = 4, status = "primary",
            title = "Ask a Question",
            textAreaInput(
              "rag_question",
              "Enter your question",
              rows = 6,
              placeholder = "Example: What are the main tipping patterns in this project?"
            ),
            checkboxInput("use_filter_context", "Use current dashboard filters as context", TRUE),
            actionButton("ask_rag", "Ask")
          ),
          box(
            width = 8, status = "primary",
            title = "RAG Answer",
            verbatimTextOutput("rag_answer")
          )
        ),
        fluidRow(
          box(
            width = 12, status = "info",
            title = "Retrieved Chunks",
            DT::dataTableOutput("rag_sources")
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
  
  filtered_data <- reactive({
    req(input$month_filter, input$weekday_filter, input$source_filter)
    
    df_final %>%
      filter(
        month %in% input$month_filter,
        pickup_weekday_name %in% input$weekday_filter,
        source_file %in% input$source_filter
      )
  })
  
  rag_context <- reactive({
    list(
      months = input$month_filter,
      weekdays = input$weekday_filter,
      sources = input$source_filter,
      n_rows = nrow(filtered_data())
    )
  })
  
  rag_result <- eventReactive(input$ask_rag, {
    req(nzchar(trimws(input$rag_question)))
    
    withProgress(message = "Generating response...", value = 0, {
      incProgress(0.3)
      
      result <- run_rag(
        question = input$rag_question,
        context = if (isTRUE(input$use_filter_context)) rag_context() else NULL,
        store = rag_store
      )
      
      incProgress(1)
      result
    })
  }, ignoreInit = TRUE)
  
  output$scatter_plot <- renderPlotly({
    req(input$x_var, input$y_var)
    
    data <- filtered_data() %>%
      filter(
        !is.na(.data[[input$x_var]]),
        !is.na(.data[[input$y_var]])
      )
    
    if (isTRUE(input$log_scale)) {
      data <- data %>%
        filter(
          .data[[input$x_var]] > 0,
          .data[[input$y_var]] > 0
        )
    }
    
    validate(
      need(nrow(data) > 0, "No data available for the selected filters.")
    )
    
    p <- ggplot(data, aes(x = .data[[input$x_var]], y = .data[[input$y_var]])) +
      geom_point(alpha = input$alpha, size = 2) +
      theme_minimal() +
      labs(
        title = as.character(paste(input$y_var, "vs", input$x_var)),
        x = as.character(input$x_var),
        y = as.character(input$y_var)
      )
    
    if (input$color_var != "none") {
      p <- p +
        aes(color = .data[[input$color_var]]) +
        labs(color = as.character(input$color_var))
    }
    
    if (isTRUE(input$log_scale)) {
      p <- p + scale_x_log10() + scale_y_log10()
    }
    
    if (isTRUE(input$add_smooth)) {
      p <- p + geom_smooth(method = "lm", se = FALSE)
    }
    
    tooltips <- if (input$color_var != "none") c("x", "y", "colour") else c("x", "y")
    
    plotly::ggplotly(p, tooltip = tooltips)
  })
  
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
    
    plotly::ggplotly(p)
  })
  
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
  
  output$data_dict <- renderPrint({
    cat(paste(data_dict, collapse = "\n"))
  })
  
  output$rag_answer <- renderText({
    req(rag_result())
    rag_result()$answer
  })
  
  output$rag_sources <- DT::renderDataTable({
    req(rag_result())
    
    rag_result()$sources %>%
      mutate(
        score = round(score, 4),
        preview = stringr::str_trunc(chunk, 250)
      ) %>%
      select(chunk_id, source, score, preview)
  },
  options = list(pageLength = 5, scrollX = TRUE),
  rownames = FALSE)
}

shinyApp(ui, server)