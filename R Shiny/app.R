library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(readr)
library(DT)

source("../RAG/rag_V2.R", local = TRUE)

if (file.exists("chunk_store_docx_only.rds")) {
  file.remove("chunk_store_docx_only.rds")
}

rag_store <- initialize_rag_v2(
  store_path = "chunk_store_docx_only.rds",
  file_path = "../RAG/Final Project RAG Document.docx",
  force_rebuild = TRUE
)

print(unique(rag_store$source))
# ---------------------------
# Load data
# ---------------------------
df_final <- read_csv("df_rshiny.csv", show_col_types = FALSE)

# Load data dictionary
data_dict <- readLines("data_description.txt", warn = FALSE)

df_final <- df_final %>%
  mutate(
    payment_type = as.factor(payment_type),
    month = factor(month.abb[as.numeric(as.character(month))], levels = month.abb),
    tourist_season = factor(tourist_season, levels = c(0, 1), labels = c("No", "Yes")),
    source_file = as.factor(source_file),
    pickup_weekday_name = factor(
      pickup_weekday_name,
      levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    ),
    dropoff_weekday_name = factor(
      dropoff_weekday_name,
      levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    )
  )

# ---------------------------
# Prepare choices
# ---------------------------
numeric_vars <- names(df_final)[sapply(df_final, is.numeric)]
cat_vars <- names(df_final)[sapply(df_final, function(x) is.character(x) || is.factor(x))]
color_vars <- setdiff(cat_vars, c("source_file", "dropoff_weekday_name", "payment_type"))

months <- levels(df_final$month)
weekdays_pickup <- levels(df_final$pickup_weekday_name)
# source_files <- sort(unique(as.character(df_final$source_file)))

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
            selectInput("color_var", "Color by", choices = c("None" = "none", color_vars), selected = "pickup_weekday_name")
          ),
          box(
            width = 4, title = "Filters",
            selectInput("month_filter", "Month", choices = months, selected = months, multiple = TRUE),
            selectInput("weekday_filter", "Pickup Weekday", choices = weekdays_pickup, selected = weekdays_pickup, multiple = TRUE)
            # selectInput("source_filter", "Source File", choices = source_files, selected = source_files, multiple = TRUE)
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
            selectInput("bar_x", "Category (X)", choices = color_vars, selected = "pickup_weekday_name"),
            selectInput("bar_fill", "Fill by", choices = c("None" = "none", color_vars), selected = "payment_type")
            # checkboxInput("bar_percent", "Show as percentage", FALSE)
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
    # req(input$month_filter, input$weekday_filter, input$source_filter)
    req(input$month_filter, input$weekday_filter)
    df_final %>%
      filter(
        as.character(month) %in% input$month_filter,
        as.character(pickup_weekday_name) %in% input$weekday_filter
        # as.character(source_file) %in% input$source_filter
      )
  })
  
  rag_context <- reactive({
    list(
      months = input$month_filter,
      weekdays = input$weekday_filter,
      # sources = input$source_filter,
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
    
    shiny::validate(
      shiny::need(nrow(data) > 0, "No data available for the selected filters.")
    )
    
    data <- data %>%
      mutate(
        .x = .data[[input$x_var]],
        .y = .data[[input$y_var]],
        .color = if (input$color_var == "none") factor("All") else factor(.data[[input$color_var]], levels = levels(df_final[[input$color_var]]))
      )
    
    data <- data %>%
      mutate(
        .tooltip = if (input$color_var == "none") {
          paste0(
            input$x_var, ": ", .x,
            "<br>", input$y_var, ": ", .y
          )
        } else {
          paste0(
            input$x_var, ": ", .x,
            "<br>", input$y_var, ": ", .y,
            "<br>", input$color_var, ": ", .color
          )
        }
      )
    
    p <- plotly::plot_ly(
      data = data,
      x = ~.x,
      y = ~.y,
      type = "scattergl",
      mode = "markers",
      color = if (input$color_var == "none") NULL else ~.color,
      text = ~.tooltip,
      hoverinfo = "text",
      marker = list(size = 6, opacity = input$alpha)
    )
    
    if (isTRUE(input$add_smooth) && nrow(data) >= 2) {
      fit <- lm(.y ~ .x, data = data)
      
      line_df <- data.frame(
        .x = seq(min(data$.x), max(data$.x), length.out = 200)
      )
      line_df$.y <- predict(fit, newdata = line_df)
      
      p <- p %>%
        plotly::add_lines(
          data = line_df,
          x = ~.x,
          y = ~.y,
          inherit = FALSE,
          name = "Trend line"
        )
    }
    
    p %>%
      plotly::layout(
        title = paste(input$y_var, "vs", input$x_var),
        xaxis = list(
          title = input$x_var,
          type = if (isTRUE(input$log_scale)) "log" else "linear"
        ),
        yaxis = list(
          title = input$y_var,
          type = if (isTRUE(input$log_scale)) "log" else "linear"
        )
      )
  })

  output$bar_plot <- renderPlotly({
    req(input$bar_x)
    
    plot_data <- filtered_data()
    
    if (input$bar_fill == "none") {
      data_sum <- plot_data %>%
        count(.data[[input$bar_x]], name = "n")
      
      if (is.factor(plot_data[[input$bar_x]])) {
        data_sum[[input$bar_x]] <- factor(
          data_sum[[input$bar_x]],
          levels = levels(plot_data[[input$bar_x]])
        )
      }
      
      p <- ggplot(data_sum, aes(x = .data[[input$bar_x]], y = n)) +
        geom_col(fill = "steelblue")
      
    } else {
      data_sum <- plot_data %>%
        count(.data[[input$bar_x]], .data[[input$bar_fill]], name = "n")
      
      if (is.factor(plot_data[[input$bar_x]])) {
        data_sum[[input$bar_x]] <- factor(
          data_sum[[input$bar_x]],
          levels = levels(plot_data[[input$bar_x]])
        )
      }
      
      if (is.factor(plot_data[[input$bar_fill]])) {
        data_sum[[input$bar_fill]] <- factor(
          data_sum[[input$bar_fill]],
          levels = levels(plot_data[[input$bar_fill]])
        )
      }
      
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
        y =  "Count"
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