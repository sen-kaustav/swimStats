library(shiny)
library(bslib)
library(shinyWidgets)
library(shinyjs)
library(tidyverse)
library(scales)
library(systemfonts)

options(shiny.useragg = TRUE)

app_swim_stats <- function() {
  # Register fonts ---------------------------------------------------------
  clear_registry()

  num_font_path_plain <- system.file(
    "fonts/FiraCode-Regular.ttf",
    package = "swimStats"
  )

  num_font_path_bold <- system.file(
    "fonts/FiraCode-Bold.ttf",
    package = "swimStats"
  )
  register_font(
    "Num Font",
    plain = num_font_path_plain,
    bold = num_font_path_bold
  )

  app_theme <- bs_theme(
    preset = "lux",
    bg = "#fff",
    fg = "#B40F20",
    base_font = font_google("Montserrat"),
    heading_font = font_google("Montserrat Alternates"),
    `enable-rounded` = TRUE
  ) |>
    bs_add_rules(sass::sass_file(system.file(
      "app/www/styles.scss",
      package = "swimStats"
    )))

  df_swims <- load_swim_data()
  choices_year <- unique(year(df_swims$activity_date))
  choices_month <- filter_months(df_swims, year(Sys.Date()))

  min_date <- floor_date(min(df_swims$activity_date), unit = "month")
  max_date <- floor_date(max(df_swims$activity_date), unit = "month")

  # Add resource path ------------------------------------------------------
  addResourcePath("www", system.file("app/www", package = "swimStats"))

  ui <- page_sidebar(
    title = "Kaustav's Swim Stats",
    theme = app_theme,
    useShinyjs(),
    tags$head(
      tags$link(
        rel = "icon",
        type = "image/png",
        href = "www/logo.png"
      ),
      tags$link(rel = "manifest", href = "www/manifest.webmanifest")
    ),
    sidebar = sidebar(
      open = "closed",
      pickerInput(
        "year",
        "Select year",
        choices = choices_year,
        selected = year(Sys.Date())
      ),
      pickerInput(
        "month",
        "Select month",
        choices = choices_month,
        selected = month(Sys.Date())
      ),
    ),
    div(
      id = "nav-control",
      actionButton(
        "btn_prev_month",
        "← Prev",
        class = "btn-sm",
        inline = TRUE
      ),
      hidden(actionButton(
        "btn_next_month",
        "Next →",
        class = "btn-sm",
        inline = TRUE
      ))
    ),
    h3(uiOutput("month_title", inline = TRUE), class = "month-title"),
    layout_column_wrap(
      width = 1 / 4,
      value_box(
        title = "Number of Sessions",
        value = uiOutput("num_sessions", inline = TRUE),
        showcase = icon("water-ladder")
      ),
      value_box(
        title = "Distance Covered",
        value = uiOutput("dist_swam", inline = TRUE),
        showcase = icon("water")
      ),
      value_box(
        title = "Average pace",
        value = uiOutput("avg_pace", inline = TRUE),
        showcase = icon("clock")
      )
    ),
    layout_columns(
      col_widths = 1 / 2,
      card(
        card_header("Calendar view"),
        card_body(plotOutput("month_swim_calendar"))
      ),
      card(
        card_header("Benchmark to previous month"),
        card_body(plotOutput("month_swim_compare"))
      )
    ),
    fluidRow(column(
      12,
      accordion(
        open = FALSE,
        accordion_panel(
          title = "Individual swim details",
          "Include a table with dates, swim times and length"
        )
      )
    ))
  )

  server <- function(input, output, session) {
    current_date <- reactive(make_date(input$year, input$month))
    choices_month <- reactiveVal()
    is_min_month <- reactive(current_date() == min_date)
    is_max_month <- reactive(current_date() == max_date)

    observeEvent(input$year, {
      freezeReactiveValue(input, "month")
      choices_month(filter_months(df_swims, input$year))
      latest_month <- choices_month()[length(choices_month())]

      updatePickerInput(
        inputId = "month",
        choices = choices_month(),
        selected = latest_month
      )
    })

    # Toggle button visibility
    observeEvent(c(input$year, input$month), {
      if (is_min_month()) {
        shinyjs::hide("btn_prev_month")
      } else {
        shinyjs::show("btn_prev_month")
      }

      if (is_max_month()) {
        shinyjs::hide("btn_next_month")
      } else {
        shinyjs::show("btn_next_month")
      }
    })

    observeEvent(input$btn_prev_month, {
      previous_date <- current_date() - months(1)

      updatePickerInput(inputId = "year", selected = year(previous_date))

      # Find the closest available month on or before the target date
      target_month <- month(previous_date, label = TRUE)
      matching_idx <- which(choices_month() == target_month)

      if (length(matching_idx) > 0) {
        updatePickerInput(
          inputId = "month",
          selected = month(previous_date, label = TRUE)
        )
      } else {
        # Fall back to the largest month less than the target
        valid_idx <- which(
          as.numeric(choices_month()) < as.numeric(target_month)
        )
        selected_month <- choices_month()[max(valid_idx)]
        updatePickerInput(
          inputId = "month",
          selected = selected_month
        )
      }
    })

    observeEvent(input$btn_next_month, {
      next_date <- current_date() + months(1)

      updatePickerInput(inputId = "year", selected = year(next_date))

      # Find the closest available month on or after the target date
      target_month <- month(next_date, label = TRUE)
      matching_idx <- which(choices_month() == target_month)

      if (length(matching_idx) > 0) {
        updatePickerInput(
          inputId = "month",
          selected = month(next_date, label = TRUE)
        )
      } else {
        # Fall back to the smallest month greater than the target
        valid_idx <- which(
          as.numeric(choices_month()) > as.numeric(target_month)
        )
        selected_month <- choices_month()[min(valid_idx)]
        updatePickerInput(
          inputId = "month",
          selected = selected_month
        )
      }
    })

    output$month_title <- renderUI({
      req(input$year, input$month)
      get_title(input$year, input$month)
    })

    output$month_swim_calendar <- renderPlot(
      {
        plot_monthly_swim_calendar(
          df_swims,
          year = as.numeric(input$year),
          month = input$month
        )
      },
      res = 96
    )

    output$month_swim_compare <- renderPlot(
      {
        plot_monthly_compare(
          df_swims,
          year = as.numeric(input$year),
          month = input$month
        )
      },
      res = 96
    )

    output$num_sessions <- renderUI({
      req(input$year, input$month)
      get_num_sessions(df_swims, input$year, input$month)
    })

    output$dist_swam <- renderUI({
      get_dist_swam(df_swims, input$year, input$month)
    })

    output$avg_pace <- renderUI({
      get_avg_pace(df_swims, input$year, input$month)
    })
  }

  shinyApp(ui, server)
}
