library(shiny)
library(bslib)
library(tidyverse)
library(scales)

app_swim_stats <- function() {
  df_swims <- load_swim_data()
  choices_year <- unique(year(df_swims$activity_date))
  choices_month <- filter_months(df_swims, year(Sys.Date()))

  ui <- page_sidebar(
    title = "Kaustav's Swim Stats",
    theme = bs_theme(
      preset = "lux",
      bg = "#fff",
      fg = "#B40F20",
      base_font = font_google("Montserrat"),
      heading_font = font_google("Montserrat Alternates"),
      `enable-rounded` = TRUE
    ),
    sidebar = sidebar(
      open = "closed",
      selectInput(
        "year",
        "Select year",
        choices = choices_year,
        selected = year(Sys.Date())
      ),
      selectInput(
        "month",
        "Select month",
        choices = choices_month,
        selected = month(Sys.Date())
      )
    ),
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
        title = "Average pace (mins per 100m)",
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
        card_body("Plot showing the progress compared to previous month")
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
    observeEvent(input$year, {
      freezeReactiveValue(input, "month")
      choices_month <- filter_months(df_swims, input$year)
      latest_month <- choices_month[length(choices_month)]

      updateSelectInput(
        inputId = "month",
        choices = choices_month,
        selected = latest_month
      )
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
