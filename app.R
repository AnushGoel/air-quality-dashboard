# =============================================================================
# US Air Quality Performance Dashboard — Shiny app (Tableau-style)
# ALY 6110 · Big Data Analysis Group Project
# =============================================================================

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(tidyr)
library(purrr)        # <-- map_dfr lives here; previous version was missing this
library(plotly)
library(DT)
#library(leaflet)
library(scales)
library(lubridate)
library(readr)
library(stringr)

# ---- Load data ------------------------------------------------------------
DATA_PATH <- if (file.exists("cleaned.csv.gz")) {
  "cleaned.csv.gz"
} else {
  "../cleaned.csv.gz"
}

if (!file.exists(DATA_PATH)) {
  stop("Cannot find cleaned.csv.gz. Place it next to app.R or in the project root.")
}

message("Loading data...")
t0 <- Sys.time()

raw <- read_csv(
  DATA_PATH,

  show_col_types = FALSE,

  # IMPORTANT: limits memory for Render free tier
  n_max = 22000,

  col_types = cols(
    .default = col_guess(),
    `Date Local` = col_date()
  )
) |>

  mutate(
    Year = year(`Date Local`),

    Month = month(`Date Local`),

    Season = case_when(
      Month %in% c(12, 1, 2)  ~ "Winter",
      Month %in% c(3, 4, 5)   ~ "Spring",
      Month %in% c(6, 7, 8)   ~ "Summer",
      Month %in% c(9, 10, 11) ~ "Fall"
    )
  )

message(sprintf(
  "  loaded %s rows in %.1fs",
  format(nrow(raw), big.mark = ","),
  as.numeric(Sys.time() - t0, units = "secs")
))

POLLUTANTS <- c("NO2", "O3", "SO2", "CO")

COLOR <- c(
  NO2 = "#1F4E79",
  O3  = "#ED7D31",
  SO2 = "#7030A0",
  CO  = "#C00000"
)

SEASONS <- c(
  "Winter",
  "Spring",
  "Summer",
  "Fall"
)

CATEGORIES <- c(
  "Good",
  "Moderate",
  "Unhealthy for Sensitive",
  "Unhealthy",
  "Very Unhealthy",
  "Hazardous"
)

# =============================================================================
# PRE-AGGREGATE AT STARTUP — optimized for Render free tier
# =============================================================================

message("Pre-aggregating summaries...")
t0 <- Sys.time()

# -----------------------------------------------------------------------------
# Master aggregate table
# -----------------------------------------------------------------------------

agg_full <- raw |>

  group_by(
    State,
    City,
    Year,
    Season,
    `AQI Category`
  ) |>

  summarise(

    NO2_sum = sum(`NO2 AQI`, na.rm = TRUE),
    NO2_n   = sum(!is.na(`NO2 AQI`)),

    O3_sum  = sum(`O3 AQI`, na.rm = TRUE),
    O3_n    = sum(!is.na(`O3 AQI`)),

    SO2_sum = sum(`SO2 AQI`, na.rm = TRUE),
    SO2_n   = sum(!is.na(`SO2 AQI`)),

    CO_sum  = sum(`CO AQI`, na.rm = TRUE),
    CO_n    = sum(!is.na(`CO AQI`)),

    Max_sum = sum(`Max AQI`, na.rm = TRUE),
    Max_n   = sum(!is.na(`Max AQI`)),

    Max_max = suppressWarnings(
      max(`Max AQI`, na.rm = TRUE)
    ),

    n_obs = n(),

    .groups = "drop"
  )

# Fix infinite max values
agg_full$Max_max[
  is.infinite(agg_full$Max_max)
] <- NA_real_

# -----------------------------------------------------------------------------
# Monthly aggregate table
# -----------------------------------------------------------------------------

agg_monthly <- raw |>

  group_by(
    State,
    Month
  ) |>

  summarise(

    NO2_sum = sum(`NO2 AQI`, na.rm = TRUE),
    NO2_n   = sum(!is.na(`NO2 AQI`)),

    O3_sum  = sum(`O3 AQI`, na.rm = TRUE),
    O3_n    = sum(!is.na(`O3 AQI`)),

    SO2_sum = sum(`SO2 AQI`, na.rm = TRUE),
    SO2_n   = sum(!is.na(`SO2 AQI`)),

    CO_sum  = sum(`CO AQI`, na.rm = TRUE),
    CO_n    = sum(!is.na(`CO AQI`)),

    .groups = "drop"
  )

# -----------------------------------------------------------------------------
# IMPORTANT: free memory after aggregation
# -----------------------------------------------------------------------------

rm(raw)
gc()

message(sprintf(
  "done in %.1fs (agg_full: %s rows, agg_monthly: %s rows)",

  as.numeric(Sys.time() - t0, units = "secs"),

  format(nrow(agg_full), big.mark = ","),

  format(nrow(agg_monthly), big.mark = ",")
))

# -----------------------------------------------------------------------------
# State abbreviations
# -----------------------------------------------------------------------------

state_abbrev <- c(

  Alabama = "AL",
  Arizona = "AZ",
  Arkansas = "AR",
  California = "CA",
  Colorado = "CO",
  Connecticut = "CT",
  Delaware = "DE",
  Florida = "FL",
  Georgia = "GA",
  Idaho = "ID",
  Illinois = "IL",
  Indiana = "IN",
  Iowa = "IA",
  Kansas = "KS",
  Kentucky = "KY",
  Louisiana = "LA",
  Maine = "ME",
  Maryland = "MD",
  Massachusetts = "MA",
  Michigan = "MI",
  Minnesota = "MN",
  Missouri = "MO",
  Montana = "MT",
  Nevada = "NV",

  `New Hampshire` = "NH",
  `New Jersey` = "NJ",
  `New Mexico` = "NM",
  `New York` = "NY",

  `North Carolina` = "NC",
  `North Dakota` = "ND",

  Ohio = "OH",
  Oklahoma = "OK",
  Oregon = "OR",
  Pennsylvania = "PA",

  `Rhode Island` = "RI",

  `South Carolina` = "SC",
  `South Dakota` = "SD",

  Tennessee = "TN",
  Texas = "TX",
  Utah = "UT",
  Vermont = "VT",
  Virginia = "VA",
  Washington = "WA",
  Wisconsin = "WI",
  Wyoming = "WY",

  `District Of Columbia` = "DC"
)
# =============================================================================
# UI
# =============================================================================
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = HTML('<span style="font-family:Georgia,serif;font-weight:700;">Air Quality Dashboard</span>'),
    titleWidth = 320
  ),

  dashboardSidebar(
    width = 320,
    tags$style(HTML("
      .main-sidebar { background-color: #1F4E79 !important; }
      .sidebar { color: white; padding: 15px; }
      .skin-blue .main-header .navbar { background-color: #1F4E79; }
      .skin-blue .main-header .logo {
        background-color: #16395a; color: white; border-bottom: 0 none;
      }
      .skin-blue .main-header .logo:hover { background-color: #16395a; }
      .control-label, .sidebar label { color: white !important; font-weight: 500; }
      .selectize-input, .form-control { color: #1A2230 !important; }
      .irs-bar, .irs-from, .irs-to, .irs-single { background-color: #ED7D31 !important; }
      .small-box { box-shadow: 0 1px 3px rgba(0,0,0,0.08); }
      .small-box .icon { color: rgba(255,255,255,0.4); }
      .box { box-shadow: 0 1px 3px rgba(0,0,0,0.06); border-top: 3px solid #1F4E79; }
      .box-header { border-bottom: 1px solid #E5E9ED; }
      .box-title { font-family: 'Source Sans 3','Segoe UI',sans-serif; font-weight: 600; color: #1A2230; }
      .content-wrapper { background-color: #F4F6F8; }
    ")),

    tags$div(style = "padding: 15px 20px 5px;",
      tags$div(style = "color: rgba(255,255,255,0.6); font-size: 0.7rem; letter-spacing: 1.5px; text-transform: uppercase;",
               "Filter Controls"),
      tags$h4("Refine the data", style = "color: white; font-family: Georgia, serif; margin-top: 5px;")
    ),

    selectInput("state", "State (one or All):",
                choices = c("All states", sort(unique(raw$State))),
                selected = "All states"),

    sliderInput("years", "Year range:",
                min = min(raw$Year), max = max(raw$Year),
                value = c(min(raw$Year), max(raw$Year)),
                step = 1, sep = ""),

    pickerInput("pollutants", "Pollutants:",
                choices = POLLUTANTS, selected = POLLUTANTS,
                multiple = TRUE,
                options = list(`actions-box` = TRUE,
                               `selected-text-format` = "count > 2",
                               `count-selected-text` = "{0} pollutants")),

    pickerInput("cities", "Cities (auto-filters by state):",
                choices = sort(unique(raw$City)),
                selected = NULL,
                multiple = TRUE,
                options = list(`actions-box` = TRUE,
                               `live-search` = TRUE,
                               `none-selected-text` = "All cities",
                               `selected-text-format` = "count > 2",
                               `count-selected-text` = "{0} cities")),

    pickerInput("seasons", "Seasons:",
                choices = SEASONS, selected = SEASONS,
                multiple = TRUE,
                options = list(`actions-box` = TRUE,
                               `selected-text-format` = "count > 2",
                               `count-selected-text` = "{0} seasons")),

    pickerInput("categories", "AQI Categories:",
                choices = CATEGORIES, selected = CATEGORIES,
                multiple = TRUE,
                options = list(`actions-box` = TRUE,
                               `selected-text-format` = "count > 2",
                               `count-selected-text` = "{0} categories")),

    tags$hr(style = "border-color: rgba(255,255,255,0.2);"),
    actionButton("reset", "Reset all filters",
                 icon = icon("undo"),
                 style = "background-color: #ED7D31; color: white; border: none; width: 90%;")
  ),

  dashboardBody(
    fluidRow(
      valueBoxOutput("kpi_obs", width = 3),
      valueBoxOutput("kpi_avg", width = 3),
      valueBoxOutput("kpi_worst", width = 3),
      valueBoxOutput("kpi_good_pct", width = 3)
    ),

    fluidRow(
      column(12,
        tags$div(style = "background:white; padding:14px 22px; margin-bottom:15px; border-radius:4px; border-left:4px solid #ED7D31;",
          tags$div(style = "font-size: 0.7rem; letter-spacing: 0.15em; text-transform: uppercase; color: #9AA4B5; font-weight: 600;",
                   "Currently viewing"),
          tags$div(style = "font-family: 'Source Serif Pro', Georgia, serif; font-size: 1.4rem; font-weight: 600; color: #1A2230;",
                   uiOutput("dynamic_title"))
        )
      )
    ),

    tabsetPanel(id = "tabs", type = "tabs",

      tabPanel("Overview", value = "overview",
        br(),
        fluidRow(
          box(width = 12, title = "Annual Trend — Filtered View",
              status = "primary", solidHeader = FALSE,
              tags$p(style = "color:#5B6677; font-size: 0.9rem; margin-top: -5px;",
                     "Each line is a selected pollutant. Hover any year for details."),
              plotlyOutput("trend_plot", height = 360))
        ),
        fluidRow(
          box(width = 6, title = "Top 15 States in Selection",
              tags$p(style = "color:#5B6677; font-size: 0.9rem; margin-top: -5px;",
                     "Mean AQI averaged across selected pollutants."),
              plotlyOutput("top_states_plot", height = 420)),
          box(width = 6, title = "Top 15 Cities in Selection",
              tags$p(style = "color:#5B6677; font-size: 0.9rem; margin-top: -5px;",
                     "Cities with at least 100 observations in the filter."),
              plotlyOutput("top_cities_plot", height = 420))
        ),
        fluidRow(
          box(width = 6, title = "Seasonality of Selected Pollutants",
              plotlyOutput("season_plot", height = 350)),
          box(width = 6, title = "AQI Category Mix",
              plotlyOutput("category_plot", height = 350))
        )
      ),

      tabPanel("Geographic Map", value = "map",
        br(),
        fluidRow(
          box(width = 12, title = "State-Level Map (zooms when a state is selected)",
              status = "primary", solidHeader = FALSE,
              tags$p(style = "color:#5B6677; font-size: 0.9rem; margin-top: -5px;",
                     "Choose a state from the sidebar — the map zooms and highlights that state. Color = mean AQI in your selection."),
              plotlyOutput("map", height = 600))
        )
      ),

      tabPanel("Pollutant Detail", value = "detail",
        br(),
        fluidRow(
          box(width = 8, title = "Distribution of Daily AQI by Pollutant",
              tags$p(style = "color:#9AA4B5; font-size: 0.78rem; margin-top: -5px;",
                     "Loaded on first visit — distributions require the full row-level data, so may take a moment."),
              plotlyOutput("dist_plot", height = 450)),
          box(width = 4, title = "Summary Statistics",
              tableOutput("stats_table"),
              tags$p(style = "color:#9AA4B5; font-size: 0.78rem; margin-top: 12px;",
                     "All values are AQI; computed on the currently filtered data."))
        ),
        fluidRow(
          box(width = 12, title = "Pollutant Correlation Matrix (within selection)",
              plotlyOutput("corr_plot", height = 380))
        )
      ),

      tabPanel("Data Table & Download", value = "data",
        br(),
        fluidRow(
          box(width = 12, title = "Filtered Site-Day Records",
              tags$div(style = "margin-bottom: 12px;",
                downloadButton("download_data", "Download filtered data (CSV)",
                               style = "background-color: #1F4E79; color: white; border: none;")
              ),
              tags$p(style = "color:#9AA4B5; font-size: 0.78rem;",
                     "Showing up to 5,000 rows for display. The download contains the full filtered set."),
              DTOutput("data_table"))
        )
      )
    ),

    fluidRow(
      column(12,
        tags$div(style = "color:#9AA4B5; font-size: 0.8rem; text-align:center; padding: 20px 0;",
                 "ALY 6110 · Big Data Analysis · Group Project · Data: EPA Outdoor Air Quality 2000–2016 (Kaggle)")
      )
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {

  # Debounce the year slider so dragging doesn't fire a render at every tick
  years_d <- reactive(input$years) |> debounce(250)

  # Cascade city choices when state changes
  observeEvent(input$state, {
    cities <- if (input$state == "All states") {
      sort(unique(raw$City))
    } else {
      sort(unique(raw$City[raw$State == input$state]))
    }
    updatePickerInput(session, "cities", choices = cities, selected = NULL)
  })

  # Reset filters
  observeEvent(input$reset, {
    updateSelectInput(session, "state", selected = "All states")
    updateSliderInput(session, "years", value = c(min(raw$Year), max(raw$Year)))
    updatePickerInput(session, "pollutants", selected = POLLUTANTS)
    updatePickerInput(session, "seasons", selected = SEASONS)
    updatePickerInput(session, "categories", selected = CATEGORIES)
    updatePickerInput(session, "cities", selected = NULL)
  })

  # SINGLE filter applied to the aggregate table (fast)
  agg_filtered <- reactive({
    yr <- years_d()
    d <- agg_full |>
      filter(Year >= yr[1], Year <= yr[2],
             Season %in% input$seasons,
             `AQI Category` %in% input$categories)
    if (!is.null(input$state) && input$state != "All states") {
      d <- d |> filter(State == input$state)
    }
    if (!is.null(input$cities) && length(input$cities) > 0) {
      d <- d |> filter(City %in% input$cities)
    }
    d
  })

  # Helper: weighted mean across SELECTED pollutants for a grouped df with _sum/_n cols
  poll_mean_across <- function(d) {
    pcols_sum <- paste0(input$pollutants, "_sum")
    pcols_n   <- paste0(input$pollutants, "_n")
    s_all <- rowSums(as.matrix(d[, pcols_sum, drop = FALSE]), na.rm = TRUE)
    n_all <- rowSums(as.matrix(d[, pcols_n,   drop = FALSE]), na.rm = TRUE)
    ifelse(n_all > 0, s_all / n_all, NA_real_)
  }

  # Dynamic title
  output$dynamic_title <- renderUI({
    yr <- years_d()
    state_str <- if (input$state == "All states") "all U.S. states" else input$state
    city_str  <- if (!is.null(input$cities) && length(input$cities) > 0) {
      paste0(" · ", length(input$cities), " cities")
    } else ""
    poll_str <- if (length(input$pollutants) == 4) "all pollutants" else paste(input$pollutants, collapse = ", ")
    season_str <- if (length(input$seasons) == 4) "" else paste0(" · ", paste(input$seasons, collapse = ", "))
    HTML(sprintf("%s — %s — %d–%d%s%s",
                 state_str, poll_str, yr[1], yr[2], season_str, city_str))
  })

  # KPI boxes (all from agg_filtered)
  output$kpi_obs <- renderValueBox({
    valueBox(format(sum(agg_filtered()$n_obs), big.mark = ","),
             "Site-days in selection",
             icon = icon("database"), color = "blue")
  })

  output$kpi_avg <- renderValueBox({
    d <- agg_filtered()
    if (nrow(d) == 0 || length(input$pollutants) == 0) {
      return(valueBox("—", "Mean AQI", icon = icon("wind"), color = "olive"))
    }
    pcols_sum <- paste0(input$pollutants, "_sum")
    pcols_n   <- paste0(input$pollutants, "_n")
    total_sum <- sum(as.matrix(d[, pcols_sum, drop = FALSE]), na.rm = TRUE)
    total_n   <- sum(as.matrix(d[, pcols_n,   drop = FALSE]), na.rm = TRUE)
    avg <- if (total_n > 0) total_sum / total_n else NA_real_
    valueBox(if (is.na(avg)) "—" else sprintf("%.1f", avg),
             paste("Mean", paste(input$pollutants, collapse = "/"), "AQI"),
             icon = icon("wind"), color = "olive")
  })

  output$kpi_worst <- renderValueBox({
    d <- agg_filtered()
    if (nrow(d) == 0) {
      return(valueBox("—", "Worst day", icon = icon("triangle-exclamation"), color = "red"))
    }
    worst <- suppressWarnings(max(d$Max_max, na.rm = TRUE))
    valueBox(if (is.infinite(worst)) "—" else sprintf("%.0f", worst),
             "Worst single-day AQI",
             icon = icon("triangle-exclamation"), color = "red")
  })

  output$kpi_good_pct <- renderValueBox({
    d <- agg_filtered()
    total <- sum(d$n_obs)
    if (total == 0) {
      return(valueBox("—", "Good-air days", icon = icon("circle-check"), color = "green"))
    }
    good <- sum(d$n_obs[d$`AQI Category` == "Good"])
    pct <- good / total * 100
    valueBox(sprintf("%.0f%%", pct),
             "Days rated 'Good'",
             icon = icon("circle-check"), color = "green")
  })

  # Trend plot
  output$trend_plot <- renderPlotly({
    d <- agg_filtered()
    if (nrow(d) == 0 || length(input$pollutants) == 0) return(NULL)

    by_year <- d |>
      group_by(Year) |>
      summarise(across(ends_with("_sum"), \(x) sum(x, na.rm = TRUE)),
                across(ends_with("_n"),   \(x) sum(x, na.rm = TRUE)),
                .groups = "drop")
    for (p in POLLUTANTS) {
      sc <- paste0(p, "_sum"); nc <- paste0(p, "_n")
      by_year[[p]] <- ifelse(by_year[[nc]] > 0, by_year[[sc]] / by_year[[nc]], NA_real_)
    }

    p <- plot_ly() |>
      layout(paper_bgcolor = "white", plot_bgcolor = "white",
             xaxis = list(title = "", gridcolor = "#E5E9ED"),
             yaxis = list(title = "Mean AQI", gridcolor = "#E5E9ED"),
             hovermode = "x unified",
             legend = list(orientation = "h", y = -0.2, x = 0.5, xanchor = "center"),
             margin = list(l = 50, r = 30, t = 10, b = 50)) |>
      config(displayModeBar = FALSE)
    for (poll in input$pollutants) {
      p <- p |> add_lines(x = by_year$Year, y = by_year[[poll]], name = poll,
                          line = list(color = COLOR[poll], width = 3, shape = "spline"),
                          marker = list(size = 6, color = COLOR[poll]),
                          mode = "lines+markers",
                          hovertemplate = paste0("<b>", poll, "</b>: %{y:.1f}<extra></extra>"))
    }
    p
  })

  # Top states
  output$top_states_plot <- renderPlotly({
    d <- agg_filtered()
    if (nrow(d) == 0 || length(input$pollutants) == 0) return(NULL)

    by_state <- d |>
      group_by(State) |>
      summarise(across(ends_with("_sum"), \(x) sum(x, na.rm = TRUE)),
                across(ends_with("_n"),   \(x) sum(x, na.rm = TRUE)),
                .groups = "drop")
    by_state$AvgAQI <- poll_mean_across(by_state)
    top <- by_state |> filter(!is.na(AvgAQI)) |>
      arrange(desc(AvgAQI)) |> head(15) |> arrange(AvgAQI)
    if (nrow(top) == 0) return(NULL)

    plot_ly(top, y = ~State, x = ~AvgAQI, type = "bar", orientation = "h",
            marker = list(color = "#1F4E79"),
            text = ~sprintf("%.1f", AvgAQI), textposition = "outside",
            hovertemplate = "<b>%{y}</b><br>AQI: %{x:.1f}<extra></extra>") |>
      layout(paper_bgcolor = "white", plot_bgcolor = "white",
             xaxis = list(title = "Mean AQI", gridcolor = "#E5E9ED"),
             yaxis = list(title = "", automargin = TRUE),
             margin = list(l = 100, r = 50, t = 10, b = 40)) |>
      config(displayModeBar = FALSE)
  })

  # Top cities
  output$top_cities_plot <- renderPlotly({
    d <- agg_filtered()
    if (nrow(d) == 0 || length(input$pollutants) == 0) return(NULL)

    by_city <- d |>
      group_by(City, State) |>
      summarise(across(ends_with("_sum"), \(x) sum(x, na.rm = TRUE)),
                across(ends_with("_n"),   \(x) sum(x, na.rm = TRUE)),
                n = sum(n_obs),
                .groups = "drop")
    by_city$AvgAQI <- poll_mean_across(by_city)
    top <- by_city |> filter(!is.na(AvgAQI), n >= 100) |>
      arrange(desc(AvgAQI)) |> head(15) |>
      mutate(Label = paste0(City, ", ", state_abbrev[State])) |>
      arrange(AvgAQI)
    if (nrow(top) == 0) return(NULL)

    plot_ly(top, y = ~Label, x = ~AvgAQI, type = "bar", orientation = "h",
            marker = list(color = "#ED7D31"),
            text = ~sprintf("%.1f", AvgAQI), textposition = "outside",
            hovertemplate = "<b>%{y}</b><br>AQI: %{x:.1f}<br>%{customdata} obs<extra></extra>",
            customdata = ~format(n, big.mark = ",")) |>
      layout(paper_bgcolor = "white", plot_bgcolor = "white",
             xaxis = list(title = "Mean AQI", gridcolor = "#E5E9ED"),
             yaxis = list(title = "", automargin = TRUE),
             margin = list(l = 130, r = 50, t = 10, b = 40)) |>
      config(displayModeBar = FALSE)
  })

  # Seasonality (uses agg_monthly directly, not year-filtered — monthly pattern is averaged across years)
  output$season_plot <- renderPlotly({
    d <- agg_monthly
    if (!is.null(input$state) && input$state != "All states") {
      d <- d |> filter(State == input$state)
    }
    if (nrow(d) == 0 || length(input$pollutants) == 0) return(NULL)

    by_month <- d |>
      group_by(Month) |>
      summarise(across(ends_with("_sum"), \(x) sum(x, na.rm = TRUE)),
                across(ends_with("_n"),   \(x) sum(x, na.rm = TRUE)),
                .groups = "drop")
    for (p in POLLUTANTS) {
      sc <- paste0(p, "_sum"); nc <- paste0(p, "_n")
      by_month[[p]] <- ifelse(by_month[[nc]] > 0, by_month[[sc]] / by_month[[nc]], NA_real_)
    }
    month_names <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

    p <- plot_ly() |>
      layout(paper_bgcolor = "white", plot_bgcolor = "white",
             xaxis = list(title = "", gridcolor = "#E5E9ED"),
             yaxis = list(title = "Mean AQI", gridcolor = "#E5E9ED"),
             hovermode = "x unified",
             legend = list(orientation = "h", y = -0.18, x = 0.5, xanchor = "center"),
             margin = list(l = 50, r = 30, t = 10, b = 60)) |>
      config(displayModeBar = FALSE)
    for (poll in input$pollutants) {
      p <- p |> add_lines(x = month_names[by_month$Month], y = by_month[[poll]],
                          name = poll,
                          line = list(color = COLOR[poll], width = 3, shape = "spline"),
                          marker = list(size = 6, color = COLOR[poll]),
                          mode = "lines+markers",
                          hovertemplate = paste0("<b>", poll, "</b>: %{y:.1f}<extra></extra>"))
    }
    p
  })

  # AQI category mix
  output$category_plot <- renderPlotly({
    d <- agg_filtered()
    if (nrow(d) == 0) return(NULL)

    cat_data <- d |>
      group_by(`AQI Category`) |>
      summarise(n = sum(n_obs), .groups = "drop") |>
      filter(!is.na(`AQI Category`)) |>
      mutate(`AQI Category` = factor(`AQI Category`, levels = CATEGORIES)) |>
      arrange(`AQI Category`)

    cat_colors <- c("Good" = "#548235", "Moderate" = "#FFC000",
                    "Unhealthy for Sensitive" = "#ED7D31",
                    "Unhealthy" = "#C00000", "Very Unhealthy" = "#7030A0",
                    "Hazardous" = "#4A1A1A")

    plot_ly(cat_data, labels = ~`AQI Category`, values = ~n,
            type = "pie", textinfo = "label+percent",
            textposition = "inside",
            marker = list(colors = cat_colors[as.character(cat_data$`AQI Category`)],
                          line = list(color = "white", width = 2)),
            hovertemplate = "<b>%{label}</b><br>%{value:,} site-days<br>%{percent}<extra></extra>") |>
      layout(paper_bgcolor = "white", showlegend = FALSE,
             margin = list(l = 20, r = 20, t = 10, b = 10)) |>
      config(displayModeBar = FALSE)
  })

  # ---- Detail tab: needs row-level data; loaded lazily, cached on filters
  raw_filtered <- reactive({
    yr <- years_d()
    d <- raw |>
      filter(Year >= yr[1], Year <= yr[2],
             Season %in% input$seasons,
             `AQI Category` %in% input$categories)
    if (!is.null(input$state) && input$state != "All states") {
      d <- d |> filter(State == input$state)
    }
    if (!is.null(input$cities) && length(input$cities) > 0) {
      d <- d |> filter(City %in% input$cities)
    }
    d
  }) |> bindCache(input$state, years_d(), input$seasons, input$categories, input$cities)

  output$dist_plot <- renderPlotly({
    req(input$tabs == "detail")
    d <- raw_filtered()
    if (nrow(d) == 0 || length(input$pollutants) == 0) return(NULL)

    dist_data <- d |>
      select(all_of(paste(input$pollutants, "AQI"))) |>
      pivot_longer(everything(), names_to = "Pollutant", values_to = "AQI") |>
      mutate(Pollutant = str_remove(Pollutant, " AQI")) |>
      filter(!is.na(AQI), AQI > 0)

    plot_ly(dist_data, y = ~AQI, color = ~Pollutant,
            colors = COLOR[unique(dist_data$Pollutant)],
            type = "violin", box = list(visible = TRUE),
            meanline = list(visible = TRUE), points = FALSE) |>
      layout(paper_bgcolor = "white", plot_bgcolor = "white",
             yaxis = list(type = "log", title = "AQI (log scale)", gridcolor = "#E5E9ED"),
             xaxis = list(title = "", gridcolor = "#E5E9ED"),
             showlegend = FALSE,
             margin = list(l = 60, r = 30, t = 10, b = 40)) |>
      config(displayModeBar = FALSE)
  })

  # Summary stats table — purrr::map_dfr is now loaded, fixed bug
  output$stats_table <- renderTable({
    req(input$tabs == "detail")
    d <- raw_filtered()
    if (nrow(d) == 0 || length(input$pollutants) == 0) return(NULL)

    map_dfr(input$pollutants, function(p) {
      col <- paste0(p, " AQI")
      x <- na.omit(d[[col]])
      if (length(x) == 0) {
        tibble(Pollutant = p, N = "0", Mean = NA_real_, Median = NA_real_,
               P95 = NA_real_, Max = NA_real_)
      } else {
        tibble(Pollutant = p,
               N = format(length(x), big.mark = ","),
               Mean = round(mean(x), 1),
               Median = round(median(x), 1),
               P95 = round(quantile(x, 0.95), 0),
               Max = round(max(x), 0))
      }
    })
  }, striped = TRUE, hover = TRUE)

  # Correlation matrix
  output$corr_plot <- renderPlotly({
    req(input$tabs == "detail")
    d <- raw_filtered()
    if (nrow(d) == 0 || length(input$pollutants) < 2) return(NULL)

    cols <- paste(input$pollutants, "AQI")
    m <- cor(d[, cols], use = "pairwise.complete.obs")
    rownames(m) <- colnames(m) <- input$pollutants

    plot_ly(z = m, x = colnames(m), y = rownames(m), type = "heatmap",
            colorscale = list(c(0, "#2E75B6"), c(0.4, "white"),
                              c(0.5, "white"), c(1, "#C00000")),
            zmin = -1, zmax = 1,
            text = matrix(sprintf("%.2f", m), length(input$pollutants), length(input$pollutants)),
            texttemplate = "%{text}",
            textfont = list(family = "Source Sans 3", size = 14),
            hovertemplate = "%{y} ↔ %{x}<br>r = %{z:.3f}<extra></extra>") |>
      layout(paper_bgcolor = "white", plot_bgcolor = "white",
             xaxis = list(side = "top"),
             yaxis = list(autorange = "reversed"),
             margin = list(l = 50, r = 30, t = 50, b = 30)) |>
      config(displayModeBar = FALSE)
  })

 # Map
output$map <- renderPlotly({

  req(input$tabs == "map" || input$tabs == "overview")

  d <- agg_filtered()

  req(nrow(d) > 0)
  req(length(input$pollutants) > 0)

  by_state <- d |>
    group_by(State) |>
    summarise(
      across(ends_with("_sum"), \(x) sum(x, na.rm = TRUE)),
      across(ends_with("_n"), \(x) sum(x, na.rm = TRUE)),
      n_obs = sum(n_obs),
      .groups = "drop"
    )

  by_state$AvgAQI <- poll_mean_across(by_state)

  by_state <- by_state |>
    filter(!is.na(AvgAQI)) |>
    left_join(state_centers, by = "State") |>
    filter(!is.na(lat))

  req(nrow(by_state) > 0)

  map_center <- list(
    lon = -98.5,
    lat = 39.8
  )

  map_zoom <- 3

  if (!is.null(input$state) &&
      input$state != "All states") {

    center <- state_centers |>
      filter(State == input$state)

    if (nrow(center) == 1) {

      map_center <- list(
        lon = center$lng,
        lat = center$lat
      )

      map_zoom <- center$zoom
    }
  }

  plot_ly(
    data = by_state,

    lat = ~lat,
    lon = ~lng,

    type = "scattermapbox",
    mode = "markers",

    marker = list(
      size = 10,
      color = ~AvgAQI,
      colorscale = "Reds",
      showscale = TRUE,
      opacity = 0.8
    ),

    text = ~paste0(
      "<b>", State, "</b><br>",
      "Mean AQI: ", round(AvgAQI, 1), "<br>",
      "Observations: ", format(n_obs, big.mark = ",")
    ),

    hoverinfo = "text"
  ) %>%

  layout(
    mapbox = list(
      style = "open-street-map",
      center = map_center,
      zoom = map_zoom
    ),

    margin = list(
      l = 0,
      r = 0,
      t = 0,
      b = 0
    )
  )

})

# Data table — lazy, only renders on the data tab
output$data_table <- renderDT({

  req(input$tabs == "data")

  d <- raw_filtered() |>
    select(
      `Date Local`,
      State,
      City,
      NO2 = `NO2 AQI`,
      O3 = `O3 AQI`,
      SO2 = `SO2 AQI`,
      CO = `CO AQI`,
      `Max AQI`,
      `AQI Category`,
      `Worst Pollutant`
    )

  if (nrow(d) > 5000)
    d <- d |> head(5000)

  datatable(
    d,
    options = list(
      pageLength = 15,
      scrollX = TRUE,
      order = list(list(7, "desc"))
    ),
    rownames = FALSE,
    class = "stripe hover compact",
    filter = "top"
  ) |>
    formatRound(
      c("NO2", "O3", "SO2", "CO", "Max AQI"),
      digits = 1
    )

})

output$download_data <- downloadHandler(

  filename = function() {
    paste0("filtered_pollution_", Sys.Date(), ".csv")
  },

  content = function(file) {
    write_csv(raw_filtered(), file)
  }

)

}

shinyApp(ui, server)
