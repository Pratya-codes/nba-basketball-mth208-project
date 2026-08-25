library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)

season_files <- c(
  '../complete_data_for_referance/team_rating_15_16.csv',
  '../complete_data_for_referance/team_rating_16_17.csv',
  '../complete_data_for_referance/team_rating_17_18.csv',
  '../complete_data_for_referance/team_rating_18_19.csv',
  '../complete_data_for_referance/team_rating_19_20.csv',
  '../complete_data_for_referance/team_rating_20_21.csv',
  '../complete_data_for_referance/team_rating_21_22.csv',
  '../complete_data_for_referance/team_rating_22_23.csv',
  '../complete_data_for_referance/team_rating_23_24.csv',
  '../complete_data_for_referance/team_rating_24_25.csv'
)

read_team_file <- function(path) {
  if (!file.exists(path)) return(NULL)
  
  df <- read_csv(path, show_col_types = FALSE)
  season <- str_extract(basename(path), "\\d{2}_?\\d{2}")
  
  df <- df %>% mutate(Season = season)
  return(df)
}

ui <- dashboardPage(
  
  dashboardHeader(
    title = span(class = "app-title", "Team Performance")
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("line-chart")),
      menuItem("Compare Teams", tabName = "compare", icon = icon("columns"))
    ),
    width = 280
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .app-title { display:block; margin:0 auto; text-align:center; font-weight:700; }
        .content-wrapper, .right-side { background-color: #f6f6f6; }
      "))
    ),
    
    tabItems(
      
      # ---- Overview Tab ----
      tabItem(tabName = "overview",
              
              fluidRow(
                box(
                  width = 3, title = "Controls",
                  status = "primary", solidHeader = TRUE,
                  
                  selectInput(
                    "metric", "Metric to plot",
                    choices = c(
                      "W.L." = "W.L.", "MOV" = "MOV", "ORtg" = "ORtg",
                      "DRtg" = "DRtg", "NRtg" = "NRtg",
                      "Points" = "PTS", "Assists" = "AST", "Rebounds" = "TRB"
                    ),
                    selected = "NRtg"
                  ),
                  
                  selectizeInput(
                    "team_select", "Select teams (max 6)",
                    choices = NULL, multiple = TRUE,
                    options = list(maxItems = 6)
                  ),
                  
                  sliderInput(
                    "seasons_range", "Seasons range (index)",
                    min = 1, max = 10, value = c(1, 10), step = 1
                  ),
                  
                  checkboxInput("smooth", "Add smoothing", value = FALSE)
                ),
                
                box(
                  width = 9, title = "Trend plot (hover for details)",
                  status = "primary", solidHeader = TRUE,
                  plotlyOutput("trend_plot", height = "520px")
                )
              ),
              
              fluidRow(
                box(
                  width = 12, title = "Summary table",
                  status = "info", solidHeader = TRUE,
                  DTOutput("summary_table")
                )
              )
      ),
      
      # ---- Compare Teams Tab ----
      tabItem(tabName = "compare",
              
              fluidRow(
                box(
                  width = 4, title = "Compare Controls",
                  status = "primary", solidHeader = TRUE,
                  
                  selectInput("x_metric", "X axis metric", choices = NULL),
                  selectInput("y_metric", "Y axis metric", choices = NULL, selected = "W.L."),
                  selectInput(
                    "color_by", "Color by",
                    choices = c("Season", "Conf", "Div", "Team"),
                    selected = "Season"
                  ),
                  checkboxInput("show_labels", "Show point labels", value = FALSE)
                ),
                box(
                  width = 8, title = "Scatter (season filter)",
                  status = "primary", solidHeader = TRUE,
                  
                  sliderInput(
                    "season_picker", "Select season(s)",
                    min = 1, max = 10, value = 10, step = 1
                  ),
                  plotlyOutput("scatter_plot", height = "520px")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  raw_data <- reactive({
    dfs <- map(season_files, read_team_file) %>% compact()
    if (length(dfs) == 0) return(tibble())
    
    combined <- bind_rows(dfs) %>%
      mutate(
        Team = as.character(Team) %||% as.character(team),
        Season = as.character(Season)
      ) %>%
      rename_with(~ str_replace_all(.x, c(
        "^W$" = "W", "^L$" = "L", "W.L." = "W.L.",
        "MOV" = "MOV", "ORtg" = "ORtg",
        "DRtg" = "DRtg", "NRtg" = "NRtg"
      )), everything()) %>%
      mutate(across(where(is.character), ~na_if(.x, "")))
    possible_numeric <- c(
      "W", "L", "W.L.", "MOV", "ORtg", "DRtg", "NRtg",
      "Adj_MOV", "Adj_ORtg", "Adj_DRtg", "Adj_NRtg",
      "PTS", "AST", "TRB"
    )
    
    for (col in possible_numeric) {
      if (col %in% names(combined)) {
        combined[[col]] <- suppressWarnings(as.numeric(combined[[col]]))
      }
    }
    
    combined
  })

  data_proc <- reactive({
    df <- raw_data()
    if (nrow(df) == 0) return(tibble())
    if (!"W.L." %in% names(df) && all(c("W", "L") %in% names(df))) {
      df <- df %>%
        mutate(`W.L.` = if_else((W + L) > 0, W / (W + L), NA_real_))
    }

    seasons_order <- df %>%
      distinct(Season) %>%
      arrange(Season) %>%
      pull(Season)
    
    df %>%
      mutate(
        Season = factor(Season, levels = seasons_order),
        season_index = as.integer(factor(Season, levels = seasons_order))
      )
  })
  observe({
    df <- data_proc()
    
    team_choices <- df %>% distinct(Team) %>% arrange(Team) %>% pull(Team)
    updateSelectizeInput(session, "team_select", choices = team_choices, server = TRUE)
    
    numeric_cols <- df %>% select(where(is.numeric)) %>% names()
    numeric_cols <- numeric_cols[!numeric_cols %in% c("...1")]
    
    metric_choices <- unique(c(
      "W.L.", "MOV", "ORtg", "DRtg", "NRtg",
      "PTS", "AST", "TRB", numeric_cols
    ))
    metric_choices <- metric_choices[metric_choices %in% names(df)]
    
    updateSelectInput(session, "metric", choices = metric_choices, selected = "NRtg")
    updateSelectInput(session, "x_metric", choices = metric_choices, selected = metric_choices[1])
    updateSelectInput(session, "y_metric", choices = metric_choices, selected = "W.L.")
    
    maxidx <- max(df$season_index, na.rm = TRUE)
    if (is.finite(maxidx) && maxidx >= 1) {
      updateSliderInput(session, "seasons_range", min = 1, max = maxidx, value = c(1, maxidx))
      updateSliderInput(session, "season_picker", min = 1, max = maxidx, value = maxidx)
    }
  })
  output$trend_plot <- renderPlotly({
    df <- data_proc()
    req(nrow(df) > 0)
    
    metric <- input$metric
    teams <- if (length(input$team_select) > 0) input$team_select else {
      df %>% distinct(Team) %>% slice(1:6) %>% pull(Team)
    }
    
    sel <- df %>%
      filter(
        Team %in% teams,
        season_index >= input$seasons_range[1],
        season_index <= input$seasons_range[2]
      )
    req(metric %in% names(sel))
    p <- ggplot(sel, aes(
      x = season_index, y = .data[[metric]],
      color = Team, group = Team,
      text = paste0(
        "Team: ", Team, "<br>Season: ", Season,
        "<br>", metric, ": ", round(.data[[metric]], 3)
      )
    )) +
      geom_line() +
      geom_point(size = 2)
    
    if (input$smooth)
      p <- p + geom_smooth(se = FALSE, method = "loess")
    
    p <- p + labs(
      x = "Season (index)", y = metric,
      title = paste(metric, "over seasons")
    )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        legend = list(
          orientation = "v",
          x = 1.05,
          y = 0.5,
          xanchor = "left"
        ),
        margin = list(r = 120)
      )
  })
  output$summary_table <- renderDT({
    df <- data_proc()
    req(nrow(df) > 0)
    
    metric <- input$metric
    
    sel <- df %>%
      filter(
        season_index >= input$seasons_range[1],
        season_index <= input$seasons_range[2]
      ) %>%
      group_by(Team) %>%
      summarise(
        mean = if (metric %in% names(.))
          round(mean(.data[[metric]], na.rm = TRUE), 3)
        else NA_real_,
        
        sd = if (metric %in% names(.))
          round(sd(.data[[metric]], na.rm = TRUE), 3)
        else NA_real_
      ) %>%
      arrange(desc(mean)) %>%
      ungroup()
    
    datatable(sel, options = list(pageLength = 10))
  })
  output$scatter_plot <- renderPlotly({
    df <- data_proc()
    req(nrow(df) > 0)
    
    x <- input$x_metric
    y <- input$y_metric
    season_idx <- input$season_picker
    
    sel <- df %>% filter(season_index == season_idx)
    req(x %in% names(sel) && y %in% names(sel))
    
    p <- ggplot(sel, aes_string(x = x, y = y, color = input$color_by)) +
      geom_point() +
      labs(title = paste0(y, " vs ", x, " (season index = ", season_idx, ")"))
    
    if (input$show_labels)
      p <- p + geom_text(aes(label = Team), vjust = -1, size = 3)
    
    ggplotly(p, tooltip = "text")
  })
}
shinyApp(ui, server)
