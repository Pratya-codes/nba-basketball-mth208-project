library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(bslib)
Player_database <- read.csv("../complete_data_for_referance/Player_database.csv")

custom_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  base_font = font_google("Poppins"),
  heading_font = font_google("Montserrat"),
  primary = "#0073C2",
  secondary = "#4CAF50"
)

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = tags$div(
      "NBA Player Age & Position Dashboard",
      style = "
        font-family: 'Montserrat';
        font-weight: 600;
        font-size: 22px;
        color: white;
        text-align: center;
        width: 100%;
        position: absolute;
        left: 0;
        right: 0;
        top: 10px;
      "
    ),
    titleWidth = "100%"
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-bar")),
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      br(),
      conditionalPanel(
        condition = "input.tabs == 'dashboard'",
        h4("Filter Options", 
           style = "text-align:center; font-weight:600; color:#4CAF50;"),
        br(),
        sliderInput(
          "age_range", "Select Age Range:",
          min = min(Player_database$Age, na.rm = TRUE),
          max = max(Player_database$Age, na.rm = TRUE),
          value = c(
            min(Player_database$Age, na.rm = TRUE),
            max(Player_database$Age, na.rm = TRUE)
          )
        ),
        selectInput(
          "position_filter", "Select Position:",
          choices = c("All", unique(Player_database$Pos))
        ),
        selectInput(
          "team_filter", "Select Team:",
          choices = c("All", unique(Player_database$Team))
        ),
        radioButtons(
          "metric", "Display as:",
          choices = c(
            "Absolute Count" = "count",
            "Percentage (by Age)" = "percent"
          ),
          selected = "count"
        )
      )
    )
  ),
  
  dashboardBody(
    theme = custom_theme,
    
    tags$head(tags$style(HTML("
      .main-header .logo { display: none; }
      .main-header .navbar {
        background-color: #0073C2 !important;
        height: 55px;
      }
      .main-header .sidebar-toggle {
        float: left;
        padding: 12px 15px;
        color: white !important;
        font-size: 20px;
      }
      .content-wrapper { background-color: #f7f9fb; }
      .box {
        border-radius: 12px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
      }
      .box-header { border-bottom: none; }
    "))),
    
    tabItems(
      # ---- Dashboard Tab ----
      tabItem(
        tabName = "dashboard",
        fluidRow(
          box(
            width = 12,
            title = span(
              "Player Distribution by Age & Position",
              style = "font-family:'Montserrat'; font-weight:600; color:white;"  # white title
            ),
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("age_position_plot", height = "550px")
          )
        )
      ),
      
      # ---- About Tab ----
      tabItem(
        tabName = "about",
        box(
          width = 12,
          title = span(
            "About This Dashboard",
            style = "font-family:'Montserrat'; font-weight:600; color:white;"  # white title
          ),
          status = "primary",
          solidHeader = TRUE,
          div(
            style = "font-family:'Poppins'; font-size:15px; line-height:1.6;",
            HTML("
              <p>This interactive dashboard visualizes the <b>distribution of NBA players</b> across 
              different <b>ages, positions, and teams</b>. The data used here has been compiled from 
              the <b>NBA Player Database</b> containing player information such as age, position, 
              and team affiliation for a given season.</p>
              
              <p><b>Features:</b></p>
              <ul>
                <li>Filter players by <b>age range</b>, <b>position</b>, and <b>team</b>.</li>
                <li>Switch between <b>absolute counts</b> and <b>percentages</b> by age.</li>
                <li>Hover over the bars to view exact values interactively.</li>
              </ul>
              
              <p><b>Methods Used:</b><br>
              The dashboard uses <code>dplyr</code> for data aggregation and <code>ggplot2</code> 
              with <code>plotly</code> for interactive visualization. Data is grouped by 
              age and player position to provide both numerical and proportional insights.</p>
              
              <p><b>Purpose:</b><br>
              This tool helps in understanding how player demographics vary across teams 
              and positions, revealing trends such as the concentration of younger players 
              in specific positions or age groups.</p>
            ")
          )
        )
      )
    )
  )
)
server <- function(input, output) {
  
  filtered_data <- reactive({
    data <- Player_database %>%
      filter(Age >= input$age_range[1],
             Age <= input$age_range[2])
    
    if (input$position_filter != "All") {
      data <- data %>% filter(Pos == input$position_filter)
    }
    if (input$team_filter != "All") {
      data <- data %>% filter(Team == input$team_filter)
    }
    data
  })
  
  output$age_position_plot <- renderPlotly({
    data <- filtered_data()
    agg_data <- data %>%
      group_by(Age, Pos) %>%
      summarise(n = n(), .groups = "drop")
    
    if (input$metric == "percent") {
      agg_data <- agg_data %>%
        group_by(Age) %>%
        mutate(percentage = 100 * n / sum(n)) %>%
        ungroup()
      
      p <- ggplot(
        agg_data,
        aes(
          x = factor(Age),
          y = percentage,
          fill = Pos,
          text = paste(
            "Age:", Age,
            "<br>Position:", Pos,
            "<br>Percentage:", sprintf('%.1f%%', percentage)
          )
        )
      ) +
        geom_bar(stat = "identity", position = "stack",
                 color = "white", alpha = 0.9) +
        scale_fill_brewer(palette = "Set2") +
        labs(
          x = "Age",
          y = "Percentage of Players",
          fill = "Position"
        ) +
        theme_minimal(base_family = "Poppins") +
        theme(
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 13, face = "bold"),
          panel.grid.major = element_line(color = "gray90"),
          legend.position = "bottom"
        )
    } else {
      p <- ggplot(
        agg_data,
        aes(
          x = factor(Age),
          y = n,
          fill = Pos,
          text = paste(
            "Age:", Age,
            "<br>Position:", Pos,
            "<br>Players:", n
          )
        )
      ) +
        geom_bar(stat = "identity", position = "dodge",
                 color = "white", alpha = 0.9) +
        scale_fill_brewer(palette = "Set2") +
        labs(
          x = "Age",
          y = "Number of Players",
          fill = "Position"
        ) +
        theme_minimal(base_family = "Poppins") +
        theme(
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 13, face = "bold"),
          panel.grid.major = element_line(color = "gray90"),
          legend.position = "bottom"
        )
    }
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(font = list(family = "Poppins", size = 12)),
        legend = list(orientation = "h", x = 0.3, y = -0.2)
      )
  })
}
shinyApp(ui, server)