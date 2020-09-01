#
# This is a template Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Author: Owen Bezick

# The source command allows one to utilize functions and data stored in a separate R script.
source("libraries.R", local = TRUE)

# Global variables (read at runtime)
iris_data <- iris

# Define UI for application 
ui <- dashboardPage(
  dashboardHeader(title = "Example Shiny Application" )
  , dashboardSidebar(
    actionBttn(inputId = "refreshData", label = "Refresh Data", icon = icon("refresh"))
  )
  , dashboardBody(
    fluidRow(
      box(width = 12, status = "primary", title = "Graphics"
          , column(width = 4
                   , echarts4rOutput("line_area")
          )
          , column(width = 4
                   , echarts4rOutput("scatter")
          )
          , column(width = 4
                   , echarts4rOutput("radial")
          )
      )
    )
    , fluidRow(
      box(width = 12, status = "primary", title = "Dataset"
          , DTOutput("dt_output")
      )
    )
  )
)


# Define server logic 
server <- function(input, output) {
  
  # Initial Data
  df <- data.frame(
    x = seq(50),
    y = rnorm(50, 10, 3),
    z = rnorm(50, 11, 2),
    w = rnorm(50, 9, 2)
  )
  # Set as reactive
  r <- reactiveValues(df = df)
  
  # Observe button press to refresh data
  observeEvent(input$refreshData, {
    r$df <- data.frame(
      x = seq(50),
      y = rnorm(50, 10, 3),
      z = rnorm(50, 11, 2),
      w = rnorm(50, 9, 2)
    )
  })
  
  # Graph one output 
  output$line_area <- renderEcharts4r({
    df <- r$df
    df %>% 
      e_charts(x) %>% 
      e_line(z) %>% 
      e_area(w) %>% 
      e_title("Line and Area Chart") %>%
      e_tooltip() %>%
      e_theme('westeros')
  })
  
  # Graph two output
  output$scatter <- renderEcharts4r({
    df <- r$df
    df %>% 
      head(10) %>% 
      e_charts(x) %>% 
      e_effect_scatter(y, z) %>% 
      e_visual_map(z) %>% # scale color
      e_legend(FALSE)  %>% # hide legend
      e_title("Scatter with Scale & Effect")%>%
      e_tooltip()%>%
      e_theme('westeros')
  })
  
  # Graph three output
  output$radial <- renderEcharts4r({
    df <- r$df
    df %>% 
      head(10) %>% 
      e_charts(x) %>% 
      e_polar() %>% 
      e_angle_axis() %>% 
      e_radius_axis(x) %>% 
      e_bar(y, coord_system = "polar") %>% 
      e_scatter(z, coord_system = "polar") %>% 
      e_title("Radial Chart")%>%
      e_tooltip()%>%
      e_theme('westeros')
  })
  
  # DT output
  output$dt_output <- renderDT({
    df <- r$df
    datatable(df, rownames = F)
  })
  
} # End of server

# Run the application 
shinyApp(ui = ui, server = server)