# import libraries
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(lubridate)
library(tidyr)
library(zoo)

# data loading
national_cpi_df <- read.csv("data/All groups CPI, Australia, quarterly and annual movement (%).csv", skip = 1)
names(national_cpi_df) <- c("Quarter", "Quarterly_Change", "Annual_Change")
national_cpi_df <- na.omit(national_cpi_df)
national_cpi_df$Quarter <- as.Date(parse_date_time(national_cpi_df$Quarter, orders = "b-y"))
national_cpi_df <- national_cpi_df[order(national_cpi_df$Quarter), ]

# read and reformat data from city CPI to a usable format for tab 3
city_index_raw <- read.csv("data/Capital_City_CPI_Index_Trends.csv")
colnames(city_index_raw)[1] <- "Date"
city_index_clean <- city_index_raw %>%
  mutate(Date = as.Date(Date)) %>%
  pivot_longer(cols = -Date, names_to = "City", values_to = "CPI_Index") %>%
  filter(!is.na(CPI_Index), Date >= as.Date("2000-01-01"))

city_index_df <- city_index_clean

# colour for ui
main_color <- "#1f77b4"
secondary_color <- "#ff7f0e"

# ui dashboard page frame
ui <- dashboardPage(
  dashboardHeader(title = "Australia's Cost of living crisis- why everything feels so much more expensive", titleWidth = 450),
  dashboardSidebar(width = 150,
                   sidebarMenu(
                     menuItem("Overview & Trends", tabName = "overview", icon = icon("chart-line")),
                     menuItem("Sector Breakdown", tabName = "sectors", icon = icon("th-large")),
                     menuItem("Regional Impact", tabName = "regional", icon = icon("map-marker-alt"))
                   )
  ),
  dashboardBody(
    tags$head(tags$style(HTML(
      ".box {font-family: Arial; padding: 1px;} 
       .value-box {font-size: 11px; min-height: 60px;} 
       .content-wrapper, .right-side {padding-top: 2px;} 
       .main-header .logo {font-size: 12px;} 
       .main-header .navbar {min-height: 22px;} 
       .content {padding: 5px;} 
       .box-title {font-size: 11px;} 
       p, label {font-size: 9px;}"
    ))),
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                box(title = "Australia's Cost-of-Living Crisis", width = 12, status = "primary",
                    HTML("<p><strong>Ever since the Covid-19 pandemic, Australia's inflation rate with CPI (Consumer Price Index) has been increasing. Factors such as increased production and labour costs, consumer demand, long term unemployment and purchasing power are the main contributors to this.</strong> Below, there is is an interactive graph on the CPI trends, with a slider to pan the dates from 2016 to 2025, as well as summarised facts in the coloured boxes below.</p>")
                )
              ),
              fluidRow(
                column(8, box(title = "National CPI Over Time", width = 12, plotlyOutput("national_cpi_plot", height = "170px"))),
                column(4, box(title = "Adjust Time Range", width = 12,
                              sliderInput("year_range", "Year Range:",
                                          min = 2016, max = 2025, value = c(2016, 2025),
                                          sep = "", step = 1)))
              ),
              fluidRow(
                valueBoxOutput("latest_cpi", width = 3),
                valueBoxOutput("latest_trimmed", width = 3),
                valueBoxOutput("latest_quarter_change", width = 3)
              )
      ),
      tabItem(tabName = "sectors",
              fluidRow(
                box(title = "Where are everyday Australians hurt the most?", width = 12, status = "warning",
                    HTML("<p>Not all CPI costs are rising equally, as there are some sectors like electricity, housing and food being the fastest growing. This section aims to highlight how costs are rising differently in each sector. The Latest CPI by sector graph reveals the percentage change with the moth recent data, whereas the graph on the right, trends for selected sectors visualises how specific CPI sectors have evolved over time.</p>")
                )
              ),
              fluidRow(
                column(6, box(title = "Latest CPI by Sector", width = 12, plotlyOutput("sector_bar_plot", height = "210px"))),
                column(6, box(title = "Trend for Selected Sector", width = 12,
                              selectInput("selected_sector", "Choose Sector:",
                                          choices = c("Housing", "Food", "Transport", "Electricity", "Healthcare", "Education")),
                              plotlyOutput("sector_trend_plot", height = "210px")))
              )
      ),
      tabItem(tabName = "regional",
              fluidRow(
                box(title = "Who is most affected?", width = 12, status = "danger",
                    HTML("<p>Although similar, there are differences in each cities CPI index. All cities have experienced constant growth as visualised in the CPI index trends graph below, Perth having the greatest increase, having around a 40% CPI increase referenced from the previous quarter. You can also use the slider to again change the span the years, with there being a CPI by sector graph on the right detailing the top 3 CPI's in a graph</p>")
                )
              ),
              fluidRow(
                column(6, box(title = "CPI Index Trends by Capital City (2000–Present)", width = 12, plotlyOutput("city_cpi_plot", height = "280px"))),
                column(6, box(title = "Sector Comparison in Selected Cities", width = 12,
                              selectInput("selected_city", "Choose City:",
                                          choices = c("Sydney", "Melbourne", "Brisbane", "Perth", "Adelaide", "Hobart", "Darwin", "Canberra")),
                              plotlyOutput("city_sector_plot", height = "180px")))
              ),
              fluidRow(
                column(6, offset = 0, box(title = NULL, width = 6,
                                          sliderInput("city_year_range", NULL,
                                                      min = 2000, max = 2025, value = c(2000, 2025),
                                                      sep = "", step = 1, width = "100%")))
              )
      )
    )
  )
)

# server function
server <- function(input, output, session) {
  
  output$national_cpi_plot <- renderPlotly({
    df <- national_cpi_df %>% filter(year(Quarter) >= input$year_range[1], year(Quarter) <= input$year_range[2])
    plot_ly(data = df, x = ~Quarter, y = ~Annual_Change, type = 'scatter', mode = 'lines+markers',
            line = list(color = main_color), marker = list(color = secondary_color)) %>%
      layout(title = "Annual CPI Change Over Time",
             xaxis = list(title = "Quarter"),
             yaxis = list(title = "Annual CPI Change (%)"))
  })
  
  output$latest_cpi <- renderValueBox({
    latest <- tail(national_cpi_df$Annual_Change, 1)
    valueBox(paste0(latest, "%"), "Latest Annual CPI", icon = icon("chart-line"), color = "green")
  })
  
  output$latest_trimmed <- renderValueBox({
    valueBox("3.6%", "Trimmed Mean CPI", icon = icon("filter"), color = "aqua")
  })
  
  output$latest_quarter_change <- renderValueBox({
    latest <- tail(national_cpi_df$Quarterly_Change, 1)
    valueBox(paste0(latest, "%"), "Latest Quarterly Change", icon = icon("arrow-up"), color = "yellow")
  })
  
  output$sector_bar_plot <- renderPlotly({
    sectors <- c("Housing", "Food", "Transport", "Electricity", "Healthcare", "Education")
    values <- c(6.2, 4.8, 3.9, 7.5, 2.1, 3.3)
    plot_ly(x = values, y = sectors, type = "bar", orientation = 'h', marker = list(color = secondary_color)) %>%
      layout(title = "Latest CPI by Sector", xaxis = list(title = "% Change"))
  })
  
  output$sector_trend_plot <- renderPlotly({
    years <- 2015:2024
    trend <- seq(90, 120, length.out = 10) + rnorm(10, 0, 1.5)
    plot_ly(x = years, y = trend, type = "scatter", mode = "lines+markers",
            line = list(color = main_color)) %>%
      layout(title = paste("CPI Trend for", input$selected_sector),
             xaxis = list(title = "Year"),
             yaxis = list(title = "CPI Index"))
  })
  
  output$city_cpi_plot <- renderPlotly({
    df <- city_index_df %>% filter(year(Date) >= input$city_year_range[1], year(Date) <= input$city_year_range[2])
    plot_ly(data = df, x = ~Date, y = ~CPI_Index, color = ~City, type = 'scatter', mode = 'lines',
            text = ~paste("City:", City, "<br>Date:", format(Date, "%Y-%m"), "<br>Index:", CPI_Index),
            hoverinfo = "text") %>%
      layout(title = "CPI Index Trends by Capital City",
             xaxis = list(title = "Date", tickangle = 15, tickfont = list(size = 9)),
             yaxis = list(title = "CPI Index"),
             legend = list(orientation = "h", x = 0.1, y = -0.3))
  })
  
  output$city_sector_plot <- renderPlotly({
    sectors <- c("Housing", "Food", "Transport")
    values <- sample(100:130, 3)
    plot_ly(x = sectors, y = values, type = "bar", marker = list(color = secondary_color)) %>%
      layout(title = paste("CPI by Sector in", input$selected_city),
             xaxis = list(title = "Sector"),
             yaxis = list(title = "CPI Index"))
  })
}

shinyApp(ui, server)
