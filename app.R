#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(sas7bdat)
library(boxr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)
library(DT)

# Data sources
icoc = read.sas7bdat("icoc0.sas7bdat")
icoc$Date = as.Date(parse_date_time(paste(icoc$year, icoc$month, "-"), "ym"))
agg_icc = icoc %>%
  group_by(year, month) %>%
  summarise(agg_icc = weighted.mean(IRR_FCF, mktvalue)) %>%
  mutate(Date = as.Date(parse_date_time(paste(year, month, "-"), "ym"))) %>%
  tidyr::complete(Date = seq.Date(min(Date), max(Date), by = "month"))

# Define UI for the application
header <- dashboardHeader(
  title = "Implied Cost of Capital"#,
  #enable_rightsidebar = TRUE,
  #rightSidebarIcon = "filter"
)

sidebar <- dashboardSidebar(
  sidebarMenu(id = "menuChoice",
              menuItem("Aggregate", tabName = "Aggregate", icon = icon("globe")),
              menuItem("Individual", tabName = "Individual", icon = icon("chart-line"))
  )
)

body = dashboardBody(
  tabItems(
    tabItem(tabName = "Aggregate",
            fluidRow(
              box(title = "Aggregate ICC of S&P 500",
                      #plotOutput("agg_icc_Plot"),
                      plotlyOutput("agg_icc_Plot"), 
                      width = 12,
                      collapsible = TRUE, 
                      solidHeader = TRUE,
                      status = "primary"
                      ),
              box(title = "Aggregate ICC of S&P 500 Table",
                      dataTableOutput('agg_icc_table'), 
                      width = 8,
                      collapsible = TRUE, 
                      solidHeader = TRUE,
                      status = "warning"
                      ),
              box(downloadButton("download_agg_icc", "Download Data"), width = 4)
            )),
    tabItem(tabName = "Individual",
            fluidRow(
              box(title = textOutput("ind_com_plot_name"),
                      #plotOutput("ind_icc_Plot"),
                      plotlyOutput("ind_icc_Plot"),
                      width = 12,
                      collapsible = TRUE, 
                      solidHeader = TRUE,
                      status = "primary"
              ),
              box(title = textOutput("ind_com_table_name"),
                      dataTableOutput('ind_icc_table'), 
                      width = 8,
                      collapsible = TRUE, 
                      solidHeader = TRUE,
                      status = "warning"
              ),
              box(downloadButton("download_ind_icc", "Download Data"), width = 4)
            )
            )
  )
)

rightsidebar = dashboardControlbar(
  skin = "dark",
  controlbarMenu(
    id = 'menu',
    controlbarItem(
      "Individual Company",
      #icon = "chart-line",
      active = TRUE,
      selectInput(
        "ind_com",
        "Company Selected",
        ""
      )
    ),
    controlbarItem(
      "Timeframe",
      #icon = "clock",
      numericInput("min_date", h3("Min Year"), value = year(min(agg_icc$Date)), min = year(min(agg_icc$Date)), 
                   max = year(max(agg_icc$Date))),
      numericInput("max_date", h3("Max Year"), value = year(max(agg_icc$Date)), min = year(min(agg_icc$Date)), 
                   max = year(max(agg_icc$Date)))
    ) 
  ),
  title = "Company and Timeframe Selection"
)

ui <- dashboardPage(header, sidebar, body, rightsidebar)

#ui <- dashboardPagePlus("Implied Cost of Capital Calculations",
#   tabPanel("Aggregate ICC",
#            plotOutput("agg_icc_Plot")),
#   tabPanel("Individual ICC")
#   
#)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   agg_icc_data <- reactive({
     agg_icc %>%
       filter(year >= input$min_date & year <= input$max_date)
   })
   icoc_data <- reactive({
     icoc %>%
       filter(year >= input$min_date & year <= input$max_date)
   })
   
   ind_com_choices <- reactive({
     sort(unique(icoc_data()$cname))
   })
   
   output$ind_com_plot_name <- renderText({
     paste("ICC of", input$ind_com)
   })
   
   output$ind_com_table_name <- renderText({
     paste("ICC of", input$ind_com, "Table")
   })
   
   observe({
     updateSelectInput(session, "ind_com",
                       choices = ind_com_choices()
     )})
   
   ind_icc_data <- reactive({
     icoc_data() %>%
       filter(cname == input$ind_com)
   })
  
   
   #output$agg_icc_Plot <- renderPlot({
   output$agg_icc_Plot <- renderPlotly({
     plot_ly(agg_icc_data(), x=~Date, y=~agg_icc, type = "scatter", mode = "lines", color = I('dark blue')) %>%
       layout(yaxis = list(title = "Aggregate ICC"))
     #ggplot(data = agg_icc, aes(x = Date, y = agg_icc)) +
     #  geom_line(color = "blue") +
     #  geom_point() +
     #  labs(y = "Aggregate ICC")
   })
   
   output$agg_icc_table <- renderDataTable(DT::datatable(agg_icc_data() %>%
                                             select(-Date) %>%
                                             dplyr::rename("Aggregate ICC" = "agg_icc"),
                                             extensions=c('Scroller'),
                                             options = list(dom = 'Bfrtip',
                                                            scrollY = 500,
                                                            scroller = TRUE,
                                                            scrollX=TRUE
                                             )))
   
   output$download_agg_icc <- downloadHandler(
     filename = function(){"agg_icc.csv"},
     content = function(file) {
       write.csv(agg_icc, file, row.names = FALSE)
     }
   )
   
   #output$ind_icc_Plot <- renderPlot(
   output$ind_icc_Plot <- renderPlotly(
     plot_ly(ind_icc_data(), x=~Date, y=~IRR_FCF, type = "scatter", mode = "lines", color = I('dark green')) %>%
       layout(yaxis = list(title = "Implied Cost of Capital"))
     #ggplot(data = ind_icc_data(), aes(x = Date, y = IRR_FCF)) +
      # geom_line(color = "blue") +
      # geom_point() +
      # labs(y = "ICC")
   )
   
   output$ind_icc_table <- renderDataTable(DT::datatable(ind_icc_data() %>%
                                             select(datadate, gvkey, cusip, cname, ticker, year, month, IRR_FCF),
                                             extensions=c('Scroller'),
                                             options = list(dom = 'Bfrtip',
                                                            scrollY = 500,
                                                            scroller = TRUE,
                                                            scrollX=TRUE
                                             )))
   
   output$download_ind_icc <- downloadHandler(
     filename = function(){paste(input$ind_com, ".csv", sep = "")},
     content = function(file) {
       write.csv(ind_icc_data()  %>%
                   select(datadate, gvkey, cusip, cname, ticker, year, month, IRR_FCF), file, row.names = FALSE)
     }
   )
}

# Run the application 
shinyApp(ui = ui, server = server)

