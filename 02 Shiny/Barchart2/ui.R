#ui.R
require(shiny)
require(shinydashboard)

dashboardPage(
  dashboardHeader(
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Barcharts, Table Calculations", tabName = "barchart", icon = icon("dashboard"))
    )
  ),
  dashboardBody(    
    tabItems(
      # Begin Barchart tab content.
      tabItem(tabName = "barchart",
              tabsetPanel(
                tabPanel("Data",
                         radioButtons("rb2", "Get data from:",
                                      c("SQL" = "SQL"), inline=T),
                         uiOutput("regions2"), # See http://shiny.rstudio.com/gallery/dynamic-ui.html
                         actionButton(inputId = "click2",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         'Here is data for the "Barchart with Table Calculation" tab',
                         hr(),
                         DT::dataTableOutput("barchartData1")
                ),
                tabPanel("Barchart with Table Calculation", "Black = Sum of Shipping Cost per Sub-Category, Red = Average Sum of Shipping Cost per Region, and  Blue = (Sum of Shipping Cost per Sub-Category - Average Sum of Shipping Cost per Region)", plotOutput("barchartPlot1", height=1500))
              )
      )
      # End Barchart tab content.
    )
  )
)
