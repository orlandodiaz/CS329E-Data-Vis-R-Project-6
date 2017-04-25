# server.R
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(data.world)
require(readr)
require(DT)
require(leaflet)
require(plotly)
require(lubridate)

online0 = TRUE

# The following query is for the select list in the Boxplots -> Simple Boxplot tab, and Barcharts -> Barchart with Table Calculation tab.
# if(online0) {
  regions = query(
    data.world(propsfile = "www/.data.world"),
    dataset="jlee/s-17-dv-project-6", type="sql",
    query="select distinct Region as D, Region as R
    from globalshipments
    where Country in ('United States')
    order by 1"
  ) # %>% View()
# } else {
#   print("Getting Regions from csv")
#   file_path = "www/globalshipments.csv"
#   df <- readr::read_csv(file_path)
#   tdf1 = df %>% dplyr::distinct(Region) %>% arrange(Region) %>% dplyr::rename(D = Region)
#   tdf2 = df %>% dplyr::distinct(Region) %>% arrange(Region) %>% dplyr::rename(R = Region)
#   regions = bind_cols(tdf1, tdf2)
# }

region_list <- as.list(regions$D, regions$R)

region_list <- append(list("All" = "All"), region_list)

region_list5 <- region_list


shinyServer(function(input, output) { 
  # These widgets are for the Barcharts tab.
  online2 = reactive({input$rb2})
  output$regions2 <- renderUI({selectInput("selectedRegions", "Choose Regions:", region_list, multiple = TRUE, selected='All') })
  
  # Begin Barchart Tab ------------------------------------------------------------------
  dfbc1 <- eventReactive(input$click2, {
    if(input$selectedRegions == 'All') region_list <- input$selectedRegions
    else region_list <- append(list("Skip" = "Skip"), input$selectedRegions)
    if(online2() == "SQL") {
      print("Getting from data.world")
      tdf = query(
        data.world(propsfile = "www/.data.world"),
        dataset="jlee/s-17-dv-project-6", type="sql",
        query="select `Segment`, Region, sum(`Sales`) as sum_sales 
        from globalshipments
        where Country in ('United States') and (? = 'All' or Region in (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?))
        group by `Segment`, Region",
        queryParameters = region_list
      ) # %>% View()
    }
    else {
      print("Getting from csv")
      file_path = "www/globalshipments.csv"
      df <- readr::read_csv(file_path)
      tdf = df %>% dplyr::filter(Country == "United States") %>% dplyr::filter(Region %in% input$selectedRegions | input$selectedRegions == "All") %>%
        dplyr::group_by(Region, `Segment`) %>% 
        dplyr::summarize(sum_sales = sum(`Sales`)) # %>% View()
    }
    # The following two lines mimic what can be done with Analytic SQL. Analytic SQL does not currently work in data.world.
    tdf2 = tdf %>% group_by(Region) %>% summarize(window_avg_sales = mean(sum_sales))
    dplyr::left_join(tdf, tdf2, by = "Region")
    # Analytic SQL would look something like this:
    # select Category, Region, sum_sales, avg(sum_sales) 
    # OVER (PARTITION BY Category ) as window_avg_sales
    # from (select Category, Region, sum(Sales) sum_sales
    #       from SuperStoreOrders
    #      group by Category, Region)
  })
  output$barchartData1 <- renderDataTable({DT::datatable(dfbc1(),
                                                         rownames = FALSE,
                                                         extensions = list(Responsive = TRUE, FixedHeader = TRUE) )
  })
  output$barchartPlot1 <- renderPlot({ggplot(dfbc1(), aes(x=`Segment`, y=sum_sales)) +
      scale_y_continuous(labels = scales::comma) + # no scientific notation
      theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=12, hjust=0.5)) +
      geom_bar(stat = "identity") + 
      facet_wrap(~Region, ncol=1) + 
      coord_flip() + 
      # Add sum_sales, and (sum_sales - window_avg_sales) label.
      geom_text(mapping=aes(x=`Segment`, y=sum_sales, label=round(sum_sales)),colour="black", hjust=-.5) +
      geom_text(mapping=aes(x=`Segment`, y=sum_sales, label=round(sum_sales - window_avg_sales)),colour="blue", hjust=-2) +
      # Add reference line with a label.
      geom_hline(aes(yintercept = round(window_avg_sales)), color="red") +
      geom_text(aes( -1, window_avg_sales, label = window_avg_sales, vjust = -.5, hjust = -.25), color="red")
  })
  
  })
