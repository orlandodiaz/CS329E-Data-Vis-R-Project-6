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
  )

region_list <- as.list(regions$D, regions$R)

region_list <- append(list("All" = "All"), region_list)

region_list5 <- region_list


# The following queries are for the Barcharts -> High Discount Orders tab data.
# if(online0) {
  # Step 1:
  highQuantity <- query(
    data.world(propsfile = "www/.data.world"),
    dataset="jlee/s-17-dv-project-6", type="sql",
    query="
    SELECT distinct `Order ID`, sum(Profit) as sumProfit
    from globalshipments
    where Country in ('United States')
    group by `Order ID`
    having sum(Profit) >= 2000"
  ) # %>% View()
  # View(highQuantity )
  
  # Step 2
  highQuantityProducts <- query(
    data.world(propsfile = "www/.data.world"),
    dataset="jlee/s-17-dv-project-6", type="sql",
    query="
    SELECT distinct `Customer Name`, City, State, `Order ID`
    from globalshipments
    where Country in ('United States')
    and `Order ID` in
    (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    order by `Order ID`",
    queryParameters = highQuantity$"Order ID"
  ) # %>% View()
  # View(highQuantityProducts)
  
  # Step 3
  stateAbreviations <- query(
    data.world(propsfile = "www/.data.world"),
    dataset="cannata/superstoreorders", type="sql",
    query="SELECT distinct name as State, abbreviation as Abbreviation
    FROM markmarkoh.`us-state-table`.`state_table.csv/state_table`
    where name in
    (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    order by name",
    queryParameters = highQuantityProducts$State
  ) # %>% View()
  # View(stateAbreviations )
  
  # Step 4
  highQuantityProducts2 <- inner_join(highQuantityProducts,
                                      stateAbreviations, by="State") # %>% View()
  # View(highDiscountCustomers2)
  
  # Step 5
  longLat <- query(
    data.world(propsfile = "www/.data.world"),
    dataset="cannata/superstoreorders", type="sql",
    query="SELECT distinct NAME as City, STATE as Abbreviation,
    LATITUDE AS Latitude,
    LONGITUDE AS Longitude
    FROM bryon.`dhs-city-location-example`.`towns.csv/towns`
    where NAME in
    (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    order by NAME",
    queryParameters = highQuantityProducts$City
  ) # %>% View()
  # View(longLat)
  
  # Step 6
  highQuantityProducts2LongLat <- 
    inner_join(highQuantityProducts2, longLat, by = c("City", "Abbreviation")) 
  # View(highDiscountCustomers2LongLat)
  
  # Step 7
  Quantity <- 
    inner_join(highQuantityProducts2LongLat, highQuantity, by="Order ID") # %>% View()
  # View(discounts)
# } else {
#   # Just faking one data point for now.
#   Customer_Name = 'Wesley Tate'
#   City = 'Chicago'
#   State = 'Illinois'
#   Order_Id = 48452
#   Abbreviation = 'IL'
#   Latitude =  41.85003
#   Longitude = -87.65005
#   sumDiscount = 0.34
#   sumSales = 7124
#   discounts <- data.frame(Customer_Name, City, State, Order_Id, Abbreviation, Latitude, Longitude, sumDiscount, sumSales)
# }
# View (Quantity)


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
        query="select `Ship Mode`, Region, sum(`Discount`) as sum_discount 
        from globalshipments
        where Country in ('United States') and (? = 'All' or Region in (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?))
        group by `Ship Mode`, Region",
        queryParameters = region_list
      ) # %>% View()
    }
    else {
      print("Getting from csv")
      file_path = "www/globalshipments.csv"
      df <- readr::read_csv(file_path)
      tdf = df %>% dplyr::filter(Country == "United States") %>% dplyr::filter(Region %in% input$selectedRegions | input$selectedRegions == "All") %>%
        dplyr::group_by(Region, `Sub-Category`) %>% 
        dplyr::summarize(sum_discount = sum(`Discount`)) # %>% View()
    }
    # The following two lines mimic what can be done with Analytic SQL. Analytic SQL does not currently work in data.world.
    tdf2 = tdf %>% group_by(Region) %>% summarize(window_avg_discount = mean(sum_discount))
    dplyr::left_join(tdf, tdf2, by = "Region")
  })
  output$barchartData1 <- renderDataTable({DT::datatable(dfbc1(),
                                                         rownames = FALSE,
                                                         extensions = list(Responsive = TRUE, FixedHeader = TRUE) )
  })
    output$barchartData2 <- renderDataTable({DT::datatable(Quantity,
                                                           rownames = FALSE,
                                                           extensions = list(Responsive = TRUE, FixedHeader = TRUE) )
  })
  output$barchartPlot1 <- renderPlot({ggplot(dfbc1(), aes(x=`Ship Mode`, y=sum_discount)) +
      scale_y_continuous(labels = scales::comma) + # no scientific notation
      theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=12, hjust=0.5)) +
      geom_bar(stat = "identity") + 
      facet_wrap(~Region, ncol=1) + 
      coord_flip() + 
      # Add sum_sales, and (sum_sales - window_avg_sales) label.
      geom_text(mapping=aes(x=`Ship Mode`, y=sum_discount, label=round(sum_discount)),colour="black", hjust=-.5) +
      geom_text(mapping=aes(x=`Ship Mode`, y=sum_discount, label=round(sum_discount - window_avg_discount)),colour="blue", hjust=-2) +
      # Add reference line with a label.
      geom_hline(aes(yintercept = round(window_avg_discount)), color="red") +
      geom_text(aes( -1, window_avg_discount, label = window_avg_discount, vjust = -.5, hjust = -.25), color="red")
  })
  
  output$barchartMap1 <- renderLeaflet({leaflet(width = 400, height = 800) %>% 
      setView(lng = -98.35, lat = 39.5, zoom = 4) %>% 
      addTiles() %>% 
      addProviderTiles("MapQuestOpen.Aerial") %>%
      addMarkers(lng = Quantity$Longitude,
                 lat = Quantity$Latitude,
                 options = markerOptions(draggable = TRUE, riseOnHover = TRUE),
                 popup = as.character(paste(Quantity$"Customer Name", 
                                            ", ", Quantity$City,
                                            ", ", Quantity$State,
                                            " Profit: ",",", formatC(as.numeric(Quantity$sumProfit), format="f", digits=2, big.mark=","))) )
  })
  
  })
