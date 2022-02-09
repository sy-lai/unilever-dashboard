server <- function(input, output, session) {
  # Q1 Chart ----
  output$Q1view <- renderPlot({
    if (input$years == 'ALL'){
      sql <- paste0(
        'SELECT products.prod_name, sum(unit_sold)::numeric as sales',
        ' FROM products ',
        'join txns on txns.prod_id=products.prod_id ' ,
        ' Group by 1',
        ' Order by sales desc',
        ' LIMIT 5;'
      )
      z <- dbGetQuery(con, sql)
      ggplot(z, aes(x=reorder(prod_name,sales), sales/1000000)) +
        geom_bar(stat="identity",fill = brewer.pal(7, "Set1")[1], width = 0.6)+
        labs(x="product name", y="Sales (million)",title = "Top 5 Sales Product")+
        coord_flip()
    }
    else if (input$years != 'ALL'){
      sql <- paste0(
        'SELECT products.prod_name, sum(unit_sold)::numeric as sales',
        ' FROM products ',
        'join txns on txns.prod_id=products.prod_id ' ,
        'WHERE date between \'',input$years,'-01-01\'',
        ' and \'',input$years,'-12-31\'',
        ' Group by 1',
        ' Order by sales desc',
        ' LIMIT 5;'
      )
      z <- dbGetQuery(con, sql)
      ggplot(z, aes(x=reorder(prod_name,sales), sales/1000000)) +
        geom_bar(stat="identity",fill = "#03a6ff", width = 0.6)+
        labs(x="product name", y="Sales (million)",title = "Top 5 Sales Product")+
        coord_flip()
    }
  }
  )
  
  # _plays title----
  output$ProductTitle <- renderText(paste0(" "))
  # _render table from SQL run----
  output$Q1table <- renderDataTable({
    if (input$years == 'ALL'){
      sql <- paste0(
        'SELECT products.prod_name, sum(unit_sold)::numeric as sales',
        ' FROM products ',
        'join txns on txns.prod_id=products.prod_id ' ,
        ' Group by 1',
        ' Order by sales desc'
      )
      z <- dbGetQuery(con, sql)
    }
    else if (input$years != 'ALL'){
      sql <- paste0(
        'SELECT products.prod_name, sum(unit_sold)::numeric as sales',
        ' FROM products ',
        'join txns on txns.prod_id=products.prod_id ',
        'WHERE date between \'',input$years,'-01-01\'',
        ' and \'',input$years,'-12-31\'',
        ' Group by 1',
        ' Order by sales desc'
      )
      z <- dbGetQuery(con, sql)
    }
  }, options = list(aLengthMenu = c(5, 30, 50), iDisplayLength = 20)
  )
    
# Q2 Chart----
  output$Q2retailer <- renderPlot(
    {
      sql <- paste0(
        'Select B.cust_name, case when date between \'2017-01-01\'::date and \'2017-12-31\'::date then \'2017\' ',
        'when date between \'2018-01-01\'::date and \'2018-12-31\'::date then \'2018\' ',
        'when date between \'2019-01-01\'::date and \'2019-12-31\'::date then \'2019\' ',
        'else \'others\' end as yr, ',
        'sum(unit_sold) as num_prods_sold, cast(sum(unit_sold*unit_price) as decimal(20,0)) as total_sales ',
        'From (Select * from txns Where unit_sold > 0) A ',
        'Join custs B On A.cust_id = B.cust_id ', 
        'Join products C On A.prod_id = C.prod_id ',
        'Join price D on A.prod_id = D.prod_id ',
        'WHERE date between \'',input$years3,'-01-01\' and \'',input$years3,'-12-31\' ',
        'Group by cust_name, case when date between \'2017-01-01\'::date and \'2017-12-31\'::date then \'2017\' ',
        'when date between \'2018-01-01\'::date and \'2018-12-31\'::date then \'2018\' ',
        'when date between \'2019-01-01\'::date and \'2019-12-31\'::date then \'2019\' ',
        'else \'others\' end ',
        'Order by yr, total_sales desc;'
      )
      z <- dbGetQuery(con, sql)
      z %>%
        mutate(cust_name = fct_reorder(cust_name, desc(total_sales))) %>%
        ggplot(aes(x=cust_name, y=total_sales)) +
        geom_bar(stat='identity', fill='steelblue') +
        labs(x = 'Customer', y = 'Total Sales') +
        theme_minimal()
      
    }
  )
    
    # render table from SQL run
    output$tbl <- renderDataTable(
      datatable(
        data = sq$out,
        rownames = FALSE,
        options = list(scrollX = TRUE)
      )
    )
    
    ### Q3: Bar Chart ----
    # y = units sold, x = 2017, 2018, 2019
    
    # _small image----
    output$custImg <- renderUI({
      img(src = paste0(str_to_lower(input$custs), '.png'), # paste0(str_to_lower(input$custs), '.png')
          height = '100px',
          contentType = 'image/png')
    })
    
    # _chart of units sold by month for X retailer and Y Product----
    output$units_sold_Chrt <- renderPlot(
      if (input$custs != '' & 
          input$prods != '' & 
          input$months_units_sold != '') {
        sql_units <- paste0(
          'SELECT date_part(\'year\', date) yr, sum(unit_sold)::numeric as units_sold ',
          'FROM products ',
          'JOIN txns t USING (prod_id) ',
          'JOIN custs c USING (cust_id) ',
          'where cust_name ilike \'', input$custs, '%\' ',
          'and prod_name ilike \'', str_to_upper(input$prods), '%\' ',
          'and date_part(\'month\', date)::integer = ', input$months_units_sold, ' ', 
          'GROUP BY 1;'
        )
        df <- dbGetQuery(
          conn = con, 
          statement = sql_units
        )
        if (nrow(df) > 0) {       
          ggplot(df, aes(x = yr, y = units_sold)) +
            geom_segment(aes(x = yr, xend = yr,
                             y = 0, yend = units_sold), size = 1.5) +
            geom_point(size = 7, color = "red", 
                       fill = alpha("orange", 0.5), 
                       alpha = 0.7, shape = 21, stroke = 5) +
            geom_smooth(method = 'lm', formula = y~x, 
                        se = FALSE, fullrange = TRUE,
                        size = 1.5, color = 'green') +
            xlim(2017, 2020) +
            scale_y_continuous(labels = comma) +
            ggtitle(paste0('Prediction for the Month of ', 
                           input$months_units_sold, '/2020')) +
            labs(x = 'Year', y = 'Units') +
            theme_minimal(base_size = 16)}

      }
      
      
    )
  
  
  # Q4----
    #Fill in the spot
  output$Q4table <- renderTable({
    if (input$years4 == 'ALL' & input$render =='ALL'){
      d <- dbGetQuery(
        conn = con, 
        statement = paste0(
        'SELECT SUM(CASE WHEN unit_price < 10 THEN unit_sold ELSE 0 END)::integer as "0～10",
        SUM(CASE WHEN unit_price BETWEEN 10 AND 20 THEN unit_sold ELSE 0 END)::integer as "10～20",
        SUM(CASE WHEN unit_price BETWEEN 20 AND 30 THEN unit_sold ELSE 0 END)::integer as "20～30",
        SUM(CASE WHEN unit_price BETWEEN 30 AND 40 THEN unit_sold ELSE 0 END)::integer as "30～40",
        SUM(CASE WHEN unit_price BETWEEN 40 AND 50 THEN unit_sold ELSE 0 END)::integer as "40～50",
        SUM(CASE WHEN unit_price BETWEEN 50 AND 60 THEN unit_sold ELSE 0 END)::integer as "50～60",
        SUM(CASE WHEN unit_price BETWEEN 60 AND 70 THEN unit_sold ELSE 0 END)::integer as "60～70",
        SUM(CASE WHEN unit_price BETWEEN 70 AND 80 THEN unit_sold ELSE 0 END)::integer as "70～80",
        SUM(CASE WHEN unit_price BETWEEN 80 AND 90 THEN unit_sold ELSE 0 END)::integer as "80～90",
        SUM(CASE WHEN unit_price BETWEEN 90 AND 100 THEN unit_sold ELSE 0 END)::integer as "90～100",
        SUM(CASE WHEN unit_price > 100 THEN unit_sold ELSE 0 END)::integer as "100～"
        FROM products a
        JOIN txns b ON a.prod_id = b.prod_id
        JOIN price p on p.prod_id = b.prod_id
        JOIN custs c ON b.cust_id = c.cust_id'
        ))
      data1 <- t(data.frame(d,row.names = T))
      data1 <- data.frame(PriceRang=c('0~10','10~20','20~30','30~40','40~50','50~60','60~70','70~80','80~90','90~100','100~'),
                          Sales=data1[,1])
    }
    
    else if (input$years4 != 'ALL' & input$render == 'ALL'){
      d <- dbGetQuery(
        conn = con, 
        statement = paste0(
        'SELECT SUM(CASE WHEN unit_price < 10 THEN unit_sold ELSE 0 END)::integer as "0～10",
        SUM(CASE WHEN unit_price BETWEEN 10 AND 20 THEN unit_sold ELSE 0 END)::integer as "10～20",
        SUM(CASE WHEN unit_price BETWEEN 20 AND 30 THEN unit_sold ELSE 0 END)::integer as "20～30",
        SUM(CASE WHEN unit_price BETWEEN 30 AND 40 THEN unit_sold ELSE 0 END)::integer as "30～40",
        SUM(CASE WHEN unit_price BETWEEN 40 AND 50 THEN unit_sold ELSE 0 END)::integer as "40～50",
        SUM(CASE WHEN unit_price BETWEEN 50 AND 60 THEN unit_sold ELSE 0 END)::integer as "50～60",
        SUM(CASE WHEN unit_price BETWEEN 60 AND 70 THEN unit_sold ELSE 0 END)::integer as "60～70",
        SUM(CASE WHEN unit_price BETWEEN 70 AND 80 THEN unit_sold ELSE 0 END)::integer as "70～80",
        SUM(CASE WHEN unit_price BETWEEN 80 AND 90 THEN unit_sold ELSE 0 END)::integer as "80～90",
        SUM(CASE WHEN unit_price BETWEEN 90 AND 100 THEN unit_sold ELSE 0 END)::integer as "90～100",
        SUM(CASE WHEN unit_price > 100 THEN unit_sold ELSE 0 END)::integer as "100～"
        FROM products a
        JOIN txns b ON a.prod_id = b.prod_id
        JOIN price p on p.prod_id = b.prod_id
        WHERE date between \'',input$years4,'-01-01\'',
          'and \'',input$years4,'-12-31\''
        ))
      data1 <- t(data.frame(d,row.names = T))
      data1 <- data.frame(PriceRang=c('0~10','10~20','20~30','30~40','40~50','50~60','60~70','70~80','80~90','90~100','100~'),
                          Sales=data1[,1])
    }
    
    else if (input$years4 == 'ALL' & input$render != 'ALL'){
      d <- dbGetQuery(
        conn = con, 
        statement = paste0(
          'SELECT SUM(CASE WHEN unit_price < 10 THEN unit_sold ELSE 0 END)::integer as "0～10",
        SUM(CASE WHEN unit_price BETWEEN 10 AND 20 THEN unit_sold ELSE 0 END)::integer as "10～20",
        SUM(CASE WHEN unit_price BETWEEN 20 AND 30 THEN unit_sold ELSE 0 END)::integer as "20～30",
        SUM(CASE WHEN unit_price BETWEEN 30 AND 40 THEN unit_sold ELSE 0 END)::integer as "30～40",
        SUM(CASE WHEN unit_price BETWEEN 40 AND 50 THEN unit_sold ELSE 0 END)::integer as "40～50",
        SUM(CASE WHEN unit_price BETWEEN 50 AND 60 THEN unit_sold ELSE 0 END)::integer as "50～60",
        SUM(CASE WHEN unit_price BETWEEN 60 AND 70 THEN unit_sold ELSE 0 END)::integer as "60～70",
        SUM(CASE WHEN unit_price BETWEEN 70 AND 80 THEN unit_sold ELSE 0 END)::integer as "70～80",
        SUM(CASE WHEN unit_price BETWEEN 80 AND 90 THEN unit_sold ELSE 0 END)::integer as "80～90",
        SUM(CASE WHEN unit_price BETWEEN 90 AND 100 THEN unit_sold ELSE 0 END)::integer as "90～100",
        SUM(CASE WHEN unit_price > 100 THEN unit_sold ELSE 0 END)::integer as "100～"
        FROM products a
        JOIN txns b ON a.prod_id = b.prod_id
        JOIN price p on p.prod_id = b.prod_id
        JOIN custs c ON b.cust_id = c.cust_id
        WHERE cust_name = \'', input$render, '\''
        ))
      data1 <- t(data.frame(d,row.names = T))
      data1 <- data.frame(PriceRang=c('0~10','10~20','20~30','30~40','40~50','50~60','60~70','70~80','80~90','90~100','100~'),
                          Sales=data1[,1])
    }
    
    else if (input$years4 != 'ALL' & input$render != 'ALL'){
      d <- dbGetQuery(
        conn = con, 
        statement = paste0(
        'SELECT SUM(CASE WHEN unit_price < 10 THEN unit_sold ELSE 0 END)::integer as "0～10",
        SUM(CASE WHEN unit_price BETWEEN 10 AND 20 THEN unit_sold ELSE 0 END)::integer as "10～20",
        SUM(CASE WHEN unit_price BETWEEN 20 AND 30 THEN unit_sold ELSE 0 END)::integer as "20～30",
        SUM(CASE WHEN unit_price BETWEEN 30 AND 40 THEN unit_sold ELSE 0 END)::integer as "30～40",
        SUM(CASE WHEN unit_price BETWEEN 40 AND 50 THEN unit_sold ELSE 0 END)::integer as "40～50",
        SUM(CASE WHEN unit_price BETWEEN 50 AND 60 THEN unit_sold ELSE 0 END)::integer as "50～60",
        SUM(CASE WHEN unit_price BETWEEN 60 AND 70 THEN unit_sold ELSE 0 END)::integer as "60～70",
        SUM(CASE WHEN unit_price BETWEEN 70 AND 80 THEN unit_sold ELSE 0 END)::integer as "70～80",
        SUM(CASE WHEN unit_price BETWEEN 80 AND 90 THEN unit_sold ELSE 0 END)::integer as "80～90",
        SUM(CASE WHEN unit_price BETWEEN 90 AND 100 THEN unit_sold ELSE 0 END)::integer as "90～100",
        SUM(CASE WHEN unit_price > 100 THEN unit_sold ELSE 0 END)::integer as "100～"
        FROM products a
        JOIN txns b ON a.prod_id = b.prod_id
        JOIN price p on p.prod_id = b.prod_id
        JOIN custs c ON b.cust_id = c.cust_id
        WHERE date between \'',input$years4,'-01-01\'',
          'and \'',input$years4,'-12-31\'',
        'AND cust_name = \'', input$render, '\''
        ))
      data1 <- t(data.frame(d,row.names = T))
      data1 <- data.frame(PriceRang=c('0~10','10~20','20~30','30~40','40~50','50~60','60~70','70~80','80~90','90~100','100~'),
                          Sales=data1[,1])
    }
    }
) 
  
  output$Q4view <- renderPlot({
    if (input$years4 == 'ALL' & input$render == 'ALL'){
      d2 <- dbGetQuery(
        conn = con, 
        statement = paste0(
        'SELECT SUM(CASE WHEN unit_price < 10 THEN unit_sold ELSE 0 END)::integer as "0～10",
        SUM(CASE WHEN unit_price BETWEEN 10 AND 20 THEN unit_sold ELSE 0 END)::integer as "10～20",
        SUM(CASE WHEN unit_price BETWEEN 20 AND 30 THEN unit_sold ELSE 0 END)::integer as "20～30",
        SUM(CASE WHEN unit_price BETWEEN 30 AND 40 THEN unit_sold ELSE 0 END)::integer as "30～40",
        SUM(CASE WHEN unit_price BETWEEN 40 AND 50 THEN unit_sold ELSE 0 END)::integer as "40～50",
        SUM(CASE WHEN unit_price BETWEEN 50 AND 60 THEN unit_sold ELSE 0 END)::integer as "50～60",
        SUM(CASE WHEN unit_price BETWEEN 60 AND 70 THEN unit_sold ELSE 0 END)::integer as "60～70",
        SUM(CASE WHEN unit_price BETWEEN 70 AND 80 THEN unit_sold ELSE 0 END)::integer as "70～80",
        SUM(CASE WHEN unit_price BETWEEN 80 AND 90 THEN unit_sold ELSE 0 END)::integer as "80～90",
        SUM(CASE WHEN unit_price BETWEEN 90 AND 100 THEN unit_sold ELSE 0 END)::integer as "90～100",
        SUM(CASE WHEN unit_price > 100 THEN unit_sold ELSE 0 END)::integer as "100～"
        FROM products a
        JOIN txns b ON a.prod_id = b.prod_id
        JOIN price p on p.prod_id = b.prod_id
        JOIN custs c ON b.cust_id = c.cust_id'
        ))
      data2 <- as.matrix(d2)
      barplot(height = data2/1000000, 
              names.arg =c('0~10','10~20','20~30','30~40','40~50','50~60','60~70','70~80','80~90','90~100','100~'),
              col="#03a6ff",
              main = 'Sales at Different Price Range',
              ylab='Sales of Ice-Cream (in Million)',
              xlab='Price Range')
    }
    if (input$years4 != 'ALL' & input$render == 'ALL'){
      d2 <- dbGetQuery(
        conn = con, 
        statement = paste0(
          'SELECT SUM(CASE WHEN unit_price < 10 THEN unit_sold ELSE 0 END)::integer as "0～10",
        SUM(CASE WHEN unit_price BETWEEN 10 AND 20 THEN unit_sold ELSE 0 END)::integer as "10～20",
        SUM(CASE WHEN unit_price BETWEEN 20 AND 30 THEN unit_sold ELSE 0 END)::integer as "20～30",
        SUM(CASE WHEN unit_price BETWEEN 30 AND 40 THEN unit_sold ELSE 0 END)::integer as "30～40",
        SUM(CASE WHEN unit_price BETWEEN 40 AND 50 THEN unit_sold ELSE 0 END)::integer as "40～50",
        SUM(CASE WHEN unit_price BETWEEN 50 AND 60 THEN unit_sold ELSE 0 END)::integer as "50～60",
        SUM(CASE WHEN unit_price BETWEEN 60 AND 70 THEN unit_sold ELSE 0 END)::integer as "60～70",
        SUM(CASE WHEN unit_price BETWEEN 70 AND 80 THEN unit_sold ELSE 0 END)::integer as "70～80",
        SUM(CASE WHEN unit_price BETWEEN 80 AND 90 THEN unit_sold ELSE 0 END)::integer as "80～90",
        SUM(CASE WHEN unit_price BETWEEN 90 AND 100 THEN unit_sold ELSE 0 END)::integer as "90～100",
        SUM(CASE WHEN unit_price > 100 THEN unit_sold ELSE 0 END)::integer as "100～"
        FROM products a
        JOIN txns b ON a.prod_id = b.prod_id
        JOIN price p on p.prod_id = b.prod_id
        JOIN custs c ON b.cust_id = c.cust_id
        WHERE date between \'',input$years4,'-01-01\'',
          'and \'',input$years4,'-12-31\''
        ))
      data2 <- as.matrix(d2)
      barplot(height = data2/1000000, 
              names.arg =c('0~10','10~20','20~30','30~40','40~50','50~60','60~70','70~80','80~90','90~100','100~'),
              col="#79bd9a",
              main = 'Sales at Different Price Range',
              ylab='Sales of Ice-Cream (in Million)',
              xlab='Price Range')
    }
    if (input$years4 == 'ALL' & input$render !='ALL'){
      d2 <- dbGetQuery(
        conn = con, 
        statement = paste0(
        'SELECT SUM(CASE WHEN unit_price < 10 THEN unit_sold ELSE 0 END)::integer as "0～10",
        SUM(CASE WHEN unit_price BETWEEN 10 AND 20 THEN unit_sold ELSE 0 END)::integer as "10～20",
        SUM(CASE WHEN unit_price BETWEEN 20 AND 30 THEN unit_sold ELSE 0 END)::integer as "20～30",
        SUM(CASE WHEN unit_price BETWEEN 30 AND 40 THEN unit_sold ELSE 0 END)::integer as "30～40",
        SUM(CASE WHEN unit_price BETWEEN 40 AND 50 THEN unit_sold ELSE 0 END)::integer as "40～50",
        SUM(CASE WHEN unit_price BETWEEN 50 AND 60 THEN unit_sold ELSE 0 END)::integer as "50～60",
        SUM(CASE WHEN unit_price BETWEEN 60 AND 70 THEN unit_sold ELSE 0 END)::integer as "60～70",
        SUM(CASE WHEN unit_price BETWEEN 70 AND 80 THEN unit_sold ELSE 0 END)::integer as "70～80",
        SUM(CASE WHEN unit_price BETWEEN 80 AND 90 THEN unit_sold ELSE 0 END)::integer as "80～90",
        SUM(CASE WHEN unit_price BETWEEN 90 AND 100 THEN unit_sold ELSE 0 END)::integer as "90～100",
        SUM(CASE WHEN unit_price > 100 THEN unit_sold ELSE 0 END)::integer as "100～"
        FROM products a
        JOIN txns b ON a.prod_id = b.prod_id
        JOIN price p on p.prod_id = b.prod_id
        JOIN custs c ON b.cust_id = c.cust_id
        WHERE cust_name = \'', input$render, '\''
        ))
      data2 <- as.matrix(d2)
      barplot(height = data2/1000000, 
              names.arg =c('0~10','10~20','20~30','30~40','40~50','50~60','60~70','70~80','80~90','90~100','100~'),
              col="#79bd9a",
              main = 'Sales at Different Price Range',
              ylab='Sales of Ice-Cream (in Million)',
              xlab='Price Range')
    }
    if (input$years4 != 'ALL' & input$render != 'ALL'){
    d2 <- dbGetQuery(
      conn = con, 
      statement = paste0(
        'SELECT SUM(CASE WHEN unit_price < 10 THEN unit_sold ELSE 0 END)::integer as "0～10",
        SUM(CASE WHEN unit_price BETWEEN 10 AND 20 THEN unit_sold ELSE 0 END)::integer as "10～20",
        SUM(CASE WHEN unit_price BETWEEN 20 AND 30 THEN unit_sold ELSE 0 END)::integer as "20～30",
        SUM(CASE WHEN unit_price BETWEEN 30 AND 40 THEN unit_sold ELSE 0 END)::integer as "30～40",
        SUM(CASE WHEN unit_price BETWEEN 40 AND 50 THEN unit_sold ELSE 0 END)::integer as "40～50",
        SUM(CASE WHEN unit_price BETWEEN 50 AND 60 THEN unit_sold ELSE 0 END)::integer as "50～60",
        SUM(CASE WHEN unit_price BETWEEN 60 AND 70 THEN unit_sold ELSE 0 END)::integer as "60～70",
        SUM(CASE WHEN unit_price BETWEEN 70 AND 80 THEN unit_sold ELSE 0 END)::integer as "70～80",
        SUM(CASE WHEN unit_price BETWEEN 80 AND 90 THEN unit_sold ELSE 0 END)::integer as "80～90",
        SUM(CASE WHEN unit_price BETWEEN 90 AND 100 THEN unit_sold ELSE 0 END)::integer as "90～100",
        SUM(CASE WHEN unit_price > 100 THEN unit_sold ELSE 0 END)::integer as "100～"
        FROM products a
        JOIN txns b ON a.prod_id = b.prod_id
        JOIN price p on p.prod_id = b.prod_id
        JOIN custs c ON b.cust_id = c.cust_id
        WHERE date between \'',input$years4,'-01-01\'',
        'and \'',input$years4,'-12-31\'',
        'AND cust_name = \'', input$render, '\''
      ))
    data2 <- as.matrix(d2)
    barplot(height = data2/1000000, 
            names.arg =c('0~10','10~20','20~30','30~40','40~50','50~60','60~70','70~80','80~90','90~100','100~'),
            col="#79bd9a",
            main = 'Sales at Different Price Range',
            ylab='Sales of Ice-Cream (in Million)',
            xlab='Price Range')
    }
  }
  ) 
  
  #Q5 walmart map----
  point <- eventReactive(input$recalc, {
    cbind(input$lng, input$lat)
  }, ignoreNULL = FALSE)
  
  greenLeafIcon <- makeIcon(
    iconUrl = "https://www.flaticon.com/svg/static/icons/svg/2897/2897777.svg",
    iconWidth = 45, iconHeight = 100,
    iconAnchorX = 22, iconAnchorY = 94
  )
  
  output$wmMap <- renderLeaflet(
    {
      sql <- 'SELECT * FROM walmart;'
      store <- dbGetQuery(con, sql)
      store %>% 
        leaflet() %>%
        addProviderTiles(
          providers$Esri.WorldImagery
        ) %>% 
        addMarkers(
          label=store$name
        ) %>%  
        addMarkers(
          data = point(), 
          label = "Walmart Distribution Center", 
          icon = greenLeafIcon
        ) %>% 
        setView(
          lat = 43, 
          lng = -74, 
          zoom = 7
        ) 
    }
  )

}
