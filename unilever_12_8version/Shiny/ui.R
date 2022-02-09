### GROUP PROJECT ----

ui <- tagList(
  
  useShinyjs(),
  
  setBackgroundImage(src = 'unilever_ice_cream.jpg'),
  
  navbarPage(
    
    id = 'navBar',
    title = 'Unilever Ice Cream',
    windowTitle = 'SQL',
    position = 'fixed-top',
    collapsible = TRUE,
    inverse = TRUE,
    theme = shinytheme('cerulean'),
    
    # Home ----
    
    tabPanel(
      title = div(
        img(src = 'UNILEVER_LOGO.png', height = 35)
      ),
      tags$head(
        tags$style(
          type = 'text/css', 
          'body {padding-top: 70px;}' 
        )
      )
    ),
    
    # Q1 Product----
    tabPanel(
      
      title = 'Product',
      div(
        class = 'outer',
        tags$head(includeCSS('styles.CSS'))
      ),
      fluidPage(    
        titlePanel("Select Years"),
        
        # Generate a row with a sidebar
        sidebarLayout(      
          
          # Define the sidebar with one input
          sidebarPanel(
            selectInput("years", "Years:", 
                        choices = c("ALL","2017","2018","2019")),
          ),
          hr()
        ),
        
        # Create a spot for the barplot
        mainPanel(
          plotOutput("Q1view"),
          htmlOutput('ProductTitle'),
          dataTableOutput("Q1table")
        ),
        
        
        
        
      )),
    
    # Q2 Retailer----
    
    tabPanel(
      
      title = 'Retailer',
      div(
        class = 'outer',
        tags$head(includeCSS('styles.CSS'))
        
      ),
      
      fluidPage(    
        titlePanel("Select Years"),
        
        # Generate a row with a sidebar
        sidebarLayout(      
          
          # Define the sidebar with one input
          sidebarPanel(
            selectInput("years3", "Years:", 
                        choices = c("2017","2018","2019")),
            tableOutput("retailerTable"),  
            column(
              width = 8,
              align = 'center',
            ),
            hr()
          ),
          
          # Create a spot for the barplot
          mainPanel(
            plotOutput("Q2retailer")  
          ),
          
          
        )
      )
    ),
    
    
    
    

    
    # Q3 Price----
    
    tabPanel(
      
      title = 'Price',
      div(
        class = 'outer',
        tags$head(includeCSS('styles.CSS'))),
      
      fluidPage(
        titlePanel("Select Years"),
        # Generate a row with a sidebar
        sidebarLayout(      
          
          # Define the sidebar with one input
          sidebarPanel(
            selectInput("years4", "Years:", 
                        choices = c("ALL","2017","2018","2019")),
            selectInput("render","Render:",
                        choices = c("ALL","amazon","walmart","target","costco","family.dollar","dollar.general","cvs","kroger","walgreens","rite.aid","ahold","safeway")),
            tableOutput("Q4table"),  
            column(
              width = 8,
              align = 'center',
            ),
            hr()
          ),
          # Create a spot for the barplot
          mainPanel(
            plotOutput("Q4view")  
          )
          
        )
      )
    ),
    
    # Q4 Demand Planning----
    
    tabPanel(
      
      title = 'Demand Planning',
      div(
        class = 'outer',
        tags$head(includeCSS('styles.CSS'))),
      
      column(width = 12,
             h3('All units sold by month, product, and retailer in 2017, 2018, and 2019', align = 'center'),
             wellPanel(
               fluidRow(
                 column(
                   width = 4,
                   # drop down list to select a retailer/custs_name----
                   pickerInput(
                     inputId = 'custs',
                     label = 'Retailer',
                     choices = retailer,
                     options = list(
                       title = 'Select a retailer'
                     )
                   ),
                   # text input to select a product name----
                   selectInput(
                     inputId = 'prods',
                     label = 'Product',
                     choices = products1,
                     selected = character(0)
                   ),
                   # drop down list to select a month----
                   pickerInput(
                     inputId = 'months_units_sold',
                     label = 'Month',
                     choices = c(1:12),
                     options = list(
                       title = 'Select a month'
                     )
                   )
                 ),
                 column(
                   width = 8,
                   align = 'center',
                   # small image of retailer----
                   uiOutput('custImg')
                 )
               )
               
             ),
             hr(),
             # chart of units sold by month for X retailer and A Product----
             htmlOutput('Units_sold'),
             plotOutput(
               outputId = 'units_sold_Chrt'
             )
      )
      
      
    ),
    
    
    # Q5 Logistics Stadium Map----
    
    tabPanel(
      
      title = 'Logistics',
      div(
        class = 'outer',
        tags$head(includeCSS('styles.CSS')),
        leafletOutput(
          outputId = 'wmMap',
          width = '100%',
          height = '100%'
        ),
        numericInput("lng", label = h3("Longitude:"), value = -75.70402032),
        numericInput("lat", label = h3("Latitude:"), value = 42.61625923),
        actionButton("recalc", "Show point")
      )
      
    )
  )
)