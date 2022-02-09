# the necessary packages----
library(DT)
library(RPostgres)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(scales)
library(RColorBrewer)
library(gridExtra)
library(ggthemes)
library(shinyjs)
library(scales)
library(leaflet)

# connect to the kpop database----
con <- dbConnect(
  drv = dbDriver('Postgres'), 
  dbname = 'team8_uni',
  host = 'db-postgresql-nyc1-44203-do-user-8018943-0.b.db.ondigitalocean.com', 
  port = 25060,
  user = 'doadmin', 
  password = 'ayxhea1w79p562zx'
)

# list of prods
sql_qq <- 'select prod_name from products'

products1 <- dbGetQuery(con, sql_qq)$prod_name


# the list of Cust_name/retailers----
retailer <- c(
  'amazon',
  'walmart', 
  'target', 
  'costco', 
  'family.dollar',
  'dollar.general',
  'cvs',
  'kroger',
  'walgreens',
  'rite.aid',
  'ahold',
  'safeway'
)

# the list of Months----
months_numeric <- c(
  1,
  2, 
  3, 
  4, 
  5,
  6,
  7,
  8,
  9,
  10,
  11,
  12
)

# the list of Months----
month <- c(
  'January',
  'Febuary', 
  'March', 
  'April', 
  'May',
  'June',
  'July',
  'August',
  'September',
  'October',
  'November',
  'December'
)
