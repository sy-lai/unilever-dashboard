library(dplyr)
library(tidyr)
library(tidyverse)
library(RPostgres)
library(reshape2)

#EXTRACT----
#_read source data----
df <- read.csv('Unilever_ic.csv')
wm <- read.csv('walmartNY.csv')
df_p <- df %>%
  group_by(category, group, product, brand, org) %>%
  summarise(price = round(mean(unit_price),2))

#TRANSFORM----
#_split product column into prod_id and product----
df <- df %>%
  separate(product, c("prod_id", "product"), " - ")

#_aggregate quantity_sold in different distribution channels----
df <- df %>% 
  group_by(org, category, group, prod_id, product, date, unit_price, brand) %>%
  summarise_if(is.integer, funs(sum))

# connection to cloud server----
con <- dbConnect(
  drv = dbDriver('Postgres'), 
  dbname = 'team8_uni',
  host = 'db-postgresql-nyc1-44203-do-user-8018943-0.b.db.ondigitalocean.com',
  port = 25060,
  user = 'team8',
  password = 'pixfqhf9nvfi9i5d'
)

#**create organizations table----
stmt <- 'CREATE TABLE organizations (
            org_id int,
            org varchar(50),
            PRIMARY KEY(org_id));'
dbExecute(con, stmt)

#**create categories table----
stmt <- 'CREATE TABLE categories (
            category_id int,
            category varchar(100),
            PRIMARY KEY(category_id));'
dbExecute(con, stmt)

#**create groups table----
stmt <- 'CREATE TABLE groups (
            grp_id int,
            grp varchar(100),
            PRIMARY KEY(grp_id));'
dbExecute(con, stmt)

#**create products table----
stmt <- 'CREATE TABLE products (
            prod_id int,
            prod_index varchar(50),
            prod_name varchar(100),
            brand varchar(50),
            org_id int,
            category_id int,
            grp_id int,
            PRIMARY KEY(prod_id),
            FOREIGN KEY(org_id) REFERENCES organizations,
            FOREIGN KEY(category_id) REFERENCES categories,
            FOREIGN KEY(grp_id) REFERENCES groups);'
dbExecute(con, stmt)

#**create price table----
stmt <- 'CREATE TABLE price(
         prod_id int,
         unit_price float,
         PRIMARY KEY(prod_id));'
dbExecute(con, stmt)        

#**create custs table---
stmt <- 'CREATE TABLE custs (
           cust_id int,
           cust_name varchar(50),
           PRIMARY KEY(cust_id));'
dbExecute(con, stmt)

#**create txns table----
stmt <- 'CREATE TABLE txns (
            txns_id int,
            prod_id int,
            date date,
            cust_id int,
            unit_sold int,
            PRIMARY KEY(txns_id),
            FOREIGN KEY(prod_id) REFERENCES products,
            FOREIGN KEY(cust_id) REFERENCES custs);'
dbExecute(con, stmt)

#**create walmart table----
stmt <- 'CREATE TABLE walmart (
            store_id int,
            name varchar(100),
            latitute numeric,
            longitude numeric,
            PRIMARY KEY(store_id));'
dbExecute(con, stmt)


#LOAD----
#_load organizations table----
# df1 stores unique org
df1 <- df %>%
  ungroup() %>%
  select(org) %>%
  distinct()
#df2 attaches the cust_id PK
df2org <- bind_cols('org_id' = 1:nrow(df1), df1)
dbWriteTable(
  con, 'organizations', df2org,
  row.names = FALSE, append = TRUE
)

#_load categories table----
# df1 stores unique categories
df1 <- df %>%
  ungroup() %>%
  select(category) %>%
  distinct()
#df2 attaches the genre_id PK
df2cat <- bind_cols('category_id' = 1:nrow(df1), df1)
dbWriteTable(
  con, 'categories', df2cat,
  row.names = FALSE, append = TRUE
)

#_load groups table----
# df1 stores unique groups
df1 <- df %>%
  ungroup() %>%
  select('grp' = group) %>%
  distinct()
#df2 attaches the genre_id PK
df2g <- bind_cols('grp_id' = 1:nrow(df1), df1)
dbWriteTable(
  con, 'groups', df2g,
  row.names = FALSE, append = TRUE
)


#_load products table----
df1 <- df %>%
  ungroup() %>%
  select(prod_index = prod_id, prod_name = product, brand, org, category, grp = group) %>%
  distinct()
df1p <- df1 %>%
  inner_join(df2org) %>%
  inner_join(df2cat) %>%
  inner_join(df2g) %>%
  select(prod_index, prod_name, brand, org_id, category_id, grp_id) %>%
  distinct()
df1pp <- bind_cols('prod_id' = 1:nrow(df1p), df1p)
dbWriteTable(
  con, 'products', df1pp,
  row.names = FALSE, append = TRUE
)

#_load price table----
df_p <- df_p %>%
  separate(product, c("prod_index", "prod_name"), " - ")
df_pp <- df_p %>%
  ungroup() %>%
  select(prod_index,prod_name, category, grp = group, org, brand, price) %>%
  inner_join(df2org) %>%
  inner_join(df2cat) %>%
  inner_join(df2g) %>%
  select(prod_index, prod_name, category_id, grp_id, org_id, brand, price)
df_ppp <- df_pp %>%
  inner_join(df1pp) %>%
  select(prod_id, unit_price = price)
dbWriteTable(
  con, 'price', df_ppp,
  row.names = FALSE, append = TRUE
)
  
#_load custs table----
df1c <- colnames(df)
df1c <- df1c[-c(1,2,3,4,5,6,13)]
df1c[2] <- 'walmart'
df1c <- as.data.frame(df1c)
names(df1c) <- c('cust_name')
df1cc <- bind_cols('cust_id' = 1:nrow(df1c), df1c)
dbWriteTable(
  con, 'custs', df1cc,
  row.names = FALSE, append = TRUE)

  
#_load txns table----
df_txns <- melt(
  df,
  id.vars = c('org', 'category', 'group', 'prod_id', 'product', 'date', 'unit_price', 'brand'),
  variable.name = 'cust_name',
  value.name = 'unit_sold'
)
df_txns <- df_txns[,-c(7)]
df_txns <- df_txns %>%
  group_by(org, category, group, prod_id, product, brand)
df1t <- df_txns %>%
  select(prod_index = prod_id, prod_name = product, brand, org, category, grp = group, date, cust_name, unit_sold) %>%
  inner_join(df2org) %>%
  inner_join(df2cat) %>%
  inner_join(df2g) %>%
  select(prod_index, prod_name, brand, org_id, category_id, grp_id, date, cust_name, unit_sold)
df1tt <- df1t %>%
  ungroup() %>%
  inner_join(df1pp) %>%
  inner_join(df1cc) %>%
  select(prod_id, date, cust_id, unit_sold) 
df1ttt <- bind_cols('txns_id' = 1:nrow(df1tt), df1tt)
dbWriteTable(
  con, 'txns', df1ttt,
  row.names = FALSE, append = TRUE
)


#_load walmart table----
df1 <- wm %>%
  ungroup() %>%
  select(name, latitude, longitude) %>%
  distinct()
df2wm <- bind_cols('store_id' = 1:nrow(df1), df1)
dbWriteTable(
  con, 'walmart', df2wm,
  row.names = FALSE, append = TRUE
)




