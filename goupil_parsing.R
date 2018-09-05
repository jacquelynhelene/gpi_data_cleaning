## Parsing prices and dimensions in Goupil

# install pirParser package
#devtools::install_github("thegetty/pirParser")

library(tidyverse)
library(pirParser)
library(knitr)
library(rematch2)
library(dplyr)
library(stringr)
library(tidyr)
library(magrittr)

# Load data from latest Goupil export

goupil_csv <- read.csv("C:\\Users\\jclements\\Desktop\\GPI_PIR\\GPI_Data_Cleaning\\gpi_data_cleaning\\data\\goupil.csv")

# Dimensions: general_dimension_extraction -- not working at the moment

toy_dimension_parse <- general_dimension_extraction(toy_dimension,
                             dimcol = "dimension",
                             idcol = "id",
                             exclusion_col = "exclude")
# get: 
# Error: 'bind_re_match' is not an exported object from 'namespace:rematch2'
# Tried rematch2:::bind_re_match in function code but get errors with pipe_message, etc, which are probably coming from other packages...

kable(toy_dimension_parse)

# Prices: parse_prices

select_goupil <- goupil_csv %>%
  select(star_no = STAR.Record.No., purch_amount = Purch..Amount, purch_currency = Purch..Currency)

goupil_prices_parsed <- parse_prices(df = select_goupil,
                                amount_col_name = "purch_amount",
                                currency_col_name = "purch_currency",
                                id_col_name = "star_no")
kable(toy_money_parse)
