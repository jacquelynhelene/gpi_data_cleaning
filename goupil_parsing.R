## Parsing prices and dimensions in Goupil

# install pirParser package
#devtools::install_github("thegetty/pirParser")

library(pirParser)
library(knitr)
library(rematch2)

# Load data from latest Goupil export

# goupil <- read.csv("goupil.csv)

# Dimensions: general_dimension_extraction -- not working at the moment

toy_dimension_parse <- general_dimension_extraction(toy_dimension,
                             dimcol = "dimension",
                             idcol = "id",
                             exclusion_col = "exclude")
kable(toy_dimension_parse)

# Prices: parse_prices

toy_money_parse <- parse_prices(toy_money,
                                amount_col_name = "price",
                                currency_col_name = "currency",
                                id_col_name = "id")
kable(toy_money_parse)
