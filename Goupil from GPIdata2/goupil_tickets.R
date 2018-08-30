library(GPIdata2)
library(tidyverse)
library(lubridate)

raw_load("raw_goupil")

# GRIPIDC-262 - Goupil Entry Dates ----

goupil_entry_dates <- raw_goupil %>%
  select(star_record_no, entry_date_year, entry_date_month, entry_date_day)

goupil_dates_missing <- goupil_entry_dates %>%
  mutate_at(vars(entry_date_year, entry_date_month, entry_date_day), funs(!is.na(.)))

goupil_dates_counts <- goupil_dates_missing %>%
  count(entry_date_year, entry_date_month, entry_date_day)

goupil_needs_month_day <- goupil_dates_missing %>%
  filter(entry_date_year, !entry_date_month, !entry_date_day)

goupil_working_dates <- goupil_entry_dates %>%
  anti_join(goupil_needs_month_day, by = "star_record_no") %>%
  filter(!is.na (entry_date_year))

goupil_count_month_day_00 <- goupil_working_dates %>%
  mutate_at(vars(entry_date_year, entry_date_month, entry_date_day), funs(.== "00"))

goupil_00_dates <- goupil_count_month_day_00 %>%
  filter(entry_date_month | entry_date_day)

goupil_complete_dates <- goupil_working_dates %>%
  anti_join(goupil_00_dates, by = "star_record_no")

goupil_parsed_dates <- goupil_complete_dates %>%
  mutate(joined_date = paste(entry_date_year, entry_date_month, entry_date_day, sep = "-"),
         parsed_date = ymd(joined_date)
  )

goupil_invalid_dates <- goupil_parsed_dates %>%
  filter(is.na(parsed_date))

make_report(goupil_invalid_dates)


# GRIPIDC-263 - Goupil Sale Dates ----

goupil_sale_dates <- raw_goupil %>%
  select(star_record_no, pi_record_no, sale_date_year, sale_date_month, sale_date_day)

goupil_sale_dates_missing <- goupil_sale_dates %>%
  mutate_at(vars(sale_date_year, sale_date_month, sale_date_day), funs(!is.na(.)))

goupil_sale_dates_counts <- goupil_sale_dates_missing %>%
  count(sale_date_year, sale_date_month, sale_date_day)

goupil_sale_needs_day <- goupil_sale_dates_missing %>%
  filter(sale_date_year, sale_date_month, !sale_date_day) %>%
  inner_join(goupil_sale_dates, by = "pi_record_no")

goupil_sale_working_dates <- goupil_sale_dates %>%
  anti_join(goupil_sale_needs_day, by = "pi_record_no") %>%
  filter(!is.na (sale_date_year))

goupil_sale_count_month_day_00 <- goupil_sale_working_dates %>%
  mutate_at(vars(sale_date_year, sale_date_month, sale_date_day), funs(.== "00"))

goupil_sale_00_dates <- goupil_sale_count_month_day_00 %>%
  filter(sale_date_month | sale_date_day)

goupil_sale_complete_dates <- goupil_sale_working_dates %>%
  anti_join(goupil_sale_00_dates, by = "star_record_no")

goupil_sale_parsed_dates <- goupil_sale_complete_dates %>%
  mutate(joined_date = paste(sale_date_year, sale_date_month, sale_date_day, sep = "-"),
         parsed_date = ymd(joined_date)
  )

goupil_sale_invalid_dates <- goupil_sale_parsed_dates %>%
  filter(is.na(parsed_date)) %>%
  inner_join(goupil_sale_dates, by = "star_record_no")


make_report(goupil_sale_needs_day)
make_report(goupil_sale_invalid_dates)


# GRIPIDC-269 - Goupil - cost vs purchase prices ----

# Goupil purchases

goupil_purchases <- raw_goupil %>%
  select(pi_record_no, purch_amount, purch_currency, purch_note)

goupil_missing_purchase_info <- goupil_purchases %>%
  filter(is.na(purch_amount))

goupil_has_purch_amount <- goupil_purchases %>%
  filter(!is.na(purch_amount))

goupil_missing_purch_currency <- goupil_purchases %>%
  filter(is.na(purch_currency))

goupil_has_purch_currency <- goupil_has_purch_amount %>%
  filter(!is.na(purch_currency))

# Goupil prices

goupil_prices <- raw_goupil %>%
  select(pi_record_no, price_amount, price_currency, price_note)

goupil_missing_price_amount <- goupil_prices %>%
  filter(is.na(price_amount))

goupil_has_price_amount <- goupil_prices %>%
  filter(!is.na(price_amount))

goupil_has_price_currency <- goupil_prices %>%
  filter(!is.na(price_currency))

filter_missing_price_currencies <- goupil_has_price_amount %>%
  filter(is.na(price_currency))

currency_but_no_price_amount <- goupil_missing_price_amount %>%
  filter(!is.na(price_currency))

make_report(goupil_has_price_amount)

# Goupil costs

goupil_costs <- raw_goupil %>%
  select(pi_record_no, cost_code, cost_translation, cost_currency, cost_description)

goupil_cost_translation <- goupil_costs %>%
  filter(!is.na(cost_translation))

goupil_missing_costs <- goupil_costs %>%
  filter(is.na(cost_translation))

goupil_has_currency_missing_cost <- goupil_missing_costs %>%
  filter(!is.na(cost_currency))

goupil_has_untranslated_cost_code <- goupil_missing_costs %>%
  filter(!is.na(cost_code))

make_report(goupil_has_untranslated_cost_code)

# GRIPIDC-394/405 - Goupil/Knoedler Joining Numbers ----

raw_load("raw_knoedler")

knoedler_info <- raw_knoedler %>%
  select(star_record_no, pi_record_no, knoedler_number, consign_no, consign_name, art_authority_1, title) %>%
  filter(!is.na(consign_name)) %>%
  filter(str_detect(consign_name, "Goupil")) %>%
  filter(!is.na(consign_no))

goupil_info <- raw_goupil %>%
  select(star_record_no, pi_record_no, goupil_number, art_authority_1, title)

shared_goupil_numbers <- knoedler_info %>%
  left_join(goupil_info, by = c("art_authority_1", "consign_no" = "goupil_number"))

joined_no_authority <- knoedler_info %>%
  left_join(goupil_info, by = c("consign_no" = "goupil_number"))

no_artist_match <- joined_no_authority %>%
  filter(art_authority_1.x != art_authority_1.y)

length(unique(shared_goupil_numbers$consign_no))

make_report(no_artist_match)

make_report(joined_no_authority)

