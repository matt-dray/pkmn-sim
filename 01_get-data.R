# Pokemon catch simulation
# Collect data
# Matt Dray
# June 2018


# Packages ----------------------------------------------------------------


library(rvest)  # webscrape
library(dplyr)  # data manipulation
library(tibble)  # tidy tables
library(stringr)  # string manipulation


# Get catch rates ---------------------------------------------------------


# read the html from the bulbapedia page about pokemon catch rates

bulb_page <- read_html(
  "https://bulbapedia.bulbagarden.net/wiki/List_of_Pok%C3%A9mon_by_catch_rate"
)

# isolate the tables from the webpage as a list

bulb_tables <- bulb_page %>% html_table(fill = TRUE)

# isolate the second table from the list (contains catch rates)
# select certain columns (number, pokemon name, catch rate) and rows (gen I)

catch_rates <- bulb_tables[[2]][c(1, 3, 4)] %>% 
  rename(
    number = `#`,
    name = Name,
    catch_rate = "Catch rate"
  ) %>% 
  filter(number < 152) %>%  # first gen pokemon only
  mutate(
    catch_rate = str_replace(catch_rate, "[*]", ""),  # remove asterisks
    catch_rate = as.integer(catch_rate)  # convert to numeric
    )


# Get rarity --------------------------------------------------------------


# rarity data from the pokemon revolution online game
# download the html for the wiki page on rarity

pro_html <- read_html(
  "https://prowiki.info/index.php?title=List_of_Pok%C3%A9mon_by_Rarity_Tier"
)

# extract the tables from the page

pro_tables <- pro_html %>% html_table(fill = TRUE)

# isolate the tables for eacg gen I pokemon's rarity
# isolate lookup table for text versions of rarity value

pokemon_tiers <- pro_tables[[3]] %>% clean_names()
rarity_tiers <- pro_tables[[1]] %>% rename(tier = Tier, rarity_text = name)


# Join rarity -------------------------------------------------------------

# join the rarity tier values to the catch rates data
# then join the lookup table for tier values to tier text descriptions

pkmn_catch_rarity <- left_join(
  x = catch_rates,
  y = select(pokemon_tiers, x_no, rarity_tier),
  by = c("number" = "x_no")
) %>% 
  left_join(
    y = rarity_tiers,
    by = c("rarity_tier" = "tier")
  )
