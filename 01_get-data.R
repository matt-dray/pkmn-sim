# Pokemon catch simulation
# Collect data
# Matt Dray
# June 2018


# Packages ----------------------------------------------------------------


library(rvest)  # webscrape
library(dplyr)  # data manipulation
library(tibble)  # tidy tables
library(stringr)  # string manipulation
library(janitor)  # clean names


# Get catch rates ---------------------------------------------------------


# read the html from the bulbapedia page about pokemon catch rates
# isolate the tables from the webpage as a list

catch_tables <- read_html(
  "https://bulbapedia.bulbagarden.net/wiki/List_of_Pok%C3%A9mon_by_catch_rate"
) %>% 
  html_table(fill = TRUE)

# isolate the second table from the list (contains catch rates)
# select certain columns (number, pokemon name, catch rate) and rows (gen I)

catch_rates <- catch_tables[[2]][c(1, 3, 4)] %>% 
  rename(
    number = `#`,
    name = Name,
    catch_rate = "Catch rate"
  ) %>% 
  filter(number < 152) %>%  # gen I pokemon only
  mutate(
    catch_rate = str_replace(catch_rate, "[*]", ""),  # remove asterisks
    catch_rate = as.integer(catch_rate)  # convert to numeric
  )


# Get rarity --------------------------------------------------------------


# rarity data from the pokemon revolution online game
# download the html for the wiki page on rarity
# extract the tables from the page

rare_tables <- read_html(
  "https://prowiki.info/index.php?title=List_of_Pok%C3%A9mon_by_Rarity_Tier"
) %>% 
  html_table(fill = TRUE)

# isolate the tables for each gen I pokemon's rarity
# isolate lookup table for text versions of rarity value

rarity <- rare_tables[[3]] %>% clean_names()
rarity_lookup <- rare_tables[[1]] %>% rename(tier = Tier, rarity_text = Name)


# Get game exclusives -----------------------------------------------------


exclusives <- data_frame(
  name = c(
    # red exclusive
    "Ekans", "Arbok", "Oddish", "Gloom", "Vileplume", "Mankey", "Primeape",
    "Growlithe", "Arcanine", "Scyther", "Electabuzz",
    # blue exclusive
    "Sandshrew", "Sandslash", "Bellsprout", "Weepinbell", "Victreebel",
    "Meowth", "Persian", "Vulpix", "Ninetales", "Pinsir", "Magma"
  ),
  excusive_to = c(rep("red", 11), rep("blue", 11))
)


# Join data ---------------------------------------------------------------


# join the rarity tier values to the catch rates data
# then join the lookup table for tier values to tier text descriptions

pkmn <- left_join(
  x = catch_rates,
  y = select(rarity, x_no, rarity_tier),
  by = c("number" = "x_no")
) %>% 
  left_join(
    y = rarity_lookup,
    by = c("rarity_tier" = "tier")
  ) %>% 
  left_join(
    y = exclusives,
    by = "name"
  )
