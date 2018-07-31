# Pokemon catch simulation
# Collect and prepare Pokemon data
# Matt Dray
# July 2018


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

catch_rate <- catch_tables[[2]][c(1, 3, 4)] %>% 
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

# clean up

rm(catch_tables)


# Get rarity --------------------------------------------------------------


# rarity data from the pokemon revolution online game
# download the html for the wiki page on rarity
# extract the tables from the page

rarity_tables <- read_html(
  "https://prowiki.info/index.php?title=List_of_Pok%C3%A9mon_by_Rarity_Tier"
) %>% 
  html_table(fill = TRUE)

# isolate the tables for each gen I pokemon's rarity

rarity_pkmn <- rarity_tables[[3]] %>%
  clean_names() %>% 
  select(number = number_no, name, rarity_tier)

# isolate lookup table for text versions of rarity value

rarity_lookup <- rarity_tables[[1]] %>%
  rename(rarity_tier = Tier, rarity_text = Name)

# join data

rarity <- left_join(
  x = rarity_pkmn,
  y = rarity_lookup,
  by = "rarity_tier"
)

# clean up

rm(rarity_tables, rarity_pkmn, rarity_lookup)


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
  exclusive_to = c(rep("red", 11), rep("blue", 11))
)


# Join data ---------------------------------------------------------------


# join data and tidy

pkmn <- left_join(
  x = catch_rate,
  y = exclusives,
  by = "name"
) %>% 
  left_join(
    y = rarity,
    by = "number"
  ) %>% 
  select(
    number, name = name.x, exclusive_to,
    catch_rate,
    rarity_tier, rarity_text
  )

# clean up

rm(catch_rate, exclusives, rarity)


# Save --------------------------------------------------------------------


saveRDS(pkmn, "data/pkmn.RDS")
