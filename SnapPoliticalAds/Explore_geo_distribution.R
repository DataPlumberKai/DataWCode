# Explore the Snap political ads data
# Author: Data Plumber

rm(list = ls())

# Load packages -----

## basic data wrangling
library(tidyverse)

## for map
library(ggthemes)
library(sf)
sf_use_s2(FALSE)

## Source of world map
library(rnaturalearth)
library(rnaturalearthdata)

## Source of US map
library(maps)

## countrycode conversion
library(countrycode)


# Download data ----

# Let's focus on 2023 data

url <- "https://storage.googleapis.com/ad-manager-political-ads-dump/political/2022/PoliticalAds.zip"

dir.create("data/raw", recursive = TRUE) # Create new directory to store data
download.file(url, destfile = "data/raw/2022.zip") # Download raw data
unzip("data/raw/2022.zip", exdir = "data/raw/2022") # Unzip raw data


# Load data ----

df <- read_csv("data/raw/2022/PoliticalAds.csv")

glimpse(df)

# Setup output directories ----

dir.create("data")
dir.create("fig")


# Summarize global distribution by country ----

sum_by_country <- df %>% count(CountryCode) %>% arrange(desc(n))

summary(sum_by_country$n)


# Visualize global distribution -----

## Load world map -----

map_world <- ne_countries(scale = "medium", type = "map_units", returnclass = "sf")

## Get a standardized country code for sum_by_country -----

"
Note: The CountryCode variable in the Snap dataset is actually country names
I use the countrycode package to match them with countries' iso3c codes
Then they can be matched with map_world data
"

sum_by_country <- sum_by_country %>%
  mutate(iso_a3 = countrycode(CountryCode, origin = "country.name", destination = "iso3c"),
         .after = CountryCode)


## Merge data with map -----

map_world_m <- map_world %>% left_join(sum_by_country, by = "iso_a3")


## Map ads global distribution -----

ggplot(map_world_m) +
  geom_sf(aes(fill = n)) +
  theme_map() +
  scale_fill_viridis_c() +
  labs(title = "Number of political ads by country",
       fill = "Number of ads") +
  theme(legend.position = "bottom")

ggsave("fig/map_global_N.png", width = 10, height = 6, dpi = 300)

## Improve the map ----

"
#################### NOTE #################### 
The map is not very informative.
It literally has only two colors! Why? Skewed distribution of data
We can improve it with a log transformation on either the data or the scale

Note: we can do this only when all data has N > 0
In our case, this is not a problem. If a contry has 0 ads, it will not be
in the dataset (shown in gray in the map).
But this is not always true.
#################### NOTE #################### 
"

### Log transformation on the data -----

ggplot(map_world_m) +
  geom_sf(aes(fill = log(n))) +
  theme_map() +
  scale_fill_viridis_c() +
  labs(title = "Number of political ads by country",
       fill = "log(Number of ads)") +
  theme(legend.position = "bottom")

ggsave("fig/map_global_logN.png", width = 10, height = 6, dpi = 300)


### Log transformation on the scale -----

ggplot(map_world_m) +
  geom_sf(aes(fill = n)) +
  theme_map() +
  scale_fill_viridis_c(trans = "log") +
  labs(title = "Number of political ads by country",
       fill = "Number of ads") +
  theme(legend.position = "bottom")

ggsave("fig/global_map_N_logScale.png", width = 10, height = 6, dpi = 300)



## Extension: Global distribution of money spent ----

sum_by_country <- df %>% group_by(CountryCode) %>% 
  summarize(total_spend = sum(Spend)) %>% arrange(desc(total_spend))

sum_by_country <- sum_by_country %>%
  mutate(iso_a3 = countrycode(CountryCode, origin = "country.name", destination = "iso3c"),
         .after = CountryCode)

map_world_m <- map_world %>% left_join(sum_by_country, by = "iso_a3")

ggplot(map_world_m) +
  geom_sf(aes(fill = total_spend)) +
  theme_map() +
  scale_fill_viridis_c(trans = "log") +
  labs(title = "Total spending on political ads by country",
       fill = "log(Total spend)") +
  theme(legend.position = "bottom")

ggsave("fig/global_map_totalSpend_logScale.png", width = 10, height = 6, dpi = 300)





# Within-country distribution: United States ----

df_us <- df %>% filter(CountryCode == "united states")

## Subset region-targeted ads -----

# Find the identifier of targeted states
df_us_targeted <- df_us %>% filter(!is.na(`Regions (Excluded)`)) %>%
  select(Spend, `Regions (Excluded)`)

# Observe the "Regions (Excluded)" column, they contain state names separated by ,
# We need to split them into separate rows

df_us_targeted_split <- df_us_targeted %>%
  mutate(state = str_split(`Regions (Excluded)`, ",")) 

df_us_targeted_split_unnested <- df_us_targeted_split %>%
  unnest(state)

sum_by_us_state <- df_us_targeted_split_unnested %>%
  group_by(state) %>%
  summarize(
    n = n(),
    total_spend = sum(Spend)
    ) %>%
  arrange(desc(total_spend))


## Load US map ------

map_us <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))

## Merge data with map ----

sum_by_us_state <- sum_by_us_state %>% mutate(state = tolower(state))

map_us_m <- map_us %>% 
  left_join(sum_by_us_state, by = c("ID" = "state"))

## Map ads distribution in the US ----

ggplot(map_us_m) +
  geom_sf(aes(fill = n)) +
  theme_map() +
  scale_fill_viridis_c() +
  labs(title = "Number of political ads by state",
       fill = "Number of targeted ads") +
  theme(legend.position = "bottom")

ggsave("fig/map_us_N.png", width = 10, height = 6, dpi = 300)

ggplot(map_us_m) +
  geom_sf(aes(fill = total_spend)) +
  theme_map() +
  scale_fill_viridis_c() +
  labs(title = "Total spending on political ads by state",
       fill = "Total money spend on targeted ads") +
  theme(legend.position = "bottom")

ggsave("fig/map_us_totalSpend.png", width = 10, height = 6, dpi = 300)
