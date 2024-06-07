# Visualize top advertisers
# Author: Data Plumber

rm(list = ls())

# Load packages -----

## basic data wrangling
library(tidyverse)

## Package to avoid label overlap
library(ggrepel)


# Download data ----

# Focus on 2022 data

url <- "https://storage.googleapis.com/ad-manager-political-ads-dump/political/2022/PoliticalAds.zip"

dir.create("data/raw", recursive = TRUE) # Create new directory to store data
download.file(url, destfile = "data/raw/2022.zip") # Download raw data
unzip("data/raw/2022.zip", exdir = "data/raw/2022") # Unzip raw data


# Load data ----

df <- read_csv("data/raw/2022/PoliticalAds.csv")

glimpse(df)

df_us <- df %>% filter(CountryCode == "united states")


# Visualize advertisers ----

# Summarize per advertiser spending
sum_candidate <- df_us %>% 
  group_by(PayingAdvertiserName) %>% 
  summarise(
    n = n(),
    totalSpend = sum(Spend)
  ) %>% 
  arrange(desc(totalSpend))

# Get top 10 advertisers
top_spender <- sum_candidate %>% 
  ungroup() %>%
  top_n(10, totalSpend)


# Bad: Overlapping labels!!

sum_candidate %>%
  ggplot(aes(x = totalSpend)) +
  geom_histogram(bins = 30, fill = "lightgray", color = "black") +
  scale_x_continuous(labels = scales::dollar) +
  labs(
    title = "Political Ads Spending on SnapChat by Advertiser, 2022",
    x = "Total money spent per advertiser",
    y = "Count",
    caption = "Source: Snap Political Ads Library. Author: DataPlumberr"
  ) +
  geom_label(data = top_spender, 
                   aes(x = totalSpend, y = 0, label = PayingAdvertiserName), 
                   hjust = 1, vjust = 1, nudge_y = 150, cex = 2) +
  theme_minimal()

ggsave("fig/histogram_spending_by_advertiser_annotate_top10_bad.png", width = 6, height = 4, dpi = 300)


# Good plot: No overlapping labels with ggrepel

sum_candidate %>%
  ggplot(aes(x = totalSpend)) +
  geom_histogram(bins = 30, fill = "lightgray", color = "black") +
  scale_x_continuous(labels = scales::dollar) +
  labs(
    title = "Political Ads Spending on SnapChat by Advertiser, 2022",
    x = "Total money spent per advertiser",
    y = "Count",
    caption = "Source: Snap Political Ads Library; Author: Data Plumber"
  ) +
  geom_label_repel(data = top_spender, 
                  aes(x = totalSpend, y = 0, label = PayingAdvertiserName), 
            hjust = 1, vjust = 1, nudge_y = 150, cex = 2, alpha = 0.8) +
  theme_minimal()

ggsave("fig/histogram_spending_by_advertiser_annotate_top10_good.png", width = 6, height = 4, dpi = 300)
