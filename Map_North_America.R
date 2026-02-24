rm(list=ls())

library(sf)
library(ggplot2)
library(rnaturalearth)
library(dplyr)
library(ggnewscale)

# US states
us_states <- ne_states(country="United States of America", returnclass="sf") %>%
  filter(!name %in% c("Hawaii", "Puerto Rico", "American Samoa",
                      "Guam", "Northern Mariana Islands", "United States Virgin Islands"))

# HHS regions
hhs_regions <- list(
  "1"=c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont"),
  "2"=c("New Jersey", "New York"),
  "3"=c("Delaware", "District of Columbia", "Maryland", "Pennsylvania", "Virginia", "West Virginia"),
  "4"=c("Alabama", "Florida", "Georgia", "Kentucky", "Mississippi", "North Carolina", "South Carolina", "Tennessee"),
  "5"=c("Illinois", "Indiana", "Michigan", "Minnesota", "Ohio", "Wisconsin"),
  "6"=c("Arkansas", "Louisiana", "New Mexico", "Oklahoma", "Texas"),
  "7"=c("Iowa", "Kansas", "Missouri", "Nebraska"),
  "8"=c("Colorado", "Montana", "North Dakota", "South Dakota", "Utah", "Wyoming"),
  "9"=c("Arizona", "California", "Nevada"),
  "10"=c("Alaska", "Idaho", "Oregon", "Washington")
)

us_states <- us_states %>%
  rowwise() %>%
  mutate(fill_us={
    r <- NA
    for (name_r in names(hhs_regions)){
      if (name %in% hhs_regions[[name_r]]) r <- name_r
    }
    r
  }) %>%
  ungroup() %>%
  mutate(fill_us=factor(fill_us, levels=as.character(1:10)))

# Canadian provinces
canada <- ne_states(country="Canada", returnclass="sf") %>%
  mutate(fill_ca=case_when(
    name %in% c("Newfoundland and Labrador", "Nova Scotia", "New Brunswick",
                "Prince Edward Island") ~ "Atlantic",
    name %in% c("British Columbia") ~ "British Columbia",
    name %in% c("Ontario") ~ "Ontario",
    name %in% c("Manitoba", "Saskatchewan", "Alberta") ~ "Prairies"
  )) %>%
  mutate(fill_ca=factor(fill_ca, levels=c("Atlantic", "British Columbia",
                                          "Ontario", "Prairies", "(Other)")))

# Mexico
mexico <-ne_countries(country="Mexico", returnclass="sf", scale="large")

# US-centered projection
us_centered_crs <- "+proj=laea +lat_0=39 +lon_0=-96 +datum=WGS84 +units=m +no_defs"

# Plot (landscape: 10 x 7)

ggplot() +
  
  # Canada
  geom_sf(data=canada, aes(fill=fill_ca), color="white", size=0.2, alpha=0.8) +
  scale_fill_manual(
    values=c("#DA5E06", "#E82789", "#6A65AE", "#109A70"),
    breaks=c("Atlantic", "British Columbia", "Ontario", "Prairies"),
    name="Canadian\nprovince/region", guide=guide_legend(order=1), na.value = "grey80") +
  ggnewscale::new_scale_fill() +   # allow a new fill scale for the US
  # US
  geom_sf(data=us_states, aes(fill=fill_us), color="white", size=0.1, alpha=0.8) +
  scale_fill_manual(values=c("grey35", rev(scales::hue_pal()(9))), name="HHS Region\n(United States)",
                    guide=guide_legend(order=2)) +
  # Mexico
  geom_sf(data=mexico, fill='grey80', color="white", size=0.2, alpha=0.8) +
  
  coord_sf(crs=us_centered_crs) +
  theme_minimal()
