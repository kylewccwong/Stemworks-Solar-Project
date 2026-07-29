library(tidyverse)
library(tidycensus)
library(ggiraph)
library(mapview)
library(leaflet)
library(mapgl)
options(scipen = 999)
library(sf)
library(here)

#1
solar_predictions <- st_read(here("oahu_solar_simple.geojson"))
#from tacc split every piece of oahu into 1 sq km grids and ran model from 


#2
oahu_med_household_income <- get_acs(
  geography = "tract",
  variables = "B19013_001",  # median household income
  state = "HI",
  county = "Honolulu",
  geometry = TRUE,
  year = 2022
)

oahu_med_household_income <- oahu_med_household_income %>%
  rename(median_income = estimate,
         income_moe = moe)

#3
st_crs(solar_predictions) 
st_crs(oahu_med_household_income) 

solar_predictions <- solar_predictions[!st_is_empty(solar_predictions), ]
solar_predictions <- st_make_valid(solar_predictions)
oahu_med_household_income <- st_make_valid(oahu_med_household_income)

solar_predictions <- st_transform(solar_predictions, st_crs(oahu_med_household_income))

#4

n <- nrow(solar_predictions)
batch_size <- 5000
batch_ids <- split(1:n, ceiling(seq_along(1:n) / batch_size))

results <- vector("list", length(batch_ids))

for (i in seq_along(batch_ids)) {
  rows <- batch_ids[[i]]
  results[[i]] <- st_join(solar_predictions[rows, ], oahu_med_household_income, join = st_intersects, largest = TRUE)
  cat("Batch", i, "of", length(batch_ids), "done\n")
}

solar_with_tract <- bind_rows(results)

#5

st_crs(solar_with_tract)

solar_with_tract_proj <- st_transform(solar_with_tract, 32604)

solar_with_tract_proj <- solar_with_tract_proj %>%
  mutate(area_m2_recalc = as.numeric(st_area(geometry)))

#6

solar_with_tract_proj %>%
  st_drop_geometry() %>%
  filter(area_m2 != area_m2_recalc) %>%
  nrow()


tract_summary <- solar_with_tract_proj %>%
  st_drop_geometry() %>%
  group_by(GEOID) %>%
  summarise(total_solar_area_m2 = sum(area_m2_recalc, na.rm = TRUE))


solar_with_tract_proj %>%
  st_drop_geometry() %>%
  mutate(pct_diff = (area_m2 - area_m2_recalc) / area_m2_recalc * 100) %>%
  summarise(mean_pct_diff = mean(pct_diff, na.rm = TRUE))

#7

final_med_income <- mutate(
  left_join(
    select(oahu_med_household_income, GEOID, NAME, median_income, income_moe),
    tract_summary,
    by = "GEOID"
  ),
  total_solar_area_m2 = round(replace_na(total_solar_area_m2, 0),0)
)

### Pre map code 

oahu_households <- get_acs(
  geography = "tract",
  variables = "B11001_001",  #total households
  state = "HI",
  county = "Honolulu",
  geometry = FALSE,
  year = 2022
) %>%
  rename(households = estimate) %>%
  select(GEOID, households)

quantile(solar_with_tract_proj$area_m2_recalc, 
         probs = c(0.9, 0.95, 0.99, 0.999), na.rm = TRUE)

farm_candidates <- solar_with_tract_proj %>%
  st_drop_geometry() %>%
  filter(area_m2_recalc > 1000) %>%
  group_by(GEOID) %>%
  summarise(
    n_large_detections = n(),
    total_large_area_m2 = sum(area_m2_recalc, na.rm = TRUE)
  ) %>%
  arrange(desc(total_large_area_m2))

farm_candidates

farm_tracts <- c("15003008301", "15003980300", "15003008931", "15003009609",
                 "15003008502", "15003008933", "15003982200", "15003009704",
                 "15003008627", "15003008940")

rooftop_only <- solar_with_tract_proj %>%
  st_drop_geometry() %>%
  filter(!(GEOID %in% farm_tracts & area_m2_recalc > 1000)) %>%
  group_by(GEOID) %>%
  summarise(rooftop_solar_area_m2 = round(sum(area_m2_recalc, na.rm = TRUE)))

#Step 3

final_med_income <- final_med_income %>%
  left_join(rooftop_only, by = "GEOID") %>% 
  left_join(oahu_households, by = "GEOID") %>%  
  mutate(
    rooftop_solar_area_m2 = replace_na(rooftop_solar_area_m2, 0),
    solar_per_household = round(rooftop_solar_area_m2 / households,2))

final_med_income <- final_med_income %>%
  mutate(solar_per_household = if_else(households > 0, solar_per_household, NA_real_))

#### NEW: kWh per household conversion ####
# Formula: area (m2) x panel efficiency x Oahu solar radiation (kWh/m2/day) x days/year x performance ratio
# 5.82 = PVWatts annual solar radiation value for Honolulu (replaces earlier generic 5.5 estimate)

final_med_income <- final_med_income %>%
  mutate(kwh_per_household = round(solar_per_household * 0.20 * 5.82 * 365 * 0.80, 2))

final_med_income <- st_as_sf(final_med_income)

#8

### Home value

oahu_home_value <- get_acs(
  geography = "tract", variables = "B25077_001",
  state = "HI", county = "Honolulu", geometry = FALSE, year = 2022
) %>% rename(home_value = estimate) %>% select(GEOID, home_value)

final_med_income <- final_med_income %>% left_join(oahu_home_value, by = "GEOID")

final_med_income <- final_med_income %>%
  mutate(home_value_10k = home_value / 100000)

## Correlation tests (kwh_per_household)

cor.test(final_med_income$median_income, final_med_income$kwh_per_household,
         method = "spearman", use = "complete.obs")
cor.test(final_med_income$median_income, final_med_income$kwh_per_household,
         method = "pearson", use = "complete.obs")

cor.test(final_med_income$home_value, final_med_income$kwh_per_household,
         method = "spearman", use = "complete.obs")
cor.test(final_med_income$home_value, final_med_income$kwh_per_household,
         method = "pearson", use = "complete.obs")

## Regression (home value -> kwh_per_household, per $100k increment)

home_value_model_kwh <- lm(kwh_per_household ~ home_value_10k, data = final_med_income)
summary(home_value_model_kwh)

## Scatter plot: kWh per household vs. home value

ggplot(data = final_med_income, aes(x = home_value, y = kwh_per_household)) +
  geom_point(alpha = 0.5, color = "#8E5572") +
  geom_smooth(method = "lm", color = "red") +
  scale_y_log10(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Estimated kWh per Household vs. Median Home Value by Tract",
       x = "Median Home Value ($)", y = "Estimated kWh per Household, log scale")

## Map 2 Bivariate (income x kwh_per_household)

final_med_income$popup <- glue::glue(
  "<strong>GEOID: </strong>{final_med_income$GEOID}<br>",
  "<strong>Estimated kWh per Household: </strong>{final_med_income$kwh_per_household}<br>",
  "<strong>Median household income: </strong>${format(round(final_med_income$median_income), big.mark = ',')}"
)

income_kwh_scale <- bivariate_scale(
  data = final_med_income,
  x = "median_income",
  y = "kwh_per_household",
  na_color = "white",
  palette = "blue_red"
)

maplibre(
  style = carto_style("positron"),
  bounds = final_med_income
) |>
  add_fill_layer(
    id = "income_kwh",
    source = final_med_income,
    fill_color = income_kwh_scale$expression,
    fill_opacity = 0.75,
    fill_outline_color = "rgba(255,255,255,0.25)",
    popup = "popup",
    hover_options = list(fill_opacity = 1)
  ) |>
  add_bivariate_legend(
    income_kwh_scale,
    legend_title = "Income and kWh per Household",
    x_title = "Higher income",
    y_title = "Higher kWh per Household",
    layer_id = "HI_income_kwh",
    style = legend_style(
      background_color = "white",
      background_opacity = 0.94,
      border_color = "#D1D5DB",
      border_width = 1,
      shadow = TRUE
    )
  )

## Map 3 Bivariate (home value x kwh_per_household)

final_med_income$popup_value <- glue::glue(
  "<strong>GEOID: </strong>{final_med_income$GEOID}<br>",
  "<strong>Estimated kWh per Household: </strong>{final_med_income$kwh_per_household}<br>",
  "<strong>Median home value: </strong>${format(round(final_med_income$home_value), big.mark = ',')}"
)

value_kwh_scale <- bivariate_scale(
  data = final_med_income, x = "home_value", y = "kwh_per_household",
  na_color = "white", palette = "purple_orange"
)

maplibre(style = carto_style("positron"), bounds = final_med_income) |>
  add_fill_layer(
    id = "value_kwh", source = final_med_income,
    fill_color = value_kwh_scale$expression, fill_opacity = 0.75,
    fill_outline_color = "rgba(255,255,255,0.25)", popup = "popup_value",
    hover_options = list(fill_opacity = 1)
  ) |>
  add_bivariate_legend(
    value_kwh_scale, legend_title = "Home Value and kWh per Household",
    x_title = "Higher home value", y_title = "Higher kWh per Household",
    layer_id = "HI_value_kwh",
    style = legend_style(background_color = "white", background_opacity = 0.94,
                         border_color = "#D1D5DB", border_width = 1, shadow = TRUE)
  )

##PVwatts is a calculator that allowed us to create an island wide estimate
#for how much kwh was generated for area meters^2 of solar panels per household
