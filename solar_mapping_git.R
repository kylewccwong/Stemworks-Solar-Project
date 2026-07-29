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

### st_crs is a SANITY CHECK
## will print out in console labeled GEOGCRS- make sure that all datasets are working
## in same 'language' ex. (WGS 84, NAD 83, etc.)
## must do before combining columns 

solar_predictions <- solar_predictions[!st_is_empty(solar_predictions), ]
solar_predictions <- st_make_valid(solar_predictions)

## st_is_empty is code specifically for cleaning empty geometry rows
#will consider empty rows FALSE
# ! at beginning in front of st_is_empty tells us to flip 82 empty rows to TRUE
# solar_predictions[TRUE] - essentially removes missing rows and displays 102,614

oahu_med_household_income <- st_make_valid(oahu_med_household_income)
#we dont have a st_is_empty for med household income because we are keeping the tracts even if theyre empty
## st_make_valid is just general cleaning of geometry - fixes broken geometry


solar_predictions <- st_transform(solar_predictions, st_crs(oahu_med_household_income))

##st_transform prepares for a join function - we can only do this if they are using
# the same GEOCRS, currently solar_predictions is WGS84 and oahu_med is NAD83
#WGS84 is more for global outlook while NAD83 is a more localized, closer look
#We can check this through st_crs

#4

n <- nrow(solar_predictions)
batch_size <- 5000
batch_ids <- split(1:n, ceiling(seq_along(1:n) / batch_size))

#naming the number of rows in solar_prediction 'n'
#naming batch_size in preparation for saying we want to process 
#batches of 5,000 rows at a time
# 1:n means process all rows in solar, seq_along(1:n) is safer than (1:n) by itself
# ceiling(...) is basically to make the batches look nice - it rounds decimal to 
# next highest number
# split(1:n,...) most importantly groups of the batches of 1-5000, 5001-10000
# into these batches

results <- vector("list", length(batch_ids))

for (i in seq_along(batch_ids)) {
  rows <- batch_ids[[i]]
  results[[i]] <- st_join(solar_predictions[rows, ], oahu_med_household_income, join = st_intersects, largest = TRUE)
  cat("Batch", i, "of", length(batch_ids), "done\n")
}

# for loop to go through 102000 detections, st_join behaves like a left_join btw
#if we werent using batch and if we only were using st_join()
#left_join means include entire left table and overlapping with right table
#right_join is vice versa, and inner_join is only overlapping both
#remember st_join is like a left_join
#st_join(left table, right table, join = st_intersects (this means inside of tract), largest = TRUE)
#largest = TRUE means assign row to tract that overlaps more with, if no largest =
# then row would go into 2 tracts that they overlap
#cat() line is basically just the text that displays after a batch is finished running
solar_with_tract <- bind_rows(results)

#bind_rows stacks rows together one on top of each other

#5

# might want to keep this code chunk -> 
#solar_predictions_test <- solar_predictions %>% 
#filter(detection_id == 1)
#st_area(solar_predictions_test$geometry)
#this was to simply see if areas matched between our given from start and what geometry has
st_crs(solar_with_tract)

solar_with_tract_proj <- st_transform(solar_with_tract, 32604)

## again st_transform to convert to 32604
#we did an earlier st_transform from step 3:
# solar_predictions <- st_transform(solar_predictions, st_crs(oahu_med_household_income))
#what this did was convert WGS84-NAD83, which are both coordinates
#to find what we eventually want which is area_m2

solar_with_tract_proj <- solar_with_tract_proj %>%
  mutate(area_m2_recalc = as.numeric(st_area(geometry)))

#as.numeric used to rm any units from st_area
# mutate adds new column and names it - again we are recalc because we don't trust areas given

#6

###beneath this is a diagnostic check (optional to keep)
# Diagnostic check: comparing original area_m2 (from source file) vs. recalculated area from geometry
# this tells me that every single detection is off from the area given in predictions and polygon

solar_with_tract_proj %>%
  st_drop_geometry() %>%
  filter(area_m2 != area_m2_recalc) %>%
  nrow()


tract_summary <- solar_with_tract_proj %>%
  st_drop_geometry() %>%
  group_by(GEOID) %>%
  summarise(total_solar_area_m2 = sum(area_m2_recalc, na.rm = TRUE))

### THIS IS THE ONLY IMPORTANT IN 6
#rename proj to tract, st_drop geometry bc it is different from a normal column
#group_by simply for join function, lastly summarise() drops all columns not listed in ()


### optional check - THIS WILL SHOW US AVERAGE DIFFERENCE IN AREA BETWEEN GIVEN AND POLYGONS
# FOR ALL 102,614 DETECTIONS (Below) 45%%%%

solar_with_tract_proj %>%
  st_drop_geometry() %>%
  mutate(pct_diff = (area_m2 - area_m2_recalc) / area_m2_recalc * 100) %>%
  summarise(mean_pct_diff = mean(pct_diff, na.rm = TRUE))

#7
#Original way i had it, i think second is easier for me to learn
#final_med_income <- oahu_med_household_income %>%
#select(GEOID, NAME, median_income, income_moe) %>%
#left_join(tract_summary, by = "GEOID") %>%
#mutate(total_solar_area_m2 = replace_na(total_solar_area_m2, 0))

final_med_income <- mutate(
  left_join(
    select(oahu_med_household_income, GEOID, NAME, median_income, income_moe),
    tract_summary,
    by = "GEOID"
  ),
  total_solar_area_m2 = round(replace_na(total_solar_area_m2, 0),0)
)

### Pre map code 

# Step 1: Households per tract (Helps normalize-each tract will have diff amount of homes in it)

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

#Step 2 Filter out solar farms (we are trying to remove solar farms and focus on households)
### Will create categories for major jumps in area_m2_recalc that will indicate likelihood of a solar farm

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
#Map 1 (Chloroploeth )

final_med_income <- st_as_sf(final_med_income)

mapview(
  final_med_income,
  zcol = "solar_per_household",
  col.regions = viridisLite::magma(100),
  layer.name = "Rooftop Solar Area per Household (m²)",
  alpha.regions = 0.8,
  label = final_med_income$NAME,
  at = quantile(final_med_income$solar_per_household, probs = seq(0, 1, 0.125), na.rm = TRUE)
)

#8

#Main-more relevant to us (spearman)
# Solar per household normalized

cor.test(final_med_income$median_income, final_med_income$solar_per_household, 
         method = "spearman", use = "complete.obs")

# Total area Solar - Secondary- (spearman)
cor.test(final_med_income$median_income, final_med_income$total_solar_area_m2, 
         method = "spearman", use = "complete.obs")

#Secondary- (pearson)

cor.test(final_med_income$median_income, final_med_income$total_solar_area_m2, 
         method = "pearson", use = "complete.obs")

#this code helps us see the strength of relationship-- Correlation coefficient
#cor.test() tests, $ helps pick 2 columns, "spearman" is type of correlation coefficient
# spearman most relevant to us (rho)
# use = complete.obs helps us get rid of irreleavnt or na rows
# if run this code sum(is.na(final_med_income$median_income)) = 19, meaning 19 tracts have NA med_income
#we can see that our rho is about 0.40, showing a moderate correlation between our variables

ggplot(data = final_med_income, aes(x = median_income, y = solar_per_household)) +
  geom_point(alpha = 0.5, color = "#2C6E91") +
  geom_smooth(method = "lm", color = "red") +
  scale_y_log10(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Rooftop Solar Area per Household vs. Median Household Income by Tract",
       x = "Median Household Income ($)",
       y = "Rooftop Solar Area per Household (m²), log scale")


ggplot(data = final_med_income, aes(x = median_income, y = total_solar_area_m2)) +
  geom_point(alpha = 0.5, color = "#2C7873") +
  geom_smooth(method = "lm", color = "red") +
  scale_y_log10(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Solar Detection Area vs. Median Household Income by Tract",
       x = "Median Household Income ($)",
       y = "Total Solar Detection Area (m²), log scale")

### Home value

oahu_home_value <- get_acs(
  geography = "tract", variables = "B25077_001",
  state = "HI", county = "Honolulu", geometry = FALSE, year = 2022
) %>% rename(home_value = estimate) %>% select(GEOID, home_value)

final_med_income <- final_med_income %>% left_join(oahu_home_value, by = "GEOID")

cor.test(final_med_income$home_value, final_med_income$solar_per_household, method = "spearman", use = "complete.obs")
cor.test(final_med_income$home_value, final_med_income$total_solar_area_m2, method = "spearman", use = "complete.obs")

ggplot(data = final_med_income, aes(x = home_value, y = solar_per_household)) +
  geom_point(alpha = 0.5, color = "#8E5572") +
  geom_smooth(method = "lm", color = "red") +
  scale_y_log10(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Rooftop Solar Area per Household vs. Median Home Value by Tract",
       x = "Median Home Value ($)", y = "Rooftop Solar Area per Household (m²), log scale")

### NHPI population (as a % of tract population, not raw count)

oahu_nhpi <- get_acs(
  geography = "tract", variables = "B02001_006",
  state = "HI", county = "Honolulu", geometry = FALSE, year = 2022
) %>% rename(nhpi_pop = estimate) %>% select(GEOID, nhpi_pop)

oahu_total_pop <- get_acs(
  geography = "tract", variables = "B01003_001",
  state = "HI", county = "Honolulu", geometry = FALSE, year = 2022
) %>% rename(total_pop = estimate) %>% select(GEOID, total_pop)

final_med_income <- final_med_income %>%
  select(-any_of(c("nhpi_pop", "total_pop", "nhpi_pop.x", "nhpi_pop.y", "total_pop.x", "total_pop.y"))) %>%
  left_join(oahu_nhpi, by = "GEOID") %>%
  left_join(oahu_total_pop, by = "GEOID") %>%
  mutate(nhpi_pct = round(nhpi_pop / total_pop * 100, 2))

cor.test(final_med_income$nhpi_pct, final_med_income$solar_per_household, method = "spearman", use = "complete.obs")

ggplot(data = final_med_income, aes(x = nhpi_pct, y = solar_per_household)) +
  geom_point(alpha = 0.5, color = "#4C7A5A") +
  geom_smooth(method = "lm", color = "red") +
  scale_y_log10(labels = scales::comma) +
  labs(title = "Rooftop Solar Area per Household vs. NHPI Population % by Tract",
       x = "Native Hawaiian/Pacific Islander Population (%)",
       y = "Rooftop Solar Area per Household (m²), log scale")

###issues we still have
#Also worth remembering (came up in your most recent solar chat): 
#there was a data quality issue where the area_m2 column in oahu_solar_simple.geojson doesn't
#match the actual polygon geometry across almost the whole dataset. The recommendation was to just use your own recalculated 
#area (area_m2_recalc, from st_area() after reprojecting) instead of the original column, 
#and note the discrepancy as a finding rather than trying to "fix" it in code.

##Map 2 Bivariate (income)

#***Bivariate Map***#
#*#Step 2: Build popup text
final_med_income$popup <- glue::glue(
  "<strong>GEOID: </strong>{final_med_income$GEOID}<br>",
  "<strong>Solar Area per Household (m2): </strong>{final_med_income$solar_per_household}<br>",
  "<strong>Median household income: </strong>${format(round(final_med_income$median_income), big.mark = ',')}"
)
#Step 3: Create the bivariate color scale
income_solar_scale <- bivariate_scale(
  data = final_med_income,
  x = "median_income",
  y = "solar_per_household",
  na_color = "white",
  palette = "purple_orange"
)

#Step 4: Build the map
maplibre(
  style = carto_style("positron"),
  bounds = final_med_income
) |>
  add_fill_layer(
    id = "income_solar",
    source = final_med_income,
    fill_color = income_solar_scale$expression,
    fill_opacity = 0.75,
    fill_outline_color = "rgba(255,255,255,0.25)",
    popup = "popup",
    hover_options = list(fill_opacity = 1)
  ) |>
  add_bivariate_legend(
    income_solar_scale,
    legend_title = "Income and Solar",
    x_title = "Higher income",
    y_title = "Higher Solar per Household",
    layer_id = "HI_income_solar",
    style = legend_style(
      background_color = "white",
      background_opacity = 0.94,
      border_color = "#D1D5DB",
      border_width = 1,
      shadow = TRUE
    )
  )

## Map 3 Bivariate (home value)

final_med_income$popup_value <- glue::glue(
  "<strong>GEOID: </strong>{final_med_income$GEOID}<br>",
  "<strong>Solar Area per Household (m2): </strong>{final_med_income$solar_per_household}<br>",
  "<strong>Median home value: </strong>${format(round(final_med_income$home_value), big.mark = ',')}"
)

value_solar_scale <- bivariate_scale(
  data = final_med_income, x = "home_value", y = "solar_per_household",
  na_color = "white", palette = "purple_orange"
)

maplibre(style = carto_style("positron"), bounds = final_med_income) |>
  add_fill_layer(
    id = "value_solar", source = final_med_income,
    fill_color = value_solar_scale$expression, fill_opacity = 0.75,
    fill_outline_color = "rgba(255,255,255,0.25)", popup = "popup_value",
    hover_options = list(fill_opacity = 1)
  ) |>
  add_bivariate_legend(
    value_solar_scale, legend_title = "Home Value and Solar",
    x_title = "Higher home value", y_title = "Higher Solar per Household",
    layer_id = "HI_value_solar",
    style = legend_style(background_color = "white", background_opacity = 0.94,
                         border_color = "#D1D5DB", border_width = 1, shadow = TRUE)
  )

## Map 4 Bivariate (NHPI %)

final_med_income$popup_nhpi <- glue::glue(
  "<strong>GEOID: </strong>{final_med_income$GEOID}<br>",
  "<strong>Solar Area per Household (m2): </strong>{final_med_income$solar_per_household}<br>",
  "<strong>NHPI population %: </strong>{final_med_income$nhpi_pct}%"
)

nhpi_solar_scale <- bivariate_scale(
  data = final_med_income, x = "nhpi_pct", y = "solar_per_household",
  na_color = "white", palette = "purple_orange"
)

maplibre(style = carto_style("positron"), bounds = final_med_income) |>
  add_fill_layer(
    id = "nhpi_solar", source = final_med_income,
    fill_color = nhpi_solar_scale$expression, fill_opacity = 0.75,
    fill_outline_color = "rgba(255,255,255,0.25)", popup = "popup_nhpi",
    hover_options = list(fill_opacity = 1)
  ) |>
  add_bivariate_legend(
    nhpi_solar_scale, legend_title = "NHPI % and Solar",
    x_title = "Higher NHPI %", y_title = "Higher Solar per Household",
    layer_id = "HI_nhpi_solar",
    style = legend_style(background_color = "white", background_opacity = 0.94,
                         border_color = "#D1D5DB", border_width = 1, shadow = TRUE)
  )









