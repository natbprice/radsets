devtools::install_github("https://github.com/hadley/usdanutrients")
library(usdanutrients)
data("food_group")
data("food")
data("nutrient")
data("nutrient_source")
data("deriv")
data("source_type")

glimpse(food)
glimpse(food_group)
glimpse(nutrient)
glimpse(nutrient_source)
glimpse(deriv)
glimpse(source_type)

foodGroups <-
  nutrient %>%
  group_by(nutr_id) %>%
  mutate(value75 = quantile(value, 0.75)) %>%
  ungroup() %>%
  filter(value >= value75,
         nutr_id %in% c(203, 204, 205)) %>%
  distinct(food_id, nutr_id) %>%
  left_join(food %>%
              select(food_id, grp_id),
            by = "food_id") %>%
  left_join(food_group %>%
              select(grp_id, group),
            by = "grp_id") %>%
  distinct(group, nutr_id) %>%
  mutate(value = 1) %>%
  spread(group, value, fill = 0)

# Define set names (user specified)
setNames <- foodGroups %>%
  select(-nutr_id) %>%
  colnames()

# Define ID column (user specified)
idName <- "nutr_id"

# Define max degree (user specified)
maxDegree <- 4

# Calculate set sizes
setSizes <-
  getSetSizes(foodGroups, setNames)

# Calculate set sizes by degree
setSizesByDegree <-
  getSetSizesByDegree(foodGroups, setNames, idName)

# Calculate edge data
setIntersections <-
  getSetIntersections(foodGroups, setNames, idName)

radialSetsData <-
  getRadialSetsData(setSizes,
                    setSizesByDegree,
                    setIntersections, maxPlotWidth = 5, minPlotWidth = 1)

buildRadialSetsPlot(setSizes,
                    setSizesByDegree,
                    setIntersections,
                    maxPlotWidth = 5,
                    focusSet = "Beef Products")
