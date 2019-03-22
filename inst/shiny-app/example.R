library(tidyverse)
library(AlgDesign)
library(purrr)
library(svglite)


# Movies ------------------------------------------------------------------

# Load sample data
data(movieSets)

# Define set names (user specified)
setNames <- movieSets %>%
  select(Action:Western) %>%
  colnames()

# Define ID column (user specified)
idName <- "movieId"

# Define max degree (user specified)
maxDegree <- 4

# Calculate set sizes
setSizes <-
  getSetSizes(movieSets, setNames)

# Calculate set sizes by degree
setSizesByDegree <-
  getSetSizesByDegree(movieSets, setNames, idName)

# Calculate edge data
setIntersections <-
  getSetIntersections(movieSets, setNames, idName)

radialSetsData <-
  getRadialSetsData(setSizes,
                    setSizesByDegree,
                    setIntersections,
                    focusSet = c("Action", "Drama"),
                    setOrder = rev(levels(setSizes$set)))

buildRadialSetsPlot(setSizes,
                    setSizesByDegree,
                    setIntersections,
                    focusSet = c("Action", "Film-Noir"),
                    setOrder = "Comedy")

metadata <- getRadialSetsMetadata(radialSetsData)

metadata$linkData %>%
  mutate(dx = lead(x) - x,
         dy = lead(y) - y,
         ds = sqrt(dx^2 + dy^2)) %>%
  group_by(set1, set2) %>%
  summarize(s = sum(ds, na.rm = TRUE))


# png("Plot3.png", width = 2100, height = 2100, units = 'px', res = 300)

svg("PlotSVG.svg", width = 12, height = 12)
par(mar=rep(0,4))
buildRadialSetsPlot(setSizes, setSizesByDegree, setIntersections)
dev.off()

png("PlotPng.png", width = 4*96, height = 4*96)
par(mar=rep(0,4))
buildRadialSetsPlot(setSizes, setSizesByDegree, setIntersections, focusSet = "Sci-Fi")
dev.off()

# ecommerce ---------------------------------------------------------------

# Load sample data
data("ecommerce")

# Define set names (user specified)
setNames <- ecommerce %>%
  select(-CustomerID) %>%
  colnames() %>%
  sample(10)

# Define ID column (user specified)
idName <- "CustomerID"

# Define max degree (user specified)
maxDegree <- 4

# Calculate set sizes
setSizes <-
  getSetSizes(ecommerce, setNames)

# Calculate set sizes by degree
setSizesByDegree <-
  getSetSizesByDegree(ecommerce, setNames, idName)

# Calculate edge data
setIntersections <-
  getSetIntersections(ecommerce, setNames, idName)

radialSetsData <-
  getRadialSetsData(setSizes, setSizesByDegree, setIntersections)

buildRadialSetsPlot(setSizes,
                    setSizesByDegree,
                    setIntersections,
                    maxLinkThickness = 10,
                    showLegend = F,
                    facing = "in")
