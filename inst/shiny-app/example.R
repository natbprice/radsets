library(tidyverse)
library(AlgDesign)
library(purrr)



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
  getRadialSetsData(setSizes, setSizesByDegree, setIntersections)

buildRadialSetsPlot(setSizes, setSizesByDegree, setIntersections)

metadata <- getRadialSetsMetadata(radialSetsData)


# png("Plot3.png", width = 2100, height = 2100, units = 'px', res = 300)
library(svglite)
svglite("PlotSVG.svg", width = 4, height = 4)
buildRadialSetsPlot(setSizes, setSizesByDegree, setIntersections, focusSet = "Sci-Fi")
dev.off()

png("PlotPng.png", width = 4*96, height = 4*96)
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
