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

# Calculate edge data
overlaps <-
  getSetIntersections(movieSets, setNames, idName) %>%
  filter(as.numeric(set1) < as.numeric(set2)) %>%
  arrange(desc(prop))

sets <- levels(overlaps$set1)
m <- length(sets)

chain <- list()
for(i in 1:m) {
  chain[i] <- sets[i]
}

pb <- txtProgressBar(min = 1, max = m)
for(overlap in 1:nrow(overlaps)) {

  j1 <- overlaps[[overlap, "set1"]]
  j2 <- overlaps[[overlap, "set2"]]
  c1 <- chain[[j1]]
  c2 <- chain[[j2]]
  if (!isTRUE(all.equal(c1,c2))) {

    setOrder <- list()
    setOrder[[1]] <- unique(c(c1, c2))
    setOrder[[2]] <- unique(c(c1, rev(c2)))
    setOrder[[3]] <- unique(c(c2, c1))
    setOrder[[4]] <- unique(c(c2, rev(c1)))

    ink <- rep(0, 4)
    for (i in 1:4) {

      # Build plot
      buildRadialSetsPlot(
        setSizes,
        setSizesByDegree,
        setIntersections,
        focusSet = setOrder[[i]],
        setOrder = setOrder[[i]]
      )

      # Radial sets data
      radialSetsData <-
        getRadialSetsData(setSizes,
                          setSizesByDegree,
                          setIntersections,
                          focusSet = setOrder[[i]],
                          setOrder = setOrder[[i]])

      # Plot metadata
      metadata <- getRadialSetsMetadata(radialSetsData)

      # Calculate link lengths
      ink[i] <- metadata$linkData %>%
        left_join(setIntersections %>%
                    select(set1, set2, w = prop),
                  by = c("set1", "set2")) %>%
        filter(set1 %in% setOrder[[i]],
               set2 %in% setOrder[[i]]) %>%
        mutate(dx = lead(x) - x,
               dy = lead(y) - y,
               ds = sqrt(dx ^ 2 + dy ^ 2),
               dInk = ds*w) %>%
        summarize(ink = sum(dInk, na.rm = TRUE)) %>%
        pull(ink)
    }

    index <- which(ink == min(ink))[1]
    chain[j1] <- setOrder[index]
    chain[j2] <- setOrder[index]

    if(length(setOrder[[index]]) == m) {
      optOrder <- setOrder[[index]]
      close(pb)
      break
    }
  }

  setTxtProgressBar(pb, length(setOrder[[index]]))

}


svg("PlotSVG.svg", width = 12, height = 12)
buildRadialSetsPlot(
  setSizes,
  setSizesByDegree,
  setIntersections,
  setOrder = c("Drama", "Romance", "Comedy", "Musical", "Animation", "Children",
               "Fantasy", "Adventure", "Action", "Mystery", "War", "Sci-Fi",
               "Thriller", "Crime", "Horror", "Film-Noir", "IMAX", "Western",
               "Documentary"),
  # axisLabels = T,
  # axisLabelFontSize = 0.5,
  # majorTick = T,
  # countScale = 1,
  disPropLim = c(-1, 1),
  # facing = "downward"
  # focusSet = "Drama",
  sectorColor = "white"
)
dev.off()

edgesMat <-
  setIntersections %>%
  arrange(set1, set2) %>%
  select(set1, set2, prop1) %>%
  spread(set2, prop1) %>%
  select(-set1) %>%
  as.matrix()
diag(edgesMat) <- 0
rownames(edgesMat) <- levels(setIntersections$set1)

edgeReorder <- corrplot(edgesMat, order = "hclust", addrect = 4)

buildRadialSetsPlot(
  setSizes,
  setSizesByDegree,
  setIntersections,
  setOrder = rownames(edgeReorder),
  disPropLim = c(-1, 1),
  # facing = "downward"
  # focusSet = "Comedy",
  sectorColor = "white"
)

