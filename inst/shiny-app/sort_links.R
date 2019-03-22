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
  filter(set1 != set2) %>%
  arrange(desc(prop))

sets <- levels(overlaps$set1)
m <- length(sets)

chain <- list()
for(i in 1:m) {
  chain[i] <- sets[i]
}

pb <- txtProgressBar(min = 1, max = nrow(overlaps))
for(i in 1:nrow(overlaps)) {

  c1 <- chain[[overlaps$set1[i]]]
  c2 <- chain[[overlaps$set2[i]]]
  if (!isTRUE(all.equal(c1,c2))) {
    cTemp <- list()
    cTemp[[1]] <-
      unique(c(chain[[overlaps$set1[i]]], chain[[overlaps$set2[i]]]))
    cTemp[[2]] <-
      unique(c(chain[[overlaps$set1[i]]], rev(chain[[overlaps$set2[i]]])))
    cTemp[[3]] <-
      unique(c(chain[[overlaps$set2[i]]], chain[[overlaps$set1[i]]]))
    cTemp[[4]] <-
      unique(c(chain[[overlaps$set2[i]]], rev(chain[[overlaps$set1[i]]])))

    s <- rep(0, 4)
    for (j in 1:4) {
      buildRadialSetsPlot(
        setSizes,
        setSizesByDegree,
        setIntersections,
        focusSet = cTemp[[j]],
        setOrder = cTemp[[j]]
      )

      metadata <- getRadialSetsMetadata(radialSetsData)

      s[j] <- metadata$linkData %>%
        filter(set1 %in% cTemp[[j]],
               set2 %in% cTemp[[j]]) %>%
        mutate(dx = lead(x) - x,
               dy = lead(y) - y,
               ds = sqrt(dx ^ 2 + dy ^ 2)) %>%
        summarize(s = sum(ds, na.rm = TRUE)) %>%
        pull(s)
    }

    index <- which(s == min(s))[1]
    chain[overlaps$set1[i]] <- cTemp[index]
    chain[overlaps$set2[i]] <- cTemp[index]

    if(length(cTemp[[index]]) == m) {
      close(pb)
      break
    }
  }

  setTxtProgressBar(pb, i)

}
optOrder <- cTemp[[index]]

svg("PlotSVG.svg", width = 12, height = 12)
buildRadialSetsPlot(
  setSizes,
  setSizesByDegree,
  setIntersections,
  setOrder = c("Musical", "Animation", "Children", "Fantasy", "Adventure",
    "Action", "Horror", "Crime", "Thriller", "Sci-Fi", "War", "Mystery",
    "Drama", "Romance", "Comedy", "Film-Noir", "Western", "IMAX",
    "Documentary"),
  # axisLabels = T,
  # axisLabelFontSize = 0.5,
  # majorTick = T,
  # countScale = 1,
  disPropLim = c(-1, 1),
  # facing = "downward"
  focusSet = "Drama",
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

# alg 2 -------------------------------------------------------------------

sets <- levels(overlaps$set1)
setOrder <- c(overlaps$set1[1], overlaps$set2[1])
for (i in 2:nrow(overlaps)) {
  set1 <- overlaps$set1[i]
  set2 <- overlaps$set2[i]
  newSets <- c(set1, set2)
  newSets <- newSets[!newSets %in% setOrder]
  if (length(newSets) > 0) {
    cTemp <- list()
    cTemp[[1]] <- c(setOrder, newSets)
    cTemp[[2]] <- c(setOrder, rev(newSets))
    cTemp[[3]] <- c(newSets, setOrder)
    cTemp[[4]] <- c(newSets, rev(setOrder))

    s <- rep(0, 4)
    for (j in 1:4) {
      buildRadialSetsPlot(
        setSizes,
        setSizesByDegree,
        setIntersections,
        focusSet = as.character(sets[cTemp[[j]]]),
        setOrder = as.character(sets[cTemp[[j]]])
      )

      metadata <- getRadialSetsMetadata(radialSetsData)

      s[j] <- metadata$linkData %>%
        filter(set1 %in% as.character(sets[cTemp[[j]]]),
               set2 %in% as.character(sets[cTemp[[j]]])) %>%
        mutate(dx = lead(x) - x,
               dy = lead(y) - y,
               ds = sqrt(dx ^ 2 + dy ^ 2)) %>%
        summarize(s = sum(ds, na.rm = TRUE)) %>%
        pull(s)
    }

    browser()

    as.character(sets[newSets])
    as.character(sets[setOrder])
    as.character(sets[cTemp[[index]]])

    index <- which(s == min(s))[1]
    setOrder <- cTemp[[index]]

    if (length(setOrder) == m) {
      break
    }
  }

}

optOrder <- as.character(sets[cTemp[[index]]])
buildRadialSetsPlot(
  setSizes,
  setSizesByDegree,
  setIntersections,
  focusSet = "Action",
  setOrder = optOrder
)
