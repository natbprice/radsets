#' Optimize link order
#'
#' Optimize link order to minimize ink
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @export
optimizeLinkOrder <- function(setSizes,
                              setSizesByDegree,
                              setIntersections,
                              linkThickness = "prop") {

    # Overlap between sets sorted largest to smallest
    overlaps <-
      setIntersections %>%
      filter(as.numeric(set1) < as.numeric(set2)) %>%
      arrange(desc(!!sym(linkThickness)))

    # Initialize chains of sets
    sets <- levels(overlaps$set1)
    m <- length(sets)
    chain <- list()
    for (i in 1:m) {
      chain[i] <- sets[i]
    }

    # Loop over overlaps
    for (overlap in 1:nrow(overlaps)) {

      # Pair of sets
      j1 <- overlaps[[overlap, "set1"]]
      j2 <- overlaps[[overlap, "set2"]]
      c1 <- chain[[j1]]
      c2 <- chain[[j2]]


      if (!isTRUE(all.equal(c1, c2))) {
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
            setOrder = setOrder[[i]],
            linkThickness = linkThickness,
          )

          # Radial sets data
          radialSetsData <-
            getRadialSetsData(
              setSizes = setSizes,
              setSizesByDegree = setSizesByDegree,
              setIntersections = setIntersections,
              linkThickness = linkThickness,
              focusSet = setOrder[[i]],
              setOrder = setOrder[[i]]
            )

          # Plot metadata
          metadata <- getRadialSetsMetadata(radialSetsData)

          # Calculate link lengths
          ink[i] <- metadata$linkData %>%
            left_join(setIntersections %>%
                        select(set1, set2, w = linkThickness),
                      by = c("set1", "set2")) %>%
            filter(set1 %in% setOrder[[i]],
                   set2 %in% setOrder[[i]]) %>%
            mutate(
              dx = lead(x) - x,
              dy = lead(y) - y,
              ds = sqrt(dx ^ 2 + dy ^ 2),
              dInk = abs(ds * w)
            ) %>%
            summarize(ink = sum(dInk, na.rm = TRUE)) %>%
            pull(ink)
        }

        index <- which(ink == min(ink))[1]
        chain[j1] <- setOrder[index]
        chain[j2] <- setOrder[index]

        if (length(setOrder[[index]]) == m) {
          optOrder <- setOrder[[index]]
          break
        }
      }

    }

    return(optOrder)
}


