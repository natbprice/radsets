#' Optimize link order
#'
#' Optimize link order to minimize ink
#'
#' @inheritParams getRadialSetsData
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
optimizeLinkOrder <- function(setSizes,
                              setSizesByDegree,
                              setIntersections,
                              linkThickness = "prop") {

    # Overlap between sets sorted largest to smallest
    overlaps <-
      setIntersections %>%
      filter(as.numeric(.data$set1) < as.numeric(.data$set2)) %>%
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

          # Calculate node positions
          nodes <-
            setSizes %>%
            mutate(set = forcats::fct_relevel(.data$set, setOrder[[i]])) %>%
            arrange(.data$set) %>%
            mutate(Ntotal = sum(.data$N),
                   pad = 1) %>%
            mutate(
              theta = .data$N / .data$Ntotal * (360 - sum(.data$pad)),
              r = 0.47,
              theta2 = 360 - (cumsum(.data$theta) + cumsum(.data$pad) - 1),
              theta1 = .data$theta2 + .data$theta,
              thetaC = .data$theta1 + (.data$theta2 - .data$theta1) / 2,
              xc = .data$r * cos(.data$thetaC * (pi / 180)),
              yc = .data$r * sin(.data$thetaC * (pi / 180))
            ) %>%
            mutate(set = as.character(.data$set))

          # Calculate ink used to plot links
          ink[i] <-
            setIntersections %>%
            filter(set1 %in% setOrder[[i]],
                   set2 %in% setOrder[[i]]) %>%
            filter(set1 != set2) %>%
            select(set1, set2, w = linkThickness) %>%
            mutate(set1 = as.character(.data$set1),
                   set2 = as.character(.data$set2)) %>%
            left_join(nodes %>% select(.data$set, x1 = .data$xc, y1 = .data$yc),
                      by = c("set1" = "set")) %>%
            left_join(nodes %>% select(.data$set, x2 = .data$xc, y2 = .data$yc),
                      by = c("set2" = "set")) %>%
            mutate(
              dx = .data$x2 - .data$x1,
              dy = .data$x2 - .data$x1,
              s = sqrt(.data$dx ^ 2 + .data$dy ^ 2),
              ink = .data$s * .data$w
            ) %>%
            summarize(ink = sum(.data$ink)) %>%
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
