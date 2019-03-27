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

          # Calculate node positions
          nodes <-
            setSizes %>%
            mutate(set = forcats::fct_relevel(set, setOrder[[i]])) %>%
            arrange(set) %>%
            mutate(Ntotal = sum(N),
                   pad = 1) %>%
            mutate(theta = N/Ntotal * (360 - sum(pad)),
                   r = 0.47,
                   theta2 = 360 - (cumsum(theta) + cumsum(pad) - 1),
                   theta1 = theta2 + theta,
                   thetaC = theta1 + (theta2 - theta1)/2,
                   xc = r*cos(thetaC*(pi/180)),
                   yc = r*sin(thetaC*(pi/180))) %>%
            mutate(set = as.character(set))

          # Calculate ink used to plot links
          ink[i] <-
            setIntersections %>%
            filter(set1 %in% setOrder[[i]],
                   set2 %in% setOrder[[i]]) %>%
            filter(set1 != set2) %>%
            select(set1, set2, w = linkThickness) %>%
            mutate(set1 = as.character(set1),
                   set2 = as.character(set2)) %>%
            left_join(nodes %>% select(set, x1 = xc, y1 = yc), by = c("set1"="set")) %>%
            left_join(nodes %>% select(set, x2 = xc, y2 = yc), by = c("set2"="set")) %>%
            mutate(dx = x2 - x1,
                   dy = x2 - x1,
                   s = sqrt(dx^2 + dy^2),
                   ink = s*w) %>%
            summarize(ink = sum(ink)) %>%
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


