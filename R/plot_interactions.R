#' Get Radial Sets metadata
#'
#' Build tooltip for radial sets network plot
#'
#' @param plotData A data frame with data used in plot
#' @param hover A named list created by \code{\link[shiny]{plotOutput}}
#'
#' @return A tooltip created as a panel using \code{\link[shiny]{wellPanel}}
#'
#' @import shiny
#' @import snakecase
#' @import dplyr
#' @importFrom glue glue
#' @importFrom purrr map pmap
#' @importFrom tidyr unnest
#'
#' @export
getRadialSetsMetadata <- function(radialSetsData, bezierW = 1, bezierHRatio = 0.5) {

  # Unpack data
  edgeWidth <- radialSetsData$edgeWidth
  sets <- radialSetsData$sets
  nSets <- radialSetsData$nSets
  maxDegree <- radialSetsData$maxDegree
  degreeMat <- radialSetsData$degreeMat
  setSizesVec <- radialSetsData$setSizesVec
  maxWidth <- radialSetsData$maxWidth


  # Sector data -----------------------------------------------------------
  sectorData <-
    tibble(set = get.all.sector.index()) %>%
    mutate(
      theta = purrr::map(
        set,
        ~ get.cell.meta.data("xplot", sector.index = .x, track.index = 2) %>%
          t() %>%
          as_tibble() %>%
          rename(theta1 = V1, theta2 = V2)
      ),
      r = purrr::map(
        set,
        ~ get.cell.meta.data("yplot", sector.index = .x, track.index = 2) %>%
          t() %>%
          as_tibble() %>%
          rename(r1 = V1, r2 = V2)
      ),
      s = purrr::map(
        set,
        ~ get.cell.meta.data("xlim", sector.index = .x, track.index = 2) %>%
          t() %>%
          as_tibble() %>%
          rename(s1 = min.data, s2 = max.data)
      )
    ) %>%
    tidyr::unnest() %>%
    mutate(theta1 = if_else(theta1 == 0, 360, theta1),
           set = factor(set, levels = levels(sets)))

  # Bar data --------------------------------------------------------------
  barData <-
    as_tibble(degreeMat) %>%
    mutate(set = factor(rownames(degreeMat), levels = levels(sets))) %>%
    gather(degree, s, -set) %>%
    mutate(degree = as.numeric(degree)) %>%
    left_join(sectorData %>%
                select(set,
                       theta1.sec = theta1,
                       theta2.sec = theta2,
                       s1.sec = s1,
                       s2.sec = s2,
                       r1.sec = r1,
                       r2.sec = r2),
              by = "set") %>%
    mutate(thetaCenter = theta2.sec + (theta1.sec - theta2.sec)/2,
           sCenter = s1.sec + (s2.sec - s1.sec)/2,
           sToTheta= (theta1.sec - theta2.sec) / (s2.sec - s1.sec),
           theta1 = thetaCenter + (s*sToTheta)/2,
           theta2 = thetaCenter - (s*sToTheta)/2,
           s1 = sCenter - s/2,
           s2 = sCenter + s/2,
           r1 = r2.sec - degree*((r2.sec - r1.sec)/maxDegree),
           r2 = r2.sec - (degree-1)*((r2.sec - r1.sec)/maxDegree)) %>%
    select(set, degree, theta1, theta2, r1, r2, s1, s2)

  # Link data -------------------------------------------------------------
  linkData <- as_tibble(edgeWidth) %>%
    mutate(set1 = factor(rownames(edgeWidth), levels = levels(sets))) %>%
    gather(set2, edge, -set1) %>%
    mutate(set2 = factor(set2, levels = levels(sets))) %>%
    mutate_all(as.factor) %>%
    filter(edge != 0) %>%
    select(set1, set2) %>%
    left_join(
      sectorData %>%
        mutate(theta1 = theta2 + (theta1 - theta2) / 2) %>%
        select(set, theta1, r1 = r1),
      by = c("set1" = "set")
    ) %>%
    left_join(
      sectorData %>%
        mutate(theta2 = theta2 + (theta1 - theta2) / 2) %>%
        select(set, theta2, r2 = r1),
      by = c("set2" = "set")
    ) %>%
    mutate(link = purrr::pmap(
      list(
        theta1 = theta1,
        theta2 = theta2,
        rou1 = r1,
        rou2 = r2
      ),
      ~ circlize:::getQuadraticPoints(..1, ..2, ..3, ..4,
                                      w = bezierW,
                                      h.ratio = bezierHRatio) %>% as_tibble()
    )) %>%
    tidyr::unnest() %>%
    rename(x = V1, y = V2) %>%
    select(set1, set2, x, y)

  metadata <- list(sectorData = sectorData,
                   barData = barData,
                   linkData = linkData)

  return(metadata)
}

#' Get Radial Sets pointer location
#'
#' Build tooltip for radial sets network plot
#'
#' @param plotData A data frame with data used in plot
#' @param hover A named list created by \code{\link[shiny]{plotOutput}}
#'
#' @return A tooltip created as a panel using \code{\link[shiny]{wellPanel}}
#'
#' @import shiny
#' @import snakecase
#' @import dplyr
#' @importFrom glue glue
#'
#' @export
getPointerLoc <- function(metadata,
                          pointer,
                          plotDomain = list(left = -1.08, right = 1.08,
                                            bottom = -1.08, top = 1.08),
                          transCoord = FALSE) {

  # Define pointer location
  pointerLoc <- list(location = NULL,
                     pointer = pointer)

  # If mouse is not on plot, return null
  if (is.null(pointer)) return(NULL)
  if (is.null(pointer$x)) return(NULL)

  sectorData <- metadata$sectorData
  barData <- metadata$barData
  linkData <- metadata$linkData

  # Map pointer to plot dat -----------------------------------------------
  # browser()
  if(transCoord) {
    imageDomain <- pointer$domain
    xImage <- pointer$x
    yImage <- pointer$y
    x <- plotDomain$left +
      (xImage - imageDomain$left) / (imageDomain$right - imageDomain$left) *
      (plotDomain$right - plotDomain$left)
    y <- plotDomain$bottom +
      (yImage - imageDomain$bottom) / (imageDomain$top - imageDomain$bottom) *
      (plotDomain$top - plotDomain$bottom)
    pointer$x <- x
    pointer$y <- y
  } else {
    # Unpack coordinates of mouse pointer
    x <- pointer$x
    y <- pointer$y
  }
  pointerLoc$x <- x
  pointerLoc$y <- y

  # Convert pointer location to polar coordinates
  r <- sqrt(x^2+y^2)
  theta <- atan2(y,x)

  # Convert angle in radians to degrees
  if (theta>0){
    theta <- theta*(180/pi)
  } else{
    theta <- 360+theta*(180/pi)
  }

  # Check radius
  if (r < min(sectorData[["r1"]])){

    # Match pointer to link
    linkMatch <-
      linkData %>%
      mutate(dist = sqrt((x - pointer$x)^2 + (y - pointer$y)^2)) %>%
      arrange(dist) %>%
      filter(dist <= 0.02) %>%
      filter(row_number() == 1)

    # Return early or update location
    if (nrow(linkMatch) == 0) {
      return(pointerLoc)
    } else {
      pointerLoc$location <- "link"
      pointerLoc$set1 <- as.character(linkMatch[["set1"]])
      pointerLoc$set2 <- as.character(linkMatch[["set2"]])
    }

  } else if (r < max(sectorData[["r2"]])){

    # Match pointer to sector
    sectorMatch <-
      sectorData %>%
      filter(theta1 > theta,
             theta2 < theta)

    # Return early or update location
    if (nrow(sectorMatch) == 0) {
      return(pointerLoc)
    } else {
      pointerLoc$location <- "sector"
      pointerLoc$set <- as.character(sectorMatch[["set"]])
    }

    # Match pointer to bar
    barMatch <-
      barData %>%
      filter(theta1 > theta,
             theta2 < theta,
             r < r2,
             r > r1)

    # Update location
    if (nrow(barMatch) == 0) {
      return(pointerLoc)
    } else {
      pointerLoc$location <- "bar"
      pointerLoc$degree <- barMatch[["degree"]]
    }
  }

  return(pointerLoc)
}

#' Tooltip for radial sets plot
#'
#' Build tooltip for radial sets network plot
#'
#' @param plotData A data frame with data used in plot
#' @param hover A named list created by \code{\link[shiny]{plotOutput}}
#'
#' @return A tooltip created as a panel using \code{\link[shiny]{wellPanel}}
#'
#' @import shiny
#' @import snakecase
#' @import dplyr
#' @importFrom glue glue
#'
#' @export
createRadialsetsTooltip <- function(setSizes,
                                    setSizesByDegree,
                                    setIntersections,
                                    pointerLoc,
                                    focusSet = "none",
                                    linkThickness = "prop") {

  location <- pointerLoc$location
  pointer <- pointerLoc$pointer
  if(is.null(location)) {
   return(NULL)
  }

  # Maximum degree
  maxDegree <- max(setSizesByDegree[["degree"]])

  # If pointer is in center of plot, return tooltip for links
  if (location == "link") {
    # Create string displaying overlap for given link
    name1 <- pointerLoc$set1
    name2 <- pointerLoc$set2
    linkName <- paste0(name1,
                       ifelse(focusSet == "none", " and ", " to "),
                       name2)

    overlap <-
      setIntersections %>%
      filter(set1 == pointerLoc$set1,
             set2 == pointerLoc$set2) %>%
      pull(linkThickness)

    if (linkThickness %in% c("prop", "prop1", "prop.relError")) {
      overlap <- overlap * 100
    }
    overlap <- round(overlap)
    if (focusSet == "none") {
      if (linkThickness %in% c("prop", "prop1", "prop.relError")) {
        label <-
          glue("{overlap}% of all {name1} and {name2} </br>items belong to both sets")
      } else {
        label <-
          glue("{overlap} items belong to both</br>{name1} and {name2}")
      }

    } else {
      if (linkThickness %in% c("prop", "prop1", "prop.relError")) {
        label <-
          glue("{overlap}% of {name1} items</br>also belong to {name2}")
      } else {
        label <- glue("{overlap} {name1} items</br>also belong to {name2}")
      }
    }

    tooltipText <- glue("<b> {linkName} </b> <br/> {label}")

  } else if (location == "bar") {
    # Bar value
    N <- setSizesByDegree %>%
      filter(set == pointerLoc$set,
             degree == pointerLoc$degree) %>%
      pull(N)

    # Bar proportion
    propValue <- setSizesByDegree %>%
      filter(set == pointerLoc$set,
             degree == pointerLoc$degree) %>%
      mutate(prop = round(prop * 100)) %>%
      pull(prop)

    # If pointer is on histogram bar, display bar value, else display sector total
    if (location == "bar") {
      if (pointerLoc$degree == maxDegree) {
        tooltipText <- glue(
          "<b>{pointerLoc$set} </b> <br/>",
          "{pointerLoc$set} and {pointerLoc$degree-1} or more other ",
          "sets: {N} ({propValue}%)"
        )
      } else if (pointerLoc$degree == 1) {
        tooltipText <- glue("<b>{pointerLoc$set}</b> <br/>",
                            "{pointerLoc$set} only: {N} ({propValue}%)")
      } else {
        tooltipText <- glue(
          "<b>{pointerLoc$set}</b> <br/>",
          "{pointerLoc$set} and {pointerLoc$degree-1} other set(s): ",
          "{N} ({propValue}%)"
        )
      }
    }
  } else if (location == "sector") {
    # Sector value
    totalValue <- setSizes %>%
      filter(set == pointerLoc$set) %>%
      pull(N)

    tooltipText <-
      glue("<b>{pointerLoc$set}</b> <br/> {totalValue} items")
  } else {
    return(NULL)
  }

  # Calculate point position INSIDE the image as percent of total dimensions
  left_pct <- (pointer$x - pointer$domain$left) / (pointer$domain$right - pointer$domain$left)
  top_pct <- (pointer$domain$top - pointer$y) / (pointer$domain$top - pointer$domain$bottom)

  # Calculate distance from left and bottom side of the picture in pixels
  left_px <- pointer$range$left + left_pct * (pointer$range$right - pointer$range$left)
  top_px <- pointer$range$top + top_pct * (pointer$range$bottom - pointer$range$top)

  # Create style property fot tooltip (transparent background, tooltip on top)
  style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                  "left:", left_px + 2, "px; top:", top_px + 2, "px;")

  # Create tooltip as wellPanel
  tooltipPanel <- wellPanel(
    style = style,
    p(HTML(tooltipText))
  )

  return(tooltipPanel)
}
