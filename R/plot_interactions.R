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
#'
#' @export
getRadialSetsMetadata <- function(networkData) {

  # Unpack data
  edges <- networkData$edges
  sets <- networkData$sets
  nSets <- networkData$nSets
  maxDegree <- networkData$maxDegree
  degreeMat <- networkData$degreeMat
  setSizesVec <- networkData$setSizesVec
  maxWidth <- networkData$maxWidth


  # Sector data -----------------------------------------------------------
  sectorData <-
    tibble(set = get.all.sector.index()) %>%
    mutate(
      theta = map(
        set,
        ~ get.cell.meta.data("xplot", sector.index = .x, track.index = 2) %>%
          t() %>%
          as_tibble() %>%
          rename(theta1 = V1, theta2 = V2)
      ),
      r = map(
        set,
        ~ get.cell.meta.data("yplot", sector.index = .x, track.index = 2) %>%
          t() %>%
          as_tibble() %>%
          rename(r1 = V1, r2 = V2)
      ),
      s = map(
        set,
        ~ get.cell.meta.data("xlim", sector.index = .x, track.index = 2) %>%
          t() %>%
          as_tibble() %>%
          rename(s1 = min.data, s2 = max.data)
      )
    ) %>%
    unnest() %>%
    mutate(theta1 = if_else(theta1 == 0, 360, theta1),
           set = as.factor(set))

  # Bar data --------------------------------------------------------------
  barData <-
    as_tibble(degreeMat) %>%
    mutate(set = as.factor(rownames(degreeMat))) %>%
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
  linkData <- as_tibble(edges) %>%
    mutate(set1 = rownames(edges)) %>%
    gather(set2, edge, -set1) %>%
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
    mutate(link = pmap(
      list(
        theta1 = theta1,
        theta2 = theta2,
        rou1 = r1,
        rou2 = r2
      ),
      ~ circlize:::getQuadraticPoints(..1, ..2, ..3, ..4) %>% as_tibble()
    )) %>%
    unnest() %>%
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
                          plotDomain =
                            list(
                              left = -1.557743,
                              right = 1.557743,
                              top = 1.08,
                              bottom = -1.08
                            ),
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
      nearPoints(linkData, pointer, xvar = "x", yvar = "y", addDist = TRUE) %>%
      arrange(dist_) %>%
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
                                    linkThickness = "percent") {

  location <- pointerLoc$location
  pointer <- pointerLoc$pointer
  if(is.null(location)) {
   return(NULL)
  }

  # Maximum degree
  maxDegree <- max(setSizesByDegree[["degree"]])

  # Edges matrix
  if(linkThickness != "percent") {
    edgeName <- "Nintersect"
  } else if (linkThickness == "percent") {
    if(focusSet == "none") {
      edgeName <- "prop"
    } else {
      edgeName <- "prop1"
    }
  }

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
      pull(edgeName)

    if (linkThickness == "percent") {
      overlap <- overlap * 100
    }
    overlap <- round(overlap)
    if (focusSet == "none") {
      if (linkThickness == "percent") {
        label <-
          glue("{overlap}% of all {name1} and {name2} </br>items belong to both sets")
      } else {
        label <-
          glue("{overlap} items belong to both</br>{name1} and {name2}")
      }

    } else {
      if (linkThickness == "percent") {
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
radialSetsClickActions <- function(setSizes,
                                    setSizesByDegree,
                                    setIntersections,
                                    plotClick,
                                    focusSet = "none",
                                    linkThickness = "percent",
                                    countScale = 1) {

  # Unpack coordinates of mouse pointer
  x <- plotClick$x
  y <- plotClick$y

  # If mouse is not on plot, return null
  if (!is.numeric(x)) return(NULL)

  # Convert pointer location to polar coordinates
  r <- sqrt(x^2+y^2)
  theta <- atan2(y,x)

  # Convert angle in radians to degrees
  if (theta>0){
    deg <- theta*(180/pi)
  } else{
    deg <- 360+theta*(180/pi)
  }

  # Get plot data
  networkData <- getRadialSetsData(setSizes,
                                   setSizesByDegree,
                                   setIntersections,
                                   linkThickness = linkThickness,
                                   focusSet = focusSet,
                                   countScale = countScale)

  # Unpack data
  edges <- networkData$edges
  sets <- networkData$sets
  nSets <- networkData$nSets
  maxDegree <- networkData$maxDegree
  degreeMat <- networkData$degreeMat
  setSizesVec <- networkData$setSizesVec
  maxWidth <- networkData$maxWidth

  # Loop over sectors collecting sector data
  secNames <- get.all.sector.index()
  xplot <- matrix(nrow = length(secNames), ncol = 2)
  yplot <- matrix(nrow = length(secNames), ncol = 2)
  xlim <- matrix(nrow = length(secNames), ncol = 2)
  xToDeg <- vector(mode = "numeric", length = length(secNames))
  for (i in c(1:length(secNames))){
    xplot[i,] <- get.cell.meta.data("xplot", sector.index = secNames[i], track.index = 2)
    yplot[i,] <- get.cell.meta.data("yplot", sector.index = secNames[i], track.index = 2)
    xlim[i,] <- get.cell.meta.data("xlim", sector.index = secNames[i], track.index = 2)
  }
  xplot[1,1] <- 360
  xToDeg <- (xplot[,1]-xplot[,2])/(xlim[,2]-xlim[,1])

  # Loop over links collecting points for each link
  links <- list()
  k <- 1
  linkInd <- matrix(nrow = nSets, ncol = nSets)
  for (i in c(1:nSets)){
    for (j in c(1:nSets)){
      if (edges[i,j] != 0){
        sector.index1 <- sets[i]
        sector.index2 <- sets[j]
        point1 <- setSizesVec[i]/2
        point2 <- setSizesVec[j]/2
        rou = min(get.cell.meta.data("yplot", sector.index = sector.index1, track.index = 2))

        theta1 <-  circlize(point1, 0, sector.index = sector.index1,
                            track.index = 0)[1, "theta"]
        theta2 <-  circlize(point2, 0, sector.index = sector.index2,
                            track.index = 0)[1, "theta"]
        links[[k]] <-  circlize:::getQuadraticPoints(theta1, theta2, rou1 = rou, rou2 = rou, h = NULL, w = 1)
        linkInd[i,j] <- k
        k <- k+1
      }
    }
  }

  # If pointer is in center of plot, return tooltip for links
  clickLink <- FALSE
  clickSection <- FALSE
  clickBar <- FALSE
  histInd <- NULL
  name1 <- NULL
  name2 <- NULL
  name <- NULL
  if ((r < min(yplot)) | (r > max(yplot))){
    for (i in c(1:length(links))){

      # Dataframe of points for ith link
      df <- data_frame(xvar = links[[i]][,1], yvar = links[[i]][,2])

      # Get matrix of nearby points
      pointsList <- nearPoints(df, plotClick, xvar = "xvar", yvar = "yvar", addDist = TRUE)

      # If found nearby link, stop search
      if (!nrow(pointsList) == 0){break}
    }

    # If search ended with no nearby links return null
    if (nrow(pointsList) == 0){return(NULL)}

    clickLink <- TRUE

    # Match the link index to the matrix of overlaps
    ind <- which(linkInd == i, arr.ind = T)

    # Create string displaying overlap for given link
    name1 <- secNames[ind[1]]
    name2 <- secNames[ind[2]]

  } else {

    clickSection <- TRUE

    # Determine which sector the pointer is in
    secInd <- which((deg < xplot[,1]) & (deg > xplot[,2]) & (r > yplot[,1]) & (r < yplot[,2]))
    if(length(secInd) == 0) {return(NULL)}

    # Calculate bounds for each radial histogram bar
    rlim <- vector(mode = "numeric", length = maxDegree+1)
    for (i in c(1:(maxDegree+1))){
      rlim[i] <- min(yplot)+((i-1)/maxDegree)*(max(yplot)-min(yplot))
    }

    # Determine which histogram bar the pointer is in
    r_lb <- rlim[maxDegree:1]
    r_ub <- rlim[(maxDegree+1):2]
    histInd <- which( r>= r_lb & r< r_ub)

    s <- degreeMat[secInd,histInd]*xToDeg[secInd]
    xc <- xplot[secInd,2]+(xplot[secInd,1]-xplot[secInd,2])/2
    if ((deg <= xc+s/2) & (deg >= xc-s/2)){
      clickBar = TRUE
    }

    name <- secNames[secInd]
  }

  return(list(name = name,
              name1 = name1,
              name2 = name2,
              degree = histInd,
              clickLink = clickLink,
              clickSection = clickSection,
              clickBar = clickBar))
}
