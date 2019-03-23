#' Count total number of items in each set
#'
#' Count total number of items in each set
#'
#' The input data frame should contain a row for each item and a binary variable
#' for each set indicating the membership of each item. The \code{setNames}
#' input should correspond to the binary indicator columns in the data frame.
#'
#' @param df A data frame indicating set membership
#' @param setNames A character vector of set names
#' @return A data frame with variables \code{set} and \code{N} indicating the
#' number of items in each set
#'
#' @examples
#' # Define set names
#' data(movies)
#' setNames <- movies %>%
#'   select(Action:Western) %>%
#'   colnames()
#'
#' # Calculate set sizes
#' getSetSizes(movies, setNames)
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom tidyr gather
#'
#' @export
getSetSizes <- function(df, setNames) {

  # Check df class
  if(!("tbl" %in% class(df) | "data.frame" %in% class(df))) {
    stop("Input 'df' should be a data frame")
  }

  # Check for required variables
  if (any(!setNames %in% colnames(df))) {
    stop(paste(
      "Specified variables not present in data frame:",
      paste(setNames[!setNames %in% colnames(df)], collapse = ", ")
    ))
  }

  # Calculate set sizes
  setSizes <- df %>%
    summarize_at(setNames, sum) %>%
    tidyr::gather(set, N) %>%
    mutate(set = as.factor(set))

  return(setSizes)

}

#' Count number of items in each set by degree
#'
#' Count  number of items in each set by degree of overlap. The degree of
#' overlap is the number of sets of which an item is a member.
#'
#' The input data frame should contain a row for each item and a binary variable
#' for each set indicating the membership of each item. The \code{setNames}
#' input should correspond to the binary indicator columns in the data frame.
#'
#' @param df A data frame indicating set membership
#' @param setNames A character vector of set names
#' @param idName A string specifying name of ID variable for each item
#' @param maxDegree A numeric input indicating upper limit on degree
#' @return A data frame with variables:
#' \itemize{
#' \item \code{set} indicating set
#' \item \code{degree} indicating degree of overlap (maximum of \code{maxDegree})
#' \item \code{degreeLabel} a factor labeling the degree variable
#' \item \code{N} number of items
#' }
#'
#' @examples
#' # Define set names
#' data(movies)
#' setNames <- movies %>%
#'   select(Action:Western) %>%
#'   colnames()
#'
#' # Calculate set sizes
#' getSetSizesByDegree(movies, setNames, "Name")
#'
#' # Calculate set sizes with max degree 3
#' getSetSizesByDegree(movies, setNames, "Name", maxDegree = 3)
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom forcats fct_reorder
#'
#' @export
getSetSizesByDegree <- function(df, setNames, idName, maxDegree = 4) {

  # Check df class
  if(!("tbl" %in% class(df) | "data.frame" %in% class(df))) {
    stop("Input 'df' should be a data frame")
  }

  # Check for required variables
  if (any(!setNames %in% colnames(df))) {
    stop(paste(
      "Specified variables not present in data frame:",
      paste(setNames[!setNames %in% colnames(df)], collapse = ", ")
    ))
  }

  # Calculate set sizes by degree
  setSizesByDegree <-
    df %>%
    tidyr::gather(set, membership, !!!syms(setNames)) %>%
    group_by_at(idName) %>%
    mutate(degree = min(sum(membership), maxDegree)) %>%
    ungroup() %>%
    filter(degree != 0) %>%
    mutate(degreeLabel = if_else(degree == maxDegree,
                                 paste0(degree, "+"),
                                 as.character(degree)),
           degreeLabel = forcats::fct_reorder(degreeLabel, degree)) %>%
    group_by(set, degree, degreeLabel) %>%
    summarize(N = sum(membership)) %>%
    group_by(set) %>%
    mutate(prop = N/sum(N)) %>%
    ungroup() %>%
    mutate(set = as.factor(set))

  return(setSizesByDegree)

}

#' Calculate sizes of intersections between sets
#'
#' Calculate sizes of unions and intersections between each pair of sets
#'
#' The input data frame should contain a row for each item and a binary variable
#' for each set indicating the membership of each item. The \code{setNames}
#' input should correspond to the binary indicator columns in the data frame.
#'
#' @param df A data frame indicating set membership
#' @param setNames A character vector of set names
#' @param idName A string specifying name of ID variable for each item
#' @return A data frame with variables:
#' \itemize{
#'  \item \code{set1} and \code{set2} pair of sets
#'  \item \code{Ninter} and \code{Nunion} number of elements in
#'  the intersection and union of the pair of sets
#'  \item \code{N1} and \code{N2} number of items in each set
#'  \item \code{prop1} and \code{prop2} proportion of items in
#'  each set that are also members of the other set
#'   (\code{prop1 = Ninter / N1})
#'  \item \code{prop} proportion of items from either set that
#'  are members of both sets (\code{prop = Ninter / Nunion})
#'  \item \code{.pred} predictions assuming marginal independence of sets
#'  \item \code{.error} difference between predictions and observations
#'  \item \code{.relError} error relative to observations
#' }
#'
#' @examples
#' # Define set names
#' data(movies)
#' setNames <- movies %>%
#'   select(Action:Western) %>%
#'   colnames()
#'
#' # Calculate set sizes
#' getSetIntersections(movies, setNames, "Name")
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom tidyr nest gather expand
#' @importFrom purrr map2 map_dbl
#'
#' @export
getSetIntersections <- function(df, setNames, idName) {

  # Check df class
  if(!("tbl" %in% class(df) | "data.frame" %in% class(df))) {
    stop("Input 'df' should be a data frame")
  }

  # Check for required variables
  if (any(!setNames %in% colnames(df))) {
    stop(paste(
      "Specified variables not present in data frame:",
      paste(setNames[!setNames %in% colnames(df)], collapse = ", ")
    ))
  }

  # Calculate set sizes
  setSizes <-
    getSetSizes(df, setNames)

  # Nest set data
  nestedSets <-
    df %>%
    # Total number of items
    mutate(Ntotal = n()) %>%
    # Convert data to long form
    tidyr::gather(set, membership, !!!syms(setNames)) %>%
    filter(membership == 1) %>%
    # Convert set to factor
    mutate(set = as.factor(set)) %>%
    # Drop any extra variables
    select(set, Ntotal, !!sym(idName)) %>%
    # Nest data by set
    tidyr::nest(-set, -Ntotal)

  # Calculate edge data
  edges <-
    # Get all set combinations
    tibble(set1 = as.factor(setNames),
           set2 = as.factor(setNames)) %>%
    tidyr::expand(set1, set2) %>%
    # Join items in each set
    left_join(nestedSets, by = c("set1" = "set")) %>%
    left_join(nestedSets %>% select(-Ntotal), by = c("set2" = "set")) %>%
    # Perform intersections and unions between sets
    mutate(
      setIntersection = map2(data.x, data.y, intersect),
      setUnion = map2(data.x, data.y, union)
    ) %>%
    # Count number of items in each intersection and union
    transmute(
      set1,
      set2,
      Ntotal,
      Ninter = map_dbl(setIntersection, nrow),
      Nunion = map_dbl(setUnion, nrow)
    ) %>%
    # Join set sizes
    left_join(setSizes %>% rename(N1 = N), by = c("set1" = "set")) %>%
    left_join(setSizes %>% rename(N2 = N), by = c("set2" = "set")) %>%
    # Calculate percent overlap
    mutate(prop = Ninter / Nunion,
           prop1 = Ninter / N1,
           prop2 = Ninter / N2) %>%
    # Calculate difference in observed vs predicted set sizes
    mutate(Ninter.pred = if_else(set1 != set2,
                                 Ntotal*((N1 / Ntotal) * (N2 / Ntotal)),
                                 N1),
           Nunion.pred = if_else(set1 != set2,
                                 Ntotal*((N1 / Ntotal) + (N2 / Ntotal) -
                                           ((N1 / Ntotal) * (N2 / Ntotal))),
                                 N1),
           prop.pred = Ninter.pred / Nunion.pred,
           prop1.pred = Ninter.pred / N1,
           Ninter.error = Ninter - Ninter.pred,
           prop.error = prop - prop.pred,
           prop1.error = prop1 - prop1.pred,
           Ninter.relError = Ninter.error / Ninter,
           prop.relError = prop.error / prop,
           prop1.relError = prop1.error / prop1
    ) %>%
    mutate(set1 = as.factor(set1),
           set2 = as.factor(set2))

  return(edges)

}

#' Prepare data for Radial Sets plot
#'
#' Create a list with data needed by \code{\link{buildRadialSetsPlot}}.
#'
#' @param setSizes Data frame of set sizes from \code{\link{getSetSizes}}
#' @param setSizesByDegree Data frame of set sizes by degree from
#' \code{\link{getSetSizesByDegree}}
#' @param setIntersections Data frame of set intersection sizes from
#' \code{\link{getSetIntersections}}
#' @param linkThickness Character vector specifying name of variable from
#' \code{setIntersections} data frame to map to link thickness
#' @param linkColor Character vector specifying name of variable from
#' \code{setIntersections} data frame to map to link color
#' @param linkColorPal Name of color palette to use for link colors. Must match
#' palette from \code{\link[RColorBrewer]{brewer.pal}}
#' @param focusSets Character vector specifying name(s) of set(s) to focus on.
#' If single set is specified, only links originating from set are shown. If
#' multiple sets are specified, only links between sets are shown.
#' @param countScale An optional input to apply multiplicative scaling to set
#' sizes
#' @param colorScaleLim An optional input to specify scale limits on link colors.
#' Values outside limits are mapped to ends of color scale.
#'
#' @import dplyr
#' @importFrom tidyr spread
#' @importFrom forcats fct_relevel
#' @importFrom RColorBrewer brewer.pal
#'
#' @export
getRadialSetsData <- function(setSizes,
                           setSizesByDegree,
                           setIntersections,
                           setOrder = NULL,
                           linkThickness = "prop",
                           linkColor = "prop.relError",
                           linkColorPal = "RdBu",
                           focusSets = "none",
                           countScale = 1,
                           colorScaleLim = c(-1, 1),
                           edgeWidthLim = NULL,
                           reverseLinkPal = FALSE,
                           maxPlotWidth = 15,
                           minPlotWidth = 1) {

  # Reorder sets
  sets <- factor(levels(setIntersections[["set1"]]))
  if(!is.null(setOrder)) {
    sets <- forcats::fct_relevel(sets, setOrder)
    setIntersections <-
      setIntersections %>%
      mutate(set1 = factor(set1, levels = levels(sets)),
             set2 = factor(set2, levels = levels(sets)))
  }
  sets <- sort(sets)
  nSets <- length(sets)

  # Maximum degree
  maxDegree <- max(setSizesByDegree[["degree"]])

  # Convert degree count to matrix
  degreeMat <-
    setSizesByDegree %>%
    mutate(set = factor(set, levels = levels(sets))) %>%
    arrange(set) %>%
    select(set, degree, N) %>%
    mutate(N = N * countScale) %>%
    tidyr::spread(degree, N) %>%
    select(-set) %>%
    as.matrix()
  rownames(degreeMat) <- sets
  colnames(degreeMat) <- 1:maxDegree

  # Number of N with each type
  setSizesVec <- setSizes %>%
    mutate(set = factor(set, levels = levels(sets))) %>%
    mutate(N = N * countScale) %>%
    arrange(set) %>%
    pull(N)
  names(setSizesVec) <- sets

  # Edge colors matrix
  edgeColor <-
    setIntersections %>%
    arrange(set1, set2) %>%
    select(set1, set2, linkColor) %>%
    spread(set2, linkColor) %>%
    select(-set1) %>%
    as.matrix()
  rownames(edgeColor) <- sets
  diag(edgeColor) <- 0

  # Edges matrix
  edgeWidth <-
    setIntersections %>%
    arrange(set1, set2) %>%
    select(set1, set2, linkThickness) %>%
    spread(set2, linkThickness) %>%
    select(-set1) %>%
    as.matrix()
  rownames(edgeWidth) <- sets

  if(!"none" %in% focusSets) {
    # Remove all links besides focus group
    if(length(focusSets) == 1) {
      edgeWidth[-which(sets %in% focusSets),] <- 0
      edgeColor[-which(sets %in% focusSets),] <- 0
    } else {
      edgeWidth[-which(sets %in% focusSets), ] <- 0
      edgeWidth[, -which(sets %in% focusSets)] <- 0
      edgeColor[-which(sets %in% focusSets), ] <- 0
      edgeColor[, -which(sets %in% focusSets)] <- 0
    }
  }
  # Remove self-links
  diag(edgeWidth) <- 0

  # Map edge width to thickness
  edgeWidthMap <- edgeWidth
  if (length(edgeWidthLim) == 2) {
    edgeWidthMap[edgeWidth < edgeWidthLim[1]] <- edgeWidthLim[1]
    edgeWidthMap[edgeWidth > edgeWidthLim[2]] <- edgeWidthLim[2]
  }

  maxWidth <- max(edgeWidthMap[!is.infinite(edgeWidthMap)], na.rm = T)
  minWidth <- min(edgeWidthMap[!is.infinite(edgeWidthMap)], na.rm = T)
  edgeWidthMap <- (edgeWidthMap - minWidth) / (maxWidth - minWidth)
  edgeWidthMap <- edgeWidthMap*(maxPlotWidth - minPlotWidth) + minPlotWidth

  # Define color palette
  n <- 100
  colorVec <- RColorBrewer::brewer.pal(8, linkColorPal)
  if(reverseLinkPal) {
    colorVec = rev(colorVec)
  }
  linkPal <- colorRampPalette(colorVec)
  if (length(colorScaleLim) == 2) {
    scaleBreaks <- c(-Inf,
                     seq(colorScaleLim[1],
                         colorScaleLim[2],
                         length.out = n - 2),
                     Inf)
  } else {
    scaleBreaks = seq(min(edgeColor[!is.infinite(edgeColor)], na.rm = T),
                      max(edgeColor[!is.infinite(edgeColor)], na.rm = T),
                      length.out = n - 2)
  }

  # Map edge color values to colors
  edgeColorMap <-
    c(linkPal(n)[as.integer(cut(edgeColor, breaks = scaleBreaks))]) %>%
    matrix(nrow = nrow(edgeWidth), ncol = ncol(edgeWidth))

  # Maximum edge width
  maxWidth <- signif(max(edgeWidth),1)

  return(list(edgeWidth = edgeWidth,
              edgeWidthMap = edgeWidthMap,
              sets = sets,
              nSets = nSets,
              maxDegree = maxDegree,
              degreeMat = degreeMat,
              setSizesVec = setSizesVec,
              maxWidth = maxWidth,
              edgeColor = edgeColor,
              edgeColorMap = edgeColorMap))
}
