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
#' data("movieSets")
#' setNames <- movieSets %>%
#'   select(Action:Western) %>%
#'   colnames()
#'
#' # Calculate set sizes
#' getSetSizes(movieSets, setNames)
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @export
getSetSizes <- function(df, setNames) {

  # Check df class
  if (!("tbl" %in% class(df) | "data.frame" %in% class(df))) {
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
    mutate(set = factor(set, levels = setNames))

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
#' data("movieSets")
#' setNames <- movieSets  %>%
#'   select(Action:Western) %>%
#'   colnames()
#'
#' # Calculate set sizes
#' getSetSizesByDegree(movieSets , setNames, "movieId")
#'
#' # Calculate set sizes with max degree 3
#' getSetSizesByDegree(movieSets , setNames, "movieId", maxDegree = 3)
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @export
getSetSizesByDegree <- function(df, setNames, idName, maxDegree = 4) {

  # Check df class
  if (!("tbl" %in% class(df) | "data.frame" %in% class(df))) {
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
    group_by(set, degree) %>%
    summarize(N = sum(membership)) %>%
    group_by(set) %>%
    mutate(prop = N / sum(N)) %>%
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
#' data("movieSets")
#' setNames <- movieSets %>%
#'   select(Action:Western) %>%
#'   colnames()
#'
#' # Calculate set sizes
#' getSetIntersections(movieSets, setNames, "movieId")
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @export
getSetIntersections <- function(df, setNames, idName) {

  # Check df class
  if (!("tbl" %in% class(df) | "data.frame" %in% class(df))) {
    stop("Input 'df' should be a data frame")
  }

  # Check for required variables
  if (any(!setNames %in% colnames(df))) {
    stop(paste(
      "Specified variables not present in data frame:",
      paste(setNames[!setNames %in% colnames(df)], collapse = ", ")
    ))
  }

  # Drop any empty sets
  if (any(colSums(df[setNames]) == 0)) {
    dropSets <- which(colSums(df[setNames]) == 0)
    warning(paste("Dropping empty sets:",
                  paste(setNames[dropSets], collapse = ", ")))
    setNames <- setNames[-dropSets]
    df <- df[c(idName, setNames)]
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
    mutate(set = factor(set, levels = setNames)) %>%
    # Drop any extra variables
    select(set, Ntotal, !!sym(idName)) %>%
    # Nest data by set
    tidyr::nest(-set, -Ntotal)

  # Create upper triangular matrix
  edgeMat <-
    matrix(
      0,
      nrow = length(setNames),
      ncol = length(setNames),
      dimnames = list(setNames, setNames)
    )
  edgeMat[upper.tri(edgeMat, diag = TRUE)] <- 1

  # Perform intersections/unions
  setInter <-
    # Get all set combinations
    as_tibble(edgeMat) %>%
    mutate(set1 = rownames(edgeMat)) %>%
    tidry::gather(set2, value, -set1) %>%
    mutate(set1 = factor(set1, levels = setNames),
           set2 = factor(set2, levels = setNames)) %>%
    filter(value == 1) %>%
    select(-value) %>%
    # Join items in each set
    left_join(nestedSets, by = c("set1" = "set")) %>%
    left_join(nestedSets %>% select(-Ntotal), by = c("set2" = "set")) %>%
    # Perform intersections and unions between sets
    mutate(
      setIntersection = purrr::map2(data.x, data.y, intersect),
      setUnion = purrr::map2(data.x, data.y, union)
    ) %>%
    # Count number of items in each intersection and union
    transmute(
      set1,
      set2,
      Ntotal,
      Ninter = purrr::map_dbl(setIntersection, nrow),
      Nunion = purrr::map_dbl(setUnion, nrow)
    )

  # Complete set intersections
  setInterFull <-
    full_join(
      setInter,
      setInter %>% rename(set1 = set2, set2 = set1),
      by = c("set1", "set2", "Ntotal", "Ninter", "Nunion")
    )

  # Calculate edge values
  edges <-
    tibble(set1 = factor(setNames, levels = setNames),
           set2 = factor(setNames, levels = setNames)) %>%
    tidyr::expand(set1, set2) %>%
    # Join intersections/unions
    left_join(setInterFull, by = c("set1", "set2")) %>%
    # Join set sizes
    left_join(setSizes %>% rename(N1 = N), by = c("set1" = "set")) %>%
    left_join(setSizes %>% rename(N2 = N), by = c("set2" = "set")) %>%
    # Calculate percent overlap
    mutate(prop = Ninter / Nunion,
           prop1 = Ninter / N1,
           prop2 = Ninter / N2) %>%
    # Calculate difference in observed vs predicted set sizes
    mutate(Ninter.pred = if_else(set1 != set2,
                                 Ntotal * ( (N1 / Ntotal) * (N2 / Ntotal)),
                                 as.numeric(N1)),
           Nunion.pred = if_else(set1 != set2,
                                 Ntotal * ( (N1 / Ntotal) + (N2 / Ntotal) -
                                           ( (N1 / Ntotal) * (N2 / Ntotal))),
                                 as.numeric(N1)),
           prop.pred = Ninter.pred / Nunion.pred,
           prop1.pred = Ninter.pred / N1,
           Ninter.error = Ninter - Ninter.pred,
           prop.error = prop - prop.pred,
           prop1.error = prop1 - prop1.pred,
           Ninter.relError = Ninter.error / Ninter,
           prop.relError = prop.error / prop,
           prop1.relError = prop1.error / prop1
    ) %>%
    mutate(set1 = factor(set1, levels = setNames),
           set2 = factor(set2, levels = setNames)) %>%
    mutate_if(is.numeric, as.double)

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
#' @param setOrder An optional input to specify order of sets (counterclockwise).
#' Partial ordering is performed using \code{\link[forcats]{fct_relevel}}
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
#' @param colorScaleLim A numeric vector specifying upper and lower scale limits
#' on link colors. Scale limits are enforced using \code{\link[scales]{censor}} or
#' \code{\link[scales]{squish}}
#' @param colorScaleMapFun A character string specifying whether to use
#' \code{\link[scales]{censor}} or \code{\link[scales]{squish}} to enforce color
#' scale limits
#' @param edgeScaleLim A numeric vector specifying upper and lower scale limits
#' on edge thickness. Scale limits are enforced using \code{\link[scales]{censor}} or
#' \code{\link[scales]{squish}}
#' @param edgeScaleMapFun A character string specifying whether to use
#' \code{\link[scales]{censor}} or \code{\link[scales]{squish}} to enforce edge
#' thickness limits
#' @param reverseLinkPal A logical input specifying if color scale should be reversed
#' @param edgeWidthRange A numeric vector specifying minimum and maximum line
#' thickness of links
#' @param dropSets A logical indicating if sets without links should be removed
#'
#' @return A list with elements:
#' \itemize{
#'  \item \code{edgeWidth} matrix of link thickness variable (original scale)
#'  \item \code{edgeWidthMap} matrix of link thickness mapped to line thickness
#'  \item \code{edgeColor} matrix of link color variable (original scale)
#'  \item \code{edgeWidthMap} matrix of link color mapped to colors
#'  \item \code{degreeMat} matrix of bar sizes
#'  \item \code{sets} character vector of set names
#'  \item \code{nSets} number of sets
#'  \item \code{maxDegree} max degree of overlaps in bar plot
#'  \item \code{setSizesVec} vector of set sizes
#'  \item \code{maxWidth} maximum width of lines for edges
#' }
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @export
getRadialSetsData <- function(setSizes,
                           setSizesByDegree,
                           setIntersections,
                           setOrder = NULL,
                           linkThickness = "prop",
                           linkColor = "prop.relError",
                           linkColorPal = "RdBu",
                           reverseLinkPal = FALSE,
                           focusSets = "none",
                           countScale = 1,
                           colorScaleLim = c(-1, 1),
                           colorScaleMapFun = "squish",
                           edgeScaleLim = c(-Inf, Inf),
                           edgeScaleMapFun = "censor",
                           edgeWidthRange = c(1, 8),
                           dropSets = FALSE) {

  # Reorder sets
  sets <- factor(levels(setIntersections[["set1"]]))
  if (!is.null(setOrder)) {
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
    mutate(degree = factor(degree, levels = 1:max(degree))) %>%
    tidyr::complete(set, degree, fill = list(N = 0)) %>%
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
    tidyr::spread(set2, linkColor) %>%
    select(-set1) %>%
    as.matrix()
  rownames(edgeColor) <- sets
  diag(edgeColor) <- NA

  # Edges matrix
  edgeWidth <-
    setIntersections %>%
    arrange(set1, set2) %>%
    select(set1, set2, linkThickness) %>%
    tidyr::spread(set2, linkThickness) %>%
    select(-set1) %>%
    as.matrix()
  rownames(edgeWidth) <- sets

  if (!"none" %in% focusSets) {
    # Remove all links besides focus group
    if (length(focusSets) == 1) {
      edgeWidth[-which(sets %in% focusSets), ] <- NA
      edgeColor[-which(sets %in% focusSets), ] <- NA
    } else {
      edgeWidth[-which(sets %in% focusSets), ] <- NA
      edgeWidth[, -which(sets %in% focusSets)] <- NA
      edgeColor[-which(sets %in% focusSets), ] <- NA
      edgeColor[, -which(sets %in% focusSets)] <- NA
    }
  }
  # Remove self-links
  diag(edgeWidth) <- 0

  # Apply edge width limits
  if (edgeScaleMapFun == "censor") {
    edgeWidthMap <- scales::censor(edgeWidth,
                                   range = edgeScaleLim,
                                   only.finite = FALSE)
  } else if (edgeScaleMapFun == "squish") {
    edgeWidthMap <- scales::squish(edgeWidth,
                                   range = edgeScaleLim,
                                   only.finite = FALSE)
  }

  # Remove edge self links
  diag(edgeWidthMap) <- NA

  # Scale edge value to line thickness
  edgeWidthMap <- scales::rescale(edgeWidthMap, to = edgeWidthRange)

  # Apply edge color limits
  if (colorScaleMapFun == "censor") {
    edgeColorMap <- scales::censor(edgeColor,
                                   range = colorScaleLim,
                                   only.finite = FALSE)
  } else if (colorScaleMapFun == "squish") {
    edgeColorMap <- scales::squish(edgeColor,
                                   range = colorScaleLim,
                                   only.finite = FALSE)
  }

  # Remove edge self links
  diag(edgeColorMap) <- NA

  # Scale edge value to color thickness
  colorVec <- scales::brewer_pal(palette = linkColorPal,
                                 direction = if_else(reverseLinkPal, -1, 1))(8)
  edgeColorMap <- scales::col_numeric(palette = colorVec,
                                      domain = colorScaleLim,
                                      na.color = NA)(edgeColorMap) %>%
      matrix(nrow = nrow(edgeWidth), ncol = ncol(edgeWidth),
             dimnames = list(rownames(edgeWidth), colnames(edgeWidth)))

  # Maximum edge width
  maxWidth <- signif(max(edgeWidth), 1)

  # Drop sets that do not have links
  if (dropSets) {

    dropColor <- is.na(edgeColorMap) | (edgeColorMap == 0) |
      is.infinite(edgeColorMap)
    dropWidth <- is.na(edgeWidthMap) | (edgeWidthMap == 0) |
      is.infinite(edgeWidthMap)
    dropInd <- which(colSums(dropColor | dropWidth) == ncol(edgeWidthMap))

    setSizesVec <- setSizesVec[-dropInd]
    sets <- sets[-dropInd]
    nSets <- length(sets)
    edgeColorMap <- edgeColorMap[-dropInd, -dropInd]
    edgeWidthMap <- edgeWidthMap[-dropInd, -dropInd]
    degreeMat <- degreeMat[-dropInd, ]
  }

  return(list(edgeWidth = edgeWidth,
              edgeWidthMap = edgeWidthMap,
              edgeColor = edgeColor,
              edgeColorMap = edgeColorMap,
              degreeMat = degreeMat,
              sets = sets,
              nSets = nSets,
              maxDegree = maxDegree,
              setSizesVec = setSizesVec,
              maxWidth = maxWidth))
}
