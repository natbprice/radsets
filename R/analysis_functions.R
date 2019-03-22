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
#'  \item \code{set1} and \code{set2} indicating a pair of sets
#'  \item \code{Ninter} and \code{Nunion} indicating numer of elements in
#'  the intersection and union of the pair of sets
#'  \item \code{N1} and \code{N2} indicating number of items in each set
#'  \item \code{prop1} and \code{prop2} indicating the proportion of items in
#'  each set that are also members of the other set
#'   (\code{prop1 = Ninter / N1})
#'  \item \code{prop} indicating the proportion of items from either set that
#'  are members of both sets (\code{prop = Ninter / Nunion})
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
#' @param linkThickness Character vector specifying if edge matrix should be
#' a count or a proportion. Valid values are "percent" or "count"
#' @param focusSet Character vector specifying a name of a set to focus on.
#' Edges not connected to focus set are removed.
#' @param countScale An optional input to apply multiplicative scaling to set
#' sizes
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
                           linkThickness = "percent",
                           focusSet = "none",
                           countScale = 1,
                           disPropLim = c(-1, 1)) {

  if(!linkThickness %in% c("count", "percent")) {
    stop("Input 'linkThickness' should be 'count' or 'percent'")
  }


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

  # Calculate disproportionality
  if("none" %in% focusSet) {
    edgesDisProp <-
      setIntersections %>%
      arrange(set1, set2) %>%
      select(set1, set2, prop.relError) %>%
      spread(set2, prop.relError) %>%
      select(-set1) %>%
      as.matrix()
    rownames(edgesDisProp) <- sets
    diag(edgesDisProp) <- 0
  } else {

    edgesDisProp <-
      setIntersections %>%
      arrange(set1, set2) %>%
      select(set1, set2, prop1.relError) %>%
      spread(set2, prop1.relError) %>%
      select(-set1) %>%
      as.matrix()
    rownames(edgesDisProp) <- sets
    diag(edgesDisProp) <- 0
  }

  # Edges matrix
  if(linkThickness != "percent") {

    # Calculate edges
    edges <-
      setIntersections %>%
      arrange(set1, set2) %>%
      select(set1, set2, Ninter) %>%
      spread(set2, Ninter) %>%
      select(-set1) %>%
      as.matrix()
    rownames(edges) <- sets

    if(!"none" %in% focusSet) {
      # Remove all links besides focus group
      if(length(focusSet) == 1) {
        edges[-which(sets %in% focusSet),] <- 0
        edgesDisProp[-which(sets %in% focusSet),] <- 0
      } else {
        edges[-which(sets %in% focusSet), ] <- 0
        edges[, -which(sets %in% focusSet)] <- 0
        edgesDisProp[-which(sets %in% focusSet), ] <- 0
        edgesDisProp[, -which(sets %in% focusSet)] <- 0
      }
    }

    # Scale edges by thousands
    edges = edges * countScale

  } else if (linkThickness == "percent") {

    if("none" %in% focusSet) {

      edges <-
        setIntersections %>%
        arrange(set1, set2) %>%
        select(set1, set2, prop) %>%
        spread(set2, prop) %>%
        select(-set1) %>%
        as.matrix()
      rownames(edges) <- sets

    } else {

      edges <-
        setIntersections %>%
        arrange(set1, set2) %>%
        select(set1, set2, prop1) %>%
        spread(set2, prop1) %>%
        select(-set1) %>%
        as.matrix()
      rownames(edges) <- sets

      # Remove all links besides focus group
      if(length(focusSet) == 1) {
        edges[-which(sets %in% focusSet),] <- 0
        edgesDisProp[-which(sets %in% focusSet),] <- 0
      } else {
        edges[-which(sets %in% focusSet), ] <- 0
        edges[, -which(sets %in% focusSet)] <- 0
        edgesDisProp[-which(sets %in% focusSet), ] <- 0
        edgesDisProp[, -which(sets %in% focusSet)] <- 0
      }
    }

    # Scale edges to percent
    edges <- edges * 100
  }

  # Remove self-links
  diag(edges) <- 0

  # Map disproportionality to colors
  n <- 101
  colorVec <- rev(RColorBrewer::brewer.pal(11, "RdBu"))
  colorPal <- c(colorRampPalette(c(colorVec[1], colorVec[6]))(51),
            colorRampPalette(c(colorVec[6], colorVec[11]))(51)[-1])
  scaleLimit <- ceiling(max(abs(range(edgesDisProp))) * 10) / 10
  edgesDisPropColors <-
    c(colorPal[as.integer(cut(edgesDisProp,
                              breaks = c(-Inf,
                                         seq(disPropLim[1],
                                             disPropLim[2],
                                             length.out = n-2),
                                         Inf)))]) %>%
    matrix(nrow = nrow(edges), ncol = ncol(edges))

  # Maximum edge width
  maxWidth <- signif(max(edges),1)

  return(list(edges = edges,
              sets = sets,
              nSets = nSets,
              maxDegree = maxDegree,
              degreeMat = degreeMat,
              setSizesVec = setSizesVec,
              maxWidth = maxWidth,
              edgesDisProp = edgesDisProp,
              edgesDisPropColors = edgesDisPropColors))
}
