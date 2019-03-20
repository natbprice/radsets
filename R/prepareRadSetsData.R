#' Prepare data for radial sets plot
#'
#' @export
prepareRadSetsData <- function(sharedCustomers,
                               degreeCount,
                               linkThickness = "percent",
                               focusGroup = "none",
                               countScale = 1 / 1e3) {

  # Types of permits and stamps
  permitStampTypes <- levels(degreeCount$permitStampType)
  nTypes <- length(permitStampTypes)

  # Maximum degree
  maxDegree <- max(degreeCount[["degree"]])

  # Convert degree count to matrix
  degreeCountMat <-
    degreeCount %>%
    arrange(permitStampType) %>%
    select(permitStampType, degree, customers) %>%
    mutate(customers = customers * countScale) %>%
    spread(degree, customers) %>%
    select(-permitStampType) %>%
    as.matrix()
  rownames(degreeCountMat) <- permitStampTypes
  colnames(degreeCountMat) <- 1:maxDegree

  # Number of customers with each type
  totalVec <- degreeCount %>%
    arrange(permitStampType) %>%
    distinct(permitStampType, total) %>%
    mutate(total = total * countScale) %>%
    pull(total)
  names(totalVec) <- permitStampTypes

  # Edges matrix
  if (linkThickness != "percent") {
    # Calculate edges
    edges <-
      sharedCustomers %>%
      arrange(type1) %>%
      select(type1, type2, customers) %>%
      mutate(customers = customers) %>%
      spread(type2, customers) %>%
      select(-type1) %>%
      as.matrix()
    rownames(edges) <- permitStampTypes

    if (focusGroup != "none") {
      # Remove all links besides focus group
      edges[-which(permitStampTypes == focusGroup),] <- 0
    }

    # Scale edges by thousands
    edges = edges * countScale

  } else if (linkThickness == "percent") {
    if (focusGroup == "none") {
      edges <-
        sharedCustomers %>%
        arrange(type1) %>%
        select(type1, type2, prop) %>%
        spread(type2, prop) %>%
        select(-type1) %>%
        as.matrix()
      rownames(edges) <- permitStampTypes

    } else {
      edges <-
        sharedCustomers %>%
        arrange(type1) %>%
        select(type1, type2, prop1) %>%
        spread(type2, prop1) %>%
        select(-type1) %>%
        as.matrix()
      rownames(edges) <- permitStampTypes

      # Remove all links besides focus group
      edges[-which(permitStampTypes == focusGroup),] <- 0
    }

    # Scale edges to percent
    edges = edges * 100
  }

  # Remove self-links
  diag(edges) <- 0

  # Maximum edge width
  maxWidth <- signif(max(edges), 1)

  return(
    list(
      edges = edges,
      permitStampTypes = permitStampTypes,
      nTypes = nTypes,
      maxDegree = maxDegree,
      degreeCountMat = degreeCountMat,
      totalVec = totalVec,
      maxWidth = maxWidth
    )
  )
}
