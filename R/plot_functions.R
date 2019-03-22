#' Build radial sets plot
#'
#' @import circlize
#' @importFrom randomcoloR randomColor
#'
#' @export
buildRadialSetsPlot <- function(setSizes,
                                setSizesByDegree,
                                setIntersections,
                                setOrder = NULL,
                                linkThickness = "percent",
                                focusSet = "none",
                                sectorLabelFontSize = 1,
                                axisLabelFontSize = 1,
                                maxLinkThickness = 15,
                                countScale = 1,
                                facing = "clockwise",
                                axisLabels = FALSE,
                                legendTitle = NULL,
                                showLegend = FALSE,
                                sectorLineWidth = 1,
                                sectorColor = "white",
                                majorTick = FALSE,
                                bezierW = 1,
                                bezierHRatio = 0.75,
                                disPropLim = c(-1,1),
                                barColor = "darkgrey") {



  # Prepare plot data
  networkData <- getRadialSetsData(setSizes,
                                setSizesByDegree,
                                setIntersections,
                                setOrder = setOrder,
                                linkThickness = linkThickness,
                                focusSet = focusSet,
                                countScale = countScale,
                                disPropLim = disPropLim)

  # Unpack data
  edges <- networkData$edges
  sets <- networkData$sets
  nSets <- networkData$nSets
  maxDegree <- networkData$maxDegree
  degreeMat <- networkData$degreeMat
  setSizesVec <- networkData$setSizesVec
  maxWidth <- networkData$maxWidth
  edgesDisPropColors <- networkData$edgesDisPropColors

  # Define color pallette
  if(length(barColor) == 1) {
    barColor <- rep(barColor, nSets)
  }

  par(mar=rep(0,4))

  # Initialize circos with 2 tracks
  circlize::circos.clear()
  circlize::circos.par(cell.padding = c(0.02, 0, 0.02, 0))

  # Track 1 - Sector labels
  circlize::circos.initialize(sets,
                              xlim = c(0, 1),
                              sector.width = setSizesVec)

  # Track 2 - Histograms
  xlim <- matrix(c(rep(0, nSets), setSizesVec),
                 nrow = nSets,
                 ncol = 2)
  circlize::circos.initialize(sets, xlim = xlim, sector.width = setSizesVec)

  # Plot track of sector names
  circlize::circos.trackPlotRegion(
    track.index = 1,
    bg.border = NA,
    ylim = c(0, 1),
    track.height = 0.3,
    panel.fun = function(x, y) {
      sector.index = circlize::get.cell.meta.data("sector.index")
      xlim = circlize::get.cell.meta.data("xlim")
      rlim = circlize::get.cell.meta.data("ylim")
      circlize::circos.text(xlim[2]/2,
                            rlim[1],
                            adj = c(0, 0.5),
                            sector.index,
                            facing = facing,
                            niceFacing = TRUE,
                            cex = sectorLabelFontSize)
    }
  )

  # Plot track with bars for sum by degree (histograms)
  circlize::circos.trackPlotRegion(
    track.index = 2,
    ylim = c(0, maxDegree),
    track.height = 0.2,
    bg.lwd = sectorLineWidth,
    bg.col = sectorColor,
    panel.fun = function(x, y) {
      sector.index = circlize::get.cell.meta.data("sector.index")
      xlim = circlize::get.cell.meta.data("xlim")
      ylim = circlize::get.cell.meta.data("ylim")

      # Create axis
      circlize::circos.axis(labels.cex = axisLabelFontSize,
                            major.tick = majorTick,
                            labels = axisLabels)

      # Index for current sector
      i <- which(sets == sector.index)

      # Color for current sector
      sectorColor <- barColor[i]

      # Loop over bars for each degree
      y1 <- maxDegree
      for (j in c(1:maxDegree)) {

        # Length of bar divided by two
        dx <- degreeMat[i, j] / 2

        # Zero postion to center bar
        zero <- setSizesVec[i] / 2

        # Vector of x coordinates
        x1 = seq(zero - dx, zero + dx, (2 * dx) / 1)

        # Vector of x coordinates out and back
        d1 = c(x1, rev(x1))

        # Repeated y coordinates
        d2 = c(rep(y1, length(x1)),
               rep(y1 - 1, length(x1)))

        # Draw bars of non-zero length
        if (dx != 0) {
          circlize::circos.polygon(d1, d2, col = sectorColor, border = "black")
        }
        y1 <- y1 - 1
      }
    }
  )

  # Draw links between sectors
  for (i in c(1:nSets)) {
    for (j in c(1:nSets)) {
      if (edges[i, j] != 0) {

        # Draw links
        circlize::circos.link(
          sets[i],
          setSizesVec[i] / 2,
          sets[j],
          setSizesVec[j] / 2,
          lwd = (edges[i, j] / maxWidth) * maxLinkThickness,
          col = edgesDisPropColors[i,j],
          w = bezierW,
          h.ratio = bezierHRatio
        )

      }
    }
  }

  if (linkThickness == "percent") {
    legendText <- paste0(seq(1,0.25,-0.25)*maxWidth, "%")
  } else {
    legendText <- paste0(seq(1,0.25,-0.25)*maxWidth*(1/countScale))
  }

  # Lengend showing link thickness
  if(showLegend){
  legend(
    "topleft",
    inset = 0.05,
    cex = sectorLabelFontSize,
    title = legendTitle,
    lwd = maxLinkThickness * seq(1,0.25,-0.25),
    col = linkColor,
    legend = legendText,
    bty = "n"
  )
  }
}
