pxToIn <- 1 / 96

svg("man/figures/logo.svg",
    width = 240*pxToIn,
    height = 278*pxToIn)

par(bg = 'transparent')

# Temporarily remove labels track
buildRadialSetsPlot(
  setSizes,
  setSizesByDegree,
  setIntersections,
  linkThickness = "prop",
  linkColor = "prop.relError",
  linkColorPal = "RdBu",
  reverseLinkPal = T,
  setOrder = optOrder[c(14:19,1:13)],
  colorScaleLim = c(-1,1),
  colorScaleMapFun = "squish",
  edgeScaleLim = c(0,1),
  edgeScaleMap = "censor",
  edgeWidthRange = c(1,1),
  sectorColor = "white",
  sectorFontCol = "white",
  focusSets = c("Drama", "Comedy", "Action", "Romance", "Crime", "Horror", "Mystery",
                "Children", "Documentary"),
  labelSectors = F
)

text(0, 0, "radsets", cex = 3)

dev.off()
