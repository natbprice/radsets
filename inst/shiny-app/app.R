#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(forcats)
library(glue)
library(stringr)
library(tidyr)

# Define UI ---------------------------------------------------------------
ui <- fluidPage(

    # Javascript and CSS files ----------------------------------------------
    tagList(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
        tags$head(singleton(tags$script(src = "windowSize.js")))
    ),
    div(
      id = 'controlsContainer',
    fluidRow(
        column(3,
               uiOutput("focusSetUI")
        ),
        column(3,
               # Link scaling input
               selectizeInput(
                   "linkThickness",
                   label = "Link thickness:",
                   choices = c("Percent shared items (normalized by union)" = "prop",
                               "Percent shared items (relative to origin)" = "prop1",
                               "Number of shared items" = "Ninter"),
                   multiple = FALSE,
                   selected = "prop"
               )
        ),
        column(3,
               # Link scaling input
               sliderInput(
                 "bezierW",
                 label = "Link shape:",
                 min = 0,
                 max = 1,
                 value = 1
               )
        ),
        column(3,
               # Link scaling input
               sliderInput(
                 "bezierHRatio",
                 label = "Link height ratio:",
                 min = 0,
                 max = 1,
                 value = 0.75
               )
        )
    )
    ),
    fluidRow(
      column(8, style='padding-right:0px;',
             div(
               id = 'bigPlotContainer',
             uiOutput("plotUI"))
             ),
      column(4,
             div(
               id = 'smallPlotContainer',
               uiOutput("ratingHistUI")
               ),
             div(
               id = 'smallPlotContainer',
             uiOutput("watchesHistUI")
             ))
    ),
    div(
      id = 'tableContainer',
    fluidRow(
      column(12, align = "center", dataTableOutput("selectedTable"))
    )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {


  # Get page height -------------------------------------------------------
  pageHeight <- reactive({
    req(input$dimension[2])
    input$dimension[2]
  })

  # Render focus set input -----------------------------------------------
  output$focusSetUI <- renderUI({
    # Define set names (user specified)
    setNames <- movieSets %>%
      select(Action:Western) %>%
      colnames()

    # Focus year input
    selectizeInput(
      "focusSet",
      label = "Focus set:",
      choices = c("none", setNames),
      multiple = FALSE,
      selected = "none"
    )
  })

  # Summarize data --------------------------------------------------------
  summaryData <- reactive({
    data(movieSets)

    # Define set names
    setNames <- movieSets %>%
      select(Action:Western) %>%
      colnames()

    # Define ID column (user specified)
    idName <- "movieId"

    # Define max degree (user specified)
    maxDegree <- 4

    # Calculate set sizes
    setSizes <-
      getSetSizes(movieSets, setNames)

    # Calculate set sizes by degree
    setSizesByDegree <-
      getSetSizesByDegree(movieSets, setNames, idName)

    # Calculate edge data
    setIntersections <-
      getSetIntersections(movieSets, setNames, idName)

    list(
      setSizes = setSizes,
      setSizesByDegree = setSizesByDegree,
      setIntersections = setIntersections
    )
  })


  # Render plot -----------------------------------------------------------
  output$radPlotImage <- renderImage({
    req(input$focusSet)

    width  <- session$clientData$output_radPlotImage_width
    height <- session$clientData$output_radPlotImage_height

    outfile <- tempfile(fileext=".svg")

    pxToIn <- 1/96

    svg(outfile,
        width = width*pxToIn,
        height = height*pxToIn)

    buildRadialSetsPlot(
      setSizes = summaryData()$setSizes,
      setSizesByDegree = summaryData()$setSizesByDegree,
      setIntersections = summaryData()$setIntersections,
      focusSet = input$focusSet,
      linkThickness = input$linkThickness,
      sectorLabelFontSize = 1,
      bezierW = input$bezierW,
      bezierHRatio = input$bezierHRatio
      # setOrder = c("Musical", "Animation", "Children", "Fantasy", "Adventure",
      #   "Action", "Horror", "Crime", "Thriller", "Sci-Fi", "War", "Mystery",
      #   "Drama", "Romance", "Comedy", "Film-Noir", "Western", "IMAX",
      #   "Documentary")
    )
    dev.off()

    list(src = outfile,
         width = width,
         height = height)

  }, deleteFile = F)
  output$radPlot <- renderPlot({
    req(input$focusSet)

    buildRadialSetsPlot(
      setSizes = summaryData()$setSizes,
      setSizesByDegree = summaryData()$setSizesByDegree,
      setIntersections = summaryData()$setIntersections,
      focusSet = input$focusSet,
      linkThickness = input$linkThickness,
      sectorLabelFontSize = 1.5
    )
  }, bg = "transparent", execOnResize = TRUE)



  # Render rating histogram -----------------------------------------------
  output$ratingHist <- renderPlot({

    plotData <-
      movieSets %>%
      mutate(label = "all items") %>%
      full_join(
        tableData() %>%
          filter(selected) %>%
          mutate(label = as.character(clickLabel())),
        by = c("title", "year",
               "avgRating", "nRating", "label")
      )

    ggplot(data = plotData %>% drop_na(avgRating),
           aes(x = avgRating,
               fill = fct_relevel(label, "all items"))) +
      geom_density(alpha = 0.5) +
      scale_y_continuous(expand = expand_scale(mult = c(0, 0.025))) +
      # scale_x_continuous(limits = c(0.5, 5.5),
      #                    expand = expand_scale(mult = c(0, 0))) +
      theme_minimal(base_size = 18) +
      labs(title = "Average Rating",
           x = "Average Rating",
           y = NULL,
           fill = NULL) +
      theme(legend.position = "bottom",
            aspect.ratio = 1 / 2,
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())

  }, bg = "transparent")

  # Render watches histogram -----------------------------------------------
  output$watchesHist <- renderPlot({

    plotData <-
      movieSets %>%
      mutate(label = "all items") %>%
      full_join(
        tableData() %>%
          filter(selected) %>%
          mutate(label = as.character(clickLabel())),
        by = c("title", "year",
               "avgRating", "nRating", "label")
      )

    ggplot(data = plotData %>% drop_na(nRating),
           aes(
             x = log10(nRating + 1),
             fill = fct_relevel(label, "all items")
           )) +
      geom_density(alpha = 0.5) +
      scale_y_continuous(expand = expand_scale(mult = c(0, 0.025))) +
      # scale_x_continuous(limits = c(0.5, 5.5),
      #                    expand = expand_scale(mult = c(0, 0))) +
      theme_minimal(base_size = 18) +
      labs(title = "Number of Ratings",
           x = "log10(Number of Ratings + 1)",
           y = NULL,
           fill = NULL) +
      theme(legend.position = "bottom",
            aspect.ratio = 1 / 2,
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())

  }, bg = "transparent")


  # Metadata --------------------------------------------------------------
  metadata <- reactive({
    # Ensure input is available
    req(summaryData(), input$focusSet, input$linkThickness)

    networkData <- getRadialSetsData(
      setSizes = summaryData()$setSizes,
      setSizesByDegree = summaryData()$setSizesByDegree,
      setIntersections = summaryData()$setIntersections,
      focusSet = input$focusSet,
      linkThickness = input$linkThickness
    )

    getRadialSetsMetadata(networkData,
                          bezierW = input$bezierW,
                          bezierHRatio = input$bezierHRatio)

  })

  # Plot tooltip ----------------------------------------------------------
  output$hover_info <- renderUI({
    # Ensure hover input is available
    req(metadata(), input$plot_hover)

    # Get pointer location
    hoverLoc <- getPointerLoc(metadata(),
                              input$plot_hover,
                              transCoord = T)

    # Create tooltip
    createRadialsetsTooltip(
      setSizes = summaryData()$setSizes,
      setSizesByDegree = summaryData()$setSizesByDegree,
      setIntersections = summaryData()$setIntersections,
      hoverLoc,
      focusSet = input$focusSet,
      linkThickness = input$linkThickness
    )
  })


  # Plot click actions ----------------------------------------------------
  observeEvent(input$plotDblClick, {
    req(dblClickLoc())

    if (dblClickLoc()$location == "sector") {
      # Set x-variable input to monthYear
      updateSelectizeInput(session = session,
                           inputId = "focusSet",
                           selected = dblClickLoc()$set)

    }


  })


  # Click location --------------------------------------------------------
  clickLoc <- reactive({
    req(metadata())

    getPointerLoc(metadata(), input$plotClick, transCoord = T)
  })
  dblClickLoc <- reactive({
    req(metadata())

    getPointerLoc(metadata(), input$plotDblClick, transCoord = T)
  })


  # Click label -----------------------------------------------------------
  clickLabel <- reactive({
    label <- ""
    if (!is.null(clickLoc())) {
      if (is.null(clickLoc()$location)) {
        label = ""
      } else if (clickLoc()$location == "sector") {
        label <- clickLoc()$set
      } else if (clickLoc()$location == "bar") {
        label <- glue("{clickLoc()$set} (degree {clickLoc()$degree})")
      } else if (clickLoc()$location == "link") {
        label <- glue("{clickLoc()$set1} and {clickLoc()$set2}")
      }
    }

    return(label)
  })



  # Plot click actions ----------------------------------------------------
  tableData <- reactive({

    # req(clickLoc())

    # Define set names
    setNames <- movieSets %>%
      select(Action:Western) %>%
      colnames()

    selectedItems <- movieSets %>%
      mutate(degree = str_count(genres, "\\|") + 1) %>%
      mutate(selected = FALSE)
    if (!is.null(clickLoc()$location)) {

      if (clickLoc()$location == "sector") {
        selectedItems <- selectedItems %>%
          mutate(selected = (!!sym(clickLoc()$set) == 1))
      } else if (clickLoc()$location == "bar") {
        selectedItems <- selectedItems %>%
          mutate(selected = (!!sym(clickLoc()$set) == 1) &
                   (degree == clickLoc()$degree))
      } else if (clickLoc()$location == "link") {
        selectedItems <- selectedItems %>%
          mutate(selected = (!!sym(clickLoc()$set1) == 1) &
                   (!!sym(clickLoc()$set2) == 1))
      }
    }

    selectedItems <- selectedItems %>%
      select(movieId,
             title,
             year,
             genres,
             avgRating,
             nRating,
             imdbId,
             tmdbId,
             degree,
             selected) %>%
      mutate(
        imdbId = glue(
          "<a href='http://www.imdb.com/title/tt{imdbId}' target='_blank'>{imdbId}</a>"
        ),
        tmdbId = glue(
          "<a href='https://www.themoviedb.org/movie/{tmdbId}' target='_blank'>{tmdbId}</a>"
        ),
        movieId = glue(
          "<a href='https://movielens.org/movies/{movieId}' target='_blank'>{movieId}</a>"
        )
      )

    return(selectedItems)


  })

  # Render data table -----------------------------------------------------
  output$selectedTable <- renderDataTable(
    tableData() %>%
      filter(selected) %>%
      select(-selected) %>%
      mutate(avgRating = round(avgRating *
                                 10) / 10) %>%
      rename(
        `Movie Lens ID` = movieId,
        Title = title,
        `Release Date` = year,
        Genre = genres,
        `Average Rating` = avgRating,
        `Number of Ratings` = nRating,
        `IMDB ID` = imdbId,
        `The Movie DB ID` = tmdbId,
        `Number of Genres` = degree
      ),
    escape = FALSE
  )

  # Output plot UI --------------------------------------------------------
  output$plotUI <- renderUI({
    # Extra div used ONLY to create positioned ancestor for tooltip
    div(
      style = "position:relative",
      # Plot output
      imageOutput(
        "radPlotImage",
        width = glue("{pageHeight()*0.95}px"),
        height = glue("{pageHeight()*0.95}px"),
        click = "plotClick",
        dblclick = "plotDblClick",
        hover = hoverOpts("plot_hover",
                          delay = 100,
                          delayType = "debounce",
                          clip = F,
                          nullOutside = F),
        inline = F
      ),
      # plotOutput(
      #   "radPlot",
      #   width = glue("{pageHeight()*0.95}px"),
      #   height = glue("{pageHeight()*0.95}px"),
      #   click = "plotClick",
      #   dblclick = "plotDblClick",
      #   hover = hoverOpts("plot_hover",
      #                     delay = 100,
      #                     delayType = "debounce",
      #                     clip = F,
      #                     nullOutside = F),
      #   inline = F
      # ),
      # Tooltip output
      uiOutput("hover_info")
    )
  })

  # Output rating histogram UI --------------------------------------------------------
  output$ratingHistUI <- renderUI({
    plotOutput(
      "ratingHist",
      width = "100%",
      height = glue("{pageHeight()*0.95*0.5-2.5}px")
    )
  })

  # Output watches histogram UI --------------------------------------------------------
  output$watchesHistUI <- renderUI({
    plotOutput(
      "watchesHist",
      width = "100%",
      height = glue("{pageHeight()*0.95*0.5-2.5}px")
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
