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
                   choices = c("Percent" = "percent",
                               "Count" = "count"),
                   multiple = FALSE,
                   selected = "percent"
               )
        )
    )
    ),
    fluidRow(
      column(8,
             div(
               id = 'plotContainer',
             uiOutput("plotUI"))
             ),
      column(4,
             div(
               id = 'plotContainer',
               uiOutput("ratingHistUI")
               ),
             div(
               id = 'plotContainer',
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
  # output$radPlot <- renderImage({
  #   req(input$focusSet)
  #
  #   width  <- session$clientData$output_radPlot_width
  #   height <- session$clientData$output_radPlot_height
  #
  #   outfile <- tempfile(fileext=".svg")
  #
  #   pxToIn <- 1/96
  #
  #   svg(outfile,
  #       width = width*pxToIn,
  #       height = height*pxToIn)
  #
  #   buildRadialSetsPlot(
  #     setSizes = summaryData()$setSizes,
  #     setSizesByDegree = summaryData()$setSizesByDegree,
  #     setIntersections = summaryData()$setIntersections,
  #     focusSet = input$focusSet,
  #     linkThickness = input$linkThickness,
  #     sectorLabelFontSize = 1.5
  #   )
  #   dev.off()
  #
  #   list(src = outfile,
  #        width = width,
  #        height = height)
  #
  # }, deleteFile = F)
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
    # req(tableData())

    # Freedman–Diaconis rule for histogram binwidth
    FD_rule <- function(x) {
      x <- x[!is.na(x)]
      binwidth <- (2 * IQR(x)) / length(x) ^ (1 / 3)
      return(binwidth)
    }

    df <- movieSets %>%
      mutate(label = "all items")

    if (isTruthy(input$plotClick)) {
      clickData <- radialSetsClickActions(
        setSizes = summaryData()$setSizes,
        setSizesByDegree = summaryData()$setSizesByDegree,
        setIntersections = summaryData()$setIntersections,
        input$plotClick,
        focusSet = input$focusSet,
        linkThickness = input$linkThickness
      )

      plotTitle <- NULL
      if (clickData$clickSection) {
        plotTitle <- clickData$name

      } else if (clickData$clickLink) {
        plotTitle <- glue("{clickData$name1} and {clickData$name2}")
      }

      if (clickData$clickBar) {
        plotTitle <- glue("{clickData$name} (degree {clickData$degree})")
      }

      plotTitle <- as.character(plotTitle)

      df <- df %>%
        full_join(
          tableData() %>% mutate(label = plotTitle),
          by = c("title", "year",
                 "avgRating", "nRating", "label")
        )
    }

    ggplot(data = df,
           aes(x = avgRating,
               fill = fct_relevel(label, "all items"))) +
      geom_density(alpha = 0.5) +
      # scale_y_continuous(expand = expand_scale(mult = c(0, 0.025))) +
      # scale_x_continuous(limits = c(0.5, 5.5),
      #                    expand = expand_scale(mult = c(0, 0))) +
      theme_light(base_size = 18) +
      labs(title = glue("Average Rating"),
           fill = NULL) +
      theme(legend.position = "bottom",
            aspect.ratio = 1 / 2)

  }, bg = "transparent")

  # Render watches histogram -----------------------------------------------
  output$watchesHist <- renderPlot({
    # req(tableData())

    # Freedman–Diaconis rule for histogram binwidth
    FD_rule <- function(x) {
      x <- x[!is.na(x)]
      binwidth <- (2 * IQR(x)) / length(x) ^ (1 / 3)
      return(binwidth)
    }

    df <- movieSets %>%
      mutate(label = "all items")

    if (isTruthy(input$plotClick)) {
      clickData <- radialSetsClickActions(
        setSizes = summaryData()$setSizes,
        setSizesByDegree = summaryData()$setSizesByDegree,
        setIntersections = summaryData()$setIntersections,
        input$plotClick,
        focusSet = input$focusSet,
        linkThickness = input$linkThickness
      )

      plotTitle <- NULL
      if (clickData$clickSection) {
        plotTitle <- clickData$name

      } else if (clickData$clickLink) {
        plotTitle <- glue("{clickData$name1} and {clickData$name2}")
      }

      if (clickData$clickBar) {
        plotTitle <- glue("{clickData$name} (degree {clickData$degree})")
      }

      plotTitle <- as.character(plotTitle)

      df <- df %>%
        full_join(
          tableData() %>% mutate(label = plotTitle),
          by = c("title", "year",
                 "avgRating", "nRating", "label")
        )
    }

    ggplot(data = df,
           aes(
             x = log10(nRating + 1),
             fill = fct_relevel(label, "all items")
           )) +
      geom_density(alpha = 0.5) +
      # scale_y_continuous(expand = expand_scale(mult = c(0, 0.025))) +
      # scale_x_continuous(limits = c(0.5, 5.5),
      #                    expand = expand_scale(mult = c(0, 0))) +
      theme_light(base_size = 18) +
      labs(title = "Number of Ratings",
           fill = NULL) +
      theme(legend.position = "bottom",
            aspect.ratio = 1 / 2)

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

    getRadialSetsMetadata(networkData)

  })

  # Plot tooltip ----------------------------------------------------------
  output$hover_info <- renderUI({
    # Ensure hover input is available
    req(metadata())

    hoverLoc <- getPointerLoc(metadata(),
                              input$plot_hover,
                              transCoord = T)

    # createRadialsetsTooltip(
    #   setSizes = summaryData()$setSizes,
    #   setSizesByDegree = summaryData()$setSizesByDegree,
    #   setIntersections = summaryData()$setIntersections,
    #   hoverLoc,
    #   focusSet = input$focusSet,
    #   linkThickness = input$linkThickness
    # )

    # pointer <- input$plot_hover
    # # browser()
    print(glue("coord: {round(hoverLoc$x*100)/100}, {round(hoverLoc$y*100)/100}"))
  })

  # Plot click actions ----------------------------------------------------
  observeEvent(input$plotDblClick, {
    req(metadata())

    dblClickLoc <- getPointerLoc(metadata(), input$plotDblClick)

    if (dblClickLoc$location == "sector") {
      # Set x-variable input to monthYear
      updateSelectizeInput(session = session,
                           inputId = "focusSet",
                           selected = dblClickLoc$set)

    }


  })

  # Plot click actions ----------------------------------------------------
  tableData <- eventReactive(input$plotClick, {
    req(metadata())

    clickLoc <- getPointerLoc(metadata(), input$plotClick)

    if (is.null(clickLoc$location)) {
      return(NULL)
    }

    # Define set names
    setNames <- movieSets %>%
      select(Action:Western) %>%
      colnames()

    df <- movieSets
    if (clickLoc$location %in% c("sector", "bar")) {
      df <- df %>%
        filter(!!sym(clickLoc$set) == 1)

    } else if (clickLoc$location == "link") {
      df <- df %>%
        filter(!!sym(clickLoc$set1) == 1,!!sym(clickLoc$set2) == 1)
    }

    df <- df %>%
      select(movieId,
             title,
             year,
             genres,
             avgRating,
             nRating,
             imdbId,
             tmdbId) %>%
      mutate(degree = str_count(genres, "\\|") + 1) %>%
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

    if (clickLoc$location == "bar") {
      df <- df %>%
        filter(degree == clickLoc$degree)
    }

    return(df)


  })

  # Render data table -----------------------------------------------------
  output$selectedTable <- renderDataTable(
    tableData() %>%
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
      # imageOutput(
      plotOutput(
        "radPlot",
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
