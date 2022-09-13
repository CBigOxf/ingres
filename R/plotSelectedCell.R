#' Plot the PBN of a cell selected in an interactive plot.
#'
#' Launches a Shiny app with an interactive tSNE plot from a Seurat object. When
#' a cell is clicked, the PBN for that cell is plotted side by side
#' with the tSNE plot.
#' Internally, it uses \code{\link{cellPbnPlot}} on the selected cell.
#'
#' @param ingres.object An \code{\linkS4class{ingres}} object with a non-null
#'  \code{single.cell.pbn} slot.
#' @param seurat.object A \code{\link[SeuratObject:Seurat-class]{Seurat object}}
#' created with the same dataset as the ingres object. Ideally, this object
#' was used to create the ingres object through
#' \code{\link{createIngresObjectFromSeurat}}.
#'
#' @return No return value, called only to launch the Shiny app.
#'
#' @examplesIf interactive()
#' ing = createIngresObjectFromSeurat(
#'   small_blca_wang, "RNA", "data", network_genes, network
#' )
#' ing@viper = viper_results
#' ing = computePbnBySingleCell(ing)
#' plotSelectedCell(ing, small_blca_wang)
#'
#' @export
plotSelectedCell = function(ingres.object, seurat.object) {
  optionalPkgs = c("Seurat", "shiny", "plotly")
  for (pkg in optionalPkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0(
        "Package \"", pkg,
        "\" needed for this function to work. Please install it."
      ),
      call. = FALSE
      )
    }
  }

  ui = shiny::fillPage(
    shiny::titlePanel("Click on a cell to see its PBN"),
    shiny::h4("You can zoom in by dragging the mouse"),
    shiny::fillRow(
      plotly::plotlyOutput("plot", height = "90%", width = "100%"),
      shiny::plotOutput("result", height = "90%", width = "100%")
    ),
    padding = 10
  )

  server = function(input, output) {
    p = Seurat::DimPlot(seurat.object, reduction = "tsne", pt.size = 1)
    p.data = p$data %>% select(-3)

    output$plot = plotly::renderPlotly({
      Seurat::HoverLocator(
        plot = p,
        information = Seurat::FetchData(object = seurat.object, vars = "ident")
      )
    })

    output$result = shiny::renderPlot({
      d = plotly::event_data("plotly_click")
      if (!is.null(d)) {
        dx = d$x
        dy = d$y
        cellrow = p.data %>%
          filter(near(p.data[[1]], dx) & near(p.data[[2]], dy)) %>%
          tibble::rownames_to_column("cell")
        cellPbnPlot(ingres.object, cellrow$cell)
      }
    })
  }
  app = shiny::shinyApp(ui, server)
  return(shiny::runApp(app))
}
