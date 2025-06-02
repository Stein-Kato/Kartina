viewer_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    titlePanel("Old Faithful Geyser Data"),
    sidebarLayout(
      sidebarPanel(
       shiny::uiOutput(ns("bins_slider")),
      ),
      shiny::mainPanel(
        shiny::uiOutput(ns("distPlot"))
      )
    )
  )
}

viewer_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      output$bins_slider <- shiny::renderUI(

        sliderInput(ns("bins"),
                    "Number of bins:",
                    min = 0,
                    max = 30,
                    value = 5
                    )
      )

      output$distPlot <- renderUI({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        shiny::renderPlot(hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
        )
      })
    }
  )
}
