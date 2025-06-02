viewer_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    titlePanel("Inspect image"),
    sidebarLayout(
      sidebarPanel(
       shiny::uiOutput(ns("image_input")),
       shiny::splitLayout(
         shiny::uiOutput(ns("channel_radio_buttons")),
         shiny::uiOutput(ns("overlay_checkbox")),
       ),
       shiny::hr(),
       shiny::uiOutput(ns("bins_slider")),
       shiny::uiOutput(ns("threshold_slider")),
      shiny::uiOutput(ns("ymax_input"))
      ),
      shiny::mainPanel(
        shiny::uiOutput(ns("image_display")),
        shiny::uiOutput(ns("image_histogram"))
      )
    )
  )
}

viewer_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Reactive values
      im <- shiny::reactive({
        shiny::req(input$image)
        imager::load.image(input$image$datapath)
      })

      # Image input
      output$image_input <- shiny::renderUI({
        shiny::fileInput(ns("image"),
                         label = "Select image",
                         accept = "image/*",
                         placeholder = "Select an image")
      })

      # Channel selector
      output$channel_radio_buttons <- shiny::renderUI({
        shiny::radioButtons(ns("channel"),
                            label = "Choose channels",
                            choiceNames = list(
                              "All",
                              "Red",
                              "Green",
                              "Blue"
                              ),
                            choiceValues = list(0, 1, 2, 3)
                            )
      })

      # Histogram bins slider
      output$bins_slider <- shiny::renderUI(
        sliderInput(ns("bins"),
                    "Number of bins:",
                    min = 0,
                    max = 30,
                    value = 5
                    )
      )

      output$threshold_slider <- shiny::renderUI({
        shiny::sliderInput(ns("threshold"),
                           "Lower threshold:",
                           min = 0,
                           max = 1,
                           step = 0.01,
                           value = 0)
      })

      output$overlay_checkbox <- shiny::renderUI({
        shiny::checkboxInput(ns("overlay"), "Overlay", FALSE)
      })

      output$ymax_input <- shiny::renderUI({
        shiny::numericInput(ns("ymax"),
                            "Y axis limit",
                            min = 0,
                            value = 50000
                            )
      })

      output$image_display <- renderUI({
        shiny::req(input$image, input$channel)
        im_raw <- im()


        channel <- as.numeric(input$channel)

        if (channel == 0) {
          im_channeled <- im_raw
        } else {
          im_channeled <- imager::channels(im_raw, channel)[[1]]
        }

        pix <- im_raw >= input$threshold

        shiny::renderPlot({
          #plot(im_channeled)
          #if (input$threshold > 0){
          #  imager::highlight(pix)
          #}
          if (input$overlay) {
            imager::colorise(im_channeled, pix, "red",alpha=.5) |> plot()
          } else {
            plot(im_channeled)
          }

        })
      })

      output$image_histogram <- shiny::renderUI({
        shiny::req(input$image)
        shiny::renderPlot({
          image_hist(im(), input$bins, input$threshold, input$ymax)
        })
      })
    }
  )
}
