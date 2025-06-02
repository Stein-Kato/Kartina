app_ui <- function() {
  app_title <- "Kartina - Image analyzer"

  shiny::tagList(
    shiny::navbarPage(
      title = app_title,
      windowTitle = app_title,
      id = "tabs",
      shiny::tabPanel(
        value = "tab1",
        shiny::span("View image",
                    title = "Tab 1"
        ),
        viewer_ui("viewer")
      ),
    )
  )
}
