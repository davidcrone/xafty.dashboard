#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @importFrom shiny tagList fluidPage sidebarLayout sidebarPanel mainPanel h1
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shiny::fluidPage(
      shiny::sidebarLayout(
        shiny::sidebarPanel(width = 3,
          shiny::h1("Excel Checker"),
          shiny::fileInput("input_check_table_main", label = "Upload File to Check", accept = ".csv"),
          shiny::fileInput("input_validity_table_main", label = "Upload File to Validate", accept = ".csv")
                     ),
        shiny::mainPanel(
          mod_button_tab_constructor_ui("button_tab_constructor_1")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "xafty.dashboard"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
