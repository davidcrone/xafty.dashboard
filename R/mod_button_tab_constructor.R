#' button_tab_constructor UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList uiOutput
mod_button_tab_constructor_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::uiOutput(ns("xafty_buttons")),
    shiny::uiOutput(ns("xafty_tabs"))

  )
}

#' button_tab_constructor Server Functions
#'
#' @noRd
#' @importFrom shiny reactive
mod_button_tab_constructor_server <- function(id, check_table, validity_table, rval_xafty_list){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    xafty_list <- shiny::reactive({
      req(check_table())
      req(validity_table())
      xafty::build_xafty_list(check_table = check_table(), validity_table = validity_table())
    })

    shiny::observe(
      rval_xafty_list$xafty_names <- names(xafty_list())
    )

    output$xafty_buttons <- shiny::renderUI({

      req(rval_xafty_list$xafty_names)

      button_names <- rval_xafty_list$xafty_names

      button_id <- sapply(button_names, \(button_name) {
        paste0(button_name, "_xafty_button")
      })

      rval_xafty_list$xafty_button_ids <- button_id

      xafty_button_list <- lapply(button_names, \(button_name) {
        shiny::actionButton(inputId = ns(paste0(button_name, "_xafty_button")), label = button_name)
      })

      do.call(tagList, xafty_button_list)

    })

    output$xafty_tabs <- shiny::renderUI({

      req(rval_xafty_list$xafty_names)

      tab_names <- rval_xafty_list$xafty_names

      tab_id <- sapply(tab_names, \(tab_name) {
        paste0(tab_name, "_xafty_tab")
      })

      rval_xafty_list$xafty_tabs_ids <- tab_id
      rval_xafty_list$xafty_tab_id <- "xafty_tabset"

      xafty_tabs_list <- lapply(tab_names, function(tab_name) {
        tabPanel(
          title = paste0(tab_name, "_xafty_tab"),
          h2(paste("Content for tab", tab_name))
        )
      })

      # Create tabsetPanel without tab titles
      do.call(tabsetPanel, c(xafty_tabs_list, list(id = ns("xafty_tabset"), type = "hidden", selected = tab_id[[2]])))

    })

    shiny::observe({

      req(rval_xafty_list$xafty_tabs_ids, rval_xafty_list$xafty_tab_id, rval_xafty_list$xafty_button_ids)

      xafty_tabs_ids <- rval_xafty_list$xafty_tabs_ids
      xafty_tabset_id <- rval_xafty_list$xafty_tab_id
      xafty_button_ids <- rval_xafty_list$xafty_button_ids

      n_buttons <- length(xafty_button_ids)
      for (i in seq(n_buttons)) {
        local({
          local_i <- i
          xafty_tab_title <- xafty_tabs_ids[[local_i]]
          xafty_button_id <- xafty_button_ids[[local_i]]

          observeEvent(input[[xafty_button_id]], {
            # Now, local_i is the value of i during this iteration of the loop
            shiny::updateTabsetPanel(session = session, inputId = xafty_tabset_id, selected = xafty_tab_title)
          })
        })

        }
      })

  })
}

## To be copied in the UI
# mod_button_tab_constructor_ui("button_tab_constructor_1")

## To be copied in the server
# mod_button_tab_constructor_server("button_tab_constructor_1")
