#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @importFrom shiny reactive
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  rval_xafty_list <- shiny::reactiveValues(
    xafty_names = NULL,
    xafty_button_ids = NULL,
    xafty_tab_id = NULL,
    xafty_tabs_ids = NULL
  )

  check_table <- shiny::reactive({

    check_table_input <- input$input_check_table_main

    if(is.null(check_table_input)) return(NULL)

    print(check_table_input$datapath)
    read.csv(check_table_input$datapath)

  })

  validity_table <- shiny::reactive({

    validity_table_input <- input$input_validity_table_main

    if(is.null(validity_table_input)) return(NULL)

    read.csv(validity_table_input$datapath)

  })

  mod_button_tab_constructor_server("button_tab_constructor_1", check_table = check_table,
                                    validity_table = validity_table, rval_xafty_list = rval_xafty_list)

}
