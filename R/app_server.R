#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @importFrom shiny reactive
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  rval_xafty_list <- shiny::reactiveValues(
    check_table = NULL,
    validity_table = NULL,
    xafty_names = NULL,
    xafty_button_ids = NULL,
    xafty_tab_id = NULL,
    xafty_tabs_ids = NULL,
    check_rules_ids = NULL, # For Checking rules with a button
    check_table_ids = NULL # For Creating UI
  )

  rval_xafty_master <- shiny::reactiveValues()

  check_table <- shiny::reactive({

    check_table_input <- input$input_check_table_main

    if(is.null(check_table_input)) return(NULL)

    read.csv(check_table_input$datapath)

  })

  validity_table <- shiny::reactive({

    validity_table_input <- input$input_validity_table_main

    if(is.null(validity_table_input)) return(NULL)

    read.csv(validity_table_input$datapath)

  })

  # aligned_check_table <- shiny::reactive({
  #   req(check_table())
  #   req(validity_table())
  #   xafty::align_column_types(check_table = check_table(), validity_table = validity_table())
  # })

  xafty_list <- shiny::reactive({
    req(check_table())
    req(validity_table())

    rval_xafty_list$check_table <- check_table()
    rval_xafty_list$validity_table <- validity_table()
    xafty_list <- xafty::build_xafty_list(check_table = check_table(), validity_table = validity_table())

    xafty_table <- xafty::build_xafty_test_table(xafty_list)

    rval_xafty_master[["xafty_table"]] <- xafty_table


    unique_id_list <- list()

    for (i in seq(nrow(xafty_table))) {

      xafty_column <- xafty_table$column[i]
      xafty_rule <- sub("##!!", "", xafty_table$rule[i])

      xafty_list_values <- xafty_list[[xafty_column]][[xafty_rule]]

      unique_id <- paste0(xafty_column, xafty_rule)

      rval_xafty_master[[unique_id]] <- xafty_list_values

      unique_id_list[[i]] <- unique_id

    }

    rval_xafty_master[["unique_ids"]] <- do.call(c, unique_id_list)

    xafty_list

  })

  mod_button_tab_constructor_server("button_tab_constructor_1",
                                    xafty_list = xafty_list,
                                    rval_xafty_list = rval_xafty_list,
                                    rval_xafty_master = rval_xafty_master)

  output$download_check_table <- shiny::downloadHandler(
      filename = paste0("check_table_repaired.csv"),
      content = function(file) {
        write.csv(rval_xafty_list$check_table, file, row.names = FALSE)
      }
  )

}
