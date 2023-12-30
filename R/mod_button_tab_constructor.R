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
mod_button_tab_constructor_server <- function(id, xafty_list, rval_xafty_list, rval_xafty_master){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    rval_sm <- shiny::reactiveValues()
    well_panels_created <- shiny::reactiveVal(FALSE)
    render_tables_created <- shiny::reactiveVal(FALSE)

    shiny::observe({
      xafty_list_columns <- names(xafty_list())
      rval_xafty_master[["xafty_list_columns"]] <- xafty_list_columns
      })

    output$xafty_buttons <- shiny::renderUI({

      req(rval_xafty_master$xafty_list_columns)

      button_names <- rval_xafty_master$xafty_list_columns
      xafty_table <- rval_xafty_master$xafty_table

      test_result_each_column <- sapply(button_names, \(column){
        all(xafty_table$test_result[xafty_table$column == column])
      })

      xafty_button_ids <- sapply(button_names, \(button_name) {
        paste0(button_name, "_xafty_button")
      })

      rval_xafty_master[["xafty_button_ids"]] <- xafty_button_ids

      xafty_button_list <- mapply(\(button_id, button_name, test_result) {
        if(test_result) {
          shiny::actionButton(inputId = ns(button_id), label = button_name, class = "btn-success fade-in")
        } else {
          shiny::actionButton(inputId = ns(button_id), label = button_name, class = "btn-danger fade-in")
        }
      }, xafty_button_ids, button_names, test_result_each_column, SIMPLIFY = FALSE)

      do.call(tagList, xafty_button_list)

    })

    output$xafty_tabs <- shiny::renderUI({

      req(rval_xafty_master$xafty_list_columns)

      xafty_list <- isolate(xafty_list())

      xafty_table <- rval_xafty_master$xafty_table
      tab_names <- rval_xafty_master$xafty_list_columns

      tab_ids <- sapply(tab_names, \(tab_name) {
        paste0(tab_name, "_xafty_tab")
      })

      rval_xafty_master[["xafty_tabs_ids"]] <- tab_ids

      xafty_tabs_list <- lapply(tab_names, function(tab_name) {
        tabPanel(
          title = paste0(tab_name, "_xafty_tab"),
          create_wellPanel_summary(xafty_list = xafty_list, tab_name = tab_name),
          create_rules_cards(ns = ns, xafty_list = xafty_list, tab_name = tab_name),
          class = "fade-in")
      })

      well_panels_created(TRUE)

      # Create tabsetPanel without tab titles
      do.call(tabsetPanel, c(xafty_tabs_list, list(id = ns("xafty_tabset"), type = "hidden", selected = tab_ids[[2]],
                                                   header= div(style = "height: 10px;"))))

    })

    # Observer to bind events to Xafty Buttons
    shiny::observe({

      req(rval_xafty_master$xafty_tabs_ids, rval_xafty_master$xafty_button_ids)

      xafty_tabs_ids <- rval_xafty_master$xafty_tabs_ids
      xafty_button_ids <- rval_xafty_master$xafty_button_ids

      n_buttons <- length(xafty_button_ids)
      for (i in seq(n_buttons)) {
        local({
          local_i <- i
          xafty_tab_title <- xafty_tabs_ids[[local_i]]
          xafty_button_id <- xafty_button_ids[[local_i]]

          observeEvent(input[[xafty_button_id]], {
            # Now, local_i is the value of i during this iteration of the loop
            shiny::updateTabsetPanel(session = session, inputId = "xafty_tabset", selected = xafty_tab_title)
          })
        })

        }
      })


    shiny::observe({

      req(well_panels_created())

      xafty_table <- rval_xafty_master$xafty_table

      n_buttons <- nrow(xafty_table)

      for (i in seq(n_buttons)) {
        local({

          local_i <- i

          add_ids_to_rval_master(i = local_i,
                                 xafty_table = xafty_table,
                                 rval_xafty_master = rval_xafty_master)

          unique_id <- rval_xafty_master$unique_ids[local_i]
          rhandsontabel_id <- rval_xafty_master[[unique_id]][["rhandsontabel_id"]]
          render_handsontable_id <- rval_xafty_master[[unique_id]][["render_handsontable_id"]]

          output[[rhandsontabel_id]] <- shiny::renderUI({
            rhandsontable::rHandsontableOutput(ns(render_handsontable_id))
            })

      })


    } # End local i
      render_tables_created(TRUE)

      })

    shiny::observeEvent(render_tables_created(), {

      req(render_tables_created())

      unique_ids <- rval_xafty_master$unique_ids

      for (unique_id in unique_ids) {
        local({

          local_unique_id <- unique_id

          xafty_items <- rval_xafty_master[[local_unique_id]]
          xafty_single_rule_button_id <- xafty_items[["xafty_button_id"]]
          render_handsontable_id <- xafty_items[["render_handsontable_id"]]
          rhandsontabel_id <- xafty_items[["rhandsontabel_id"]]
          xafty_card_header_id <- xafty_items[["xafty_card_header_id"]]

          observeEvent(input[[xafty_single_rule_button_id]], {
            # Now, local_i is the value of i during this iteration of the loop

            check_table <- rval_xafty_list$check_table
            validity_table <- rval_xafty_list$validity_table
            xafty_rule_items <- rval_xafty_master[[local_unique_id]]

            req(isFALSE(rval_xafty_master[[local_unique_id]]$test_result))

            check_result_logical <- xafty_rule_items$filter_function(check_table, validity_table = validity_table,
                                                                     filter_column = xafty_rule_items$column_name,
                                                                     xafty_rule = xafty_rule_items$rule_syntax,
                                                                     xafty_values = xafty_rule_items$values)
            new_test_result <-  all(!check_result_logical)

            rval_xafty_master[[local_unique_id]]$test_result <- new_test_result

            if (rval_xafty_master[[local_unique_id]]$test_result) {

              shinyjs::removeClass(id = xafty_card_header_id, class = "bg-danger")
              shinyjs::addClass(id = xafty_card_header_id, class = "bg-success")
              shinyjs::hide(id = rhandsontabel_id, anim = TRUE, animType = "fade", time = 0.5)
              #shinyjs::hide(id = xafty_single_rule_button_id, anim = TRUE, animType = "fade", time = 0.5)

              xafty_rules <- sub("##!!", "", rval_xafty_master$xafty_table$rule[rval_xafty_master$xafty_table$column == xafty_rule_items$column_name])
              unique_ids <- paste0(xafty_rule_items$column_name, xafty_rules)

              test_result_all_rules <-  sapply(unique_ids, \(unique_id){
                rval_xafty_master[[unique_id]]$test_result
              })

              if(all(test_result_all_rules)) {

                xafty_button_id <- paste0(xafty_rule_items$column_name, "_xafty_button")

                shinyjs::removeClass(id = xafty_button_id, class = "btn-danger")
                shinyjs::addClass(id = xafty_button_id, class = "bg-success")

              }

            } else {

              row_highlight <- which(check_result_logical) - 1
              col_highlight <- which(colnames(check_table) == xafty_rule_items$column_name) - 1

              js_row_highlight <- paste0(row_highlight, collapse = ",")
              js_row_highlight <- paste0("[", js_row_highlight, "]")

              js_col_highlight <- paste0(col_highlight, collapse = ",")
              js_col_highlight <- paste0("[", col_highlight, "]")


              output[[render_handsontable_id]] <-  rhandsontable::renderRHandsontable({

                rhandsontable::rhandsontable(check_table) |>
                                            rhandsontable::hot_cols(renderer = paste0("
                          function(instance, td, row, col, prop, value, cellProperties) {
                          Handsontable.renderers.TextRenderer.apply(this, arguments);

                          tbl = this.HTMLWidgets.widgets[0]

                          hcols = ", js_col_highlight, "
                          hrows = ", js_row_highlight,"

                          if (hcols.includes(col) && hrows.includes(row)) {
                            td.style.background = 'pink';
                          }

                          return td;
                        }")) # |>

              })


            }
          })

          observeEvent(input[[render_handsontable_id]]$changes,{

            req(input[[render_handsontable_id]]$changes$changes)

            row_change <- input[[render_handsontable_id]]$changes$changes[[1]][[1]] + 1
            col_change <- input[[render_handsontable_id]]$changes$changes[[1]][[2]] + 1

            changed_to <- input[[render_handsontable_id]]$changes$changes[[1]][[4]]

            check_table <- rval_xafty_list$check_table

            check_table[row_change, col_change] <- changed_to

            rval_xafty_list$check_table <- check_table

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
