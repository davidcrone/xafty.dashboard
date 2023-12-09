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
mod_button_tab_constructor_server <- function(id, xafty_list, rval_xafty_list){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    rval_sm <- shiny::reactiveValues()
    render_tables_created <- shiny::reactiveVal(FALSE)

    shiny::observe(
      rval_xafty_list$xafty_names <- names(xafty_list())
    )

    output$xafty_buttons <- shiny::renderUI({

      req(rval_xafty_list$xafty_names)

      button_names <- rval_xafty_list$xafty_names
      xafty_list <- isolate(xafty_list())

      xafty_test_results <- sapply(button_names, \(column){
        xafty::get_xafty_list_items(xafty_list = xafty_list, column = column, item = "test_result")
      })

      all_true_list <- suppressWarnings(lapply(xafty_test_results, all))

      button_id <- sapply(button_names, \(button_name) {
        paste0(button_name, "_xafty_button")
      })
      rval_xafty_list$xafty_button_ids <- button_id

      xafty_button_list <- mapply(\(button_name, test_result) {
        if(test_result) {
          shiny::actionButton(inputId = ns(paste0(button_name, "_xafty_button")), label = button_name, class = "btn-success fade-in")
        } else {
          shiny::actionButton(inputId = ns(paste0(button_name, "_xafty_button")), label = button_name, class = "btn-danger fade-in")
        }
      }, button_names, all_true_list, SIMPLIFY = FALSE)

      do.call(tagList, xafty_button_list)

    })

    output$xafty_tabs <- shiny::renderUI({

      req(rval_xafty_list$xafty_names)

      xafty_list <- isolate(xafty_list())
      xafty_table <- xafty::build_xafty_test_table(xafty_list)
      tab_names <- rval_xafty_list$xafty_names

      tab_id <- sapply(tab_names, \(tab_name) {
        paste0(tab_name, "_xafty_tab")
      })

      rval_xafty_list$xafty_tabs_ids <- tab_id
      rval_xafty_list$xafty_tab_id <- "xafty_tabset"

      rval_xafty_list$check_rules_ids <- sub("##!!", "",xafty_table$rule)
      rval_xafty_list$check_table_ids <- xafty_table$column

      xafty_tabs_list <- lapply(tab_names, function(tab_name) {
        tabPanel(
          title = paste0(tab_name, "_xafty_tab"),
          create_wellPanel_summary(xafty_list = xafty_list, tab_name = tab_name),
          create_rules_cards(ns = ns, xafty_list = xafty_list, tab_name = tab_name),
          class = "fade-in")
      })

      # Create tabsetPanel without tab titles
      do.call(tabsetPanel, c(xafty_tabs_list, list(id = ns("xafty_tabset"), type = "hidden", selected = tab_id[[2]],
                                                   header= div(style = "height: 10px;"))))

    })

    # Observer to bind events to Buttons
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

    shiny::observe({

      req(rval_xafty_list$check_rules_ids, rval_xafty_list$check_table_ids)

      xaft_rules <- rval_xafty_list$check_rules_ids
      xafty_columns <- rval_xafty_list$check_table_ids

      xafty_rules_ids <- paste0("check_", xaft_rules, "_", xafty_columns)
      xafty_table_ids <- paste0("table_", xaft_rules, "_", xafty_columns)
      xafty_card_header_ids <- paste0("card_header_", xaft_rules,"_", xafty_columns)

      n_buttons <- length(xafty_rules_ids)

      xafty_list <- xafty_list()


      for (i in seq(n_buttons)) {
        local({

          local_i <- i
          xafty_button_id <- xafty_rules_ids[[local_i]]
          rhandsontabe_id <- xafty_table_ids[[local_i]]
          render_handsontable_id <- paste0("r_", rhandsontabe_id)
          xafty_card_header_id <- xafty_card_header_ids[[local_i]]

          xafty_column <- xafty_columns[[local_i]]
          xafty_rule <- xaft_rules[[local_i]]

          unique_id <- paste0(xafty_column, xafty_rule)
          xafty_list_values <- xafty_list[[xafty_column]][[xafty_rule]]

          xafty_list_values[["unique_id"]] <- unique_id
          xafty_list_values[["xafty_button_id"]] <- xafty_button_id
          xafty_list_values[["rhandsontabe_id"]] <- rhandsontabe_id
          xafty_list_values[["render_handsontable_id"]] <- render_handsontable_id
          xafty_list_values[["xafty_card_header_id"]] <- xafty_card_header_id

          rval_sm[[unique_id]] <- xafty_list_values

          output[[rhandsontabe_id]] <- shiny::renderUI({
            rhandsontable::rHandsontableOutput(ns(render_handsontable_id))
            })

      })


    } # End local i
      render_tables_created(TRUE)

      })

    shiny::observeEvent(render_tables_created(), {

      req(render_tables_created())

      unique_ids <- names(rval_sm)

      for (unique_id in unique_ids) {
        local({

          local_unique_id <- unique_id

          xafty_items <- rval_sm[[local_unique_id]]
          xafty_button_id <- xafty_items[["xafty_button_id"]]
          render_handsontable_id <- xafty_items[["render_handsontable_id"]]
          xafty_card_header_id <- xafty_items[["xafty_card_header_id"]]

          observeEvent(input[[xafty_button_id]], {
            # Now, local_i is the value of i during this iteration of the loop

            check_table <- rval_xafty_list$check_table
            validity_table <- rval_xafty_list$validity_table
            xafty_rule_items <- rval_sm[[local_unique_id]]

           if (xafty_rule_items$rule_type == "data_type") {
             values_check_table <- check_table[, xafty_rule_items$column_name]
             changed_values_check_table <- xafty_rule_items$change_type_function(values_check_table)

             if (identical(values_check_table, as.character(changed_values_check_table))) {

               check_table[[xafty_rule_items$column_name]] <- changed_values_check_table
               xafty_rule_items$filter_result <- rep(FALSE, length(changed_values_check_table))

             }

           }

            single_col_check_table <- check_table[xafty_rule_items$column_name]
            new_test_result <- xafty_rule_items$check_function(single_col_check_table, validity_table)
            rval_sm[[local_unique_id]]$test_result <- new_test_result$Check_Result

            if (!any(xafty_rule_items$filter_result)) {

              shinyjs::removeClass(id = xafty_card_header_id, class = "bg-danger")
              shinyjs::addClass(id = xafty_card_header_id, class = "bg-success")

              shinyjs::hide(id = render_handsontable_id, anim = TRUE, animType = "fade", time = 0.5)

              xafty_list <- xafty_list()
              xafty_rules <- names(xafty_list[[xafty_rule_items$column_name]])
              unique_id <- paste0(xafty_rule_items$column_name, xafty_rules)

              test_result_all_rules <-  sapply(unique_id, \(x){
                rval_sm[[x]]$test_result
              })

              if(all(test_result_all_rules)) {

                xafty_button_id <- paste0(xafty_rule_items$column_name, "_xafty_button")

                shinyjs::removeClass(id = xafty_button_id, class = "btn-danger")
                shinyjs::addClass(id = xafty_button_id, class = "bg-success")

              }

            } else {

              check_result_logical <- xafty_rule_items$filter_function(check_table, validity_table = validity_table,
                                                                       filter_column = xafty_rule_items$column_name,
                                                                       xafty_rule = xafty_rule_items$rule_syntax,
                                                                       xafty_values = xafty_rule_items$values)

              # Write changes back to reactive value
              rval_sm[[local_unique_id]]$filter_result <- check_result_logical

              false_values <- check_table[[xafty_rule_items$column_name]][check_result_logical]

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
