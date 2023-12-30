##

create_wellPanel_summary <- function(xafty_list, tab_name) {

  xafty_messages <- xafty::get_xafty_list_items(xafty_list = xafty_list, column = tab_name, item = "message")
  xafty_restults <- xafty::get_xafty_list_items(xafty_list = xafty_list, column = tab_name, item = "test_result")
  xafty_rules <- names(xafty_list[[tab_name]])

  xafty_info <- mapply(\(message, rule, result){
    if(result) {
      HTML(paste0("&#x2705; ", strong(paste0("Message of Rule ", rule, ": ")), message, br()))
    } else {
      HTML(paste0("&#9940; ", strong(paste0("Message of Rule ", rule, ": ")), message, br()))
    }
  },
  xafty_messages, xafty_rules, xafty_restults, SIMPLIFY = FALSE)

  xaft_messages_span <- lapply(xafty_info, span)

  messages_tagList <- do.call(tagList, xaft_messages_span)

  shiny::wellPanel(
    h2(paste0("Test Summary of Column: ", tab_name)),
    messages_tagList
    )

}


create_rules_buttons <- function(ns, xafty_list, tab_name) {

  xafty_rules <- names(xafty_list[[tab_name]])
  xafty_restults <- xafty::get_xafty_list_items(xafty_list = xafty_list, column = tab_name, item = "test_result")

  group_button_ids <- sapply(xafty_rules, \(rule) {
    ns(paste0(tab_name, "_", rule, "_button"))
    })

  names(group_button_ids) <- NULL

  shinyWidgets::actionGroupButtons(group_button_ids, xafty_rules, status = "primary")

}

create_rules_cards <- function(ns, xafty_list, tab_name) {

  xafty_rules <- names(xafty_list[[tab_name]])
  xafty_results <- xafty::get_xafty_list_items(xafty_list = xafty_list, column = tab_name, item = "test_result")

  xafty_cards <- lapply(xafty_rules, \(xafty_rule){

    unique_id <- paste0(tab_name, xafty_rule)

    test_result <- xafty_results[[xafty_rule]]

    if (test_result) {
      header_class <- "bg-success"
    } else {
      header_class <- "bg-danger"
    }

    bslib::card(
      bslib::card_header(id = ns(paste0("card_header_", xafty_rule,"_", tab_name)),
                         class = header_class,
                         paste0("Card for Rule: ", xafty_rule)
                         ),
      bslib::card_body(
        shiny::actionButton(ns(paste0("check_", xafty_rule,"_", tab_name)), label = paste0("Test for ", xafty_rule)),
        shiny::uiOutput(ns(paste0("table_", xafty_rule,"_", tab_name)))
      )
    , full_screen = TRUE)

  })

  do.call(tagList, xafty_cards)


}
