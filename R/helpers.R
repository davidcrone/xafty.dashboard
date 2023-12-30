##

## Outsourcing the logic of adding values to a function


add_ids_to_rval_master <- function(i, xafty_table, rval_xafty_master) {

  xafty_rule <- sub("##!!", "", xafty_table$rule[i])
  xafty_column <- xafty_table$column[i]

  xafty_button_id <- paste0("check_", xafty_rule, "_", xafty_column)
  rhandsontabel_id <- paste0("table_", xafty_rule, "_", xafty_column)
  xafty_card_header_id <- paste0("card_header_", xafty_rule,"_", xafty_column)
  render_handsontable_id <- paste0("r_", rhandsontabel_id)

  unique_id <- rval_xafty_master$unique_ids[i]

  rval_xafty_master[[unique_id]][["xafty_button_id"]] <- xafty_button_id
  rval_xafty_master[[unique_id]][["xafty_button_id"]] <- xafty_button_id
  rval_xafty_master[[unique_id]][["rhandsontabel_id"]] <- rhandsontabel_id
  rval_xafty_master[[unique_id]][["render_handsontable_id"]] <- render_handsontable_id
  rval_xafty_master[[unique_id]][["xafty_card_header_id"]] <- xafty_card_header_id

}
