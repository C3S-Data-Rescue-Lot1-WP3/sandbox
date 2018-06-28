context('data')
test_data_frame <- function(data, columns = NULL,
  eq_condition = "equivalent",
  undefined_msg = NULL,
  undefined_cols_msg = NULL,
  incorrect_msg = NULL) {
  obj_state <- ex() %>% check_object(data = data,
    undefined_msg = undefined_msg,
    append = is.null(undefined_msg))

  if (is.null(columns)) {
    columns <- data(obj_state$get("solution_object"))
  }

  for (col in columns) {
    obj_state %>%
      check_column(col, col_missing_msg = undefined_cols_msg, append = is.null(undefined_cols_msg)) %>%
      check_equal(eq_condition = eq_condition, incorrect_msg = incorrect_msg, append = is.null(incorrect_msg))
  }
}
