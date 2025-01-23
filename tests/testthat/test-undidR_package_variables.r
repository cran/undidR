test_that(".populate_undid_env() populates env with expected variables", {
  test_env <- new.env()
  .populate_undid_env(test_env)

  # Check just the existence variables
  expect_true(exists("date_formats_general", envir = test_env))
  expect_true(exists("date_formats_r", envir = test_env))
  expect_true(exists("other_formats", envir = test_env))
  expect_true(exists("date_format_dict_to_r", envir = test_env))
  expect_true(exists("date_format_dict_from_r", envir = test_env))
  expect_true(exists("month_dict", envir = test_env))
  expect_true(exists("month_dict_reverse", envir = test_env))
  expect_true(exists("freq_map", envir = test_env))
  expect_true(exists("staggered_columns", envir = test_env))
  expect_true(exists("common_columns", envir = test_env))
  expect_true(exists("interpolation_options", envir = test_env))
})
