test_that("`undid_stage_two()` processes staggered adoption properly",
  {
    empty_diff_filepath <- system.file("extdata/staggered", "empty_diff_df.csv",
                                       package = "undidR")
    df <- undid_stage_two(empty_diff_filepath, "71", silo71, "year", "coll",
                          "yyyy")
    expect_equal(df$diff_df$diff_estimate[1], 0.129166666666667)
    expect_equal(df$diff_df$diff_estimate_covariates[1], 0.116348471515476)
    expect_equal(df$trends_data$mean_outcome[1], 0.306122448979592)
    expect_equal(df$trends_data$mean_outcome_residualized[1], 0.199879951980792)
  }
)

test_that("`undid_stage_two()` processes common adoption properly",
  {
    empty_diff_filepath <- system.file("extdata/common", "empty_diff_df.csv",
                                       package = "undidR")
    df <- undid_stage_two(empty_diff_filepath, "71", silo71, "year", "coll",
                          "yyyy")
    expect_equal(df$diff_df$diff_estimate[1], 0.0587978333042113)
  }
)

test_that("argument consider_covariates works as intended",
  {
    empty_diff_filepath <- system.file("extdata/staggered", "empty_diff_df.csv",
                                       package = "undidR")
    df <- undid_stage_two(empty_diff_filepath, "71", silo71, "year", "coll",
                          "yyyy", consider_covariates = FALSE)
    expect_equal(df$diff_df$diff_estimate[1], 0.129166666666667)
    expect_equal(df$diff_df$diff_estimate_covariates[1], NA_real_)
    expect_equal(df$trends_data$mean_outcome[1], 0.306122448979592)
    expect_equal(df$trends_data$mean_outcome_residualized[1], NA_real_)
  }
)

test_that("warning displayed when some diff_estimates cannot be comptued",
  {
    empty_diff_filepath <- system.file("extdata/staggered", "empty_diff_df.csv",
                                       package = "undidR")
    expect_warning(undid_stage_two(empty_diff_filepath, "71",
                                   silo71[silo71$year != "1993", ], "year",
                                   "coll", "yyyy", consider_covariates = FALSE))
  }
)
