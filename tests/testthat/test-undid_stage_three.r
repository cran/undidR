test_that("undid_stage_three() works for common adoption and nperm warnings",
  {
    dir <- system.file("extdata/common", package = "undidR")
    # Expect both warnings and correct result
    expect_warning(
      expect_warning(
        {
          result <- undid_stage_three(dir, agg = "none", weights = "diff")
          expect_equal(round(result$agg$att, 6), round(0.02381393, 6))
        },
        "'nperm' is less than 399."
      ),
      "nperm.*only 5"
    )
  }
)

test_that("undid_stage_three() works for common (silo) and nperm warnings",
  {
    dir <- system.file("extdata/common", package = "undidR")
    # Expect both warnings and correct result
    expect_warning(
      expect_warning(
        {
          result <- undid_stage_three(dir, agg = "silo")
          expect_equal(round(result$agg$att[1], 6), round(0.02397045, 6))
        },
        "'nperm' is less than 399."
      ),
      "nperm.*only 5"
    )
  }
)

test_that("undid_stage_three() works for staggered adoption, agg = gt",
  { expect_warning({
    dir <- system.file("extdata/staggered", package = "undidR")
    result <- undid_stage_three(dir, agg = "gt", nperm = 10, verbose =  NULL)
    expect_equal(round(result$agg$att[1], 6), round(0.07607095, 6))
  }, "'nperm' is less than 399."
  )
  }
)

test_that("undid_stage_three() works for staggered adoption, agg = g",
  { expect_warning({
    dir <- system.file("extdata/staggered", package = "undidR")
    result <- undid_stage_three(dir, agg = "g", nperm = 10, verbose = NULL)
    expect_equal(round(result$agg$att[1], 6), round(0.07611833, 6))
  }, "'nperm' is less than 399."
  )
  }
)

test_that("undid_stage_three() works for staggered adoption, agg = silo",
  { expect_warning({
    dir <- system.file("extdata/staggered", package = "undidR")
    result <- undid_stage_three(dir, agg = "silo", nperm = 10, verbose = NULL)
    expect_equal(round(result$agg$att[1], 6), round(0.07639371, 6))
  }, "'nperm' is less than 399."
  )
  }
)

test_that("undid_stage_three() works for staggered adoption, agg = sgt",
  { expect_warning({
    dir <- system.file("extdata/staggered", package = "undidR")
    result <- undid_stage_three(dir, agg = "sgt", nperm = 10, verbose = NULL)
    expect_equal(round(result$agg$att[1], 6), round(0.07585688, 6))
  }, "'nperm' is less than 399."
  )
  }
)

test_that("undid_stage_three() works for staggered adoption, agg = time",
  { expect_warning({
    dir <- system.file("extdata/staggered", package = "undidR")
    result <- undid_stage_three(dir, agg = "time", nperm = 10, verbose = NULL)
    expect_equal(round(result$agg$att[1], 6), round(0.06840526, 6))
  }, "'nperm' is less than 399."
  )
  }
)

test_that("covariates argument works",
  { expect_warning({
    dir <- system.file("extdata/staggered", package = "undidR")
    result <- undid_stage_three(dir, agg = "silo", nperm = 10,
                                covariates = TRUE, verbose = NULL)
    expect_equal(round(result$agg$att[1], 6), round(0.0732032, 6))
  }, "'nperm' is less than 399."
  )
  }
)

test_that("works with some missing data", {
  dir <- system.file("extdata/errors/missingdata", package = "undidR")
  data_list <- list()
  # Each agg method will produce warnings about missing data and RI
  for (i in seq_along(c("g", "gt", "sgt", "silo", "time"))) {
    agg <- c("g", "gt", "sgt", "silo", "time")[i]
    expect_warning(
      expect_warning(
        {
          result <- undid_stage_three(dir, agg = agg, nperm = 10,
                                      weights = "none",
                                      covariates = TRUE, verbose = NULL)
          data_list[[i]] <- result
        },
        "Missing value count for diff_estimate_covariates"
      ),
      "missing differences for gt values required"
    )
    # Check that each result is valid
    expect_s3_class(data_list[[i]], "UnDiDObj")
  }
  # Test specific value from last result (agg = "none")
  expect_equal(round(data_list[[5]]$agg$att[1], 6), round(0.05000527, 6))
  # Verify all 5 methods produced results
  expect_equal(length(data_list), 5)
})

test_that("undid_stage_three() works with atypical options",
  { expect_warning({
    dir <- system.file("extdata/staggered", package = "undidR")
    result <- undid_stage_three(dir, agg = "gt", nperm = 10, verbose = NULL,
                                weights = "none", notyet = TRUE, omit = "71")
    expect_equal(round(result$agg$att[1], 6), round(0.04880939, 6))
  }, "'nperm' is less than 399."
  )
  }
)

test_that("S3 methods work for all aggregation methods", {
  # Test with (missing) staggered adoption data
  dir_staggered <- system.file("extdata/errors/missingdata", package = "undidR")

  agg_methods <- c("g", "gt", "sgt", "silo", "time")

  for (agg in agg_methods) {
    # Expect warnings about missing data and RI
    # For agg = "none", there's an additional warning about weights
    expect_warning(
      expect_warning(
        {
          obj <- undid_stage_three(dir_staggered, agg = agg, nperm = 10,
                                   covariates = TRUE, verbose = NULL)
        },
        "Missing value count for diff_estimate_covariates"
      ),
      "missing differences for gt values required"
    )

    # Test print method
    expect_output(print(obj, level = "agg"), "Aggregate ATT")
    expect_output(print(obj, level = "agg"), agg)

    expect_output(print(obj, level = "sub"), "Sub-Aggregate ATTs")

    # Test summary method
    expect_output(summary(obj, level = "agg"), "Aggregate Results")
    expect_output(summary(obj, level = "agg"), "ATT")
    expect_output(summary(obj, level = "all"), "Weighting")

    expect_output(summary(obj, level = "sub"), "Subaggregate Results")
    expect_output(summary(obj, level = "all"), "Weight")

    # Test coef method
    coef_agg <- coef(obj, level = "agg")
    expect_true(is.numeric(coef_agg))
    expect_length(coef_agg, 1)

    coef_sub <- coef(obj, level = "sub")
    expect_s3_class(coef_sub, "data.frame")
    expect_true("Group" %in% names(coef_sub))
    expect_true("ATT" %in% names(coef_sub))
    expect_true(nrow(coef_sub) > 0)

  }
})

test_that("S3 methods work for common adoption (silo and none only)", {
  dir_common <- system.file("extdata/common", package = "undidR")

  # Test silo aggregation
  expect_warning(
    expect_warning(
      {
        obj_silo <- undid_stage_three(dir_common, agg = "silo", nperm = 10)
      },
      "'nperm' is less than 399."
    ),
    "nperm.*only 5"
  )

  expect_output(print(obj_silo, level = "agg"), "Aggregate ATT")
  expect_output(print(obj_silo, level = "sub"), "Sub-Aggregate ATTs")
  expect_output(summary(obj_silo, level = "all"), "Subaggregate Results")

  coef_agg <- coef(obj_silo, level = "agg")
  expect_true(is.numeric(coef_agg))

  coef_sub <- coef(obj_silo, level = "sub")
  expect_s3_class(coef_sub, "data.frame")
  expect_true(nrow(coef_sub) > 0)

  # Test none aggregation
  expect_warning(
    expect_warning(
      expect_warning(
        {
          obj_none <- undid_stage_three(dir_common, agg = "none", nperm = 10)
        },
        "If 'agg = none' then 'weights' can only be either 'none' or 'diff'"
      ),
      "'nperm' is less than 399."
    ),
    "nperm.*only 5"
  )

  expect_output(print(obj_none, level = "agg"), "Aggregate ATT")
  expect_output(print(obj_none, level = "sub"), "No sub-aggregate estimates")
  expect_output(summary(obj_none, level = "agg"), "Aggregate Results")

  coef_agg <- coef(obj_none, level = "agg")
  expect_true(is.numeric(coef_agg))

  expect_output(coef(obj_none, level = "sub"), "No sub-aggregate estimates")
})

test_that("plot method works for parallel trends", {
  dir <- system.file("extdata/errors/missingdata", package = "undidR")

  expect_warning(
    expect_warning(
      {
        result <- undid_stage_three(dir, agg = "g", nperm = 10,
                                    covariates = TRUE, verbose = NULL)
      },
      "Missing value count for diff_estimate_covariates"
    ),
    "missing differences for gt values required"
  )

  # Test that plot runs without error
  expect_silent(plot(result, event = FALSE))

  # Test that it returns the object invisibly (or nothing)
  expect_invisible(plot(result, event = FALSE))
})

test_that("plot method works for event study", {
  dir <- system.file("extdata/errors/missingdata", package = "undidR")

  expect_warning(
    expect_warning(
      {
        result <- undid_stage_three(dir, agg = "g", nperm = 10,
                                    covariates = TRUE, verbose = NULL)
      },
      "Missing value count for diff_estimate_covariates"
    ),
    "missing differences for gt values required"
  )

  # Test that event study plot runs (may produce NaNs for single obs)
  expect_silent(
    plot(result, event = TRUE)
  )

  # Test with event window
  expect_silent(
    plot(result, event = TRUE, event_window = c(-2, 5))
  )

  # Test with custom CI
  expect_silent(
    plot(result, event = TRUE, ci = 0.90)
  )
})

test_that("plot method validates inputs correctly", {
  dir <- system.file("extdata/errors/missingdata", package = "undidR")

  expect_warning(
    expect_warning(
      {
        result <- undid_stage_three(dir, agg = "g", nperm = 10,
                                    covariates = TRUE, verbose = NULL)
      },
      "Missing value count for diff_estimate_covariates"
    ),
    "missing differences for gt values required"
  )

  # Test invalid CI
  expect_error(plot(result, ci = 1.5), "`ci` must be between 0 and 1")
  expect_error(plot(result, ci = 0), "`ci` must be between 0 and 1")
  expect_error(plot(result, ci = -0.5), "`ci` must be between 0 and 1")

  # Test invalid event_window
  expect_error(plot(result, event = TRUE, event_window = c(1, 2, 3)),
               "`event_window` must be a numeric vector of length 2")

  # Test event_window with no observations
  expect_error(plot(result, event = TRUE, event_window = c(100, 200)),
               "No obs. after applying 'event_window'")
})

test_that("plot method works with common adoption data", {
  dir <- system.file("extdata/common", package = "undidR")

  expect_warning(
    expect_warning(
      {
        result <- undid_stage_three(dir, agg = "silo", nperm = 10)
      },
      "'nperm' is less than 399."
    ),
    "nperm.*only 5"
  )

  # Parallel trends should work
  expect_silent(plot(result, event = FALSE))

  # Event study should work
  expect_silent(plot(result, event = TRUE))
})