test_that("undid_stage_three() works for common adoption and nperm warnings",
  {
    dir <- system.file("extdata/common", package = "undidR")
    # Expect both warnings and correct result
    expect_warning(
      expect_warning(
        {
          result <- undid_stage_three(dir)
          expect_equal(round(result$agg_ATT, 6), round(0.02139120, 6))
        },
        "Randomization inference may not be valid with `nperm` < 500"
      ),
      "nperm.*only 6"
    )
  }
)

test_that("undid_stage_three() works for staggered adoption, agg = gt",
  { expect_warning({
    dir <- system.file("extdata/staggered", package = "undidR")
    result <- undid_stage_three(dir, agg = "gt", nperm = 10, verbose = FALSE)
    expect_equal(round(result$agg_ATT[1], 6), round(0.04756747, 6))
  }, "Randomization inference may not be valid with `nperm` < 500"
  )
  }
)

test_that("undid_stage_three() works for staggered adoption, agg = g",
  { expect_warning({
    dir <- system.file("extdata/staggered", package = "undidR")
    result <- undid_stage_three(dir, agg = "g", nperm = 10, verbose = FALSE)
    expect_equal(round(result$agg_ATT[1], 6), round(0.04543183, 6))
  }, "Randomization inference may not be valid with `nperm` < 500"
  )
  }
)

test_that("undid_stage_three() works for staggered adoption, agg = silo",
  { expect_warning({
    dir <- system.file("extdata/staggered", package = "undidR")
    result <- undid_stage_three(dir, agg = "silo", nperm = 10, verbose = FALSE)
    expect_equal(round(result$agg_ATT[1], 6), round(0.05755899, 6))
  }, "Randomization inference may not be valid with `nperm` < 500"
  )
  }
)

test_that("covariates argument works",
  { expect_warning({
    dir <- system.file("extdata/staggered", package = "undidR")
    result <- undid_stage_three(dir, agg = "silo", nperm = 10,
                                covariates = TRUE, verbose = FALSE)
    expect_equal(round(result$agg_ATT[1], 6), round(0.04797293, 6))
  }, "Randomization inference may not be valid with `nperm` < 500"
  )
  }
)

test_that("interpolation methods work",
  {
    # Make sure error and warning are thrown if data is missing
    # and interpolation is not specified
    dir <- system.file("extdata/errors/missingdata", package = "undidR")
    expect_error(
      expect_warning({
        undid_stage_three(dir, agg = "g", nperm = 501,
                          verbose = FALSE)
      }, "No values for"
      ),
      "Consider setting `interpolation` to one of the following:"
    )

    # Ensure warning exists but aggregate ATT can still be calculated with
    # various interpolation methods
    expect_warning({
      dir <- system.file("extdata/errors/missingdata", package = "undidR")
      result <- undid_stage_three(dir, agg = "g", nperm = 501,
                                  verbose = FALSE,
                                  interpolation = "linear_function")
      expect_equal(round(result$agg_ATT[1], 6), round(0.04698432, 6))
    }, "No values for"
    )
    expect_warning({
      dir <- system.file("extdata/errors/missingdata", package = "undidR")
      result <- undid_stage_three(dir, agg = "g", nperm = 501,
                                  verbose = FALSE,
                                  interpolation = "piecewise_linear")
      expect_equal(round(result$agg_ATT[1], 6), round(0.04588463, 6))
    }, "No values for"
    )
    expect_warning({
      dir <- system.file("extdata/errors/missingdata", package = "undidR")
      result <- undid_stage_three(dir, agg = "g", nperm = 501,
                                  verbose = FALSE,
                                  interpolation = "nearest_value")
      expect_equal(round(result$agg_ATT[1], 6), round(0.04588463, 6))
    }, "No values for"
    )

    # Edge case: linear_function is selected but there aren't enough obs
    expect_error({
      expect_warning(
        {
          dir <- system.file("extdata/errors/moremissingdata",
                             package = "undidR")
          undid_stage_three(dir, agg = "g", nperm = 501, verbose = FALSE,
                            interpolation = "linear_function")
        }, "No values for"
      )
    }, "Not able to impute missing values.")
  }
)
