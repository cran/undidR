test_that("freq and freq_multiplier args are handled correctly",
  {
    init_csv <- system.file("extdata/staggered", "init.csv", package = "undidR")
    expect_equal(create_diff_df(init_csv, "yyyy", "yearly",
                                freq_multiplier = 4)$freq[1], "4 years")
    expect_equal(create_diff_df(init_csv, "yyyy", "monthly",
                                freq_multiplier = 20)$freq[1], "20 months")
  }
)

test_that("changing covariates works",
  {
    init_csv <- system.file("extdata/common", "init.csv", package = "undidR")
    diff_df <- create_diff_df(init_csv, "yyyy", "yearly",
                              covariates = c("asian", "black"))
    expect_equal(diff_df$covariates[1], "asian;black")
  }
)

test_that("alternative date formats work, common adoption",
  {
    init_csv <- system.file("extdata/altdateformats/common/ddmonyyyy",
                            "init_ddmonyyyy.csv", package = "undidR")
    expect_equal(create_diff_df(init_csv, "ddmonyyyy", "yearly",
                                freq_multiplier = 8)$common_treatment_time[1],
                 "01jan1991")

    init_csv <- system.file("extdata/altdateformats/common/yyyy_mm_dd",
                            "init_yyyy_mm_dd.csv", package = "undidR")
    expect_equal(create_diff_df(init_csv, "%Y/%m/%d", "yearly",
                                freq_multiplier = 8)$common_treatment_time[1],
                 "1991/01/01")
  }
)

test_that("alternative date formats work, staggered adoption",
  {
    init_csv <- system.file("extdata/altdateformats/staggered/yyyym00",
                            "init_yyyym00.csv", package = "undidR")
    diff_times <- create_diff_df(init_csv, "yyyym00", "yearly",
                                 freq_multiplier = 8)$diff_times[1]

    expect_equal(diff_times, "1991m1;1983m1")

    init_csv <- system.file("extdata/altdateformats/staggered/yyyy_mm_dd",
                            "init_yyyy_mm_dd.csv", package = "undidR")
    expect_equal(create_diff_df(init_csv, "yyyy/mm/dd", "yearly",
                                freq_multiplier = 8)$diff_times[1],
                 "1991/01/01;1983/01/01")
  }
)

test_that("freq and freq_multiplier throw errors for bad inputs",
  {
    init_csv <- system.file("extdata/staggered", "init.csv", package = "undidR")
    expect_error(create_diff_df(init_csv, "yyyy", "biweekly"))
    expect_error(create_diff_df(init_csv, "yyyy", "yearly",
                                freq_multiplier = TRUE))
  }
)
