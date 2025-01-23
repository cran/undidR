test_that("plot_parallel_trends() works with combine = FALSE",
  {
    dir <- system.file("extdata/staggered", package = "undidR")
    trends <- plot_parallel_trends(dir, combine = FALSE)
    expect_equal(names(trends), c("silo_name", "treatment_time",
                                  "time", "mean_outcome",
                                  "mean_outcome_residualized",
                                  "covariates", "date_format",
                                  "freq", "y"))

    dir <- system.file("extdata/staggered", package = "undidR")
    trends <- plot_parallel_trends(dir, combine = FALSE, covariates = TRUE)
    expect_equal(names(trends), c("silo_name", "treatment_time",
                                  "time", "mean_outcome",
                                  "mean_outcome_residualized",
                                  "covariates", "date_format",
                                  "freq", "y"))
  }
)

test_that("plot_parallel_trends() works with combine = TRUE",
  {
    dir <- system.file("extdata/staggered", package = "undidR")
    trends <- plot_parallel_trends(dir, combine = TRUE)
    expect_equal(names(trends), c("time", "silo_name", "y",
                                  "treatment_time", "date_format"))

    dir <- system.file("extdata/staggered", package = "undidR")
    trends <- plot_parallel_trends(dir, combine = TRUE, covariates = TRUE)
    expect_equal(names(trends), c("time", "silo_name", "y",
                                  "treatment_time", "date_format"))
  }
)

test_that("interpolate throws proper errors and works for either combine arg",
  {
    dir <- system.file("extdata/errors/missingdata", package = "undidR")
    expect_warning({
      plot_parallel_trends(dir)
    }, "`NA` values found, consider setting `interpolate = TRUE`")
    expect_warning({
      plot_parallel_trends(dir, combine = TRUE)
    }, "`NA` values found, consider setting `interpolate = TRUE`")

    trends <- plot_parallel_trends(dir, interpolate = TRUE)
    trends <- trends[trends$silo_name == "71" &
                       trends$time == as.Date("1993-01-01"), ]
    expect_equal(round(trends$mean_outcome[1], 6), round(0.3189362, 6))

    trends <- plot_parallel_trends(dir, combine = TRUE, interpolate = TRUE)
    trends <- trends[trends$silo_name == "Treatment" &
                       trends$time == as.Date("1993-01-01"), ]
    expect_equal(round(trends$y[1], 6), round(0.4007178, 6))

  }
)

test_that("simplify_legend can be set to FALSE with no issues",
  {
    dir <- system.file("extdata/staggered", package = "undidR")
    trends <- plot_parallel_trends(dir, combine = FALSE,
                                   simplify_legend = FALSE)
    expect_equal(names(trends), c("silo_name", "treatment_time",
                                  "time", "mean_outcome",
                                  "mean_outcome_residualized",
                                  "covariates", "date_format",
                                  "freq", "y"))
  }
)

test_that("plot saves as .png and .csv properly",
  {
    dir <- system.file("extdata/staggered", package = "undidR")
    trends <- suppressMessages({
      plot_parallel_trends(dir, combine = FALSE, filenamepng = "undid_plot.png",
                           save_png = TRUE, save_csv = TRUE,
                           filenamecsv = "trends_all_silos.csv",
                           filepath = tempdir())
    })
    full_path_png <- normalizePath(file.path(tempdir(), "undid_plot.png"),
                                   winslash = "/", mustWork = FALSE)
    full_path_csv <- normalizePath(file.path(tempdir(), "trends_all_silos.csv"),
                                   winslash = "/", mustWork = FALSE)
    expect_true(file.exists(full_path_png))
    expect_true(file.exists(full_path_csv))
  }
)

test_that("additional plotting args work",
  {
    dir <- system.file("extdata/staggered", package = "undidR")

    # Check that pch works
    expect_error(plot_parallel_trends(dir, combine = FALSE,
                                      simplify_legend = TRUE,
                                      pch = 2),
                 regexp = NA)
    # Check that American spelling for colour args works
    expect_error(plot_parallel_trends(dir, combine = FALSE,
                                      simplify_legend = TRUE,
                                      control_color = c("red", "coral"),
                                      treatment_color = c("grey", "blue")),
                 regexp = NA)
  }
)
