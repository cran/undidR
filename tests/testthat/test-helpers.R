test_that("init.csv missing column throws error",
  {
    init_filepath <- system.file("extdata/errors", "init_3columns.csv",
                                 package = "undidR")
    init_df <- read.csv(init_filepath, header = TRUE, sep = ",",
                        stringsAsFactors = FALSE)
    expect_error(.init_checks(init_df))
  }
)

test_that("init.csv improperly named column throws error",
  {
    init_filepath <- system.file("extdata/errors", "init_badnames.csv",
                                 package = "undidR")
    init_df <- read.csv(init_filepath, header = TRUE, sep = ",",
                        stringsAsFactors = FALSE)
    expect_error(.init_checks(init_df))
  }
)

test_that("start_time < treat_time < end_time is enforced with errors",
  {
    # Check that treat_time > end_time throws error
    init_filepath <- system.file("extdata/errors", "init_badchronology.csv",
                                 package = "undidR")
    init_df <- read.csv(init_filepath, header = TRUE, sep = ",",
                        stringsAsFactors = FALSE)
    init_df <- data.frame(lapply(init_df, as.character),
                          stringsAsFactors = FALSE)
    init_df$treatment_time <- tolower(init_df$treatment_time)
    expect_null(.init_checks(init_df))
    init_df$start_time <- as.Date(vapply(
      init_df$start_time,
      .parse_string_to_date,
      FUN.VALUE = as.Date(NA, origin = "1970-01-01"),
      date_format = "yyyy"
    ), origin = "1970-01-01")
    init_df$end_time <- as.Date(vapply(
      init_df$end_time, .parse_string_to_date,
      FUN.VALUE = as.Date(NA, origin = "1970-01-01"),
      date_format = "yyyy"
    ), origin = "1970-01-01")
    expect_error(.start_treat_end_time_check(init_df, "yyyy"))

    # Check that start_time > end_time throws error
    init_filepath <- system.file("extdata/errors", "init_badchronology2.csv",
                                 package = "undidR")
    init_df <- read.csv(init_filepath, header = TRUE, sep = ",",
                        stringsAsFactors = FALSE)
    init_df <- data.frame(lapply(init_df, as.character),
                          stringsAsFactors = FALSE)
    init_df$treatment_time <- tolower(init_df$treatment_time)
    expect_null(.init_checks(init_df))
    expect_error(.start_treat_end_time_check(init_df, "yyyy"))
  }
)

test_that(".read_diff_df() throws error if named silo not found",
  {
    diff_df_filepath <- system.file("extdata/staggered", "empty_diff_df.csv",
                                    package = "undidR")
    expect_error(.read_diff_df(diff_df_filepath, silo_name = "Ontario",
                               stage = 2))
  }
)

test_that(".read_diff_df() throws error if column structure unexpected",
  {
    diff_df_filepath <- system.file("extdata/errors",
                                    "empty_diff_df_badcolumns_staggered.csv",
                                    package = "undidR")
    expect_error(.read_diff_df(diff_df_filepath, silo_name = "71",
                               stage = 2))

    diff_df_filepath <- system.file("extdata/errors",
                                    "empty_diff_df_badcolumns_common.csv",
                                    package = "undidR")
    expect_error(.read_diff_df(diff_df_filepath, silo_name = "71",
                               stage = 3))
  }
)

test_that(".read_diff_df() reads .csv generated from Julia correctly",
  {
    diff_df_filepath <- system.file("extdata/julia",
                                    "empty_diff_df_jl.csv",
                                    package = "undidR")
    expect_equal(.read_diff_df(diff_df_filepath, silo_name = "11",
                               stage = 2)$diff_estimate[1], NA_real_)
  }
)

test_that("check .parse_date_to_string() for ddmonyyyy format",
  {
    date_str <- .parse_date_to_string(as.Date("1991-01-01"), "ddmonyyyy")
    expect_equal(date_str, "01jan1991")
  }
)

test_that(".parse_string_to_date() works in many contexts",
  {
    # Ensure mm/yyyy (or mm-yyyy) format is processed correctly
    # (other formats are tested implicitly through other tests)
    date_str <- .parse_string_to_date("04/1991", "mm/yyyy")
    expect_equal(date_str, as.Date("1991-04-01"))

    expect_error({
      .parse_string_to_date("25 Sept 1997", "dd month year")
    }, "Please enter a valid date format.")
  }
)
