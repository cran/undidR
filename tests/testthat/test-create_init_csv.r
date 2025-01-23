test_that("non character arguments result in error",
  {
    expect_error(create_init_csv(silo_names = c(71, 73),
                                 start_times = c("1989", "1989"),
                                 end_times = c("2000", "2000"),
                                 treatment_times = c("1991", "control")))
    expect_error(create_init_csv(silo_names = c("71", "73"),
                                 start_times = c(1989, 1989),
                                 end_times = c("2000", "2000"),
                                 treatment_times = c("1991", "control")))
    expect_error(create_init_csv(silo_names = c("71", "73"),
                                 start_times = c("1989", "1989"),
                                 end_times = c(2000, 2000),
                                 treatment_times = c("1991", "control")))
  }
)

test_that("whitespace is removed",
  {
    expect_equal(create_init_csv(
      silo_names = c("71", "73"),
      start_times = c("1989  ", " 1989"),
      end_times = c("2000", "2000"),
      treatment_times = c("1991", "control")
    )$start_time, c("1989", "1989"))
    expect_equal(create_init_csv(
      silo_names = c("71", "73"),
      start_times = c("1989", "1989"),
      end_times = c("  2000", "  2000   "),
      treatment_times = c("1991", "control")
    )$end_time, c("2000", "2000"))
    expect_equal(create_init_csv(
      silo_names = c("71", "73"),
      start_times = c("1989", "1989"),
      end_times = c("2000", "2000"),
      treatment_times = c("   1991", "  control ")
    )$treatment_time, c("1991", "control"))
  }
)

test_that("start and end times can be entered as a single value",
  {
    expect_equal(create_init_csv(
      silo_names = c("71", "73"),
      start_times = "1989",
      end_times = "2000",
      treatment_times = c("1991", "control ")
    )$start_time, c("1989", "1989"))
    expect_equal(create_init_csv(
      silo_names = c("71", "73"),
      start_times = "1989",
      end_times = "2000",
      treatment_times = c("1991", "control ")
    )$end_time, c("2000", "2000"))

  }
)

test_that("mismatched silo_names and treatment_times lengths results in error",
  {
    expect_error(create_init_csv(
      silo_names = c("71"),
      start_times = "1989",
      end_times = "2000",
      treatment_times = c("1991", "control")
    ))
    expect_error(create_init_csv(
      silo_names = c("71", "73"),
      start_times = "1989",
      end_times = "2000",
      treatment_times = c("1991")
    ))
  }
)

test_that("covariates get squished to one string separated by ;",
  {
    expect_equal(create_init_csv(
      silo_names = c("71", "73"),
      start_times = "1989",
      end_times = "2000",
      treatment_times = c("1991", "control"),
      covariates = c("asian", "black", "male")
    )$covariates[1], c("asian;black;male"))
  }
)
