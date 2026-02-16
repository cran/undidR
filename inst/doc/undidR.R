## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(undidR)

## ----schematic, echo=FALSE, fig.align='center', out.width='90%', fig.cap="Schematic of the UNDID framework."----
knitr::include_graphics("figures/undidR_schematic.png")

## -----------------------------------------------------------------------------
# First, an initializing CSV is created detailing the silos
# and their treatment times. Control silos (here, 73 and 46)
# should be labelled with "control".
init <- create_init_csv(silo_names = c("73", "46", "71", "58"),
                        start_times = "1989",
                        end_times = "2000",
                        treatment_times = c("control", "control",
                                            "1991", "1991"))
init

# After the initializing CSV file is created, `create_diff_df()`
# can be called. This creates the empty differences data frame which
# will then be filled out at each individual silo for its respective portion.
init_filepath <- normalizePath(file.path(tempdir(), "init.csv"),
                               winslash = "/", mustWork = FALSE)
empty_diff_df <- create_diff_df(init_filepath, date_format = "yyyy",
                                freq = "yearly", weights = "both")
empty_diff_df

## -----------------------------------------------------------------------------
# The initializing CSV for staggered adoption is created in the same way.
# When `create_diff_df()` is run, it will automatically detect whether or not
# the initial setup is for a common adoption or staggered adoption scenario.
init <- create_init_csv(silo_names = c("73", "46", "54", "23", "86", "32",
                                       "71", "58", "64", "59", "85", "57"),
                        start_times = "1989",
                        end_times = "2000",
                        treatment_times = c(rep("control", 6),
                                            "1991", "1993", "1996", "1997",
                                            "1997", "1998"))
init

# Creating the empty differences data frame and associated CSV file is
# the same for the case of staggered adoption as it is for common adoption.
init_filepath <- normalizePath(file.path(tempdir(), "init.csv"),
                               winslash = "/", mustWork = FALSE)
empty_diff_df <- create_diff_df(init_filepath, date_format = "yyyy",
                                freq = "yearly", weights = "both",
                                covariates = c("asian", "black", "male"))
head(empty_diff_df, 4)

## -----------------------------------------------------------------------------
# When calling `undid_stage_two()`, ensure that the `time_column` of
# the `silo_df` contains only character values, i.e. date strings.
silo_data <- silo71
silo_data$year <- as.character(silo_data$year)
empty_diff_filepath <- system.file("extdata/common", "empty_diff_df.csv",
                                   package = "undidR")
stage2 <- undid_stage_two(empty_diff_filepath, silo_name = "71",
                          silo_df = silo_data, time_column = "year",
                          outcome_column = "coll", silo_date_format = "yyyy")
head(stage2$diff_df, 4)
head(stage2$trends_data, 4)

## -----------------------------------------------------------------------------
# Here we can see that calling `undid_stage_two()` for staggered adoption
# is no different than calling `undid_stage_two()` for common adoption.
silo_data <- silo71
silo_data$year <- as.character(silo_data$year)
empty_diff_filepath <- system.file("extdata/staggered", "empty_diff_df.csv",
                                    package = "undidR")
stage2 <- undid_stage_two(empty_diff_filepath, silo_name = "71",
                          silo_df = silo_data, time_column = "year",
                          outcome_column = "coll", silo_date_format = "yyyy")
head(stage2$diff_df, 4)
head(stage2$trends_data, 4)

## ----fig.width=6, fig.height=4, out.width = "70%", dpi=300, fig.align="center"----
# `undid_stage_three()`, given a `dir_path`, will search that folder
# for all CSV files that begin with "filled_diff_df_" and stitch
# them together in order to compute the group level ATTs, aggregate ATT
# and associated standard errors and p-values.
dir_path <- system.file("extdata/common", package = "undidR")
results <- undid_stage_three(dir_path, covariates = FALSE, nperm = 399)
summary(results)
plot(results)


## ----fig.width=6, fig.height=4, out.width = "70%", dpi=300, fig.align="center"----
# When calling `undid_stage_three()` for staggered adoption it is
# important to specify the aggregation method, `agg`.
dir_path <- system.file("extdata/staggered", package = "undidR")
results <- undid_stage_three(dir_path, agg = "silo", covariates = TRUE,
                             nperm = 399)
head(results$diff, 4)

head(results$trends, 4)

summary(results)

plot(results, main = "My Parallel Trends Plot")

## ----fig.width=6, fig.height=4, out.width = "70%", dpi=300, fig.align="center"----
plot(results, event = TRUE, ci = 0.9, event_window = c(-2, 5),
     main = "My Event Study Plot")

## -----------------------------------------------------------------------------
citation("undidR")

## -----------------------------------------------------------------------------
print(citation("undidR"), bibtex = TRUE)

