#' Computes UNDID results
#'
#' Takes in all of the filled diff df CSV files and uses them to compute
#' group level ATTs as well as the aggregate ATT and its standard errors
#' and p-values.
#'
#' @details
#' The `agg` parameter specifies the aggregation method used in the
#' case of staggered adoption. By default it is set to `"silo"` so that the ATTs
#' are aggregated across silos with each silo having equal weight, but can be
#' set to `"gt"` or `"g"` instead. Aggregating across `"g"` calculates ATTs for
#' groups based on when the treatment time was, with each `"g"` group having
#' equal weight. Aggregating across `"gt"` calculates ATTs for groups based on
#' when the treatment time was and the time for which the ATT is calculated.
#' The `agg` parameter is ignored in the case of a common treatment time and
#' only takes effect in the case of staggered adoption. For common adoption,
#' refer to the `weights` parameter.
#'
#' @param dir_path A character specifying the filepath to the folder containing
#'  all of the filled diff df CSV files.
#' @param agg A character which specifies the aggregation methodology for
#'  computing the aggregate ATT in the case of staggered adoption.
#'  Options are: `"silo"`, `"g"`, or `"gt"`. Defaults to `"silo"`.
#' @param weights A logical value (either `TRUE` or `FALSE`) which determines
#'  whether or not the weights should be used in the case of common adoption.
#'  Defaults to `TRUE`.
#' @param covariates A logical value (either `TRUE` or `FALSE`) which specifies
#'  whether to use the `diff_estimate` column or the `diff_estimate_covariates`
#'  column from the filled diff df CSV files when computing ATTs.
#' @param interpolation A logical value or a character which specifies which,
#'  if any, method of interpolation/extrapolation for missing values of
#'  `diff_estimate` or `diff_estimate_covariates` should be used.
#'  There must be at least one `diff_estimate` or `diff_estimate_covariates`
#'  value for the (silo,g) group for which a missing value is being estimated
#'  in order for interpolation to work. Options are: `"linear_function"`,
#'  `"nearest_value"`, or `"piecewise_linear"`. Defaults to `FALSE`.
#' @param save_csv A logical value, either `TRUE` or `FALSE` (default),
#'  which determines if a CSV copy of the UNDID results will be saved or not.
#' @param filename A string filename for the created CSV file.
#'  Defaults to `"UNDID_results.csv"`
#' @param filepath Filepath to save the CSV file. Defaults to `tempdir()`.
#' @param nperm Number of random permutations of gvar & silo pairs to consider
#'  when calculating the randomization inference p-value. Defaults to `1001`.
#' @param verbose A logical value (either `TRUE` or `FALSE`) which toggles
#'  messages showing the progress of the randomization inference.
#'  Defaults to `TRUE`.
#'
#' @returns A data frame containing the aggregate ATT and its
#' standard errors and p-values from two-sided tests of `agg_ATT` == 0.
#' Also returns group (silo, g, or gt) level ATTs for staggered adoption.
#'
#' @examples
#'
#' # Execute `undid_stage_three()`
#' dir <- system.file("extdata/staggered", package = "undidR")
#' undid_stage_three(dir, agg = "g", nperm = 501, verbose = FALSE)
#'
#' @importFrom stats pt na.omit setNames
#' @importFrom grDevices adjustcolor colorRampPalette dev.off png
#' @export
undid_stage_three <- function(dir_path, agg = "silo", weights = TRUE,
                              covariates = FALSE, interpolation = FALSE,
                              save_csv = FALSE, filename = "UNDID_results.csv",
                              filepath = tempdir(), nperm = 1001,
                              verbose = TRUE) {
  if (identical(save_csv, TRUE)) {
    filepath <- .filename_filepath_check(filename, filepath)
  } else if (!identical(save_csv, FALSE)) {
    stop("`save_csv` must be set to `TRUE` or `FALSE`.")
  }

  diff_df <- .combine_diff_data(dir_path, covariates, interpolation)
  if (covariates == FALSE) {
    diff_df$y <- diff_df$diff_estimate
    diff_df$y_var <- diff_df$diff_var
  } else if (covariates == TRUE) {
    diff_df$y <- diff_df$diff_estimate_covariates
    diff_df$y_var <- diff_df$diff_var_covariates
  }

  # Compute staggered adoption
  if ("diff_times" %in% names(diff_df)) {
    agg <- tolower(agg)
    if (agg == "silo") {
      results <- .stage_three_silo(diff_df, nperm, verbose)
    } else if (agg == "g") {
      results <- .stage_three_g(diff_df, nperm, verbose)
    } else if (agg == "gt") {
      results <- .stage_three_gt(diff_df, nperm, verbose)
    } else {
      stop("`agg` must be either \"silo\", \"g\", or \"gt\".")
    }

  }

  # Compute common adoption
  if ("common_treatment_time" %in% names(diff_df)) {
    has_both <- all(c(0, 1) %in%  diff_df$treat)
    if (has_both == FALSE) {
      untreated <- sum(diff_df$treat == 0)
      treated <- sum(diff_df$treat == 1)
      stop(paste("Found", untreated, "untreated silos and", treated,
                 "treated silos.\nUnable to compute aggregate ATT."))
    }
    results <- .stage_three_common(diff_df, weights, nperm)
  }

  return(results)

}

#' @keywords internal
#' Return a combined dataframe of all filled_diff_df csv files from a folder
.combine_diff_data <- function(dir_path, covariates = FALSE,
                               interpolation = FALSE) {

  files <- list.files(dir_path,
                      pattern = "^filled_diff_df_.*\\.csv$", full.names = TRUE)
  if (length(files) == 0) stop(paste("No filled diff df CSV files found in:",
                                     dir_path))
  diff_df <- do.call(rbind, lapply(files, .read_diff_df))

  if (identical(covariates, FALSE)) {
    diff_estimate_na_count <- sum(is.na(diff_df$diff_estimate))
    value <- "diff_estimate"
  } else if (identical(covariates, TRUE)) {
    diff_estimate_na_count <- sum(is.na(diff_df$diff_estimate_covariates))
    value <- "diff_estimate_covariates"
  } else {
    stop("Set `covariates` to either `TRUE` or `FALSE`.")
  }

  if (diff_estimate_na_count > 0) {
    silos <- unique(diff_df[is.na(diff_df[[value]]), "silo_name"])
    if ("common_treatment_time" %in% names(diff_df)) {
      warning(paste("No values found for", value, "for silos:",
                    paste(silos, collapse = ", "),
                    "\nDropping the aforementioned silos."))
      diff_df <- diff_df[!diff_df$silo_name %in% silos, ]
    } else {
      for (silo in silos) {
        gt_missing <- diff_df[(diff_df$silo_name == silo)
                              & is.na(diff_df$diff_estimate), "gt"]
        warning(paste("No values for", value, "found for silo:",
                      silo, "\nfor gt groups:", paste(gt_missing,
                                                      collapse = ", ")))
      }
      if (identical(interpolation, FALSE)) {
        stop(paste("Consider setting `interpolation` to one of the following:",
                   paste(.undid_env$interpolation_options, collapse = ", ")))
      }
    }
  }

  if (!identical(FALSE, interpolation)) {
    if (!is.character(interpolation) ||
          (!interpolation %in% .undid_env$interpolation_options)) {
      stop(paste("`interpolation` must be set to `FALSE` or one of:",
                 paste(.undid_env$interpolation_options, collapse = ", ")))
    }
    diff_df <- .interpolate_extrapolate(diff_df, interpolation, value)
  }

  return(diff_df)
}

#' @keywords internal
#' Returns the combined_diff_df with imputed values or stops
#' if unable to compute these values
.interpolate_extrapolate <- function(diff_df, interpolation, value) {
  silos <- unique(diff_df[is.na(diff_df[[value]]), "silo_name"])
  for (silo in silos) {
    gvars <- unique(diff_df[is.na(diff_df[[value]]) &
                              diff_df$silo_name == silo, "gvar"])
    for (gvar in gvars) {
      mask <- diff_df$gvar == gvar & diff_df$silo_name == silo
      non_missing_count <- nrow(diff_df[mask & !is.na(diff_df[[value]]), ])
      if (non_missing_count < 1) {
        stop(paste0("No other values of ", value, " found for (silo, gvar) = (",
                    silo, ", ", as.Date(gvar), ")",
                    ".\nNot able to impute missing values."))
      } else if (non_missing_count == 1) {
        non_missing_value <- diff_df[mask &
                                       !is.na(diff_df[[value]]), value]
        diff_df[mask &
                  is.na(diff_df[[value]]), value] <- non_missing_value

      } else if (non_missing_count > 1) {
        y <- diff_df[mask, value]
        diff_df[
          mask & is.na(diff_df[[value]]), value
        ] <- .extrap_wrapper(y, interpolation)
      }
    }
  }
  return(diff_df)
}

#' @keywords internal
#' Simple wrapper to reduce cyclomatic complexity in `.interpolate_extrpolate`
.extrap_wrapper <- function(y, interpolation) {
  if (interpolation == "linear_function") {
    y_hat <- .extrapolate_linear(y)
  }
  if (interpolation == "nearest_value") {
    y_hat <- .get_nearest_value(y)
  }
  if (interpolation == "piecewise_linear") {
    y_hat <- .piecewise_linear_function(y)
  }
  return(y_hat)
}

#' @keywords internal
#' Runs stage three calculations with agg == "silo"
.stage_three_silo <- function(diff_df, nperm, verbose) {

  # Set up results data frame
  treated_silos <- unique(diff_df[diff_df$treat == 1, "silo_name"])
  results <- data.frame(silo_name = treated_silos, silo_ATT = NA_real_,
                        silo_SE = NA_real_, silo_p_val = NA_real_,
                        silo_jknife_SE = NA_real_, silo_jknife_p_val = NA_real_,
                        agg_ATT = NA_real_, agg_ATT_SE = NA_real_,
                        agg_ATT_p_val = NA_real_, agg_ATT_jknife_SE = NA_real_,
                        agg_ATT_jknife_p_val = NA_real_,
                        agg_ATT_RI_p_val = NA_real_)

  # Loop through treated silos and compute results
  for (i in seq_along(treated_silos)) {
    silo <- treated_silos[i]
    treated_mask <- diff_df$silo_name == silo & diff_df$treat == 1
    gvar_treated <- diff_df[treated_mask, "gvar"][1]
    control_mask <- diff_df$treat == 0 & diff_df$gvar == gvar_treated
    subset <- rbind(diff_df[treated_mask, ], diff_df[control_mask, ])
    n <- nrow(subset)
    x <- cbind(rep(1, n), subset$treat)
    y <- subset$y
    reg <- .regress(x, y)
    att_s <- reg$beta_hat[2]
    att_s_se <- sqrt(reg$beta_hat_var[2])

    results$silo_ATT[i] <- att_s
    results$silo_SE[i] <- att_s_se
    results$silo_p_val[i] <- 2 * (1 - pt(abs(att_s / att_s_se), n - 2))
    results$silo_jknife_SE[i] <- .compute_jknife_se(x, y, att_s)
    results$silo_jknife_p_val[i] <- 2 *
      (1 - pt(abs(att_s / results$silo_jknife_SE[i]), n - 2))

  }
  n <- nrow(results)
  reg <- .regress(rep(1, n), results$silo_ATT)
  results$agg_ATT[1] <- reg$beta_hat[1]
  results$agg_ATT_SE[1] <- sqrt(reg$beta_hat_var[1])
  results$agg_ATT_p_val[1] <- 2 *
    (1 - pt(abs(results$agg_ATT[1] / results$agg_ATT_SE[1]), n - 1))
  results$agg_ATT_jknife_SE[1] <- .compute_jknife_se(rep(1, n),
                                                     results$silo_ATT,
                                                     reg$beta_hat[1])
  results$agg_ATT_jknife_p_val[1] <- 2 *
    (1 - pt(abs(reg$beta_hat[1] / results$agg_ATT_jknife_SE[1]), n - 2))

  # Now do randomization inference
  results$agg_ATT_RI_p_val[1] <- .compute_ri_pval(diff_df, "silo", nperm,
                                               results$agg_ATT[1], verbose)

  return(results)

}

#' @keywords internal
#' Runs stage three calculations with agg == "g"
.stage_three_g <- function(diff_df, nperm, verbose) {

  # Set up results data frame
  gvars <- sort(unique(diff_df$gvar))
  results <- data.frame(gvar = gvars, gvar_ATT = NA_real_,
                        gvar_SE = NA_real_, gvar_p_val = NA_real_,
                        gvar_jknife_SE = NA_real_, gvar_jknife_p_val = NA_real_,
                        agg_ATT = NA_real_, agg_ATT_SE = NA_real_,
                        agg_ATT_p_val = NA_real_, agg_ATT_jknife_SE = NA_real_,
                        agg_ATT_jknife_p_val = NA_real_,
                        agg_ATT_RI_p_val = NA_real_)

  for (i in seq_along(gvars)) {
    gvar <- gvars[i]

    results$gvar[i] <- gvar
    mask <- diff_df$gvar == gvar & diff_df$treat != -1

    x <- cbind(1, diff_df[mask, "treat"])
    y <- diff_df[mask, "y"]
    reg <- .regress(x, y)
    gvar_att <- reg$beta_hat[2]
    gvar_se <- sqrt(reg$beta_hat_var[2])

    results$gvar_ATT[i] <- gvar_att
    results$gvar_SE[i] <- gvar_se
    results$gvar_p_val[i] <- 2 *
      (1 - pt(abs(gvar_att / gvar_se), length(y) - 2))
    results$gvar_jknife_SE[i] <- .compute_jknife_se(x, y, gvar_att)
    results$gvar_jknife_p_val[i] <- 2 *
      (1 - pt(abs(gvar_att / results$gvar_jknife_SE[i]), length(y) - 2))
  }

  n <- nrow(results)
  reg <- .regress(rep(1, n), results$gvar_ATT)
  results$agg_ATT[1] <- reg$beta_hat[1]
  results$agg_ATT_SE[1] <- sqrt(reg$beta_hat_var[1])
  results$agg_ATT_p_val[1] <- 2 *
    (1 - pt(abs(results$agg_ATT[1] / results$agg_ATT_SE[1]), n - 1))
  results$agg_ATT_jknife_SE[1] <- .compute_jknife_se(rep(1, n),
                                                     results$gvar_ATT,
                                                     reg$beta_hat[1])
  results$agg_ATT_jknife_p_val[1] <- 2 *
    (1 - pt(abs(reg$beta_hat[1] / results$agg_ATT_jknife_SE[1]), n - 2))

  results$gvar <- as.Date(results$gvar)

  # Now do randomization inference
  results$agg_ATT_RI_p_val[1] <- .compute_ri_pval(diff_df, "g", nperm,
                                               results$agg_ATT[1], verbose)

  return(results)
}

#' @keywords internal
#' Runs stage three calculations with agg == "gt"
.stage_three_gt <- function(diff_df, nperm, verbose) {

  # Set up results data frame
  diff_df <- diff_df[order(diff_df$gvar, diff_df$t), ]
  gts <- unique(diff_df$gt)
  results <- data.frame(gt = gts, gt_ATT = NA_real_,
                        gt_SE = NA_real_, gt_p_val = NA_real_,
                        gt_jknife_SE = NA_real_, gt_jknife_p_val = NA_real_,
                        agg_ATT = NA_real_, agg_ATT_SE = NA_real_,
                        agg_ATT_p_val = NA_real_, agg_ATT_jknife_SE = NA_real_,
                        agg_ATT_jknife_p_val = NA_real_,
                        agg_ATT_RI_p_val = NA_real_)

  for (i in seq_along(gts)) {
    gt <- gts[i]
    mask <- diff_df$gt == gt & diff_df$treat != -1
    x <- cbind(1, diff_df[mask, "treat"])
    y <- diff_df[mask, "y"]
    reg <- .regress(x, y)
    gt_att <- reg$beta_hat[2]
    gt_se <- sqrt(reg$beta_hat_var[2])

    results$gt_ATT[i] <- gt_att
    results$gt_SE[i] <- gt_se
    results$gt_p_val[i] <- 2 *
      (1 - pt(abs(gt_att / gt_se), length(y) - 2))
    results$gt_jknife_SE[i] <- .compute_jknife_se(x, y, gt_att)
    results$gt_jknife_p_val[i] <- 2 *
      (1 - pt(abs(gt_att / results$gt_jknife_SE[i]), length(y) - 2))
  }

  n <- nrow(results)
  reg <- .regress(rep(1, n), results$gt_ATT)
  results$agg_ATT[1] <- reg$beta_hat[1]
  results$agg_ATT_SE[1] <- sqrt(reg$beta_hat_var[1])
  results$agg_ATT_p_val[1] <- 2 *
    (1 - pt(abs(results$agg_ATT[1] / results$agg_ATT_SE[1]), n - 1))
  results$agg_ATT_jknife_SE[1] <- .compute_jknife_se(rep(1, n),
                                                     results$gt_ATT,
                                                     reg$beta_hat[1])
  results$agg_ATT_jknife_p_val[1] <- 2 *
    (1 - pt(abs(reg$beta_hat[1] / results$agg_ATT_jknife_SE[1]), n - 2))

  # Now do randomization inference
  results$agg_ATT_RI_p_val[1] <- .compute_ri_pval(diff_df, "gt", nperm,
                                               results$agg_ATT[1], verbose)

  return(results)
}


#' @keywords internal
#' Procedure for computing randomization inference p-value
#' Takes in the combined_diff data, the method of aggregation,
#' the number of randomizations (nperm), the agg_att to compare to the
#' ri_att's, and an option to print progress of computing the nperm iterations
.compute_ri_pval <- function(diff_df, agg, nperm, agg_att, verbose) {

  # Stop if verbose is not `TRUE` or `FALSE`
  .validate_logical_args(verbose, "verbose")

  # Reorder dataframe, grab unique gt groups
  diff_df <- diff_df[order(diff_df$gvar, diff_df$t), ]
  gts <- unique(diff_df$gt)

  # Reconstruct init.csv from first stage
  treated_silos <- unique(diff_df[diff_df$treat == 1,
                                  c("silo_name", "gvar", "treat")])
  control_silos <- data.frame(silo_name = unique(diff_df[diff_df$treat == 0,
                                                         "silo_name"]),
                              gvar = NA_real_, treat = 0)
  init <- rbind(treated_silos, control_silos)
  rownames(init) <- NULL
  n_unique_perms <- choose(nrow(init), nrow(init[init$treat == 1, ]))

  # Stop if nperm is not numeric, display warning if nperm < 500
  .nperm_check(nperm, n_unique_perms)

  # Take gvar assignment from init and randomize across silos `nperm` times
  # Enforce unique permutations
  gvar <- init$gvar
  seen <- new.env(hash = TRUE, size = nperm, parent = emptyenv())
  key <- paste(gvar, collapse = "")
  seen[[key]] <- TRUE
  i <- 1
  while (i < nperm) {
    new_perm <- sample(gvar)
    key <- paste(new_perm, collapse = "")
    if (is.null(seen[[key]])) {
      init[[paste0("gvar_randomized_", i)]] <- new_perm
      seen[[key]] <- TRUE
      i <- i + 1
    }
  }
  rm(seen)

  # Create preallocation vector to store agg_ATT from each randomization
  ri_att <- rep(NA_real_, nperm - 1)

  # Loop through gvar randomizations nperm times
  for (j in seq_len(nperm - 1)) {

    # Creates a mask for init that selects control silos
    mask_gvar <- is.na(init[[paste0("gvar_randomized_", toString(j))]])

    # Grabs gvars in the same order as the silos they are assigned to
    new_gvars <- as.Date(init[[paste0("gvar_randomized_",
                                      toString(j))]])[!mask_gvar]

    # Selects the treated and control silos for this iteration
    new_treated_silos <- init$silo_name[!mask_gvar]
    new_control_silos <- init$silo_name[mask_gvar]

    # Begin constructing the diff dataframe for this iteration by selecting
    # all of the rows from diff_df with this iteration's control silos
    ri_df <- diff_df[diff_df$silo_name %in% new_control_silos, ]

    # Re-write this iteration's control silos to have treat = 0
    ri_df$treat <- 0

    # Construct the rest of this iteration's diff dataframe by adding
    # treated silos
    for (i in seq_along(new_gvars)) {
      # Loop through this iteration's treated silos their newly associated gvars
      silo <- new_treated_silos[i]
      gvar <- new_gvars[i]

      # Add to this itertation's diff dataframe only the rows from
      # diff_df where the mask below is true
      mask <- diff_df$silo_name == silo & diff_df$gvar == gvar
      ri_df <- rbind(ri_df, diff_df[mask, ])
      ri_mask <- ri_df$silo_name == silo & ri_df$gvar == gvar

      # Re-write those rows as having treat = 1
      ri_df$treat[ri_mask] <- 1
    }

    # Reset row number index
    rownames(ri_df) <- NULL

    # Compute agg_ATT conditional on aggregation method and send to ri_att
    ri_att[j] <- .ri_agg_att(ri_df, agg, new_gvars, gts)

    # Add progress indicator
    if (verbose && j %% 100 == 0) {
      message(sprintf("Completed %d of %d permutations", j, nperm - 1))
    }
  }
  return(sum(abs(ri_att) > abs(agg_att)) / length(ri_att))
}

#' @keywords internal
#' Checks that nperm is numeric and is less than or equal to n_unique_perms
#' and greater than 500
.nperm_check <- function(nperm, n_unique_perms) {
  if (!is.numeric(nperm)) {
    stop("`nperm` must be a numeric value.")
  } else if (nperm > n_unique_perms) {
    stop(paste0("`nperm` = ", nperm,
                " is greater than the number of unique permutations (",
                n_unique_perms, ").\nSet `nperm` <= ", n_unique_perms))
  } else if (nperm < 500) {
    warning("Randomization inference may not be valid with `nperm` < 500.")
  }
}

#' @keywords internal
#' Compute agg_att within `.compute_ri_pval()` for a permutation
.ri_agg_att <- function(ri_df, agg, new_gvars, gts) {
  if (agg == "silo") {
    treated_silos <- unique(ri_df[ri_df$treat == 1, "silo_name"])
    att_vector <- rep(NA_real_, length(treated_silos))
    for (i in seq_along(treated_silos)) {
      silo <- treated_silos[i]
      treated_mask <- ri_df$silo_name == silo & ri_df$treat == 1
      gvar_treated <- ri_df[treated_mask, "gvar"][1]
      control_mask <- ri_df$treat == 0 & ri_df$gvar == gvar_treated
      subset <- rbind(ri_df[treated_mask, ], ri_df[control_mask, ])
      x <- cbind(1, subset$treat)
      y <- subset$y
      reg <- .regress(x, y)
      att_vector[i] <- reg$beta_hat[2]
    }
  }

  if (agg == "g") {
    att_vector <- rep(NA_real_, length(new_gvars))
    for (i in seq_along(new_gvars)) {
      gvar <- new_gvars[i]
      mask <- ri_df$gvar == gvar
      x <- as.matrix(cbind(1, ri_df[mask, "treat"]))
      y <- as.vector(ri_df[mask, "y"])
      reg <- .regress(x, y)
      att_vector[i] <- reg$beta_hat[2]
    }
  }

  if (agg == "gt") {
    att_vector <- rep(NA_real_, length(gts))
    for (i in seq_along(gts)) {
      gt <- gts[i]
      mask <- ri_df$gt == gt
      x <- as.matrix(cbind(1, ri_df[mask, "treat"]))
      y <- as.vector(ri_df[mask, "y"])
      reg <- .regress(x, y)
      att_vector[i] <- reg$beta_hat[2]
    }
  }

  return(mean(att_vector))
}

#' @keywords internal
#' Runs stage three calculations for common adoption
.stage_three_common <- function(diff_df, weights, nperm) {

  # Construct weights
  w <- .stg_thr_weights(weights, diff_df)

  # Create results data frame
  results <- data.frame(treatment_time = diff_df$common_treatment_time[1],
                        agg_ATT = NA_real_, agg_ATT_SE = NA_real_,
                        pval = NA_real_, agg_ATT_jknife_SE = NA_real_,
                        jknife_pval = NA_real_, RI_pval = NA_real_)

  # Count number of treat & control silos
  trt_silos <- sum(diff_df$treat == 1)
  ctrl_silos <- sum(diff_df$treat == 0)
  nsilos <- trt_silos + ctrl_silos

  x <- cbind(1, diff_df$treat)
  y <- diff_df$y

  if (trt_silos == 1 && ctrl_silos == 1) {
    results$agg_ATT[1] <- diff_df[diff_df$treat == 1, "y"] -
      diff_df[diff_df$treat == 0, "y"]
    results$agg_ATT_SE[1] <- sqrt(diff_df[diff_df$treat == 1, "y_var"] +
                                    diff_df[diff_df$treat == 0, "y_var"])
  } else {
    reg <- .regress(x, y, w)
    results$agg_ATT[1] <- reg$beta_hat[2]
    results$agg_ATT_SE[1] <- sqrt(reg$beta_hat_var[2])
    results$pval <- 2 *
      (1 - pt(abs(results$agg_ATT[1] / results$agg_ATT_SE[1]), nsilos - 2))
  }

  if (trt_silos >= 2 && ctrl_silos >= 2) {
    results$agg_ATT_jknife_SE[1] <- .compute_jknife_se(x, y,
                                                       results$agg_ATT[1], w)
    results$jknife_pval[1] <- 2 *
      (1 - pt(abs(results$agg_ATT[1] / results$agg_ATT_jknife_SE[1]),
              nsilos - 2))
  }

  # Compute RI pval
  unique_permutations <- choose(nsilos, trt_silos)
  if (nperm > unique_permutations) {
    warning(paste("`nperm` was set to", nperm, "but the number of unique",
                  "permutations is only", unique_permutations,
                  "\nOverriding `nperm` to:", unique_permutations))
    nperm <- unique_permutations
  }
  if (nperm < 500) {
    warning("Randomization inference may not be valid with `nperm` < 500.")
  }

  # Enforce unique permutations
  ri_atts <- rep(NA_real_, nperm - 1)
  perm_matrix <- matrix(NA_real_, ncol = nperm - 1, nrow = length(y))
  seen <- new.env(hash = TRUE, size = nperm, parent = emptyenv())
  key <- paste(x[, 2], collapse = "")
  seen[[key]] <- TRUE
  i <- 1
  while (i < nperm) {
    new_perm <- sample(x[, 2])
    key <- paste(new_perm, collapse = "")
    if (is.null(seen[[key]])) {
      perm_matrix[, i] <- new_perm
      seen[[key]] <- TRUE
      i <- i + 1
    }
  }
  for (i in seq_len(nperm - 1)) {
    x_ri <- cbind(1, perm_matrix[, i])
    reg <- .regress(x_ri, y, w)
    ri_atts[i] <- reg$beta_hat[2]
  }
  rm(seen)

  ri_pval <- (sum(abs(ri_atts) > abs(results$agg_ATT[1]))) / length(ri_atts)
  results$RI_pval[1] <- ri_pval

  return(results)
}

#' @keywords internal
#' Returns weights depending on if weights is true or false
.stg_thr_weights <- function(weights, diff_df) {
  if ((identical(weights, TRUE))) {
    w <- diff_df$weights
    sum_w <- sum(w)
    w <- w / sum_w
    return(diag(w))
  } else if ((identical(weights, FALSE))) {
    return(diag(rep(1, nrow(diff_df))))
  } else {
    stop("`weights` must be set to `TRUE` or `FALSE`.")
  }
}