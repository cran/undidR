#' Computes UNDID results
#'
#' Takes in all of the filled diff df CSV files and uses them to compute
#' group level ATTs as well as the aggregate ATT and its standard errors
#' and p-values. Also takes in the trends data CSV files and uses them
#' to produce parallel trends and event study plots.
#'
#' @param dir_path A character specifying the filepath to the folder containing
#'  all of the filled diff df CSV files.
#' @param agg A character which specifies the aggregation methodology for
#'  computing the aggregate ATT in the case of staggered adoption.
#'  Options are: `"silo"`, `"g"`, `"gt"`, `"sgt"`, `"time"`, `"none"`.
#'  Defaults to `"g"`. `"silo"` computes a subaggregate ATT for each silo,
#'  `"g"` computes a subaggregate ATT for each unique treatment time,
#'  `"gt"` computes a subaggregate ATT for each unique treatment time
#'  & post-treatment period pair, `"sgt"` computes a subaggregate ATT
#'  for each treatment time & post-treatment pair, separated by silo, `"time"`
#'  computes subaggregate ATTs for grouped by time since treatment, and `"none"`
#'  does not compute any subaggregate ATTs, but rather computes an aggregate ATT
#'  directly from the differences.
#' @param weights A string, determines which of the weighting methodologies
#'  should be used. Options are: `"none"`, `"diff"`, `"att"`, or `"both"`.
#'  Defaults to the weighting choice specified in the filled diff CSV files.
#' @param covariates A logical value (either `TRUE` or `FALSE`) which specifies
#'  whether to use the `diff_estimate` column or the `diff_estimate_covariates`
#'  column from the filled diff df CSV files when computing ATTs.
#' @param omit A character vector of silos to omit. Defaults to `NULL`.
#' @param only A character vector of silos to include. Defaults to `NULL`.
#' @param notyet A logical value which declares if the not-yet-treated
#'  differences from treated silos should be used as controls when computing
#'  relevant sub-aggregate ATTs. Defaults to `FALSE`.
#' @param nperm Number of random permutations of treatment assignment to use
#'  when calculating the randomization inference p-value. Defaults to `999`.
#' @param verbose A numeric value (or `NULL`) which toggles messages showing
#'  the progress of the randomization inference once every `verbose` iterations.
#'  Defaults to `100`.
#' @param check_anon_size A logical value, which if `TRUE` displays which
#'  silos enabled the `anonymize_weights` argument in stage two, and their
#'  respective `anonymize_size` values. Defaults to `FALSE`.
#' @param hc Specify which heteroskedasticity-consistent covariance matrix
#'  estimator (HCCME) should be used. Options are `0`, `1`, `2`, `3`, and
#'  `4` (or `"hc0"`, `"hc1"`, `"hc2"`, `"hc3"`, `"hc4"`).
#' @param max_attempts A numeric value. Sets the maximum number of attempts
#'  to find a new unique random permutations during the randomization
#' inference procedure. Defaults to `100`.
#'
#' @returns An UnDiDObj with S3 methods of `summary()`, `plot()`,
#'  `print()`, and `coef()`.
#'
#' @examples
#'
#' # Execute `undid_stage_three()`
#' dir <- system.file("extdata/staggered", package = "undidR")
#' \donttest{
#'    # Recommended: nperm >= 399 for reasonable precision
#'    # (~15 seconds on typical hardware)
#'    result <- undid_stage_three(dir, agg = "g", nperm = 399, verbose = NULL)
#'
#'    # View the summary of results
#'    summary(result)
#'
#'    # View the parallel trends plot
#'    plot(result)
#'
#'    # View the event study plot
#'    plot(result, event = TRUE)
#' }
#' @importFrom stats pt qt
#' @importFrom grDevices colorRampPalette adjustcolor
#' @importFrom graphics plot lines segments abline legend axis
#' @export
undid_stage_three <- function(dir_path, agg = "g", weights = "both",
                              covariates = FALSE, notyet = FALSE,
                              nperm = 999, verbose = 100,
                              check_anon_size = FALSE, hc = "hc3",
                              only = NULL, omit = NULL, max_attempts = 100) {

  # Check logical args
  args <- list(
    notyet = notyet,
    covariates = covariates,
    check_anon_size = check_anon_size
  )
  for (arg_name in names(args)) {
    args[[arg_name]] <- args[[arg_name]][1]
    if (!(args[[arg_name]] %in% c(TRUE, FALSE))) {
      stop(paste0("'", arg_name, "' must be either TRUE or FALSE."))
    }
  }
  notyet <- args$notyet
  covariates <- args$covariates
  check_anon_size <- args$check_anon_size

  # Check verbose
  if (!is.null(verbose) && !is.numeric(verbose)) {
    stop("'verbose' must be either numeric or NULL.")
  }
  if (is.numeric(verbose)) {
    verbose <- round(verbose)
  }

  # Define agg options
  agg_options <- c("silo", "g", "gt", "sgt", "time", "none")
  agg <- trimws(tolower(agg[1]))
  if (!agg %in% agg_options) {
    stop(paste("'agg' must be one of:", paste(agg_options, collapse = ", ")))
  }

  # Check max attempts
  if (!is.numeric(max_attempts) || max_attempts < 1) {
    stop("'max_attempts' must be >= 1.")
  }
  max_attempts <- round(max_attempts)

  # Check nperm
  nperm <- nperm[1]
  if (is.numeric(nperm)) {
    nperm <- round(nperm)
  } else {
    stop("'nperm' must be numeric.")
  }
  if (nperm < 1) {
    stop("'nperm' must be greater than 0.")
  }

  # Check hc arg
  hc <- hc[1]
  if (is.numeric(hc)) {
    if (!hc %in% seq(0, 4)) {
      stop("'hc' must be 0, 1, 2, 3, or 4.")
    }
    hc <- paste0("hc", hc)
  }
  hc_options <- c("hc0", "hc1", "hc2", "hc3", "hc4")
  if (!hc %in% hc_options) {
    stop(paste("'hc' must be one of", paste(hc_options, collapse = ", ")))
  }

  # Check only and omit
  if (!is.null(only) && !is.character(only)) {
    stop("'only' must be NULL or a character value.")
  }
  if (!is.null(omit) && !is.character(omit)) {
    stop("'omit' must be NULL or a character value.")
  }

  # Read in and combine all the filled diff df CSV files
  diff_df <- .combine_diff_data(dir_path, covariates)
  trends <- .combine_trends_data(dir_path, covariates)
  if (!is.null(only)) {
    diff_df <- diff_df[diff_df$silo_name %in% only, ]
    trends <- trends[trends$silo_name %in% only, ]
  }
  if (!is.null(omit)) {
    diff_df <- diff_df[!diff_df$silo_name %in% omit, ]
    trends <- trends[!trends$silo_name %in% omit, ]
  }
  if ("diff_times" %in% names(diff_df)) {
    diff_df <- diff_df[order(diff_df$gvar, diff_df$t), ]
  }

  # Get weighting option
  weight_options <- c("none", "diff", "att", "both")
  if (is.null(weights)) {
    weights <- diff_df$weights[1]
  }
  weights <- trimws(tolower(weights[1]))
  if (!weights %in% weight_options) {
    stop(paste("'weights' must be one of:",
               paste(weight_options, collapse = ", ")))
  }
  # Check agg
  if ("common_treatment_time" %in% names(diff_df)) {
    if (agg == "sgt") {
      agg <- "silo"
    }
    if (agg %in% c("g", "gt", "time")) {
      agg <- "none"
    }
  } else if ("diff_times" %in% names(diff_df) && agg == "none") {
    warning("'agg' cannot be 'none' for staggered adoption. Setting 'agg = g'")
    agg <- "g"
  }
  # Check that if agg is none that weighting is not both or att
  if (agg == "none" && weights %in% c("att", "both")) {
    warning(
      paste0(
        "If 'agg = none' then 'weights' can only be either 'none' or 'diff'.\n",
        "Setting weights to 'diff'."
      )
    )
    weights <- "diff"
  }
  if (weights %in% c("att", "both")) {
    if (all(is.na(diff_df$n_t))) {
      warning(
        paste(
          "Selected weights require values for n_t but",
          "all n_t values are missing. Setting `weights=\"diff\"`."
        )
      )
      weights <- "diff"
    }
  }
  if (weights %in% c("diff", "both")) {
    if (all(is.na(diff_df$n_t))) {
      warning(
        paste(
          "Selected weights require values for n but",
          "all n values are missing. Setting `weights=\"none\"`."
        )
      )
      weights <- "none"
    }
  }

  if (covariates == FALSE) {
    diff_df$y <- diff_df$diff_estimate
    diff_df$y_var <- diff_df$diff_var
    diff_label <- "diff_estimate"
  } else if (covariates == TRUE) {
    diff_df$y <- diff_df$diff_estimate_covariates
    diff_df$y_var <- diff_df$diff_var_covariates
    diff_label <- "diff_estimate_covariates"
  }

  # Check if notyet is enabled and make the appropriate adjustments
  if (("diff_times" %in% names(diff_df)) && (notyet)) {
    treatdf <- unique(diff_df[diff_df$treat == 1, c("silo_name", "gvar")])
    treatment_times <- treatdf$gvar
    treated_states <- treatdf$silo_name
    for (i in seq_along(treatment_times)) {
      gvar <- treatment_times[i]
      s <- treated_states[i]
      mask <- diff_df$treat == -1 & diff_df$silo_name == s & diff_df$t < gvar
      diff_df$treat[mask] <- 0
    }
  }

  # Do check for missing values
  any_missing_y <- sum(is.na(diff_df$y))
  any_missing_n <- "n/a"
  any_missing_n_t <- "n/a"
  if (weights %in% c("diff", "both")) {
    any_missing_n <- sum(is.na(diff_df$n))
  }
  if (weights %in% c("att", "both")) {
    any_missing_n_t <- sum(is.na(diff_df$n_t))
  }
  if (any_missing_y > 0 ||
        (is.numeric(any_missing_n) && any_missing_n > 0) ||
        (is.numeric(any_missing_n_t) && any_missing_n_t > 0)) {
    warning(paste0("Missing value count for ", diff_label, ":", any_missing_y,
                   "\nMissing value count for n: ", any_missing_n,
                   "\nMissing value count for n_t: ", any_missing_n_t,
                   "\nDropping rows with missing values."))
    diff_df <- diff_df[!is.na(diff_df$y), ]
    if ((weights %in% c("diff", "both")) && (any_missing_n > 0)) {
      diff_df <- diff_df[!is.na(diff_df$n), ]
    }
    if ((weights %in% c("att", "both")) && (any_missing_n_t > 0)) {
      diff_df <- diff_df[!is.na(diff_df$n_t), ]
    }
    # Ensure there are enough gt values to actually compute something
    if ("diff_times" %in% names(diff_df)) {
      gt_values <- unique(diff_df$gt[diff_df$treat %in% c(0, 1)])
      valid_gt <- sapply(gt_values, function(g) {
        subset_data <- diff_df[diff_df$gt == g & diff_df$treat %in% c(0, 1), ]
        any(subset_data$treat == 1) && any(subset_data$treat == 0)
      })
      valid_gt <- gt_values[valid_gt]
      diff_df <- diff_df[diff_df$gt %in% valid_gt, ]
    }
    if (nrow(diff_df) == 0) {
      stop(paste(
        "Not enough differences to compute anything after dropping rows",
        "with missing values."
      ))
    }
  }

  # Common adoption check
  if ("common_treatment_time" %in% names(diff_df)) {
    has_both <- all(c(0, 1) %in%  diff_df$treat)
    if (has_both == FALSE) {
      untreated <- sum(diff_df$treat == 0)
      treated <- sum(diff_df$treat == 1)
      stop(paste("Found", untreated, "untreated silos and", treated,
                 "treated silos.\nUnable to compute aggregate ATT."))
    }
    if (agg %in% c("sgt", "silo")) {
      agg <- "silo"
    } else {
      agg <- "none"
    }
    diff_df$gvar <- diff_df$common_treatment_time
  }

  # Do check to see that there are enough differences for RI
  randomize <- TRUE
  if ("diff_times" %in% names(diff_df)) {
    states <- unique(diff_df$silo_name)
    gts <- unique(diff_df$gt)
    valid_states <- sapply(states, function(s) {
      state_gts <- unique(diff_df$gt[diff_df$silo_name == s])
      all(gts %in% state_gts)
    })
    bad_states <- states[!valid_states]
    if (length(bad_states) > 0) {
      randomize <- FALSE
      warning(paste(
        "The following states are missing differences for gt values required",
        " for randomization inference:\n", paste(bad_states, collapse = ", ")
      ))
    }
  }

  # Do a last check for notyet
  if ("common_treatment_time" %in% names(diff_df)) {
    if (isTRUE(notyet)) {
      warning(
        "'notyet' arg is not applicable to common treatment time setting."
      )
      notyet <- FALSE
    }
  }

  # Display anon size if requested
  if (check_anon_size) {
    display_anon <- unique(diff_df[, c("silo_name", "anonymize_size")])
    message("Displaying each silo's choice of anonymize_size:\n")
    print(display_anon)
  }

  # Compute
  if (agg == "silo") {
    iterable <- unique(diff_df[diff_df$treat == 1, "silo_name"])
    results <- data.frame(silo_name = iterable)
  } else if (agg == "g") {
    iterable <- unique(diff_df[diff_df$treat == 1, "gvar"])
    results <- data.frame(gvar = iterable)
  } else if (agg == "gt") {
    iterable <- unique(diff_df[diff_df$treat == 1, "gt"])
    results <- data.frame(gvar_time = iterable)
  } else if (agg == "sgt") {
    diff_df <- diff_df[order(diff_df$silo_name, diff_df$gvar, diff_df$t), ]
    diff_df$sgt <- paste0(diff_df$silo_name, ": ", diff_df$gt)
    iterable <- unique(diff_df[diff_df$treat == 1, ]$sgt)
    results <- data.frame(sgt = iterable)
  } else if (agg == "time") {
    # A bit of extra processing for time agg
    match_to_these_dates <- seq.Date(
      from = as.Date(diff_df$start_time[1],
                     origin = "1970-01-01"),
      to = as.Date(diff_df$end_time[1],
                   origin = "1970-01-01"),
      by = diff_df$freq[1]
    )
    ordered_t <- match_to_these_dates
    t_period <- match(as.Date(diff_df$t, origin = "1970-01-01"), ordered_t)
    treated_time_period <- match(as.Date(diff_df$gvar, origin = "1970-01-01"),
                                 ordered_t)
    diff_df$time_since_treatment <- t_period - treated_time_period
    iterable <- sort(
      unique(diff_df[diff_df$treat == 1, "time_since_treatment"])
    )
    results <- data.frame(time_since_treatment = iterable)
  } else if (agg == "none") {
    results <- data.frame()
    iterable <- NULL
  }
  results <- .stage_three_compute(diff_df, nperm, verbose, hc,
                                  agg, iterable, results, weights, randomize,
                                  max_attempts, notyet)

  results <- create_undid_object(results, weights, agg, covariates,
                                 notyet, hc, diff_df, trends)

  return(results)

}

#' @title UnDiDObj
#'
#' @description Objects of this class store all of the results for a given
#' choice of aggregation, weighting, notyet, covariates (and other)
#' specifications.
#'
#' @param result DataFrame of results from DiDInt.jl
#' @param weights The weighting method that was used.
#' @param agg The aggregation method that was used.
#' @param covariates The covariate option that was selected.
#' @param notyet The notyet option that was selected.
#' @param hc The hc option that was selected.
#' @param diff_df The dataset use to compute results.
#' @return UnDiDObj with class "UnDiDObj"
#' @keywords internal
#' @noRd
create_undid_object <- function(result, weights, agg, covariates,
                                notyet, hc, diff_df, trends) {

  # Select covariates?
  if (isTRUE(covariates)) {
    covariates <- paste(
      strsplit(diff_df$covariates[1], ";")[[1]], collapse = ", "
    )
  } else {
    covariates <- "none"
  }

  # Create aggregate results dataframe
  agg_df <- data.frame(
    att = result$agg_att[1],
    se = result$agg_att_se[1],
    pval = result$agg_att_pval[1],
    ri_pval = result$agg_att_ri_pval[1],
    jknife_se = result$agg_att_jknife_se[1],
    jknife_pval = result$agg_att_jknife_pval[1]
  )

  group_title <- NULL
  if (agg == "sgt") {
    group_title <- "State: gvar;time"
  } else if (agg == "gt") {
    group_title <- "gvar;time"
  } else if (agg == "time") {
    group_title <- "Periods Post Treatment"
  } else if (agg == "g") {
    group_title <- "Treatment Time"
  } else if (agg == "silo") {
    group_title <- "Silo"
  }

  # Create sub-aggregate results dataframe if available
  sub_df <- NULL
  if (agg != "none") {
    sub_df <- data.frame(
      group = result[, 1],
      att = result$sub_att,
      se = result$sub_se,
      pval = result$sub_pval,
      ri_pval = result$sub_ri_pval,
      jknife_se = result$sub_jknife_se,
      jknife_pval = result$sub_jknife_pval,
      weights = result$weights,
      stringsAsFactors = FALSE
    )
  }

  # Create object
  out <- list(
    agg = agg_df,
    sub = sub_df,
    diff = diff_df,
    trends = trends,

    specs = list(
      period = diff_df$freq[1],
      start_date = .parse_date_to_string(
        as.Date(diff_df$start_time[1], origin = "1970-01-01"),
        diff_df$date_format[1]
      ),
      end_date = .parse_date_to_string(
        as.Date(diff_df$end_time[1], origin = "1970-01-01"),
        diff_df$date_format[1]
      ),
      nperm = result$nperm[1],
      notyet = notyet,
      covariates = covariates,
      weighting = weights,
      hc = hc,
      agg = agg
    ),

    group_title = group_title
  )

  class(out) <- "UnDiDObj"
  return(out)
}

#' @title Print method for \code{UnDiDObj}
#'
#' @param x A \code{UnDiDObj} object.
#' @param level Specify either `"agg"` or `"sub"` to view the aggregate
#'   or sub-aggregate results.
#' @param ... other arguments
#' @export
print.UnDiDObj <- function(x, level = c("agg", "sub"), ...) {
  level <- match.arg(level)
  cat("\n")
  cat(sprintf("  Weighting: %s\n", x$specs$weighting))
  cat(sprintf("  Covariates: %s\n", x$specs$covariates))
  cat(sprintf("  Aggregation: %s\n\n", x$specs$agg))
  if (level == "agg") {
    cat(sprintf("  Aggregate ATT:  %.4f\n", x$agg$att[1]))
    if (!is.null(x$sub)) {
      cat(
        sprintf(
          "\n(%d sub-aggregate estimates available via print(., level='sub'))\n",
          nrow(x$sub)
        )
      )
    }
  } else {
    if (is.null(x$sub)) {
      cat("No sub-aggregate estimates available.\n")
    } else {
      cat("Sub-Aggregate ATTs:\n")
      cat("-------------------\n")
      sub_print <- data.frame(
        Group = x$sub[[1]],
        ATT = sprintf("%.4f", x$sub$att),
        stringsAsFactors = FALSE
      )
      names(sub_print)[1] <- x$group_title
      print(sub_print, row.names = FALSE, right = TRUE)
    }
  }
  invisible(x)
}

#' @title Summary method for \code{UnDiDObj}
#'
#' @param object A \code{UnDiDObj} object
#' @param level Specify either `"agg"`, `"sub"`, or `"all"`, to view the
#'   results at the aggregate level, the sub-aggregate level, or to view
#'   both simultaneously.
#' @param ... other arguments
#' @export
summary.UnDiDObj <- function(object, level = c("all", "agg", "sub"), ...) {
  level <- match.arg(level)
  if (level == "all") {
    level <- c("agg", "sub")
  }
  cat("\n")
  cat(sprintf("  Weighting: %s\n", object$specs$weighting))
  cat(sprintf("  Aggregation: %s\n", object$specs$agg))
  cat(sprintf("  Not-yet-treated: %s\n", object$specs$notyet))
  cat(sprintf("  Covariates: %s\n", object$specs$covariates))
  cat(sprintf("  HCCME: %s\n", object$specs$hc))
  cat(sprintf("  Period Length: %s\n", object$specs$period))
  cat(sprintf("  First Period: %s\n", object$specs$start_date))
  cat(sprintf("  Last Period: %s\n", object$specs$end_date))
  cat(sprintf("  Permutations: %d\n\n", object$specs$nperm))

  if ("agg" %in% level) {
    agg_display <- object$agg
    names(agg_display) <- c("ATT", "Std. Error", "p-value",
                            "RI p-value", "Jackknife SE", "Jackknife p-value")
    cat("Aggregate Results:\n")
    print(agg_display, row.names = FALSE, right = TRUE)
    cat("\n")
  }

  if ("sub" %in% level) {
    if (is.null(object$sub)) {
      cat("No sub-aggregate estimates available.\n")
    } else {
      cat("Subaggregate Results:\n")
      # Print header
      cat(sprintf("%-20s %10s %10s %10s %10s %10s %10s %10s\n",
                  object$group_title, "ATT", "SE", "p-value",
                  "RI p-val", "JK SE", "JK p-val", "Weight"))
      cat(strrep("-", 110), "\n")
      # Print out sub-agg results
      for (i in seq_len(nrow(object$sub))) {
        cat(sprintf("%-20s %10.4f %10.4f %10.4f %10.4f %10.4f %10.4f %10.4f\n",
                    object$sub[[1]][i],
                    object$sub$att[i],
                    object$sub$se[i],
                    object$sub$pval[i],
                    object$sub$ri_pval[i],
                    object$sub$jknife_se[i],
                    object$sub$jknife_pval[i],
                    object$sub$weights[i]))
      }
    }
  }
  invisible(object)
}

#' @title Extract coefficients from \code{UnDiDObj}
#'
#' @param object A \code{UnDiDObj} object
#' @param level Specify either `"agg"` or `"sub"` to view the aggregate
#'   or sub-aggregate results.
#' @param ... other arguments
#' @return A data frame of coefficient estimates
#' @export
coef.UnDiDObj <- function(object, level = c("agg", "sub"), ...) {
  level <- match.arg(level)
  if (level == "agg") {
    return(object$agg$att)
  } else {
    if (is.null(object$sub)) {
      cat("No sub-aggregate estimates available.\n")
      return(NULL)
    } else {
      return(data.frame(Group = object$sub[[1]],
                        ATT = object$sub$att))
    }
  }
}

#' @title Plot method for \code{UnDiDObj}
#'
#' @param x A \code{UnDiDObj} object
#' @param event Logical. If \code{TRUE}, creates an event study plot. If
#'   \code{FALSE} (default), creates a parallel trends plot.
#' @param event_window Numeric vector of length 2 specifying the event window
#'   as c(start, end). Default is \code{NULL} (use all available periods).
#' @param ci Numeric between 0 and 1 specifying confidence level.
#'   Default is 0.95.
#' @param lwd Linewidth arg passed to `lines()`, `abline()`, and `segments()`.
#'  Defaults to `1`.
#' @param legend Keywords for indicating desired legend location. Defaults to
#'  `"topright"`. Other options include any of the keywords used as
#'  x in `legend(x, ...)` or `NULL` to omit a legend.
#' @param ... other arguments passed to plot
#' @export
plot.UnDiDObj <- function(x, event = FALSE,
                          event_window = NULL, ci = 0.95,
                          lwd = 1, legend = "topright", ...) {
  if (ci <= 0 || ci >= 1) {
    stop("`ci` must be between 0 and 1")
  }

  trends <- x$trends
  hc <- x$specs$hc
  covariates <- x$specs$covariates
  weights <- x$specs$weighting
  trends <- trends[!is.na(trends$y), ]

  if (covariates != "none") {
    ylab <- "Average Outcome (Residualized)"
  } else {
    ylab <- "Average Outcome"
  }

  if (weights == "none") {
    weights <- FALSE
  } else {
    weights <- TRUE
  }

  if (event) {
    # Event study plot
    treated <- trends[!is.na(trends$time_since_treatment), ]

    # Apply event window if specified
    if (!is.null(event_window)) {
      if (length(event_window) != 2) {
        stop("`event_window` must be a numeric vector of length 2")
      }
      treated <- treated[treated$time_since_treatment >= event_window[1] &
                           treated$time_since_treatment <= event_window[2], ]
      if (nrow(treated) == 0) stop("No obs. after applying 'event_window'.")
    }

    # Check if weights should be used
    if (all(is.na(trends$n)) && weights) {
      warning("'n' is missing. Setting 'weights = FALSE'.")
      weights <- FALSE
    }

    if (weights) {
      treated <- treated[!is.na(treated$n), ]
      if (nrow(treated) == 0) stop("No obs. after dropping missing 'n' values.")
    }

    # Calculate weighted means and standard errors by time since treatment
    event_times <- sort(unique(treated$time_since_treatment))
    event_data <- data.frame(
      time_since_treatment = event_times,
      y = NA,
      se = NA,
      ci_lower = NA,
      ci_upper = NA
    )
    for (i in seq_along(event_times)) {
      et <- event_times[i]
      et_data <- treated[treated$time_since_treatment == et, ]

      if (weights) {
        total_n <- sum(et_data$n)
        w <- et_data$n / total_n
      } else {
        w <- NULL
      }
      x_mat <- as.matrix(rep(1, nrow(et_data)))
      y <- as.vector(et_data$y)
      reg <- .regress(x_mat, y, w = w, hc = hc) #nolint
      event_data$y[i] <- reg$beta_hat[1]
      event_data$se[i] <- reg$beta_hat_se[1]
      df <- nrow(et_data) - 1
      if (df > 0) {
        t_crit <- qt(1 - (1 - ci) / 2, df)
      } else {
        t_crit <- NA
      }
      event_data$ci_lower[i] <- reg$beta_hat[1] - t_crit * reg$beta_hat_se[1]
      event_data$ci_upper[i] <- reg$beta_hat[1] + t_crit * reg$beta_hat_se[1]
    }

    # Create plot
    y_range <- range(c(event_data$y, event_data$ci_lower, event_data$ci_upper),
                     na.rm = TRUE)

    plot(event_data$time_since_treatment, event_data$y,
         type = "n",
         xlab = "Periods Since Treatment",
         ylab = ylab,
         ylim = y_range,
         ...)

    # Add confidence intervals if available
    for (i in seq_len(nrow(event_data))) {
      if (!is.na(event_data$ci_lower[i]) && !is.na(event_data$ci_upper[i])) {
        x_pos <- event_data$time_since_treatment[i]

        # Vertical line (the main CI bar)
        segments(x_pos, event_data$ci_lower[i],
                 x_pos, event_data$ci_upper[i],
                 col = "skyblue", lwd = lwd / 1.2)

        # Horizontal caps
        cap_width <- 0.1

        # Bottom cap
        segments(x_pos - cap_width, event_data$ci_lower[i],
                 x_pos + cap_width, event_data$ci_lower[i],
                 col = "skyblue", lwd = lwd / 1.2)

        # Top cap
        segments(x_pos - cap_width, event_data$ci_upper[i],
                 x_pos + cap_width, event_data$ci_upper[i],
                 col = "skyblue", lwd = lwd / 1.2)
      }
    }

    # Add points and line
    lines(event_data$time_since_treatment, event_data$y,
          type = "b", pch = 16, lwd = lwd)

    # Add vertical line at treatment time
    abline(v = 0, lty = 2, col = adjustcolor("red", alpha.f = 0.5),
           lwd = lwd / 1.2)

  } else {
    # Parallel trends plot
    silos <- unique(trends$silo_name)
    n_silos <- length(silos)
    colors <- c(
      "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
      "#D55E00", "#CC79A7", "#999999"
    )
    if (n_silos > 8) {
      colors <- colorRampPalette(colors)(n_silos)
    } else {
      colors <- colors[1:n_silos]
    }

    # Get unique period-label pairs for x-axis
    period_labels <- unique(trends[, c("period", "time_label")])
    period_labels <- period_labels[order(period_labels$period), ]

    # Initialize plot (suppress x-axis)
    plot(NA,
         xlim = range(trends$period),
         ylim = range(trends$y, na.rm = TRUE),
         xlab = "Time Period",
         ylab = ylab,
         xaxt = "n",  # Suppress automatic x-axis
         ...)

    # Add custom x-axis with time labels
    axis(1, at = period_labels$period, labels = period_labels$time_label)

    # Plot each silo
    for (i in seq_along(silos)) {
      silo_data <- trends[trends$silo_name == silos[i], ]
      silo_data <- silo_data[order(silo_data$period), ]

      # Determine line type (solid for treated, dashed for control)
      lty <- if (silo_data$treatment_time[1] == "control") 2 else 1

      lines(silo_data$period, silo_data$y,
            col = colors[i], lwd = lwd, lty = lty)
    }

    # Add vertical lines for treatment times
    treatment_times <- unique(
      trends$treatment_time[trends$treatment_time != "control"]
    )
    if (length(treatment_times) > 0) {
      for (tt in treatment_times) {
        tt_date <- .parse_string_to_date(tt, trends$date_format[1])
        tt_period <- match(as.Date(tt_date, origin = "1970-01-01"),
                           sort(unique(trends$time)))
        if (!is.na(tt_period)) {
          abline(v = tt_period, lty = 3, col = "red", lwd = lwd / 1.2)
        }
      }
    }

    # Add legend
    if (!is.null(legend)) {
      legend(legend,
             legend = silos,
             col = colors,
             lwd = lwd,
             lty = ifelse(sapply(silos, function(s) {
               trends$treatment_time[trends$silo_name == s][1] == "control"
             }), 2, 1),
             cex = 0.7)
    }
  }
}

#' @keywords internal
#' Return a combined dataframe of all filled_diff_df csv files from a folder
.combine_diff_data <- function(dir_path, covariates = FALSE) {

  files <- list.files(dir_path,
                      pattern = "^filled_diff_df_.*\\.csv$", full.names = TRUE)
  if (length(files) == 0) stop(paste("No filled diff df CSV files found in:",
                                     dir_path))
  diff_df <- do.call(rbind, lapply(files, .read_diff_df))

  return(diff_df)
}

#' @keywords internal
#' Runs stage three calculations
.stage_three_compute <- function(diff_df, nperm, verbose, hc, agg,
                                 iterable, results, weights, randomize,
                                 max_attempts, notyet) {

  # Set up results data frame
  if (agg != "none") {
    results <- cbind(
      results,
      data.frame(
        sub_att = NA_real_,
        sub_se = NA_real_, sub_pval = NA_real_,
        sub_jknife_se = NA_real_, sub_jknife_pval = NA_real_,
        sub_ri_pval = NA_real_,
        agg_att = NA_real_, agg_att_se = NA_real_,
        agg_att_pval = NA_real_, agg_att_jknife_se = NA_real_,
        agg_att_jknife_pval = NA_real_,
        agg_att_ri_pval = NA_real_, weights = NA_real_,
        nperm = NA_real_
      )
    )
  } else if (agg == "none") {
    results <- data.frame(agg_att = NA_real_, agg_att_se = NA_real_,
                          agg_att_pval = NA_real_, agg_att_jknife_se = NA_real_,
                          agg_att_jknife_pval = NA_real_,
                          agg_att_ri_pval = NA_real_, 
                          nperm = NA_real_)
    mask <- diff_df$treat != -1
    subset <- diff_df[mask, ]
    x <- cbind(1, subset$treat)
    y <- subset$y
    w <- NULL
    if (weights %in% c("diff", "both")) {
      w <- subset$n
      w <- w / sum(w, na.rm = TRUE)
    }
    reg <- .regress(x, y, w = w, hc = hc)  # nolint: object_usage_linter
    results$agg_att[1] <- reg$beta_hat
    results$agg_att_se[1] <- reg$beta_hat_se
    results$agg_att_pval[1] <- reg$beta_hat_pval

    # Now do randomization inference and jackknife procedure
    results <- .jackknife_procedure(results, diff_df, agg, weights, iterable)
    if (randomize) {
      results <- .compute_ri_pval(results, diff_df, nperm, agg, weights,
                                  max_attempts, notyet, iterable, verbose)
    }

    return(results)
  }

  # Loop through treated silos and compute results
  for (i in seq_along(iterable)) {
    # Create the subset
    subgroup <- iterable[i]
    mask <- .stage_three_mask(agg, diff_df, subgroup)
    subset <- diff_df[mask, ]
    x <- cbind(1, subset$treat)
    y <- subset$y

    # Add dummies for time since treatment aggregation
    if (agg == "time") {
      for (gvar in unique(subset$gvar)) {
        x <- cbind(x, as.numeric(subset$gvar == gvar))
      }
      rank <- qr(x)$rank
      ncolx <- ncol(x)
      while ((rank < ncolx) && (ncolx >= 4)) {
        x <- x[, c(1, 2, seq(ncolx, 4, -1))]
        ncolx <- ncol(x)
        rank <- qr(x)$rank
      }
      if (rank < ncolx) {
        x <- x[, c(1, 2)]
      }
    }

    # Record weights
    w <- NULL
    if (weights %in% c("diff", "both")) {
      w <- subset$n
      w <- w / sum(w, na.rm = TRUE)
    }
    if (weights %in% c("att", "both")) {
      results$weights[i] <- sum(subset[subset$treat == 1, ]$n_t)
    }

    # Run regression, store results
    reg <- .regress(x, y, w = w, hc = hc)  # nolint: object_usage_linter
    results$sub_att[i] <- reg$beta_hat
    results$sub_se[i] <- reg$beta_hat_se
    results$sub_pval[i] <- reg$beta_hat_pval

  }

  # Decide whether to use weights for aggregate ATT
  w <- NULL
  if (weights %in% c("att", "both")) {
    w <- results$weights
    w <- w / sum(w, na.rm = TRUE)
    results$weights <- w
  }

  # Run aggregate regression
  n <- nrow(results)
  reg <- .regress(rep(1, n), results$sub_att, w = w, hc = hc)  # nolint: object_usage_linter
  results$agg_att[1] <- reg$beta_hat
  results$agg_att_se[1] <- reg$beta_hat_se
  results$agg_att_pval[1] <- reg$beta_hat_pval

  # Now do randomization inference and jackknife procedure
  results <- .jackknife_procedure(results, diff_df, agg, weights, iterable)
  if (randomize) {
    results <- .compute_ri_pval(results, diff_df, nperm, agg, weights,
                                max_attempts, notyet, iterable, verbose)
  }

  # Convert gvar to string for display purposes
  if (agg == "g") {
    results$gvar <- .parse_date_to_string(
      as.Date(results$gvar, origin = "1970-01-01"),
      diff_df$date_format[1]
    )
  }

  return(results)

}

#' @keywords internal
#' Procedure for computing randomization inference p-value
#' Takes in the combined_diff data, the method of aggregation,
#' the number of randomizations (nperm), the agg_att to compare to the
#' ri_att's, and an option to print progress of computing the nperm iterations
.compute_ri_pval <- function(results, diff_df, nperm, agg, weights,
                             max_attempts, notyet, iterable, verbose) {

  # PART ONE: CREATE RANDOMIZED TREATMENT COLUMNS
  original_treated <- unique(
    diff_df[diff_df$treat == 1, c("silo_name", "gvar")]
  )
  k <- nrow(original_treated)
  treatment_times <- original_treated$gvar
  treatment_states <- original_treated$silo_name
  all_states <- unique(diff_df$silo_name)

  # Compute n of unique assignments (excluding the actual assignment)
  ln_numer <- lfactorial(length(all_states))
  ln_denom <- lfactorial(length(all_states) - length(treatment_times))
  for (g in unique(treatment_times)) {
    ln_denom <- ln_denom + lfactorial(sum(treatment_times == g))
  }
  n_unique_assignments <- (round(exp(ln_numer - ln_denom))) - 1
  if (nperm > n_unique_assignments) {
    warning(paste("'nperm' was set to", nperm, "but only",
                  n_unique_assignments, "exist.\nSetting nperm = ",
                  n_unique_assignments))
    nperm <- n_unique_assignments
  }
  if (nperm < 399) {
    warning("'nperm' is less than 399.")
  }

  # Create hash for storing permutations and loop through i:nperm creating
  # new columns of randomized treatment for the silos
  i <- 1
  seen <- new.env(hash = TRUE, size = nperm + 1, parent = emptyenv())
  key <- paste(sort(paste(treatment_states, treatment_times,
                          sep = "-")), collapse = "|")
  seen[[key]] <- TRUE
  attempts <- 1
  while (i <= nperm && attempts < max_attempts) {
    attempts <- 1

    new_treated_states <- sample(all_states)[1:k]
    new_treated_times <- treatment_times[sample(k)]
    key <- paste(sort(paste(new_treated_states, new_treated_times,
                            sep = "-")), collapse = "|")

    if (!is.null(seen[[key]])) {
      attempts <- attempts + 1
      next
    } else {
      seen[[key]] <- TRUE
    }

    new_treat <- rep(NA_real_, nrow(diff_df))
    for (row_idx in seq_len(nrow(diff_df))) {
      state <- diff_df$silo_name[row_idx]
      trt_time <- diff_df$gvar[row_idx]
      if (state %in% new_treated_states) {
        if (trt_time == new_treated_times[new_treated_states == state]) {
          new_treat[row_idx] <- 1
        } else {
          if (notyet) {
            t <- diff_df$t[row_idx]
            if (t < new_treated_times[new_treated_states == state]) {
              new_treat[row_idx] <- 0
            } else {
              new_treat[row_idx] <- -1
            }
          } else {
            new_treat[row_idx] <- -1
          }
        }
      } else {
        new_treat[row_idx] <- 0
      }
    }

    diff_df[[paste0("random_treat_", as.character(i))]] <- new_treat
    i <- i + 1
  }
  nperm <- i - 1

  # PART TWO: COMPUTE RI_ATT & RI_ATT_SUBGROUP
  att_ri <- rep(NA_real_, nperm)
  att_ri_sub <- matrix(NA_real_, nrow = nperm, ncol = length(iterable))
  for (j in seq_len(nperm)) {
    treat_col <- paste0("random_treat_", as.character(j))
    # Edge case when agg is none
    if (agg == "none") {
      subset <- diff_df[diff_df[[treat_col]] != -1, ]
      x <- as.vector(subset[[treat_col]])
      y <- as.vector(subset$y)
      if (weights %in% c("diff", "both") && (agg != "time")) {
        w <- subset$n
        w <- w / sum(w, na.rm = TRUE)
        att_ri[j] <- (sum(w[x == 1] * y[x == 1]) / sum(w[x == 1])) -
          (sum(w[x == 0] * y[x == 0]) / sum(w[x == 0]))
      } else if (weights %in% c("att", "none") && (agg != "time")) {
        att_ri[j] <- mean(y[x == 1]) - mean(y[x == 0])
      }
    } else {
      # Iterate thru the same iterable given for original computations
      w_agg <- rep(NA_real_, length(iterable))
      for (i in seq_along(iterable)) {
        subgroup <- iterable[i]
        mask <- .stage_three_mask(agg, diff_df, subgroup,
                                  treat_col = treat_col, ri = TRUE)
        subset <- diff_df[mask, ]
        x <- as.vector(subset[[treat_col]])
        y <- as.vector(subset$y)

        # Handle the edge case of time agg
        if (agg == "time") {
          x <- cbind(1, x)
          x <- as.matrix(x)
          for (gvar in unique(subset$gvar)) {
            x <- cbind(x, as.numeric(subset$gvar == gvar))
          }
          rank <- qr(x)$rank
          ncolx <- ncol(x)
          while ((rank < ncolx) && (ncolx >= 4)) {
            x <- x[, c(1, 2, seq(ncolx, 4, -1))]
            ncolx <- ncol(x)
            rank <- qr(x)$rank
          }
          if (rank < ncolx) {
            x <- x[, c(1, 2)]
          }
          # Record weights
          w <- NULL
          if (weights %in% c("diff", "both")) {
            w <- subset$n
            w <- w / sum(w, na.rm = TRUE)
            sw <- sqrt(w)
            x <- x * sw
            y <- y * sw
          }
          att_ri_sub[j, i] <- .safe_solve(x, y)[2]
        }

        # Make computation for cases when agg is not time
        if (weights %in% c("diff", "both") && (agg != "time")) {
          w <- subset$n
          w <- w / sum(w, na.rm = TRUE)
          att_ri_sub[j, i] <- (sum(w[x == 1] * y[x == 1]) / sum(w[x == 1])) -
            (sum(w[x == 0] * y[x == 0]) / sum(w[x == 0]))
        } else if (weights %in% c("att", "none") && (agg != "time")) {
          att_ri_sub[j, i] <- mean(y[x == 1]) - mean(y[x == 0])
        }


        if (weights %in% c("att", "both")) {
          w_agg[i] <- sum(
            subset[subset[[treat_col]] == 1, ]$n_t
          )
        }
      }

      # Compute aggregate ATT
      if (weights %in% c("att", "both")) {
        w_agg <- w_agg / sum(w_agg)
      } else if (weights %in% c("none", "diff")) {
        w_agg <- rep(1 / length(w_agg), length(w_agg))
      }
      att_ri[j] <- sum(w_agg * att_ri_sub[j, ])
    }
    if (!is.null(verbose) && j %% verbose == 0) {
      message(sprintf("Completed %d of %d permutations", j, nperm))
    }
  }

  # PART THREE: COMPUTE P-VALS AND ASSIGN TO RESULTS
  if (agg != "none") {
    for (i in seq_along(iterable)) {
      att <- results[i, "sub_att"]
      results[i, "sub_ri_pval"] <- (sum(abs(att_ri_sub[, i]) > abs(att))) /
        nperm
    }
  }
  results[1, "agg_att_ri_pval"] <- (sum(abs(att_ri) >
                                          abs(results[1, "agg_att"]))) / nperm
  results[1, "nperm"] <- nperm

  return(results)

}

#' @keywords internal
#' Create the mask for computing subaggregate ATTs
.stage_three_mask <- function(agg, diff_df, subgroup, treat_col = "treat",
                              ri = FALSE) {

  if (agg == "silo") {
    if (isFALSE(ri)) {
      treated_mask <- diff_df$silo_name == subgroup & diff_df[[treat_col]] == 1
      gvar_treated <- diff_df[treated_mask, "gvar"][1]
      control_mask <- diff_df[[treat_col]] == 0 & diff_df$gvar == gvar_treated
    } else {
      gvar <- diff_df[diff_df$silo_name == subgroup &
                        diff_df$treat == 1, ]$gvar[1]
      new_treated_silo <- sample(
        unique(
          diff_df[diff_df$gvar == gvar & diff_df[[treat_col]] == 1, ]$silo_name
        )
      )[1]
      treated_mask <- diff_df$silo_name == new_treated_silo &
        diff_df[[treat_col]] == 1
      control_mask <- diff_df[[treat_col]] == 0 & diff_df$gvar == gvar
    }
    mask <- treated_mask | control_mask
  } else if (agg == "g") {
    mask <- diff_df[[treat_col]] != -1 & diff_df$gvar == subgroup
  } else if (agg == "gt") {
    mask <- diff_df[[treat_col]] != -1 & diff_df$gt == subgroup
  } else if (agg == "sgt") {
    gvar <- unique(diff_df[diff_df$sgt == subgroup, ]$gvar)[1]
    time <- unique(diff_df[diff_df$sgt == subgroup, ]$t)[1]
    if (isFALSE(ri)) {
      silo <- unique(diff_df[diff_df$sgt == subgroup, ]$silo_name)[1]
    } else {
      silo <- sample(
        unique(diff_df[diff_df$gvar == gvar & diff_df$t == time &
                         diff_df[[treat_col]] == 1, ]$silo_name)
      )[1]
    }
    mask <- diff_df[[treat_col]] != -1 &
      diff_df$gvar == gvar & diff_df$t == time &
      (diff_df$silo_name == silo | diff_df[[treat_col]] == 0)
  } else if (agg == "time") {
    mask <- diff_df$time_since_treatment == subgroup &
      diff_df[[treat_col]] != -1
  } else if (agg == "none") {
    mask <- diff_df[[treat_col]] != -1
  }
  mask <- mask & diff_df[[treat_col]] != -1
  return(mask)
}

#' @keywords internal
#' Procedure for computing jackknfie SEs and pvals
.jackknife_procedure <- function(results, diff_df, agg, weights, iterable) {

  all_states <- unique(diff_df[diff_df$treat != -1, ]$silo_name)
  if (agg == "none") {
    iterable <- "none"
  }

  # Make the jackdf preallocation dataframe
  without <- rep(all_states, each = length(iterable))
  sub_group <- rep(iterable, times = length(all_states))
  jackdf <- data.frame(without = without, sub_group = sub_group,
                       att = NA_real_, weights = NA_real_)

  # Cycle through the withouts and sub_groups and compute coefficients
  idx_jack <- 1
  for (without in all_states) {
    without_mask <- diff_df$silo_name != without & diff_df$treat != -1
    for (sg in iterable) {
      mask <- .stage_three_mask(agg, diff_df, sg) & without_mask
      temp <- diff_df[mask, ]
      x <- as.vector(temp$treat)
      y <- as.vector(temp$y)

      # Record att as NA if temp is blank or no variation in treatment
      if (nrow(temp) == 0 || length(unique(temp$treat)) < 2) {
        att <- NA_real_

        # Handle the time edge case
      } else if (agg == "time") {
        x <- cbind(1, x)
        x <- as.matrix(x)
        for (gvar in unique(temp$gvar)) {
          x <- cbind(x, as.numeric(temp$gvar == gvar))
        }
        rank <- qr(x)$rank
        ncolx <- ncol(x)
        while ((rank < ncolx) && (ncolx >= 4)) {
          x <- x[, c(1, 2, seq(ncolx, 4, -1))]
          ncolx <- ncol(x)
          rank <- qr(x)$rank
        }
        if (rank < ncolx) {
          x <- x[, c(1, 2)]
        }
        # Record weights
        if (weights %in% c("diff", "both")) {
          w <- temp$n
          w <- w / sum(w, na.rm = TRUE)
          sw <- sqrt(w)
          x <- x * sw
          y <- y * sw
        }
        att <- .safe_solve(x, y)[2]

        # Do computation using weighted dot product otherwise
      } else {
        if (weights %in% c("diff", "both")) {
          w <- temp$n
          w <- w / sum(w, na.rm = TRUE)
          att <- (sum(w[x == 1] * y[x == 1]) / sum(w[x == 1])) -
            (sum(w[x == 0] * y[x == 0]) / sum(w[x == 0]))
        } else if (weights %in% c("att", "none")) {
          att <- mean(y[x == 1]) - mean(y[x == 0])
        }
      }

      # Record weights
      if (weights %in% c("att", "both")) {
        if (nrow(temp) == 0 || length(unique(temp$treat)) < 2) {
          w <- 0
        } else {
          w <- sum(temp[temp$treat == 1, ]$n_t)
        }
      } else {
        w <- NA_real_
      }

      # Assign values to jackdf
      jackdf$att[idx_jack] <- att
      jackdf$weights[idx_jack] <- w
      idx_jack <- idx_jack + 1
    }
  }

  # Use jackknife computations to compute aggregate standard errors and pvals
  agg_jacks <- rep(NA_real_, length(all_states))
  i <- 1
  for (without in all_states) {
    mask <- jackdf$without == without & !is.na(jackdf$att)
    temp <- jackdf[mask, ]
    if (nrow(temp) == 0) {
      agg_jacks[i] <- NA_real_
    } else {
      if (weights %in% c("att", "both")) {
        agg_jacks[i] <- sum((temp$weights / sum(temp$weights)) * temp$att)
      } else if (weights %in% c("none", "diff")) {
        agg_jacks[i] <- mean(temp$att)
      }
    }
    i <- i + 1
  }
  agg_jacks <- agg_jacks[!is.na(agg_jacks)]
  n <- length(agg_jacks)
  if (n >= 2) {
    jknife_se <- sqrt(sum((agg_jacks - results$agg_att[1])^2) * ((n - 1) / n))
  } else {
    jknife_se <- NA_real_
  }
  dof_jack <- n - 1
  if (dof_jack > 0 && !is.na(jknife_se)) {
    pval_jknife <- 2 * (1 - pt(abs(results$agg_att[1]/ jknife_se), dof_jack))
  } else {
    pval_jknife <- NA_real_
  }
  results$agg_att_jknife_pval[1] <- pval_jknife
  results$agg_att_jknife_se[1] <- jknife_se

  # Use jackknife computations to compute subaggregate standard errors & pvals
  if (agg %in% c("sgt", "silo")) {
    results$sub_jknife_pval <- NA_real_
    results$sub_jknife_se <- NA_real_
  } else if (agg %in% c("g", "gt", "time")) {
    sg_counter <- 1
    for (sg in iterable) {
      sub_jacks <- jackdf[jackdf$sub_group == sg, ]$att
      n_sub <- length(sub_jacks)
      has_missing <- any(is.na(sub_jacks))
      sg_att <- results$sub_att[sg_counter]
      if (has_missing) {
        dof_jack_sub <- 0
      } else {
        dof_jack_sub <- n_sub - 1
      }
      if (n_sub >= 2 && !has_missing) {
        jknife_se_sub <- sqrt(
          sum((sub_jacks - sg_att)^2) * ((n_sub - 1) / n_sub)
        )
      } else {
        jknife_se_sub <- NA_real_
      }
      if (dof_jack_sub > 0 && !is.na(jknife_se_sub)) {
        pval_att_jknife_sub <- 2 *
          (1 - pt(abs(sg_att / jknife_se_sub), dof_jack_sub))
      } else {
        pval_att_jknife_sub <- NA_real_
      }

      results$sub_jknife_pval[sg_counter] <- pval_att_jknife_sub
      results$sub_jknife_se[sg_counter] <- jknife_se_sub

      sg_counter <- sg_counter + 1
    }
  }

  return(results)

}


#' @keywords internal
# Combines trends data
.combine_trends_data <- function(dir_path, covariates) {

  files <- list.files(dir_path,
                      pattern = "^trends_data_.*\\.csv$", full.names = TRUE)
  if (length(files) == 0) return(NULL)

  trends <- do.call(rbind, lapply(files, read.csv))
  trends$silo_name <- as.character(trends$silo_name)
  trends$treatment_time <- as.character(trends$treatment_time)
  trends$time_label <- as.character(trends$time)
  trends$time <- mapply(.parse_string_to_date,
                        as.character(trends$time),
                        trends$date_format)
  trends$time <- as.Date(trends$time, origin = "1970-01-01")


  if (identical(covariates, TRUE)) {
    trends$y <- trends$mean_outcome_residualized
  } else if (identical(covariates, FALSE)) {
    trends$y <- trends$mean_outcome
  } else {
    stop("`covariates` must be set to either `TRUE` or `FALSE`")
  }

  # Do conversion from Julia missing to R NA if necessary
  numeric_cols <- c("y", "n")
  for (col_name in numeric_cols) {
    trends[[col_name]][trends[[col_name]] %in% c("missing", "NA")] <- NA
    trends[[col_name]] <- as.numeric(trends[[col_name]])
  }

  # Convert time to period index
  ordered_t <- sort(unique(trends$time))
  t_period <- match(trends$time, ordered_t)
  trends$period <- t_period
  trends$time_since_treatment <- sapply(seq_len(nrow(trends)), function(i) {
    if (trends$treatment_time[i] == "control") {
      return(NA)
    }

    treatment_date <- .parse_string_to_date(
      trends$treatment_time[i],
      trends$date_format[i]
    )

    return(
      trends$period[i] - match(
        as.Date(treatment_date, origin = "1970-01-01"),
        ordered_t
      )
    )
  })

  return(trends)

}