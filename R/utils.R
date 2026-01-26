#' @keywords internal
#' Attempts to to regress y on x
.safe_solve <- function(x, y) {
  tryCatch({
    return(solve(t(x) %*% x) %*% t(x) %*% y)
  }, error = function(e) {
    return(NULL)
  })
}

#' @keywords internal
#' Compute beta coefficients and their SE's
.regress <- function(x, y, w = NULL, hc = "hc0") {

  # Make sure x is a matrix and y is a vector
  x <- as.matrix(x)
  y <- as.vector(y)

  # Check for and remove missing values
  x_valid <- complete.cases(x)
  y_valid <- !is.na(y)
  if (is.null(w)) {
    valid_mask <- x_valid & y_valid
  } else {
    w_valid <- !is.na(w)
    valid_mask <- x_valid & y_valid & w_valid
  }
  x <- as.matrix(x[valid_mask, ])
  y <- as.vector(y[valid_mask])

  # Make sure we have enough observations left
  ncolx <- ncol(x)
  n <- length(y)
  if (n < ncolx) {
    return(list(beta_hat = NA,
                beta_hat_se = NA,
                beta_hat_pval = NA))
  }

  # Computations without weights
  if (is.null(w)) {
    beta_hat <- .safe_solve(x, y)
    if (is.null(beta_hat)) {
      return(list(beta_hat = NA,
                  beta_hat_se = NA,
                  beta_hat_pval = NA))
    }
    if (n > ncolx) {
      resid <- y - (x %*% beta_hat)
      beta_hat_cov <- .compute_hc_covariance(x, resid, hc)
    }

    # Computations with weights
  } else {
    w <- w / sum(w)
    sw <- sqrt(w)
    xw <- x * sw
    yw <- y * sw
    beta_hat <- .safe_solve(xw, yw)
    if (is.null(beta_hat)) {
      return(list(beta_hat = NA,
                  beta_hat_se = NA,
                  beta_hat_pval = NA))
    }
    if (n > ncolx) {
      resid_w <- yw - (xw %*% beta_hat)
      beta_hat_cov <- .compute_hc_covariance(xw, resid_w, hc)
    }
  }

  # When the the beta_hat_cov was calculated, take the 2nd value
  # (treatment indicator) unless the ncols was 1 (for the aggregation
  # of all the subaggregate ATTs). Also extract the beta_hat value
  if (ncolx == 1) {
    extract <- 1
  } else {
    extract <- 2
  }
  beta_hat <- beta_hat[extract]
  if ((n > ncolx) && (!is.null(beta_hat_cov))) {
    beta_hat_se <- sqrt(diag(beta_hat_cov)[extract])
  } else {
    beta_hat_se <- NA
  }

  # Get pval if possible
  dof <- n - ncolx
  if (dof > 0 && !is.na(beta_hat_se)) {
    pval <- 2 * (1 - pt(abs(beta_hat / beta_hat_se), dof))
  } else {
    pval <- NA
  }

  return(list(beta_hat = beta_hat,
              beta_hat_se = beta_hat_se,
              beta_hat_pval = pval))
}


#' @keywords internal
#' Returns a HCCME of the specified type
.compute_hc_covariance <- function(x, resid, hc) {

  x <- as.matrix(x)
  resid <- as.vector(resid)

  n <- nrow(x)
  k <- ncol(x)
  XXinv <- tryCatch({
    solve(t(x) %*% x)
  }, error = function(e) {
    return(NULL)
  })

  if (is.null(XXinv)) {
    return(NULL)
  }

  # Compute hat matrix diagonal if needed for HC2/HC3/HC4
  if (hc %in% c("hc2", "hc3", "hc4")) {
    h <- sapply(1:n, function(i) {
      t(x[i, ]) %*% XXinv %*% x[i, ]
    })
  }

  # Construct omega based on HC type
  if (hc == "hc0") {
    omega_diag <- resid^2
  } else if (hc == "hc1") {
    omega_diag <- (n / (n - k)) * (resid^2)
  } else if (hc == "hc2") {
    omega_diag <- (resid^2) / (1 - h)
  } else if (hc == "hc3") {
    omega_diag <- (resid^2) / ((1 - h)^2)
  } else if (hc == "hc4") {
    h_bar <- mean(h)
    delta <- pmin(4, h / h_bar)
    omega_diag <- (resid^2) / ((1 - h)^delta)
  }

  omega <- diag(omega_diag)

  # Sandwich estimator
  return(XXinv %*% (t(x) %*% omega %*% x) %*% XXinv)

}

#' @keywords internal
#' Checks that input is given as TRUE or FALSE
.validate_logical_args <- function(logical, arg_name) {
  if (!(identical(logical, TRUE) || identical(logical, FALSE))) {
    stop(paste(arg_name, "must be a single `TRUE` or `FALSE` value."))
  }
}

#' @keywords internal
#' Checks that matrix is full rank, useful for regression
.ensure_full_rank <- function(x, protect = TRUE) {
  if (protect) {
    # Protect first 2 columns (intercept and treatment)
    if (ncol(x) <= 2) {
      return(x)
    }
    protected <- x[, 1:2, drop = FALSE]
    covariates <- x[, 3:ncol(x), drop = FALSE]

    # Iteratively drop columns from covariates until full rank
    while (qr(covariates)$rank < ncol(covariates) && ncol(covariates) > 0) {
      qr_decomp <- qr(covariates)
      # Drop the last dependent column
      drop_col <- qr_decomp$pivot[ncol(covariates)]
      covariates <- covariates[, -drop_col, drop = FALSE]
    }
    return(cbind(protected, covariates))

  } else {
    # No protection - drop from all columns
    while (qr(x)$rank < ncol(x) && ncol(x) > 0) {
      qr_decomp <- qr(x)
      # Drop the last dependent column
      drop_col <- qr_decomp$pivot[ncol(x)]
      x <- x[, -drop_col, drop = FALSE]
    }
    return(x)
  }
}