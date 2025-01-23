#' @keywords internal
#' Computes the inverse using `solve()` and if not invertible tries `.pinv()`
.inv <- function(x) {
  tryCatch({
    return(solve(x))
  }, error = function(e) {
    return(.pinv(x))
  })
}

#' @keywords internal
#' Compute Moore-Penrose pseudoinverse
.pinv <- function(x, tol = sqrt(.Machine$double.eps)) {
  sv <- svd(x)
  pos <- sv$d > max(tol * sv$d[1], 0)
  if (all(!pos)) return(array(0, dim = c(ncol(x), nrow(x))))
  sv$v[, pos, drop = FALSE] %*% (1 / sv$d[pos] * t(sv$u[, pos, drop = FALSE]))
}

#' @keywords internal
#' Compute beta coefficients and their SE's
.regress <- function(x, y, w = NULL) {
  if (is.null(w)) {
    beta_hat <- .inv(t(x) %*% x) %*% t(x) %*% y
  } else {
    beta_hat <- .inv(t(x) %*% w %*% x) %*% t(x) %*% w %*% y
  }

  resid <- y - x %*% beta_hat
  sigma_sq <- resid^2
  omega <- diag(as.vector(sigma_sq))
  beta_hat_cov <- .inv(t(x) %*% x) %*% t(x) %*%
    omega %*% x %*% .inv(t(x) %*% x)
  beta_hat_var <- diag(beta_hat_cov)
  return(list(beta_hat = as.vector(beta_hat),
              beta_hat_var = as.vector(beta_hat_var)))
}


#' @keywords internal
#' Searches vector for missing/non-missing values
#' and returns imputed values using a linear function
.extrapolate_linear <- function(y) {
  n <- length(y)
  x_full <- as.matrix(cbind(rep(1, n), seq(1, n)))
  x_subset <- x_full[!is.na(y), ]
  y_subset <- y[!is.na(y)]
  reg <- .regress(x_subset, y_subset)
  y_hat <- x_full %*% reg$beta_hat

  return(y_hat[is.na(y)])
}

#' @keywords internal
#' Searches vector for missing/non-missing values
#' and returns imputed values by fetching nearest values
.get_nearest_value <- function(y) {

  y_copy <- y
  is_na <- which(is.na(y))
  is_not_na <- which(!is.na(y))
  for (i in is_na) {
    abs_distance_to_value <- abs(is_not_na - i)
    min_dist <- min(abs_distance_to_value)
    duplicated_min <- abs_distance_to_value == min_dist
    if (sum(duplicated_min) > 1) {
      closest_indices <- is_not_na[duplicated_min]
      y_copy[i] <- mean(y[closest_indices])
    } else {
      closest_idx <- is_not_na[which.min(abs_distance_to_value)]
      y_copy[i] <- y[closest_idx]
    }
  }

  return(y_copy[is_na])
}

#' @keywords internal
#' Searches vector for missing/non-missing values
#' and returns imputed values using piecewise linear functions
.piecewise_linear_function  <- function(y) {

  n <- length(y)
  x <- seq(1, n)
  y_copy <- y
  is_na <- which(is.na(y))
  is_not_na <- which(!is.na(y))
  for (i in is_na) {
    rel_dist_to_value <- is_not_na - i
    if (all(rel_dist_to_value > 0) || all(rel_dist_to_value < 0)) {
      closest_idx <- is_not_na[which.min(abs(rel_dist_to_value))]
      y_copy[i] <- y[closest_idx]
    } else {
      pos_idx <- i + min(rel_dist_to_value[rel_dist_to_value > 0])
      neg_idx <- i + max(rel_dist_to_value[rel_dist_to_value < 0])
      x1 <- x[neg_idx]
      x2 <- x[pos_idx]
      y1 <- y[neg_idx]
      y2 <- y[pos_idx]
      y_copy[i] <- y1 + (x[i] - x1) * (y2 - y1) / (x2 - x1)
    }
  }
  return(y_copy[is_na])
}

#' @keywords internal
#' Compute the jackknife SE for beta hat given X and Y
.compute_jknife_se <- function(x, y, agg_att, w = NULL) {

  x <- as.matrix(x)
  n_col <- ncol(x)
  beta_hat_jknives <- rep(NA_real_, length(y))
  for (i in seq_along(y)) {
    x_sub <- x[-i, , drop = FALSE]
    y_sub <- y[-i]
    if (!is.null(w)) {
      w_sub <- w[-i, -i, drop = FALSE]
      w_sum <- sum(w_sub)
      w_sub <- w_sub / w_sum
      reg <- .regress(x_sub, y_sub, w_sub)
    } else {
      reg <- .regress(x_sub, y_sub)
    }
    beta_hat_jknives[i] <- reg$beta_hat[n_col]
  }
  jknife_se <- sqrt(
    sum((beta_hat_jknives - agg_att)^2) *
      ((length(beta_hat_jknives) - 1) / length(beta_hat_jknives))
  )
  return(jknife_se)
}

#' @keywords internal
#' Checks that input is given as TRUE or FALSE
.validate_logical_args <- function(logical, arg_name) {
  if (!(identical(logical, TRUE) || identical(logical, FALSE))) {
    stop(paste(arg_name, "must be a single `TRUE` or `FALSE` value."))
  }
}