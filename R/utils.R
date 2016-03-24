#' Number of Cells at Time t
#'
#' This function gives the number of cells or absorbance (N) at time t when
#' the parameters to the logistic equation are K, N0, and r.
#' @param k       The carrying capacity
#' @param n0      The initial population size (absorbance or individuals)
#' @param r       The exponential "growth rate"
#' @param t       The time at which you want to know N
#' @return        The number of cells, or N, at time t
#' @export
NAtT <- function(k, n0, r, t) {
  return( k / (1 + ((k - n0) / n0) * exp(-r * t)))
}


# Slope At Time t
#
# This function gives the growth rate (or slope of the curve) when
# the parameters to the logistic equation are K, N0, and r.
# @param k       The carrying capacity
# @param n0      The initial population size (absorbance or individuals)
# @param r       The exponential "growth rate"
# @param t       The time at which you want to know the growth rate
# @return        The number of cells, or N, at time t
SlopeAtT <- function(k, n0, r, t) {
  n <- NAtT(k, n0, r, t)
  return(r * n * (k - n) / k)
}

# Fastest Doubling Time
#
# This function gives you the maximum doubling time (DT) assuming exponential
# growth.
# @param r       The exponential "growth rate"
# @return        The maximum doubling time
MaxDt <- function(r) {
  return(log(2) / r)
}


# Doubling (Generation) Time at Time t
#
# This function gives you the doubling time (DT) at time t when the parameters
# of the logistic equation are K, N0, and r.
# @param k       A single integer specifying the carrying capacity
# @param n0      The initial population size
#                (in either absorbance or individuals)
# @param r       The exponential "growth rate"
# @param t       The time at which you want to know the doubling time
# @return        The doubling time at time t
DtAtT <- function(k, n0, r, t) {
  n_t <- NAtT(k, n0, r, t)
  n_halft <- 0.5 * n_t
  return(( 1 / r) * log((n_t * (k - n_halft)) / ((k - n_t) * n_halft)))
}


# Time at Inflection Point
#
# This function returns the time of the inflection point
# of the logistic equation with parameters K, N0, and r.
# @param k       A single integer specifying the carrying capacity
# @param n0      The initial population size
#                (in either absorbance or individuals)
# @param r       The exponential "growth rate"
# @return        The time of the inflection point, which occurrs when the
#                the population size N reaches half its maximum value, K
TAtInflection <- function(k, n0, r) {
  if (n0 == 0) {
    warning("Initial population size (n0) cannot be 0.")
    return(0)
  }
  t_inflection <- log(abs(k - n0) / n0) / r
  return(t_inflection)
}


# Area Under the Logistic Curve
#
# This function gives you the area under the curve from time t_min to t_max,
# when the parameters of the logistic equation are K, N0, and r. This value
# essentially combines the lag phase, growth rate, and carrying capacity
# into a single value.
# @param k       The carrying capacity
# @param n0      The initial population size
#                (in either absorbance or number of individuals)
# @param r       The exponential "growth rate"
# @param t_min   The time from which you want to know the area under the curve
#                (e.g., from time = t_min to t_max)
# @param t_max   The time to which you want to know the area under the curve
#                (e.g., from time = t_min to t_max)
# @return        The area under the curve for logistic equation with the
#                given parameters, for the specificed time range
AreaUnderCurve <- function(k, n0, r, t_min = 0, t_max) {
  auc_l <- stats::integrate(function(x) NAtT(k, n0, r, x), t_min, t_max)
  return(auc_l)
}



# Area Under the Empirical Curve
#
# This function returns the empirical "area under the curve". It uses the input
# data to do so (rather than using the logistic fit).
# @param data_t    A vector of timepoints (data_n must also
#                  be provided and be the same length).
# @param data_n    A vector of cell counts or absorbance readings.
# @param t_trim    Add up the area under the curve from the beginning to
#                  t_trim. Defaults to 0, which means don't trim.
# @return          The area under the curve
EmpiricalAreaUnderCurve <- function(data_t, data_n, t_trim = 0) {
  # make sure that both inputs are vectors
  if (!is.vector(data_t) | !is.vector(data_n)) {
    stop("Error: The input data (data_t and data_n) must be vectors.")
  }
  if (!is.numeric(data_t) | !is.numeric(data_n)) {
    stop("Error: The input data (data_t and data_n) must be numeric.")
  }
  if (t_trim > 0) {
    idx_to_keep <- data_t <= t_trim                # keep the early measurements
  }
  else {
    idx_to_keep < rep(TRUE, length(data_t))       # keep all measurements
  }

  auc_e <- caTools::trapz(data_t[idx_to_keep], data_n[idx_to_keep])
  return(auc_e)
}
