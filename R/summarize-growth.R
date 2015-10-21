#' Summarize Growth Curves
#'
#' This function finds the parameters that describe the input data's growth.
#' It does so by fitting the logistic curve to your growth curve measurements.
#' The logistic curve equation is
#' \deqn{N_t = \frac{N_0 K} {N_0 + (K-N_0)e^{-rt}}} where N_t is the number
#' of cells (or the absorbance reading) at time t, \eqn{N_0} is the initial
#' cell count (or absorbance reading), K is the carrying capacity, and r is the
#' growth rate.
#'
#' The fitness proxies returned are the parameters of the logistic equation
#' and the area under the curve (a measure that integrates the effects
#' of N_0, K, and r). See \code{\link{gcfit}} for more documentation on these.
#' @param data_t    A vector of timepoints (data_n must also
#'                  be provided and be the same length).
#' @param data_n    A vector of cell counts or absorbance readings.
#' @param t_trim    Measurements taken after this time should not be included
#'                  in fitting the curve. If stationary phase is variable,
#'                  this may give you a better fit. A value of 0 means no
#'                  trimming. Defaults to no trimming (0).
#' @return          An object of type gcfit containing the "fitness" proxies,
#'                  as well as the input data and the fitted model.
#'                  See \code{\link{gcfit}}
#' @examples
#' # We can check that the parameters that are found are the same
#' # as we use to generate fake experimental data. To do so, let's first
#' # generate the "experimental" data using the logistic equation,
#' # e.g., absorbance readings from a single well in a plate reader over time.
#'
#' k_in <- 0.5   # the initial carrying capacity
#' n0_in <- 1e-5 # the initial absorbance reading
#' r_in <- 1.2   # the initial growth rate
#' N <- 50       # the number of "measurements" collected during the growth
#'               # curve experiment
#'
#' data_t <- 0:N * 24 / N   # the times the measurements were made (in hours)
#' data_n <- NAtT(k = k_in, n0 = n0_in, r = r_in, t = data_t) # the measurements
#'
#' # Now summarize the "experimental" growth data that we just generated
#' gc <- SummarizeGrowth(data_t, data_n)
#'
#' # Get the possible metrics for fitness proxies
#' gc$vals$r           # growth rate is a common choice for fitness
#' gc$vals$t_gen       # doubling time, or generation time, is also common
#' gc$vals$k
#' gc$vals$n0
#' gc$vals$auc
#' gc$vals$quant_auc
#' gc$vals$t_mid
#'
#' # Compare the data with the fit visually by plotting it
#' plot(gc)
#'
#' @export
SummarizeGrowth <- function(data_t, data_n, t_trim = 0) {

  # make sure that both inputs are vectors
  if (is.list(data_t) == TRUE) {
    tryCatch( {data_t <- unlist(data_t)},
              error = function(e) {stop("data_t is not a vector")}
            )
  }
  if (is.list(data_n) == TRUE) {
    tryCatch( {data_n <- unlist(data_n)},
              error = function(e) {stop("data_n is not a vector")}
            )
  }
  stopifnot(is.vector(data_t))
  stopifnot(is.vector(data_n))

  # check t_trim parameter and set dependent variables
  if (t_trim > 0) {
    t_max <- t_trim
    data_n <- data_n[data_t < t_trim]
    data_t <- data_t[data_t < t_trim]
  } else {
    t_max <- max(data_t, na.rm=TRUE)
  }


  q_AUC <- QuantitativeAreaUnderCurve(data_t, data_n, t_max)
  log_mod <- FitLogistic(data_t, data_n)

  p <- summary(log_mod)$coefficients
  k <- p[1]
  k_se <- p[4]
  k_p <- p[10]
  n0 <- p[2]
  n0_se <- p[5]
  n0_p <- p[11]
  r <- p[3]
  r_se <- p[6]
  r_p <- p[12]

  # get the inflection point
  t_inflection <- growthcurver:::TAtInflection(k, n0, r)

  DT <- growthcurver:::MaxDt(r)
  n_at_t_inflection <- growthcurver:::NAtT(k, n0, r, t_inflection)
  auc <- growthcurver:::AreaUnderCurve(k, n0, r, 0, t_max)$value
  sigma <- summary(log_mod)$sigma
  df <- summary(log_mod)$df[2]

  vals <- c(k, k_se, k_p, n0, n0_se, n0_p,
            r, r_se, r_p, sigma, df,
            t_inflection, DT, n_at_t_inflection, auc, q_AUC)
  val_names <- c("k", "k_se", "k_p", "n0", "n0_se", "n0_p",
                 "r", "r_se", "r_p", "sigma", "df",
                 "t_mid", "t_gen", "N_at_t_mid", "auc", "quant_auc")
  vals <- setNames(as.list(vals), val_names)
  class(vals) <- "gcvals"

  ret <- list("vals" = vals, "model" = log_mod,
              "data"=list("t" = data_t, "N" = data_n))
  class(ret) <- "gcfit"
  return(ret)
}


