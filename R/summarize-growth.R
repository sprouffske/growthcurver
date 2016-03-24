#' Summarize Growth Curves
#'
#' This function finds the parameters that describe the input data's growth.
#' It does so by fitting the logistic curve to your growth curve measurements.
#'
#' The logistic curve equation is
#' \deqn{N_t = \frac{N_0 K} {N_0 + (K-N_0)e^{-rt}}}{N_t = N_0 K / (N_0 + (K - N_0) exp(-rt))}
#' where \eqn{N_t} is the number
#' of cells (or the absorbance reading) at time t, \eqn{N_0} is the initial
#' cell count (or absorbance reading), K is the carrying capacity, and r is the
#' growth rate.
#'
#' The fitness proxies returned are the parameters of the logistic equation
#' and the area under the curve (a measure that integrates the effects
#' of \eqn{N_0}, K, and r). See \code{\link{gcfit}} for more documentation on these.
#' @param data_t     A vector of timepoints (data_n must also
#'                   be provided and be the same length).
#' @param data_n     A vector of cell counts or absorbance readings.
#' @param t_trim     Measurements taken after this time should not be included
#'                   in fitting the curve. If stationary phase is variable,
#'                   this may give you a better fit. A value of 0 means no
#'                   trimming. Defaults to no trimming (0).
#' @param bg_correct The background correction method to use. No background
#'                   correction is performed for the default "none". Specifying
#'                   "min" subtracts the smallest value in a column from all the
#'                   rows in that column, and specifying "blank" subtracts
#'                   the values from the blank vector from the data_n vector.
#' @param blank      A vector of absorbance readings from a blank well
#'                   (typically contains only media) used for background
#'                   correction. The corresponding blank value is subtracted
#'                   from the data_n vector for each timepoint. Defaults to NA.
#' @return           An object of type gcfit containing the "fitness" proxies,
#'                   as well as the input data and the fitted model.
#' @seealso
#' See the accompanying Vignette for an example of how to use and interpret
#' SummarizeGrowth. \url{bit.ly/1p7w6dJ}.
#'
#' See also \code{\link{gcfit}}.
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
#' gc$vals$auc_l
#' gc$vals$auc_e
#' gc$vals$t_mid
#'
#' # Compare the data with the fit visually by plotting it
#' plot(gc)
#'
#' @export
SummarizeGrowth <- function(data_t, data_n, t_trim = 0,
                            bg_correct = "min", blank = NA) {

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

  # make sure that the inputs are valid
  if (!is.numeric(data_t) |!is.numeric(data_n)) {
    stop("Error: The input data (data_t and data_n) must be numeric.")
  }
  if (length(data_t) != length(data_n)) {
    stop("Error: The input data (data_t and data_n) must have the same number of rows")
  }

  # make sure that the background correction method is valid
  if (!bg_correct %in% c("none", "min", "blank")) {
    stop(paste0(bg_correct, "is not a valid option for bg_correct"))
  }

  # check for correctness of the blank (background correction) vector
  if (bg_correct == "blank") {
    if (is.list(blank) == TRUE) {
      tryCatch( {blank <- unlist(blank)},
                error = function(e) {stop("blank is not a vector")}
      )
      stopifnot(is.vector(blank))

      if (!is.numeric(blank)) {
        stop("Error: The blank data must be numeric.")
      }
      if (length(blank) != length(data_n)) {
        stop("Error: The input data (data_n) and the background correction data (blank) must have the same number of rows.")
      }
    }
  }

  # check t_trim parameter and set dependent variables
  if (t_trim > 0) {
    t_max <- t_trim
    data_n <- data_n[data_t < t_trim]
    data_t <- data_t[data_t < t_trim]
    if (bg_correct == "blank") {
      blank <- blank[data_t < t_trim]
    }
  } else {
    t_max <- max(data_t, na.rm=TRUE)
  }

  #do the background correction
  if (bg_correct == "blank") {
    data_n <- data_n - blank
    data_n[data_n < 0] <- 0    # ensure readings are at least 0
  }
  else if (bg_correct == "min") {
    data_n <- data_n - min(data_n)
  }

  tryCatch(
    # code block
    {log_mod = FitLogistic(data_t, data_n)},
    # error handling block
    error = function(e) {}
    )

  # the data did not fit a logistic model
  if (exists("log_mod") == FALSE) {
    vals <- c(0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0,
              0, 0, 0, 0)
  }
  else {
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

    # get the inflection point, DT, auc, sigma, df
    t_inflection <- TAtInflection(k, n0, r)

    DT <- MaxDt(r)
    sigma <- summary(log_mod)$sigma
    df <- summary(log_mod)$df[2]

    auc_l <- AreaUnderCurve(k, n0, r, 0, t_max)$value
    auc_e <- EmpiricalAreaUnderCurve(data_t, data_n, t_max)
    vals <- c(k, k_se, k_p, n0, n0_se, n0_p,
              r, r_se, r_p, sigma, df,
              t_inflection, DT, auc_l, auc_e)
  }

  val_names <- c("k", "k_se", "k_p", "n0", "n0_se", "n0_p",
                 "r", "r_se", "r_p", "sigma", "df",
                 "t_mid", "t_gen", "auc_l", "auc_e")
  vals <- stats::setNames(as.list(vals), val_names)
  class(vals) <- "gcvals"

  if (exists("log_mod") == FALSE) {
    vals$note <- "cannot fit data"
    log_mod <- ""
  }
  else if (k < n0) {
    vals$note <- "questionable fit (k < n0)"
  }
  else if (t_inflection < 0) {
    vals$note <- "questionable fit"
  }
  else {
    vals$note <- ""
  }

  ret <- list("vals" = vals, "model" = log_mod,
              "data"=list("t" = data_t, "N" = data_n))
  class(ret) <- "gcfit"
  return(ret)
}


