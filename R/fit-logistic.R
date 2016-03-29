# Fits a logistic curve to data.
#
# This function fits a logistic curve to the supplied data, namely
# n(t) = K / (1 + ( (K - N0) / N0) * exp(-r * t), where
# N(t) is the number of cells (or density) at time t,
# K is the carrying capacity,
# N0 is the initial cell count or density, and
# r is the "growth rate".
# @param data_t    A vector of timepoints (data_n must also
#                  be provided and be the same length).
# @param data_n    A vector of cell counts or absorbance readings.
# @return          An object of class nls.
# @keywords        growth curves
FitLogistic <- function(data_t, data_n) {

  # make sure that the inputs are valid
  if (!is.vector(data_t) | !is.vector(data_n)) {
    stop("Error: The input data (data_t and data_n) must be vectors.")
  }
  if (!is.numeric(data_t) |!is.numeric(data_n)) {
    stop("Error: The input data (data_t and data_n) must be numeric.")
  }
  if (length(data_t) != length(data_n)) {
    stop("Error: The input data (data_t and data_n) must have the same length.")
  }

  # put together data
  d <- data.frame(cbind(data_t, data_n))
  names(d) <- c("t", "n")

  # make some guesses for the initial parameter values
  k_init <- max(data_n)   # carrying capacity is near the max
  n0_init <- min(data_n[data_n > 0])  # init population size is near the min

  # make an initial estimate for r
  glm_mod <- stats::glm(n / k_init ~ t,
                        family = stats::quasibinomial("logit"),
                        data = d)

  r_init <- stats::coef(glm_mod)[[2]]   # slope
  if (r_init <= 0) {
    # the slope should only be positive for a growing culture, so default
    # to something small
    r_init <- 0.001
  }

  suppressWarnings(
    nls_mod <- tryCatch(
                 minpack.lm::nlsLM(n ~ k / (1 + ( (k - n0) / n0) * exp(-r * t)),
                                   start = list(k = k_init,
                                                n0 = n0_init,
                                                r = r_init),
                                   control = list(maxiter = 500),
                                   lower = c(stats::median(data_n), 0, 0),
                                   upper = c(Inf, max(data_n), Inf),
                                   data = d),
                 error = function(e) {
                   stop("Error: Growthcurver FitLogistic cannot fit data.")
                 }
               )
  )
  return(nls_mod)
}
