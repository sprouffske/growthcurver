# Prints a gcvals object
# @param x         An object of class "gcvals"
# @param ...       Additional parameters
#' @export
print.gcvals <- function(x, ...) {
  cat(names(x[1:6]), sep = "\t")
  cat("\n")
  cat(c(mapply(x[1:2][], FUN = function(x) {round(x, 3)}),
        format(x[3], scientific = TRUE, digits = 1),
        mapply(x[4:5][], FUN = function(x) {round(x, 3)}),
        format(x[6], scientific = TRUE, digits = 1)),
      sep = "\t")
  cat("\n\n")

  cat(names(x[7:12]), sep = "\t")
  cat("\n")
  cat(c(mapply(x[7:8][], FUN = function(x) {round(x, 3)}),
        format(x[9], scientific = TRUE, digits = 1),
        mapply(x[10:12][], FUN = function(x) {round(x, 3)})),
      sep = "\t")
  cat("\n\n")

  cat(names(x[c(13,15:16)]), sep = "\t")
  cat("\n")
  cat(mapply(x[c(13,15:16)][], FUN = function(x) {round(x, 3)}), sep = "\t")
  cat("\n\n")
}

# Summarizes objects of class gcfit
# @param object         An object of class gcfit
# @param ...            Additional parameters
#' @export
summary.gcfit <- function(object, ...) {
  cat("Fit data to K / (1 + ((K - N0) / N0) * exp(-r * t))", "\n\n")
  print(object)
}


# Prints objects of class gcfit
# @param x         An object of class gcfit
# @param ...       Additional parameters
#' @export
print.gcfit <- function(x, ...) {

  # Show the fit
  cat("Fit data to K / (1 + ((K - N0) / N0) * exp(-r * t)):", "\n")
  cat("  ")
  cat("","K","N0", "r", sep= "\t")
  cat("\n")
  cat("  ")
  cat(c("val:", mapply(c(x$vals$k, x$vals$n0, x$vals$r),
                       FUN= function(x) {round(x, 3)})),
      sep="\t")
  cat("\n")
  cat("  ")
  cat("Residual standard error:", x$vals$sigma, "on", x$vals$df, "degrees of freedom")
  cat("\n\n")

  # Show other interesting metrics
  cat("Other useful metrics:\n")
  cat("  ")
  cat("DT", "1 / DT", "auc", "quant_auc", sep = "\t")
  cat("\n")
  cat("  ")
  cat(c(round(x$vals$t_gen, 2),
        format(1 / x$vals$t_gen, scientific = TRUE, digits = 2),
        round(x$vals$auc, 2),
        round(x$vals$quant_auc, 2)), sep = "\t")
  cat("\n")

}

# Plots the data and the best fitting curve
# @param x         An object of class gcfit
# @param ...       Additional parameters
#' @export
plot.gcfit <- function(x, ...) {
  old_par <- par(mfrow = c(1,1), mar = c(4,4,1,1))
  plot(x$data$t, x$data$N, xlab="Time t", ylab="Number N", pch=19, type="b")
  lines(x$data$t, fitted(x$model), col="red")
  par(old_par)
}

#' Creates an object of type gcvals.
#'
#' Constructor function for the "gcvals" class. This object is most often
#' obtained when calling  \code{\link{SummarizeGrowth}} (it is the first
#' parameter in the \code{\link{gcvals}} object).
#' @param k         The carrying capacity parameter
#' @param k_se      The standard error of the carrying capacity parameter
#' @param k_p       The p value of the carrying capacity parameter
#' @param n0        The initial population size
#' @param n0_se     The standard error of the initial population size
#' @param n0_p      The p value of the initial population size
#' @param r         The growth rate
#' @param r_se      The standard error of the growth rate
#' @param r_p       The p value of the growthrate
#' @param sigma     Residual standard error from non-linear least squares fit
#'                  of the model to the data
#' @param df        Degrees of freedom
#' @param t.mid     The time at the inflection point of the logistic curve
#'                  (occurs at half of the carrying capacity)
#' @param dt        The maximum doubling time, obtained by evaluating the
#'                  the unrestrained growth of the population with growth rate r
#' @param N.at.t.mid The size of the population (or its absorbance) at the
#'                  inflection point. This should be equal to 0.5 * k
#' @param auc       The area under the curve of the fitted logistic equation
#'                  from time 0 to time t
#' @param auc.quant The area under the curve of the measurements.
#' @return          An object of class gcvals.
#' @export
gcvals <- function(k, k_se, k_p, n0, n0_se, n0_p, r, r_se, r_p, sigma, df,
                   t.mid, dt, N.at.t.mid, auc, auc.quant) {
  val.names <- c("k", "k_se", "k_p", "n0", "n0_se", "n0_p",
                 "r", "r_se", "r_p", "sigma", "df",
                 "t.mid", "dt", "N.at.t.mid", "auc", "quant.auc")
  vals <- setNames(as.list(k, k_se, k_p, n0, n0_se, n0_p, r, r_se, r_p,
                           sigma, df, t.mid, dt, N.at.t.mid, auc, auc.quant),
                   val.names)
}

#' Creates an object of class gcfit.
#'
#' This is a constructor function for the "gcfit" class. This class is most
#' often obtained as the return value when calling
#' \code{\link{SummarizeGrowth}}.
#' @param gc_vals   An object of class gcvals that contains the summarized
#'                  metrics from fitting the growth model to a set of
#'                  experimental observations. This is where the fitnes proxy
#'                  parameters can be found. See \code{\link{gcvals}} for
#'                  more information the information found in this object.
#' @param log_mod    An object of class nlsModel that contains the results
#'                  of fitting the logistic growth model to the data
#' @param data_t    A numeric vector of times
#' @param data_n    A numeric vector of cell count or absorbance readings
#' @return          An object of class gcfit, which is a list of three objects,
#'                  that combines the parameters (vals = gc_vals,
#'                  model = log_mod, data = list(data_t, data_n))
#' @export
gcfit <- function(gc_vals, log_mod, data_t, data_n) {

  ret <- list("vals" = gc_vals, "model" = log_mod,
              "data"=list("t" = data_t, "N" = data_n))
  class(ret) <-"gcfit"
  return(ret)
}

#' Growth curve data from Escherichia coli.
#'
#' A dataset containing absorbance measurements over time of E. coli
#' growing in a plate reader for 1 day. The growth curves for three wells are
#' included. The background absorbance readings have already been subtracted
#' from the values by subtracting the minimum absorbance reading in a given well
#' from the rest of the measurements in that well.
#'
#' @format A data frame with 416 observations and 4 variables:
#' \describe{
#'   \item{t}{time, in hours}
#'   \item{N1}{absorbance readings of well 1}
#'   \item{N2}{absorbance readings of well 2}
#'   \item{N3}{absorbance readings of well 3}
#' }
"growthcurves"
