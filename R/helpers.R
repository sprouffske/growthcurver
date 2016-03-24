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

  cat(names(x[c(13:15)]), sep = "\t")
  cat("\n")
  cat(mapply(x[c(13:15)][], FUN = function(x) {round(x, 3)}), sep = "\t")
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
  cat("DT", "1 / DT", "auc_l", "auc_e", sep = "\t")
  cat("\n")
  cat("  ")
  if (x$vals$note == "") {
    cat(c(round(x$vals$t_gen, 2),
          format(1 / x$vals$t_gen, scientific = TRUE, digits = 2),
          round(x$vals$auc_l, 2),
          round(x$vals$auc_e, 2)), sep = "\t")
  }
  else {
    cat("0", "0", "0", "0", sep = "\t")
    cat("\n\n")
    cat(c("Note: ", x$vals$note), sep = "")
  }
  cat("\n")
}

# Plots the data and the best fitting curve
# @param x         An object of class gcfit
# @param ...       Additional parameters
#' @export
plot.gcfit <- function(x, ...) {
  dots <- list(...)

  if (is.null(dots$xlab)) {
    dots$xlab <- "Time t"
  }
  if (is.null(dots$ylab)) {
    dots$ylab <- "Number N"
  }
  if (is.null(dots$pch)) {
    dots$pch <- 19
  }

  args_plot <- dots[setdiff(names(dots), "col")]
  args_plot$x <- x$data$t
  args_plot$y <- x$data$N
  args_plot$type <- "b"
  args_plot$ylim <- c(0, max(x$data$N))
    if (!is.null(dots$col)) {
    args_plot$col <- "black"
  }

  args_lines <- dots
  args_lines$x <- max(x$data$t) * (1 : 30) / 30
  args_lines$y <- NAtT(x$vals$k,
                       x$vals$n0,
                       x$vals$r,
                       args_lines$x)
  if (is.null(dots$col)) {
    args_lines$col <- "red"
  }

  old_par <- graphics::par(mfrow = c(1,1), mar = c(4,4,1,1))

  do.call(graphics::plot, args_plot)
  do.call(graphics::lines, args_lines)
  graphics::par(old_par)
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
#' @param t_mid     The time at the inflection point of the logistic curve
#'                  (occurs at half of the carrying capacity)
#' @param dt        The maximum doubling time, obtained by evaluating the
#'                  the unrestrained growth of the population with growth rate r
#' @param auc_l     The area under the curve of the fitted logistic equation
#'                  from time 0 to time t
#' @param auc_e     The area under the curve of the measurements.
#' @param note      Feedback on common problems with fitting the logistic curve
#'                  to the data
#' @return          An object of class gcvals.
#' @export
gcvals <- function(k, k_se, k_p, n0, n0_se, n0_p, r, r_se, r_p, sigma, df,
                   t_mid, dt, auc_l, auc_e, note) {
  val.names <- c("k", "k_se", "k_p", "n0", "n0_se", "n0_p",
                 "r", "r_se", "r_p", "sigma", "df",
                 "t_mid", "dt", "auc_l", "auc_e", "note")
  vals <- stats::setNames(as.list(k, k_se, k_p, n0, n0_se, n0_p, r, r_se, r_p,
                           sigma, df, t_mid, dt,  auc_l, auc_e, note),
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

#' Simulated growth curve data
#'
#' A dataset containing absorbance measurements over time of microbes
#' growing in a plate reader for 1 day. The growth curves for a whole plate are
#' included.
#'
#' @format A data frame with 145 observations and 97 variables:
#' \describe{
#'   \item{time}{time, in hours}
#'   \item{A1}{absorbance readings of well A1}
#'   \item{A2}{absorbance readings of well A2}
#'   \item{A3}{absorbance readings of well A3}
#'   \item{A4}{absorbance readings of well A4}
#'   \item{A5}{absorbance readings of well A5}
#'   \item{A6}{absorbance readings of well A6}
#'   \item{A7}{absorbance readings of well A7}
#'   \item{A8}{absorbance readings of well A8}
#'   \item{A9}{absorbance readings of well A9}
#'   \item{A10}{absorbance readings of well A10}
#'   \item{A11}{absorbance readings of well A11}
#'   \item{A12}{absorbance readings of well A12}
#'   \item{B1}{absorbance readings of well B1}
#'   \item{B2}{absorbance readings of well B2}
#'   \item{B3}{absorbance readings of well B3}
#'   \item{B4}{absorbance readings of well B4}
#'   \item{B5}{absorbance readings of well B5}
#'   \item{B6}{absorbance readings of well B6}
#'   \item{B7}{absorbance readings of well B7}
#'   \item{B8}{absorbance readings of well B8}
#'   \item{B9}{absorbance readings of well B9}
#'   \item{B10}{absorbance readings of well B10}
#'   \item{B11}{absorbance readings of well B11}
#'   \item{B12}{absorbance readings of well B12}
#'   \item{C1}{absorbance readings of well C1}
#'   \item{C2}{absorbance readings of well C2}
#'   \item{C3}{absorbance readings of well C3}
#'   \item{C4}{absorbance readings of well C4}
#'   \item{C5}{absorbance readings of well C5}
#'   \item{C6}{absorbance readings of well C6}
#'   \item{C7}{absorbance readings of well C7}
#'   \item{C8}{absorbance readings of well C8}
#'   \item{C9}{absorbance readings of well C9}
#'   \item{C10}{absorbance readings of well C10}
#'   \item{C11}{absorbance readings of well C11}
#'   \item{C12}{absorbance readings of well C12}
#'   \item{D1}{absorbance readings of well D1}
#'   \item{D2}{absorbance readings of well D2}
#'   \item{D3}{absorbance readings of well D3}
#'   \item{D4}{absorbance readings of well D4}
#'   \item{D5}{absorbance readings of well D5}
#'   \item{D6}{absorbance readings of well D6}
#'   \item{D7}{absorbance readings of well D7}
#'   \item{D8}{absorbance readings of well D8}
#'   \item{D9}{absorbance readings of well D9}
#'   \item{D10}{absorbance readings of well D10}
#'   \item{D11}{absorbance readings of well D11}
#'   \item{D12}{absorbance readings of well D12}
#'   \item{E1}{absorbance readings of well E1}
#'   \item{E2}{absorbance readings of well E2}
#'   \item{E3}{absorbance readings of well E3}
#'   \item{E4}{absorbance readings of well E4}
#'   \item{E5}{absorbance readings of well E5}
#'   \item{E6}{absorbance readings of well E6}
#'   \item{E7}{absorbance readings of well E7}
#'   \item{E8}{absorbance readings of well E8}
#'   \item{E9}{absorbance readings of well E9}
#'   \item{E10}{absorbance readings of well E10}
#'   \item{E11}{absorbance readings of well E11}
#'   \item{E12}{absorbance readings of well E12}
#'   \item{F1}{absorbance readings of well F1}
#'   \item{F2}{absorbance readings of well F2}
#'   \item{F3}{absorbance readings of well F3}
#'   \item{F4}{absorbance readings of well F4}
#'   \item{F5}{absorbance readings of well F5}
#'   \item{F6}{absorbance readings of well F6}
#'   \item{F7}{absorbance readings of well F7}
#'   \item{F8}{absorbance readings of well F8}
#'   \item{F9}{absorbance readings of well F9}
#'   \item{F10}{absorbance readings of well F10}
#'   \item{F11}{absorbance readings of well F11}
#'   \item{F12}{absorbance readings of well F12}
#'   \item{G1}{absorbance readings of well G1}
#'   \item{G2}{absorbance readings of well G2}
#'   \item{G3}{absorbance readings of well G3}
#'   \item{G4}{absorbance readings of well G4}
#'   \item{G5}{absorbance readings of well G5}
#'   \item{G6}{absorbance readings of well G6}
#'   \item{G7}{absorbance readings of well G7}
#'   \item{G8}{absorbance readings of well G8}
#'   \item{G9}{absorbance readings of well G9}
#'   \item{G10}{absorbance readings of well G10}
#'   \item{G11}{absorbance readings of well G11}
#'   \item{G12}{absorbance readings of well G12}
#'   \item{H1}{absorbance readings of well H1}
#'   \item{H2}{absorbance readings of well H2}
#'   \item{H3}{absorbance readings of well H3}
#'   \item{H4}{absorbance readings of well H4}
#'   \item{H5}{absorbance readings of well H5}
#'   \item{H6}{absorbance readings of well H6}
#'   \item{H7}{absorbance readings of well H7}
#'   \item{H8}{absorbance readings of well H8}
#'   \item{H9}{absorbance readings of well H9}
#'   \item{H10}{absorbance readings of well H10}
#'   \item{H11}{absorbance readings of well H11}
#'   \item{H12}{absorbance readings of well H12}
#' }
"growthdata"
