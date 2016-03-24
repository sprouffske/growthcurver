#' Summarize Growth Curves
#'
#' This function finds the parameters that describe the input data's growth
#' for a plate of growth curves.
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
#'
#' This method expects that your data adhere to a particular format.
#'\itemize{
#'  \item The data are provided in a data.frame
#'  \item One column in the data.frame is named "time" and contains the
#'        time measurements (e.g., hours).
#'  \item Each remaining column contains the
#'        readings from a single well in a plate reader. The name of the column
#'        will be used to identify the sample in the output data.
#'  \item There are no missing values or non-numeric data in the data.frame.
#'}
#'
#' @param plate      A data.table with at least two columns. One column contains
#'                   timepoints that measurements were taken (e.g., hours) and
#'                   must be named "time". An optional column can be included
#'                   called "blank" that contains the blank readings for
#'                   background correction (make sure to select the "blank"
#'                   bg_correct option if you provide a blank column).
#'                   Each remaining column contains the
#'                   absorbance readings from a single well in a plate.
#' @param t_trim     Measurements taken after this time should not be included
#'                   in fitting the curve. If stationary phase is variable,
#'                   this may give you a better fit. A value of 0 means no
#'                   trimming. Defaults to no trimming (0).
#' @param bg_correct The background correction method to use. No background
#'                   correction is performed for "none". Specifying
#'                   "min" subtracts the smallest value in a column from all the
#'                   rows in that column, and specifying "blank" subtracts
#'                   the values from the blank vector from the data_n vector.
#' @param plot_fit   TRUE if you want to generate a pdf file that plots all
#'                   columns provided in the plate along with the growthcurver's
#'                   fit. The default value is FALSE, which generates no plots.
#' @param plot_file  The name of the file to save the plots to if you set
#'                   plot_fit to TRUE. The default file is called
#'                   "growthcurver.pdf".
#' @return           A data.table containing the summary metrics and residual
#'                   error from the fit of the logistic curve to the data.
#'                   The names of the input columns are used to identify each
#'                   well (or sample).
#' @seealso
#' See the accompanying Vignette for an example of how to use and interpret
#' SummarizeGrowthByPlate. \url{bit.ly/1p7w6dJ}
#'
#' @examples
#' #Get the summary metrics for the entire plate of sample data provided
#' #with the Growthcurver package
#'
#' #First, load the example data provided with Growthcurver. Note that there is
#' #a column named "time" -- this is necessary for Growthcurver to know which
#' #column contains the time measurements. In this dataset, the repeated
#' #measurements from a single well in a plate are given in a column of data.
#'
#' myPlate <- growthdata
#' names(myPlate)
#'
#' #Next, do the analysis for all the columns.
#' summary_plate <- SummarizeGrowthByPlate(plate = myPlate)
#'
#' #The output is a data frame that contains the information on the best
#' #fit for each column of data.
#' head(summary_plate)      # Use head to display just the first few rows
#'
#' @export
SummarizeGrowthByPlate <- function(plate,
                                   t_trim = 0,
                                   bg_correct = "min",
                                   plot_fit = FALSE,
                                   plot_file = "growthcurver.pdf") {

  # make sure that the input is a data.frame
  if (is.data.frame(plate) != TRUE) {
    stop("The 'plate' input data must be formatted as a data.frame.", call. = FALSE)
  }

  # make sure that there is a column named "time" in the input
  if (sum(grep("time", names(plate), ignore.case = TRUE)) != 1) {
    stop("There must be exactly one column named 'time' in the 'plate' data.frame.",
         call. = FALSE)
  }
  # make sure "time" column is named as expected (in lowercase)
  names(plate)[grep("time", names(plate), ignore.case = TRUE)] <- "time"

  # make sure that there are at least two columns in the input
  if (length(names(plate)) < 2) {
    stop("You must have at least two columns in the 'plate' data.frame: one for time, and the other for absorbance.")
  }

  if (bg_correct == "blank") {
    # check that there is a column in the plate data.frame containing the blanks
    if (sum(grep("blank", names(plate), ignore.case = TRUE)) != 1) {
      stop("There must be exactly one column named 'blank' in the 'plate' data.frame if you have selected the bg_correct 'plate' option.",
           call. = FALSE)
    }
    # make sure "blank" column is named as expected (in lowercase)
    names(plate)[grep("blank", names(plate), ignore.case = TRUE)] <- "blank"
  }

  # create the output data frame
  n <- length(plate) -
       sum(grepl("time|plate", names(plate), ignore.case = TRUE))
  d_gc <- data.frame(sample = character(n),
                     k = numeric(n),
                     n0  = numeric(n),
                     r = numeric(n),
                     t_mid = numeric(n),
                     t_gen = numeric(n),
                     auc_l = numeric(n),
                     auc_e = numeric(n),
                     sigma = numeric(n),
                     note = character(n),
                     stringsAsFactors = FALSE)

  if (plot_fit == TRUE) {
    grDevices::cairo_pdf(plot_file, width = 12, height = 8)
    old_par <- graphics::par(mfcol = c(8, 12), mar = c(0.25, 0.25, 0.25, 0.25))
    idx_to_plot <- length(plate$time) * 1:20 / 20
    y_lim_max <- max(plate[,setdiff(names(plate), "time")]) -
                 min(plate[,setdiff(names(plate), "time")])
  }

  n <- 1
  for (col_name in names(plate)) {
    if (!col_name %in% c("time", "blank")) {
      # process just the well columns, skipping the time and blank columns

      if (bg_correct == "blank") {
        gc_fit <- SummarizeGrowth(data_t = plate$time,
                                  data_n = plate[, col_name],
                                  t_trim = t_trim,
                                  bg_correct = bg_correct,
                                  blank = plate$blank)
      }
      else {
        gc_fit <- SummarizeGrowth(data_t = plate$time,
                                  data_n = plate[, col_name],
                                  t_trim = t_trim,
                                  bg_correct = bg_correct)
      }

      # now, add the metrics from this column to the output data
      d_gc$sample[n] <- col_name
      d_gc$k[n] <- gc_fit$vals$k
      d_gc$n0[n] <- gc_fit$vals$n0
      d_gc$r[n] <- gc_fit$vals$r
      d_gc$t_mid[n] <- gc_fit$vals$t_mid
      d_gc$t_gen[n] <- gc_fit$vals$t_gen
      d_gc$auc_l[n] <- gc_fit$vals$auc_l
      d_gc$auc_e[n] <- gc_fit$vals$auc_e
      d_gc$sigma[n] <- gc_fit$vals$sigma
      d_gc$note[n] <- gc_fit$vals$note
      n <- n + 1

      # plot, if necessary
      if (plot_fit == TRUE) {
        graphics::plot(gc_fit$data$t[idx_to_plot],
                  gc_fit$data$N[idx_to_plot],
                  pch = 20,
                  ylim = c(0, y_lim_max),
                  cex = 0.6,
                  xaxt = "n", yaxt = "n")
        graphics::text(x = max(gc_fit$data$t) / 4,
                  y = y_lim_max,
                  labels = col_name, pos = 1)
        if (gc_fit$vals$note == "") {
          graphics::lines(gc_fit$data$t,
                          stats::predict(gc_fit$model),
                          col = "red")
        }
      }
    }
  }

  if (plot_fit == TRUE) {
    grDevices::dev.off()
  }

  return(d_gc)

}
