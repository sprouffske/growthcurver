# growthcurver 0.3.0

This is a minor release containing a bug fix and removing a dependency.

* Fixed bug in `SummarizeGrowthByPlate` that did not properly recognize the `time` and `blank` columns in some instances
* Updated `EmpiricalAreaUnderCurve` to compute the area under the curve using the trapezoid method, thereby removing the dependency on `trapz` from the orphaned `caTools` package 

# growthcurver 0.2.1

This is a minor release containing several bug fixes.

* Handled errors raised by `FitLogistic` when the model cannot be fit
* Added a `note` to the `gc_val` object and the output of 
  `SummarizeGrowthByPlate` to indicate failed or poor fits
* Prevented warnings raised by `TAtInflection` when $k < N_0$
* Updated the vignette to explain the new `note` value

# growthcurver 0.2.0

This is a minor release containing one new function to summarize the growth 
curves for many wells with a single command. Background correction has also
been added.

* Added `SummarizeGrowthByPlate` 
* Extended the `plot` method for `gc_fit` objects to take standard plot options
* Added background correction (option `bg_correct`)
* Updated the vignette to include the new options
* Set the default `bg_correct` option to `min`

# growthcurver 0.1.0

* Initial release
