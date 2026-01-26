# undidR 3.0.0

* Added new aggregation methods for staggered adoption: `"sgt"`, `"time"`, and `"none"`.
  * `"sgt"` computes subaggregate ATTs for each treatment-time and post-treatment period pair, further separated by silo.
  * `"time"` computes subaggregate ATTs grouped by time since treatment.
  * `"none"` skips subaggregate ATT computation and directly computes the aggregate ATT from the underlying differences.

* `undid_stage_three()` now returns an object of class `UnDiDObj`, providing a unified S3 interface.
  * Added S3 methods: `print()`, `summary()`, `coef()`, and `plot()`.

* Added support for event study plots via the `plot()` method for `UnDiDObj` objects.

* Added randomization inference p-values for subaggregate ATTs.

* Fixed a miscalculation of jackknife standard errors and p-values.

* Expanded weighting options for ATT computation: `"none"`, `"diff"`, `"att"`, and `"both"`.
  * `"diff"` applies weights when computing subaggregate ATTs.
  * `"att"` applies weights when aggregating subaggregate ATTs.
  * `"both"` applies weighting at both stages.
  * `"none"` disables weighting entirely.
  
# undidR 2.0.0

* start_time and end_time columns are now stored as strings in the format specified by date_format column in the empty_diff_df CSV file.
* Fixed `as.Date()` calls in `undid_stage_two()`, `undid_stage_three()`, and `plot_parllel_trends()` to ensure compatability for R versions 4.0.0 and onwards.

# undidR 1.0.2

* Made compatible for R versions 4.0.0 onwards. 

# undidR 1.0.1

* Fixed the miscalculation of standard weights during stage two under common adoption scenarios.

# undidR 1.0.0

* Initial CRAN submission.
* Updated installation instructions.

# undidR 0.1.0

* Initial development version (not yet submitted to CRAN).
* Provides a framework for implementing difference-in-differences with unpoolable data.