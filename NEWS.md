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