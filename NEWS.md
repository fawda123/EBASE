# EBASE 1.0.2

* Added check in `ebase_prep()` for ascending date
* Fix to model formula in JAGS file and `ebase_prep()` for correct handling of Z either as fixed of continuous. This is a minor change and does not affect the results of the model for fixed Z, but previously produced incorrect rates if continuous.
* `ebase_prep()` no longer converts dissolved oxygen to areal (as above)

# EBASE 1.0.1

* Minor update to documentation for core EBASE equation and model file.  All calculations are the same.

# EBASE 1.0.0

* Initial CRAN submission.
