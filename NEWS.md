# womblR 1.0.1 (2017-07-17)

* New `STBDwDM()` functionality, related to the adjacency weights that can be used. A new option is included, `Weights` allows for the `binary` weights of the Lee and Mitchell (2011) specification.

* New `PlotVFTimeSeries()` functionality, related to the location specific regression line (including `line.col`, `line.reg`, and `line.type`). 

* Default bounds for lower and upper bounds for `Phi` are now specific to the temporal correlation structure specification (i.e., if `TemporalStructure` = `"ar1"` the bounds are now appropriate).

* Adjusted the truncated normal random sampler to allow it to use the inverse CDF method or accept-reject method of the `msm` package when necessary. 

* Tidying of the help pages.

# womblR 1.0.0 (2017-06-17)

* First release.
