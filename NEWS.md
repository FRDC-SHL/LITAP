# LITAP 0.4.0 (2021-02-12)

- Added `facet_mapper()` based on FacetMapR
- Added `wepp_mapper()` based on WeppMapR
- Internal changes to increase `form_mapper()` speed
- Added ARULE deriving based on Li et al. 2011. Canadian Journal of Soil Science 91(2), 251-266.
- Added both forms of calculating qweti, qarea, and lnqarea to `form_mapper()`
  (Originally values based on actual grid-cell area, now, qweti1, qarea1, lnqarea1 based on
  number of cells, and qweti2, qarea2, lnqarea2 based on grid-cell area).
- Reduced number of output files
- Updated reports

# LITAP 0.3.1 (2020-01-24)

- Update to `tidyr` v1.0.0

# LITAP 0.3.0 (2019-07-11)

- Added `form_mapper()` which mimics FormMapR
- Renamed `complete_run()` to `flow_mapper()` to clarify what it refers to
- Added new comparison reports
- Fixed bugs related to incorrect `vol2fl`, `parea` and `mm2fl`
- Added calculation of cumulative elevation differences to `flow_mapper()` which results in `local_elev_diff` (calculated after pit smoothing) and `elev_diff` (calculated after fill pits)

# LITAP 0.2.1 (2018-06-04)

- Minor bug fixes relating to fixing large flat pits

# LITAP 0.2.0 (2018-04-13)

- Major speed improvements, in some cases as much as 30x faster!
- Added support for multiple input file types (`?load_file` for details)

# LITAP 0.1.0 (2017-08-30)

- Initial build of LITAP including functions to mimic FlowMapR
- Runs include log and report of results
- Added a `NEWS.md` file to track changes to the package.



