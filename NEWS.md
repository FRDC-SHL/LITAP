# LITAP 0.7.0
- fix missing `ddir` in `form_mapper()`
- clarify rule file usage and errors in `facet_mapper()`
- Tweaks to align form_mapper flow decisions with the original LandMapR
- Increase speed of QWETI calculations
- Fix number of edge rows/cols removed
- Add Topographical Summary outputs similar to those in Sheng et al. 2011 (`summary_tables()` etc. are now run by `facet_mapper()`)
- Fixed an discrepancy in `wepp_mapper()` which resulted in different channel mappings.
- General tweaks and clean up of older usages

# LITAP 0.6.0
- Import x/y coords or create them if they don't exist
- flow_mapper() now requires grid or infers from x/y value of input files
   - form_mapper() and wepp_mapper() now use grid inferred from x/y value of 
     flow_mapper() output files
- flow_mapper() now has upslope_m (upslope cells * grid^2)
- flow_mapper() calculates UCED
- facet_mapper() calculates buffer edges
- Simplify output with 'debug' argument (if false, removes intermediate files)
- Simplify output columns by removing intermediate ones
- Remove option to 'end' a run prematurely (required due to simplified output)
- Remove dbf output option because it truncates column names
- Fix flow_mapper() inconsistencies
- Add extra data output "topographical_derivatives" in facet_mapper
- Initial work on all_points data output

# LITAP 0.5.0
- Load files forces to numeric (fixes problems where some imports in character)
- Corrected `slope_gc()` directions (fixes #10)
- Clarify output format and allow csv (fixes #13)
- `flow_mapper()` has simplified output, similar to the other functions (fixes #12)
- `facet_mapper()` has facets by number as well as by name, and ordered by crule (fixes #9)
- `form_mapper()` values that are not on missing cells but can't be calculated 
due to local missingness, are assigned 0
- Can load files even if `dem` in folder name (fixes #7)
- Add rounding to match original LandMapR values where possible
- Fix bug where grid parameter not passed on in `form_mapper()`

# LITAP 0.4.1
- Added `slope_gc()` to calculate slope gradients and curvature (added to `dem_fill` through `flow_mapper()`)
- Added `merge_flow_form()` to create a single dem file from `flow_mapper()` `dem_fill` and all `form_mapper()` outputs.

# LITAP 0.4.0

- Added `facet_mapper()` based on FacetMapR
- Added `wepp_mapper()` based on WeppMapR
- Internal changes to increase `form_mapper()` speed
- Added ARULE deriving based on Li et al. 2011. Canadian Journal of Soil Science 91(2), 251-266.
- Added both forms of calculating qweti, qarea, and lnqarea to `form_mapper()`
  (Originally values based on actual grid-cell area, now, qweti1, qarea1, lnqarea1 based on
  number of cells, and qweti2, qarea2, lnqarea2 based on grid-cell area).
- Reduced number of output files
- Updated reports

# LITAP 0.3.1

- Update to `tidyr` v1.0.0

# LITAP 0.3.0

- Added `form_mapper()` which mimics FormMapR
- Renamed `complete_run()` to `flow_mapper()` to clarify what it refers to
- Added new comparison reports
- Fixed bugs related to incorrect `vol2fl`, `parea` and `mm2fl`
- Added calculation of cumulative elevation differences to `flow_mapper()` which results in `local_elev_diff` (calculated after pit smoothing) and `elev_diff` (calculated after fill pits)

# LITAP 0.2.1

- Minor bug fixes relating to fixing large flat pits

# LITAP 0.2.0

- Major speed improvements, in some cases as much as 30x faster!
- Added support for multiple input file types (`?load_file` for details)

# LITAP 0.1.0

- Initial build of LITAP including functions to mimic FlowMapR
- Runs include log and report of results
- Added a `NEWS.md` file to track changes to the package.



