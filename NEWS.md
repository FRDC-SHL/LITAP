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



