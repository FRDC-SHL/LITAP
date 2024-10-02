# Article reports


library(LITAPReports)

# AB provost site -------------------------------------------------------------
# Need to give grid size

f <- "../Runs - LITAP/11_Ab02PV"

report_compare_flow(
  file = "../Runs - FlowMapR/Steffi_LandMapR_tests/11_Ab02PV/FlowMapR/021ELEV.DBF",
  nrow = 184, ncol = 187, grid = 5, max_area = 1, max_depth = 0.1,
  out_folder = f)
#pkgdown::build_article("reports/11_Ab02PV_compare_report_flow")

report_compare_form(
  folder = f,
  orig_loc = "../Runs - FlowMapR/Steffi_LandMapR_tests/11_Ab02PV/FlowMapR/",
  str_val = 200, ridge_val = 200)
#pkgdown::build_article("reports/11_Ab02PV_compare_report_form")

report_compare_facet(
  folder = f,
  arule = NULL, crule = system.file("extdata", "c7rule.dbf", package = "LITAP"),
  orig_loc = "../Runs - FlowMapR/Steffi_LandMapR_tests/11_Ab02PV/FlowMapR/")
#pkgdown::build_article("reports/11_Ab02PV_compare_report_facet")

report_compare_wepp(
  folder = f,
  orig_loc = "../Runs - FlowMapR/Steffi_LandMapR_tests/11_Ab02PV/FlowMapR/",
  chan_length = 200, upslope_threshold = 300)

# BR3 ------------------------------------------------------------------------
# Should detect grid size of 1
f <- "../Runs - LITAP/BR3"

report_compare_flow(
  file = "../Runs - FlowMapR/BR3/31MELEV.DBF",
  nrow = 309, ncol = 181, max_area = 100, max_depth = 0.1,
  out_folder = f)
#pkgdown::build_article("reports/BR3_compare_report_flow")

report_compare_form(
  folder = f,
  orig_loc = "../Runs - FlowMapR/BR3/",
  str_val = 300, ridge_val = 300)
#pkgdown::build_article("reports/BR3_compare_report_form")

report_compare_facet(
  folder = f,
  arule = NULL,
  crule = system.file("extdata", "c7rule.dbf", package = "LITAP"),
  orig_loc = "../Runs - FlowMapR/BR3/")
#pkgdown::build_article("reports/BR3_compare_report_facet")

# Ehsan ---------------------------------------------------------------------
# Need to give grid size
f <- "../Runs - LITAP/Ehsan"

report_compare_flow(
  file = "../Runs - FlowMapR/Ehsan/05ELEV.dbf", grid = 5,
  nrow = 50, ncol = 42, max_depth = 0.2, max_area = 10,
  out_folder = f)
#pkgdown::build_article("reports/Ehsan_compare_report_flow")

report_compare_form(
  folder = f,
  orig_loc = "../Runs - FlowMapR/Ehsan/",
  str_val = 200, ridge_val = 200)
#pkgdown::build_article("reports/Ehsan_compare_report_form")

report_compare_facet(
  folder = f,
  arule = "../Runs - LITAP/Ehsan/LM3ARULE.dbf",
  crule = "../Runs - LITAP/Ehsan/LM3CRULE.dbf",
  orig_loc = "../Runs - FlowMapR/Ehsan/")
#pkgdown::build_article("reports/Ehsan_compare_report_facet")
