library(testthat)
options(dbms = "sqlite")
test_check("CohortDiagnostics")

test_dir("inst/shiny/DiagnosticsExplorer/tests")