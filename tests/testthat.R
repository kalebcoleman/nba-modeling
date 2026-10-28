library(testthat)
for (f in list.files("R", full.names = TRUE)) source(f)
test_dir("tests/testthat")
