library(testthat)
library(farspractise)
setwd(system.file("extdata",package = "farspractise"))

test_that("fars read test",{
  my_data<-fars_read("accident_2013.csv.bz2")
  expect_that(my_data,is_a("data.frame"))
  expect_that(dim(my_data), equals(c(30202, 50)))
})


test_that("make filename test",{
  filename_2013<-make_filename(2013)
  expect_that(filename_2013,is_a("character"))
  expect_that(filename_2013,is_identical_to("accident_2013.csv.bz2"))
})

test_that("fars read years test",{
  lista<- fars_read_years(2013)
  expect_that(lista,is_a("list"))
  expect_that(lista[[1]],is_a("data.frame"))
})

test_that("summary test",{
  summary_years<-fars_summarize_years(2013:2015)
  expect_that(summary_years,is_a("data.frame"))
  expect_that(dim(summary_years), equals(c(12, 4)))
})
