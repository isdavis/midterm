context("A test for the EAP function")
test_that("EAP function is correct", {
  f1<-new("rasch", name="a", a=c(1,3,5), y=c(1,0,1))
  expect_equal(rasch.EAP(f1), -0.3587, tolerance=1e-3)
})

context("A test for the EAP function")
test_that("EAP function returns reasonable thetas for different performances", {
  f2<-new("rasch", name="a", a=c(1,3,5), y=c(0,0,1))
  expect_equal(rasch.EAP(f1), 2.153, tolerance=1e-3)
})

