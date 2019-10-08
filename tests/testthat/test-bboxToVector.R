context("test-bboxToVector")

test_that("works for vectors of floats", {
  expect_equal( bboxToVector(c(-110, -60, 25, 45)), c(-110, -60, 25, 45) )
  expect_equal( bboxToVector(c(-60, -110, 45, 25)), c(-110, -60, 25, 45) )
  expect_error( bboxToVector(c(-190, -60, 25, 45)) )
  expect_error( bboxToVector(c(-110, -60, 25, 95)) )
  expect_error( bboxToVector(c(-110, -60, 25)) )
  expect_error( bboxToVector(-110, -60, 25, 45) )
})
