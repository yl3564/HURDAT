
test_that( "landfall looks ok", {
  storm_id<-"AL031869"
  expect_equal(
    is.landfall(storm_id),
    0
  )
})
