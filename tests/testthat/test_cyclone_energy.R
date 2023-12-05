
test_that( "cyclone energy looks ok", {
  storm_id<-"AL031869"
  expect_equal(
    cyclone_energy(storm_id),
    1.96
  )
})
