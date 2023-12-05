
test_that( "storm track looks ok", {
  storm_id<-"AL031869"
  expect_equal(
    as.character(storm_track(storm_id)[1,]),
    c("UNNAMED", "AL031869","0000", "32.7","46.7")
  )
})
