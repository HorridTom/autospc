test_that("word_for_number returns readable caption text", {
  expect_equal(word_for_number(6), "Six")
  expect_equal(word_for_number(8), "Eight")
  expect_equal(word_for_number(9), "Nine")
  expect_equal(word_for_number(10), "10")
  expect_equal(word_for_number(5), "5")
})
