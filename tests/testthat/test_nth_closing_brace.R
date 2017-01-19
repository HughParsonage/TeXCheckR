context("nth arg positions")

test_that("Closing brace for nth argument", {
  expect_equal(nth_arg_positions("A \\abc[12]{def}{agh} and \\abc{de}[3]{fghi}.", "abc", n = 2L), 
               list(data.table::data.table(starts = c(16, 37), stops = c(20, 42))))
})

