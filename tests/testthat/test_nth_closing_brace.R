context("nth closing brace")

test_that("Closing brace for nth argument", {
  expect_equal(nth_arg_closes_at("A \\abc[12]{def}{agh} and \\abc{de}[3]{fghi}.", "abc", n = 2L), 
               list(c(20, 42)))
})
