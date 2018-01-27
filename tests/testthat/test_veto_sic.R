context("veto sic") 

test_that("Veto quote", {
  res <- 
    veto_sic(  c("A large dog", "Rob remarked, ``sdoifhsdfi'' [sic] ok?", "Yet some don't. Are you sdfih [sic]."))
  expect_equal(res,
               c("A large dog", "Rob remarked,                      ok?", "                                   ."))
  
})

test_that("Veto sentence", {
  res <- 
    veto_sic(  c("A large dog", "Rob remarked, ``sdoifhsdfi'' [sic] ok?", "Yet some don't. Are you sdfih [sic]."),
               quote = FALSE, sentence = TRUE)
  
  expect_equal(res,
               c("A large dog", "                                   ok?", "Yet some don't.                    ."))
  
})

test_that("Veto words before", {
  res <- 
    veto_sic(  c("A large dog", "Rob remarked, ``sdoifhsdfi'' [sic] ok?", "Yet some don't. Are you sdfih [sic]."),
               quote = FALSE, sentence = FALSE, words_ante = 1L)
  
  expect_equal(res,
               c("A large dog", "Rob remarked,                      ok?", "Yet some don't. Are you            ."))
})
