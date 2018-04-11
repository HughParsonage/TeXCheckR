context("utils")

test_that("Seq_union always length one", {
  expect_equal(Seq_union(1, 5), 1:5)
  expect_equal(Seq_union(1, c(5, 10)), 1:10)
  expect_equal(Seq_union(c(73, 100), c(83, 105)), c(73:83, 100:105))
  expect_equal(Seq_union(c(1, 10), c(9, 19)), 1:19)
})


test_that("stringi replacements work if stringi absent", {
  skip_if_not_installed("stringi")
  Lines <- readLines("Example-report.Rnw")
  expect_identical(stri_locate_first_fixed_no_stringi(Lines, " - "), 
                   stringi::stri_locate_first_fixed(Lines, " - "))
  
  expect_identical(stri_count_fixed_no_stringi(c("ABCAB", "DABAAAABEF", "A"), "AA"), 
                   stringi::stri_count_fixed(c("ABCAB", "DABAAAABEF", "A"), "AA"))
  
  
  some_lines <- c("", "abc", "sdoifhsdfiohsdf dsfugdsfiugsdfiug", "", " X ")
  i1 <- sample.int(max(nchar(some_lines)), size = 1L)
  i2 <- sample.int(max(nchar(some_lines)), size = 1L)
  j1 <- -1L *sample.int(max(nchar(some_lines)), size = 1L)
  j2 <- -1L * sample.int(max(nchar(some_lines)), size = 1L)
  
  expect_identical(stringi::stri_sub(some_lines, i1, i2), stri_sub_no_stringi(some_lines, i1, i2))
  expect_identical(stringi::stri_sub(some_lines, j1, j1), stri_sub_no_stringi(some_lines, j1, j1))
  expect_identical(stringi::stri_sub(some_lines, i1, j1), stri_sub_no_stringi(some_lines, i1, j1))
})


test_that("Any brace", {
  expect_true(any_brace(c("sdf", "sfd{")))
  expect_false(any_brace("sdf"))
})



