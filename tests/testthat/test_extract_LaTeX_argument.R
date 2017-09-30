context("Extract arguments")

test_that("Extract textbf", {
  out <- extract_LaTeX_argument("The contents of \\textbf{ABC} is abc.", "textbf")
  expect_equal(out[["extract"]], "ABC")
  expect_equal(out[["start_column"]], nchar("The contents of \\textbf{"))
  expect_equal(out[["stop_column"]], nchar("The contents of \\textbf{ABC"))
})

test_that("Extract textcites", {
  out <- extract_LaTeX_argument("Some citation by \\textcites{Knuth194}{Knuth195}", "textcites", n = 2L)
  expect_equal(out[["extract"]], "Knuth195")
  expect_equal(out[["start_column"]], nchar("Some citation by \\textcites{Knuth194}{"))
  expect_equal(out[["stop_column"]], nchar("Some citation by \\textcites{Knuth194}{Knuth195"))
})

test_that("Extract nested", {
  out <- extract_LaTeX_argument("What should this be \\emph{Some emphasized text \\emph{Double emphasized} here.}",
                                "emph")
  
  expect_equal(out[["extract"]], 
               c("Some emphasized text \\emph{Double emphasized} here.",
                 "Double emphasized"))
})

test_that("Blank line", {
  out <- extract_LaTeX_argument("", "foo")
  expect_true(is.na(out[["extract"]]))
})

test_that("Optional argument", {
  out <- extract_LaTeX_argument("See \\textcite[Ante][Post]{Knuth1} [if you want].",
                                command_name = "textcite", 
                                n = 2L, 
                                optional = TRUE)
  expect_equal(out[["extract"]], "Post")
  
  out <- extract_LaTeX_argument("See \\textcite[Ante][[ex] Post]{Knuth1} [if you want].",
                                command_name = "textcite", 
                                n = 2L, 
                                optional = TRUE)
  
  expect_equal(out[["extract"]], "[ex] Post")
  
  tex_lines_with_optional <- 
    c("Sometimes \\footcite[][3]{Daley2016} we have optional args; other times not.\\footcite{Daley2016}.")
  
  output_excl_optional <- extract_LaTeX_argument(tex_lines_with_optional, "footcite", star = FALSE)
  expect_equal(output_excl_optional[["extract"]], c("Daley2016", "Daley2016"))
  output_incl_optional <- extract_LaTeX_argument(tex_lines_with_optional,
                                                 command_name = "footcite",
                                                 star = FALSE,
                                                 optional = TRUE, 
                                                 n = 2L)
  expect_equal(output_incl_optional[["extract"]], c("3", NULL))
})

test_that("Multi-line starred", {
  skip("Undecided test outcome")
  out <- extract_LaTeX_argument(c("This \\footnote{", "ends", "here.}"), "footnote", star = FALSE)
  expect_equal(out$starts, c(15, NA, NA))
  expect_equal(out$stops, c(6, NA, NA))
  expect_equal(out$stops_line_no, c(3, NA, NA))
  
  
  out <- extract_LaTeX_argument(c("This \\footnote{ends quickly} where this \\footnote{", "ends", "here.}"), "footnote")
  expect_equal(out$starts, c(15, NA, NA))
  expect_equal(out$stops, c(6, NA, NA))
  expect_equal(out$stops_line_no, c(3, NA, NA))
  
})

test_that("Multi-line", {
  tex_lines <- 
    c("This is some \\textbf{bold text} and \\footnote{this is a footnote with some \\textbf{text also in boldface}}",
      "whereas \\footnote{this footnote ",
      "extends over",
      "\\emph{more}",
      "than one line.}")
  output <- extract_LaTeX_argument(tex_lines, "footnote", star = FALSE)
  expect_equal(output[["extract"]],
               c("this is a footnote with some \\textbf{text also in boldface}",
                 "this footnote ",
                 "extends over",
                 "\\emph{more}", 
                 "than one line."))

  tex_lines <- 
    c("This is some \\emph{emph text} and \\emph{this",
      "text \\textbf{has} \\emph{double}",
      "emphasis.}")
  
  output <- extract_LaTeX_argument(tex_lines, "emph", star = FALSE)
  expect_equal(output[["extract"]],
               c("emph text",
                 "this",
                 "text \\textbf{has} \\emph{double}",
                 "emphasis.", 
                 "double"))
  expect_equal(output[["line_no"]], c(1, 1, 2, 3, 2))
  expect_equal(output[["command_no"]], c(1, 2, 2, 2, 3))
  
  

})

test_that("Optional argument interference", {
  expect_equal(extract_LaTeX_argument("\\abc{def}", "abc")[["extract"]], "def")
  expect_equal(extract_LaTeX_argument("\\abc{def}{ghij}", "abc")[["extract"]], "def")
  expect_equal(extract_LaTeX_argument("\\abc{def}{ghij}", "abc", n = 2)[["extract"]], "ghij")
  expect_equal(extract_LaTeX_argument("\\abc[xyz]{def}{ghij}", "abc", n = 2)[["extract"]], "ghij")
  expect_equal(extract_LaTeX_argument("\\abc[xyz]{def}{ghij}", "abc")[["extract"]], "def")
  expect_equal(extract_LaTeX_argument("\\abc[xyz][]{def}{ghij}", "abc")[["extract"]], "def")
  expect_equal(extract_LaTeX_argument("\\abc[xyz][\\abc{DEF}]{def}{ghij}", "abc", star = FALSE)[["extract"]], "def")
  expect_equal(extract_LaTeX_argument("\\abcd[xyz][\\abc{DEF}]{def}{ghij}", "abc")[["extract"]], "DEF")
  expect_equal(extract_LaTeX_argument("\\abc[xyz][\\abc{DEF}]{def}{ghij}", "abc", optional = TRUE, n = 2)[["extract"]], "\\abc{DEF}")
  expect_equal(extract_LaTeX_argument("\\abc[xyz][\\abc{DEF}]{def}{ghij}", "abc", optional = TRUE, n = 1)[["extract"]], "xyz")
})

test_that("Optional argument interference", {
  expect_equal(extract_LaTeX_argument2("\\abc{def}", "abc")[["extract"]], "def")
  expect_equal(extract_LaTeX_argument2("\\abc{def}{ghij}", "abc")[["extract"]], "def")
  expect_equal(extract_LaTeX_argument2("\\abc{def}{ghij}", "abc", n = 2)[["extract"]], "ghij")
  expect_equal(extract_LaTeX_argument2("\\abc[xyz]{def}{ghij}", "abc", n = 2)[["extract"]], "ghij")
  expect_equal(extract_LaTeX_argument2("\\abc[xyz]{def}{ghij}", "abc")[["extract"]], "def")
  expect_equal(extract_LaTeX_argument2("\\abc[xyz][]{def}{ghij}", "abc")[["extract"]], "def")
  expect_equal(extract_LaTeX_argument2("\\abc[xyz][\\abc{DEF}]{def}{ghij}", "abc")[["extract"]], c("def", "DEF"))
  expect_equal(extract_LaTeX_argument2("\\abcd[xyz][\\abc{DEF}]{def}{ghij}", "abc")[["extract"]], "DEF")
  expect_equal(extract_LaTeX_argument2("\\abc[xyz][\\abc{DEF}]{def}{ghij}", "abc", optional = TRUE, n = 2)[["extract"]], "\\abc{DEF}")
  expect_equal(extract_LaTeX_argument2("\\abc[xyz][\\abc{DEF}]{def}{ghij}", "abc", optional = TRUE, n = 1)[["extract"]], "xyz")
})



