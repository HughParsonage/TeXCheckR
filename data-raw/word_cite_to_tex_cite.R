# Convert Word citations to LaTeX citations

HE_endnote_library <- 
{
  raw <- readLines("Higher_ed_master.bib")
  
  raw <- gsub("–", "--", raw, fixed = TRUE)
  
  # weird autthor instead of author sometimes

  raw <- gsub("^(\\s*)autthor(\\s*=)", "\\1author \\2", raw, perl = TRUE)
  #                                              ^ (extra space to ensure line-up)
  
  # check editors 
  raw <- ifelse(grepl("^\\s*(editor)\\s*=\\s*\\{[A-Za-z\\s]+,[A-Za-z\\s]+;", raw, perl = TRUE),
                gsub(",}", "}",
                     gsub(";", "", 
                          gsub("([A-Z]),\\s([A-Za-z]+)", 
                               "\\1. \\2,",
                               raw, 
                               perl = TRUE), 
                          fixed = TRUE),
                     fixed = TRUE),
                raw)
  
  # e.g. title     = {Australian higher education research policies and performance 1987author = {{Deloitte Access Economics}}10},
  raw <- ifelse(raw == "  title     = {Australian higher education research policies and performance 1987author = {{Deloitte Access Economics}}10},", 
                "  title     = {Australian higher education research policies and performance 1987-2010},\nauthor = {{Deloitte Access Economics}},", 
                raw)
  if (any(str_count(raw, pattern = "=\\s+\\{") - str_count(raw, "\\n") > 1)){
    for (e in raw[str_count(raw, pattern = "=\\s+\\{") > 1]){
      message("Check line", e, "for malformed entry file (twice number of equals signs)")
    }
    stop("Check of bib file failed.")
  }
  
  writeLines(raw, "Higher_ed_master_no_autthors.bib")
  
  raw_mark <- ifelse(nchar(raw) == 0, "bibtexBoundaryHere", raw)
  
  raw_linear <- paste0(raw_mark, collapse = "")
  
  number_entries <- sum(grepl("^@", raw, perl = TRUE))
  
  # Goal: one entry per element
  raw_one_each <- strsplit(raw_linear, split = "bibtexBoundaryHere")[[1]]
  
  raw_one_each <<- raw_one_each[grep("^@", raw_one_each, perl = TRUE)]
  
  types <- gsub("^@([A-Za-z0-9]+)\\{.*$", "\\1", raw_one_each, perl = TRUE)
  
  bibtex_keys <- gsub("^@[A-Za-z0-9]+\\{([^,]+),.*$", "\\1", raw_one_each, perl = TRUE)
  
  authors <- ifelse(grepl("author\\s*=", raw_one_each, perl = TRUE),
                    gsub(paste0("^.*author\\s*=\\s*", 
                                # for authors in double braces
                                "\\{+", 
                                # string until closing brace 
                                "([^\\}]+)", 
                                "\\}+", 
                                ".*$"), 
                         "\\1", raw_one_each, perl = TRUE), 
                    NA_character_)
  
  double_braced <- grepl("^.*author\\s*=\\s*\\{\\{", raw_one_each, perl = TRUE)
  
  # space for booktitle
  titles <<- ifelse(grepl("\\stitle\\s*=", raw_one_each, perl = TRUE),
                   gsub(paste0("^.*\\stitle\\s*=\\s*", 
                               "\\{", 
                               # string until closing brace 
                               "([^\\}]+)", 
                               "\\}", 
                               ".*$"), 
                        "\\1", raw_one_each, perl = TRUE), 
                   NA_character_)
  
  years <- ifelse(grepl("year\\s*=", raw_one_each, perl = TRUE), 
                  gsub(paste0("^.*year\\s*=\\s*", 
                              "\\{", 
                              # string until closing brace 
                              "([^\\}]+)", 
                              "\\}", 
                              ".*$"), 
                       "\\1", raw_one_each, perl = TRUE),
                  NA_character_)
  
  journals <- ifelse(grepl("journal\\s*=", raw_one_each, perl = TRUE), 
                     gsub(paste0("^.*journal\\s*=\\s*", 
                                 "\\{", 
                                 # string until closing brace 
                                 "([^\\}]+)", 
                                 "\\}", 
                                 ".*$"), 
                          "\\1", raw_one_each, perl = TRUE),
                     NA_character_)
  
  booktitles <<- ifelse(grepl("booktitle\\s*=", raw_one_each, perl = TRUE), 
                       gsub(paste0("^.*booktitle\\s*=\\s*", 
                                   "\\{", 
                                   # string until closing brace 
                                   "([^\\}]+)", 
                                   "\\}", 
                                   ".*$"), 
                            "\\1", raw_one_each, perl = TRUE),
                       NA_character_)
  
  data.table(entry_no = 1:number_entries, 
             type = types,
             bibtex_key = bibtex_keys,
             author = authors, 
             double_braced = double_braced,
             title = titles,
             booktitle = booktitles,
             journal = journals,
             year = years) %>%
    
    # Stobbs2015 e.g.
    mutate(journal = ifelse(is.na(journal) & type == "InBook", 
                            booktitle, 
                            journal)) %>%
    select(-booktitle) %>%  # no longer required.
    
    mutate(      
      n_authors = str_count(author, pattern = "\\sand\\s") + 1,
      et_al = n_authors > 2,
      short_author = ifelse(double_braced, 
                            # Nothing should be changed
                            author, 
                            
                            ifelse(n_authors == 1, 
                                   ifelse(grepl(",", author), 
                                          gsub(",.*$", "", author), author),
                                   ifelse(n_authors == 2, 
                                          
                                          ifelse(grepl(",", author), 
                                                 
                                                 # Smith, John and Black, Jack
                                                 # Smith, J. and Black, J.
                                                 # Smith, J.D. and Black, J. Q.
                                                 gsub(",\\s[A-Za-z]+\\.?(\\s?[A-Z]\\.)?", "", author, perl = TRUE), 
                                                 
                                                 # John Smith and Jack Black
                                                 # words before 'and' or at end of string
                                                 # words not before and or the end of a string
                                                 gsub("\\s+", " and ", gsub("([A-Za-z]+\\s(?=(and)))", "", author, perl = TRUE))),
                                          
                                          paste(ifelse(grepl(",", author), 
                                                       
                                                       # Smith, John and Black, Jack
                                                       # Smith, J. and Black, J.
                                                       gsub(",\\s[A-Za-z]+\\.?.*$", "", author, perl = TRUE), 
                                                       
                                                       # John Smith and Jack Black
                                                       # words before 'and' or at end of string
                                                       # words not before and or the end of a string
                                                       sub("([A-Za-z]+\\s(?=(and)))", "", author, perl = TRUE)), 
                                                "et al"))))
      )
}

match_hyperlinks <- function(lines){
  # \protect\hyperlink{ux5fENREFux5f132}{GCA (2015e});
  cite_instances <- 
    # Some hyperlinks are malformed e.g. the above (brace before closing parenthesis)
    data.table(cite_key = gsub("^.*\\\\protect\\\\hyperlink\\{([A-Za-z0-9]+)\\}\\{[A-Za-z\\s\\.]+\\(([0-9]{4}[a-z]{1,2})|(various years)|(multiple years)).*$",
                               "\\1",
                               lines, 
                               perl = TRUE), 
               apparent_citation = paste0(gsub("^.*\\\\protect\\\\hyperlink\\{([A-Za-z0-9]+)\\}\\{([A-Za-z\\s\\.]+\\(([0-9]{4}[a-z]{1,2})|(various years)|(multiple years))).*$", 
                                               "\\2", 
                                               lines, 
                                               perl = TRUE), 
                                          ")"))
  msword_bibliography <- 
    data.table(author = gsub("^\\s*([A-Z][^\\(]+)\\s+\\(.*$", "\\1", bibliography_lines, perl = TRUE)) %>%
    mutate(year_raw = gsub("^[^\\(]+\\((([0-9]{4}[a-z]*)|(various years)|([0-9]{4}.[0-9]{4}))\\).*$", "\\1", bibliography_lines, perl = TRUE), 
           year = gsub("[^0-9]", "", year_raw), 
           title = gsub("^.*\\\\emph\\{([^\\}]+)\\},.*$", "\\1", bibliography_lines, perl = TRUE)) %>%
    filter(nchar(title) > 0)
  
  bibliography_entries <- 
    data.table(bibl_key = readLines("bib_hypertargets_ordered.txt"), 
               citation_from_references = paste0(short_author, " (", year_raw, ")"))
  
}

ms_bib_to_parse <- readLines("tests/ms-bib-to-parse.tex")

parse_ms_bib <- function(bibliography_lines){
  msword_bibliography <- 
    data.table(author = gsub("^\\s*([A-Z][^\\(]+)\\s+\\(.*$", "\\1", bibliography_lines, perl = TRUE)) %>%
    mutate(year_raw = gsub("^[^\\(]+\\((([0-9]{4}[a-z]*)|(various.years(.[a-c])?)|([0-9]{4}.[0-9]{4}))\\).*$", "\\1", bibliography_lines, perl = TRUE), 
           year = gsub("[^0-9]", "", year_raw),
           # If italic title, article, else misc
           # Either emph or ' delimit titles.
           title = ifelse(grepl(paste0(paste0("^.*",
                                              "\\(",
                                              "(",
                                              "(",
                                              # year or various years
                                              "([0-9]{4})", "|", "(various.years(.[a-c])?)", 
                                              ")",
                                              # Optional year disambiguator
                                              "([a-z]{1,2})?",
                                              ")",
                                              "\\)\\s"), 
                                       "\\\\emph\\{"), 
                                bibliography_lines, 
                                perl = TRUE), 
                          gsub(paste0("^.*\\\\emph\\{", 
                                      # Title (emph text until '}')
                                      "([^\\}]+)", 
                                      # ... or until ",}" -- for some reason, 
                                      # Endnote sometimes italicizes the comma!
                                      "((\\},)|(,\\}))", 
                                      ".*$"), 
                               "\\1", bibliography_lines, perl = TRUE), 
                          gsub(paste0(paste0("^.*",
                                             "\\(",
                                             "(?:",
                                             "(?:",
                                             # year or various years
                                             "(?:[0-9]{4})", "|", "(?:various.years(?:.[a-c])?)", 
                                             ")",
                                             # Optional year disambiguator
                                             "(?:[a-z]{1,2})?",
                                             ")",
                                             "\\)\\s"), 
                                      "\\'", 
                                      # Title (text until "'")
                                      # too difficult with things like apostrophes in titles...
                                      "(.+)", 
                                      # ... or until ",'" -- for some reason, 
                                      # Endnote sometimes italicizes the comma!
                                      "((\\',)|(,\\'))", 
                                      ".*$"), 
                               "\\1", bibliography_lines, perl = TRUE)), 
           
           # lack of italics immediately after year
           journal = ifelse(grepl(paste0(paste0("^.*",
                                                "\\(",
                                                "(?:",
                                                "(?:",
                                                # year or various years
                                                "(?:[0-9]{4})", "|", "(?:various.years(?:.[a-c])?)", 
                                                ")",
                                                # Optional year disambiguator
                                                "(?:[a-z]{1,2})?",
                                                ")",
                                                "\\)\\s"), 
                                         "\\'", 
                                         # Title (emph text until "'")
                                         "([^\\']+)", 
                                         # ... or until ",'" -- for some reason, 
                                         # Endnote sometimes italicizes the comma!
                                         "((\\',)|(,\\'))", 
                                         ".*$"), 
                                  bibliography_lines, 
                                  perl = TRUE),
                            # check if journal title present before attempt to extract
                            ifelse(grepl("\\\\emph\\{", bibliography_lines, perl = TRUE),
                                   gsub(paste0("^.*\\\\emph\\{", 
                                               # Title (emph text until '}')
                                               "([^\\}]+)", 
                                               # ... or until ",}" -- for some reason, 
                                               # Endnote sometimes italicizes the comma!
                                               "((\\},)|(,\\}))", 
                                               ".*$"), 
                                        "\\1", bibliography_lines, perl = TRUE),
                                   NA_character_),
                            
                            # if not
                            NA_character_)
           
    ) %>%
    mutate(title = ifelse(is.na(title) & !is.na(journal), journal, title)) %>%
    
    filter(nchar(title) + nchar(author) + nchar(year_raw) > 0)
}

testthat::test_that("ms-bib-to-parse.tex parsed correctly", {
  parse_attempt <- parse_ms_bib(ms_bib_to_parse)
  
  correctly_parsed_DT <-
    data.table(author = c("ARC, CSIRO and NHMRC", "Arkoudis, S., Baik, C. and Richardson, S.", "ARWU", "ATO", 
                          "Bentley, P. J., Coates, H., Dobson, I. R., Goedegebuure, L. and Meek, V. L.", 
                          "Stobbs, N."), 
               year_raw = c(2002, 2012, 2015, "2016a", "2013b", 2015), 
               year = as.character(c(2002, 2012, 2015, 2016, 2013, 2015)), 
               title = c("National survey of research commercialisation, year 2000", 
                         "English language standards in higher education: from entry to exit", 
                         "Academic Ranking of World Universities", 
                         "HELP and TSL repayment rates and thresholds", 
                         "Factors associated with job satisfaction amongst Australian university academics and future workforce implications", 
                         "Academic freedom and university autonomy"), 
               journal = c(NA, NA, NA, NA, 
                           "Job Satisfaction around the Academic World", 
                           "Higher education and the law"))
  
  expect_identical(parse_attempt, correctly_parsed_DT)
})

transform_citations <- function(filename){
  
  the_lines <- readLines(filename)
  bibliography_lines <<- the_lines[seq(max(grep("\\\\chapter", the_lines)) + 1, length(the_lines) - 1)]
  
  msword_bibliography <<- parse_ms_bib(bibliography_lines = bibliography_lines)
  
  possible_join <<- 
    left_join(msword_bibliography, HE_endnote_library, by = c("title", "year")) %>%
    
    mutate(ms_word_citation = paste0(short_author, " (", year_raw, ")"), 
           textcite = paste0("\\textcite{", bibtex_key, "}"))
  
  
  if (any(is.na(possible_join["bibtex_key"]))){
    possible_join %>%
      filter(is.na(bibtex_key)) %>%
      select(author.x, year_raw, title) %>%
      write_csv("warnings/unmatched-word-bibliography-entires.csv")
      
    n <- sum(is.na(possible_join[["bibtex_key"]]))
    warning(n, " entries in Word bibliography not matched in .bib file. See warnings/unmatched-word-bibliography-entires.csv")
  }
  
  cleanse_et_als <- function(lines){
    gsub("\\\\emph\\{, et al.\\}", " et al", lines)
  }
 
  clean_by_observed_emph <- function(lines){
    # "\\source{\\protect\\hyperlink{ux5fENREFux5f28}{\\emph{AQF (2013}}\\emph{)}}"
    gsub(paste0(
      # \1  Year within a citation **without** closing parenthesis
      "(\\([0-9]{4}[a-z]?)", 
      
      # followed by two braces
      "\\}\\}", 
      
      # followed by an emph
      "\\\\emph\\{", 
      
      "(\\);?)", 
      
      "\\}"
    ), 
    
    "\\1\\2}}", 
    
    lines, 
    
    perl = TRUE)
  }
  
  the_lines <- cleanse_et_als(the_lines)
  the_lines <- clean_by_observed_emph(the_lines)
  
  writeLines(the_lines, "DELETE.tex")
  
  replacements <- possible_join[["textcite"]]
  patterns <- possible_join[["ms_word_citation"]]
  
  for (p in seq_along(patterns)){
    the_lines <- gsub(patterns[p], replacements[p], the_lines, fixed = TRUE)
  }
  
  postnote_replacer <- function(x){
    # Split up each instance of textcite
    # First ensure a space before each textcite
    x <- gsub("(.)\\\\textcite", "\\1 \\\\textcite", x, perl = TRUE)
    y <- strsplit(x, split = ".(?=(\\\\textcite))", perl = TRUE)
    
    
    postnote_replacer_list <- function(element){
      ifelse(!grepl("\\\\textcite\\{[^\\}]+\\},?\\s+p\\.?\\s*[0-9]", element, perl = TRUE), 
             element,
             # Is there a page range?
             ifelse(grepl("\\\\textcite\\{[^\\}]+\\},?\\s+p\\.?\\s*[0-9]+[\\s-][0-9]", element, perl = TRUE),
                    
                    gsub(paste0("\\\\textcite\\{", 
                                
                                # \1  Bibkey
                                "(", 
                                "[^\\}]+", 
                                ")", 
                                
                                # Space (preceded by an optional comma)
                                # followed by a p, 
                                #  an optional period or space
                                "\\},?\\s+p\\.?\\s*", 
                                
                                # \2  Page range 1 
                                "(", 
                                "[0-9]+", 
                                ")", 
                                
                                
                                "[\\s-]", 
                                
                                # \3  Page range 2
                                "(", 
                                "[0-9]+", 
                                ")"), 
                         
                         "\\\\textcite[][\\2--\\3]{\\1}", 
                         
                         element), 
                    
                    # No page range:
                    gsub(paste0("\\\\textcite\\{", 
                                
                                # \1  Bibkey
                                "(", 
                                "[^\\}]+", 
                                ")", 
                                
                                # Space, preceded by optional comma,
                                # followed by a p, 
                                #  an optional period or space
                                "\\},?\\s+p\\.?\\s*", 
                                
                                # \2  Page
                                "(", 
                                "[0-9]+", 
                                ")"),
                         
                         "\\\\textcite[][\\2]{\\1}", 
                         
                         element)
             ))
    }
    
    y.out <- 
      # Insert the postnotes
      lapply(y, postnote_replacer_list) %>%
      # Unsplit
      lapply(function(x) paste0(x, collapse = ""))
    
    # Unlist
    unlist(y.out)
  }
  
  the_lines <- postnote_replacer(the_lines)
  
  the_lines
}
