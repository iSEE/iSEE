library(iSEE)
# R/*.R
for (slotname in names(iSEEslots)) {
    regex_search <- paste0("\\.", slotname, "([^A-Za-z])")
    replace_string <- paste0("iSEEslots$", slotname, "\\1")
    for (filename in list.files("R/", pattern = ".R$", full.names = TRUE)) {
        # cat("filename: ", filename, "\n")
        filelines <- readLines(filename)
        filelines <- gsub(pattern = regex_search, replacement = replace_string, x = filelines)
        writeLines(filelines, filename)
    }
}
# tests/testthat/*.R
for (slotname in names(iSEEslots)) {
    regex_search <- paste0("iSEE:::\\.", slotname, "([^A-Za-z])")
    for (filename in list.files("tests/testthat/", pattern = ".R$", full.names = TRUE)) {
        filelines <- readLines(filename)
        filelines <- gsub(pattern = regex_search, replacement = paste0("iSEEslots$", slotname, "\\1"), x = filelines)
        writeLines(filelines, filename)
    }
}
