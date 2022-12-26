library(iSEE)
for (slotname in names(iSEEconstants)) {
    regex_search <- paste0("\\.", slotname, "([^A-Za-z])")
    for (filename in list.files("R", full.names = TRUE)) {
        filelines <- readLines(filename)
        filelines <- gsub(pattern = regex_search, replacement = paste0("iSEEconstants$", slotname, "\\1"), x = filelines)
        writeLines(filelines, filename)
    }
}
