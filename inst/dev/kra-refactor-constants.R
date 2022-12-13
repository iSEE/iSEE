for (slotname in names(iSEEslots)) {
    fixed_search_string <- paste0("\\.", slotname, "[^A-Za-z]")
    for (filename in list.files("R", full.names = TRUE)) {
        filelines <- readLines(filename)
        hits <- grep(fixed_search_string, filelines)
        if (length(hits)) {
            cat("Search: ", fixed_search_string, "\n", sep = "")
            cat("Filename: ", filename, "\n", sep = "")
            cat(sprintf("%i: %s\n", hits, filelines[hits]), sep = "")
            stop("Fix this!")
        }
    }
}

iSEEslots$sizeByField