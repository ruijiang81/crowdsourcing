################################################################################
## Analyse Reports - Mean ans SD
################################################################################
## Initialization
rm(list = ls())
cat("\014")
successful_runs <- 0


## Source Path
Analysis_Name <- "Mean and Sd"
repository_path <- file.path("C:", "Dropbox", "Research", "economic labelling", "shared_results")


## Scan all .csv files in the reposetory
csv_path <- list.files(repository_path, full.names = TRUE, recursive = TRUE, pattern = ".*\\.csv")

for (k in 1:length(csv_path)) { # length(csv_path)
    tryCatch({
        ## Load a csv file
        report <- read.csv(csv_path[k])


        ## Perform analysis
        report_agg <- aggregate(subset_AUC ~ pay + batch,
            data = report,
            function(x) c("Mean" = mean(x), "SD" = sd(x))
        )


        ## Extract the sub string to the metadata data frame
        ### Extract Folder from path
        index_metadata <- gregexpr("^(.*?)\\(", csv_path[k], TRUE)
        match_start <- index_metadata[[1]][1]
        match_length <- attributes(index_metadata[[1]])$match.length[1]
        file_folder <- substr(csv_path[k], match_start, match_start + match_length - 2)
        ### Extract Name from path
        index_metadata <- gregexpr("\\((.*?)\\)", csv_path[k], TRUE)
        file_name <- c()
        for (l in 1:length(index_metadata[[1]])) {
            match_start <- index_metadata[[1]][l]
            match_length <- attributes(index_metadata[[1]])$match.length[l]
            Word <- substr(
                csv_path[k],
                match_start + 1,
                match_start + match_length - 2
            )
            file_name <- paste0(file_name, "(", Word, ")")
        } # end Phrase
        file_name <- paste0(file_name, ".csv")


        ## Store analysis
        store_path <- file.path(file_folder, Analysis_Name)
        dir.create(store_path, showWarnings = F)
        write.csv(report_agg, file.path(store_path, file_name), row.names = F)
        successful_runs <- successful_runs + 1
    }, error = function(cond) {
        cat("\nFAILED", csv_path[k])
        return()
    }) # end try catch
} # end for

successful_runs
