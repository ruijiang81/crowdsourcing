################################################################################
#                                    Setup                                     #
################################################################################
#'
#' Configurations
options(digits = 4)
Sys.setlocale("LC_TIME", "English")
set.seed(NULL)
UID <<- paste0(sample(c(letters, 0:9, toupper(letters)), 20, replace = T),
    collapse = ""
)
#' Worst-case execution time
watchdog_simulation <<- as.difftime(24 * 7, units = "hours")
#'
#' Define folder paths
k_path_project <<- getwd()
k_path_code <<- file.path(k_path_project, "code")
k_path_results <<- file.path(k_path_project, "results")
k_path_docs <<- file.path(k_path_project, "docs")
#' Define subfolders paths
## Code
k_path_scripts <<- file.path(k_path_code, "scripts")
k_path_modules <<- file.path(k_path_code, "modules")
k_path_tests <<- file.path(k_path_code, "tests")
k_path_functions <<- file.path(k_path_code, "R")
k_path_analytics <<- file.path(k_path_code, "analytics")
## Results
k_path_reports <<- file.path(k_path_results, "reports")
k_path_ledgers <<- file.path(k_path_results, "ledgers")
k_path_metadata <<- file.path(k_path_results, "metadata")
k_path_temporary <<- file.path(k_path_results, "temp", UID)
k_path_figures <<- file.path(k_path_results, "figures")
## Docs
k_path_dictionaries <<-  file.path(k_path_docs, "dictionaries")
#'
#' Load project's functions
invisible(
    sapply(
        list.files(
            pattern = "[.]R$",
            path = k_path_functions,
            full.names = TRUE
        ),
        source
    )
)
#'
#' Load project's modules
invisible(
    sapply(
        list.files(
            pattern = "[.]R$",
            path = k_path_modules,
            full.names = TRUE
        ),
        source
    )
)
#'
#' Load project's libraries
source(file.path(k_path_scripts, "load-libraries.R"))
#'
#' Create project's folders
folders <- ls()[ls() %>% str_detect("^k_path_")]
for (folder in folders) {
    dir.create(get(folder), show = FALSE, recursive = TRUE)
    if (!base::dir.exists(get(folder))) {
        stop(
            "Couldn't create the following dir:\n",
            get(folder)
        )
    }
}
#'
#' Performs various substitutions in all .R files
# style_dir(k_path_project,
#     exclude_files = c(
#         file.path(k_path_project, "code", "scripts", "setup.R"),
#         file.path(k_path_project, "code", "main.R")),
#     transformers = tidyverse_style(indent_by = 4)
# )
#'
