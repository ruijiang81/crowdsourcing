get_the_data <- function(DATABASE_NAME) {
  assertive::is_single_character(DATABASE_NAME)
  switch(tolower(DATABASE_NAME),
    "otto" = source("./data/Otto/import dataset.R"),
    "spam" = {
      library("kernlab")
      data(spam)
      dataset <- spam
    },
    "synthetic_balanced" = source("scripts/generate_balanced_dataset.R"),
    "synthetic_unbalanced" = source("scripts/generate_unbalanced_dataset.R"),
    "tax audit" = source("./data/Tax Audit/import dataset.R"),
    "mushroom" = source("./data/Mushroom/import dataset.R"),
    "adult" = source("./data/Adult/import dataset.R"),
    "pen digits" = source("./data/Pen Digits/import dataset.R"),
    "movies reviews" = source("./data/Movie Review/import dataset.R"),
    stop("Unknown data set")
  )
  dataset <<- setVariablesNames(dataset)
  return(invisible())
}
