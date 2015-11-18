names_type_pairs = c("age",            "numeric",
                     "workclass",      "factor",
                     "fnlwgt",         "numeric",
                     "education",      "factor",
                     "education-num",  "numeric",
                     "marital-status", "factor",
                     "occupation",     "factor",
                     "relationship",   "factor",
                     "race",           "factor",
                     "sex",            "factor",
                     "capital-gain",   "numeric",
                     "capital-loss",   "numeric",
                     "hours-per-week", "numeric",
                     "native-country", "factor",
                     "income",         "factor")
dataset = read.csv("./data/Adult/dataset.csv", header=FALSE,
                   col.names=names_type_pairs[seq(1,30,2)],
                   colClasses=names_type_pairs[seq(2,30,2)])
rm(names_type_pairs)
