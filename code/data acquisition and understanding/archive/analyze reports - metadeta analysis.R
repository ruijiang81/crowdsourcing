################################################################################
## Analyse Reports - Metadeta Analysis
################################################################################
## Initialization
cat("\014")
rm(list = ls())
source("scripts/load_libraries.R")
sapply(list.files(pattern = "[.]R$", path = "./functions/", full.names = TRUE), source)


################################################################################
## Get the data
################################################################################
### import metadata
metadata_folder <- file.path(getwd(), "reports", "metadata")
metadata <- import.reports(metadata_folder,
    # Remove the "random" rule metadata
    random.rm = TRUE
)

metadata <- aggregate(
    pay ~ batch + repetition + payment_selection_criteria,
    metadata, mean
)
metadata <- arrange(metadata, payment_selection_criteria, repetition, batch)


rules <- unique(metadata$payment_selection_criteria)


################################################################################
## Distribution of payments per repetition (Batch-Wise)
################################################################################
library(manipulate)
library(pander)

manipulate({
    cat("\014")
    rules <- c("delta_auc_div_total_cost", "max_quality", "max_ratio", "max_total_ratio")
    flags <- c(f1, f2, f3, f4)

    sub_dataset <- metadata[metadata$payment_selection_criteria %in% rules[flags], ]
    sub_dataset <- subset(sub_dataset, select = c(batch, pay))

    long_dataset <- table(sub_dataset)

    colnames(long_dataset) <- paste0(colnames(long_dataset), "$")
    barplot(colSums(long_dataset), main = "Payment selection distribution (Batch-Wise)")
    head(long_dataset, n_batch)
},
n_batch = slider(1, max(metadata$batch), 10, step = 1),
f1 = checkbox(F, rules[1]),
f2 = checkbox(F, rules[2]),
f3 = checkbox(T, rules[3]),
f4 = checkbox(F, rules[4])
) # end payment distribution manipulate


################################################################################
## Distribution of payments per repetition (Repetition-Wise)
################################################################################
library(manipulate)
library(pander)

manipulate({
    cat("\014")
    rules <- c("delta_auc_div_total_cost", "max_quality", "max_ratio", "max_total_ratio")
    flags <- c(f1, f2, f3, f4)

    sub_dataset <- metadata[metadata$payment_selection_criteria %in% rules[flags], ]
    sub_dataset <- subset(sub_dataset, select = c(repetition, pay))

    long_dataset <- round(prop.table(table(sub_dataset), 1), 2)

    colnames(long_dataset) <- paste0(colnames(long_dataset), "$")
    barplot(colSums(table(sub_dataset)), main = "Payment selection distribution (Repetition-Wise)")
    print(long_dataset)
    # apply(long_dataset,1,sd)
},
f1 = checkbox(F, rules[1]),
f2 = checkbox(F, rules[2]),
f3 = checkbox(T, rules[3]),
f4 = checkbox(F, rules[4])
) # end payment distribution manipulate


################################################################################
## Discrete Markov Chains
################################################################################
library(markovchain)
manipulate({
    sub_dataset <- metadata[metadata$payment_selection_criteria %in% rule, ]
    ## Create transition matrices
    tm <- list()
    for (r in 1:max(sub_dataset$repetition))
    {
        sequence <- subset(sub_dataset,
            repetition == r,
            select = pay
        )
        sequence <- paste0(t(sequence), "$")
        tm[[r]] <- markovchainFit(data = sequence)$estimate@transitionMatrix
    } # end


    ## Select transition matrix
    if (f_rep) { ## Aggregate all repetitions
        transitionMatrix <- tm[[1]]
        for (r in 2:max(sub_dataset$repetition)) transitionMatrix <- transitionMatrix + tm[[r]]
        transitionMatrix <- transitionMatrix / r
    } else {
        transitionMatrix <- tm[[n_rep]]
    }


    ## Plot Heatmap/State Machine
    if (f_heat) {
        ## Plot Heatmap
        heatmap.2(transitionMatrix,
            dendrogram = "none", Rowv = FALSE, Colv = FALSE,
            trace = "none", cellnote = round(transitionMatrix, 2)
        )
    } else {
        dtmcA <- new("markovchain",
            transitionMatrix = transitionMatrix,
            # states=c(),
            name = "Payment transition matrix"
        ) # create the DTMC
        set.seed(2015)
        plot(dtmcA)
    }


    ## Print Chain
    cat("\nThe probability from row payment into column payment\n")
    round(transitionMatrix, 2)
},
n_rep = slider(1, max(metadata$repetition), 1, "Choose Repetition", 1),
rule = picker("delta_auc_div_total_cost", "max_quality", "max_ratio", "max_total_ratio"),
f_rep = checkbox(FALSE, "All repetition"),
f_heat = checkbox(FALSE, "Plot heatmap?")
) # end HMM manipulation
