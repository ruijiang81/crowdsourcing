dataset = subset(read.csv('./data/Tax Audit/dataset.csv', header=TRUE), 
                 select=c(-Revenue,-ACTIVE))
