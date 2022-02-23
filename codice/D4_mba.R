#### MARKET BASKET ANALYSIS 
#Market Basket Analysis is one of the key techniques used by large retailers to uncover associations between items.
#It works by looking for combinations of items that occur together frequently in transactions.
#To put it another way, it allows retailers to identify relationships between the items that people buy.
#Association Rules are widely used to analyze retail basket or transaction data, and are intended to identify strong rules discovered in transaction data using measures of interestingness, based on the concept of strong rules.



#Load dataset

data_mba <- df_7_tic_clean_final %>% filter(IMPORTO_LORDO > 0) 
data_mba$ID_CLI_TIC_DATETIME <- paste0(data_mba$ID_CLI, "-", data_mba$TIC_DATETIME)
data_mba <- data_mba %>% select(ID_CLI_TIC_DATETIME, ID_ARTICOLO)
data_mba$ID_CLI_TIC_DATETIME <- as.factor(data_mba$ID_CLI_TIC_DATETIME)
data_mba$ID_ARTICOLO <- as.factor(data_mba$ID_ARTICOLO)
write.table(data_mba, file = tmp <- file(), row.names = FALSE)

#Convert data to transaction format for Arules library

itemTransactions <- read.transactions(tmp, format = "single",
                                      header = TRUE, cols = c("ID_CLI_TIC_DATETIME", "ID_ARTICOLO"))
close(tmp)

# Get the rules

rules <- apriori(itemTransactions, parameter = list(supp = 0.001, conf = 0.8))

# Show how many rules have been found and view the top 5 rules

rules
inspect(rules[1:10])