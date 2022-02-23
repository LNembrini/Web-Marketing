##MODELLO CHURN
#Churn rate, when applied to a customer base, refers to the proportion of contractual customers or subscribers who leave a supplier during a given time period.
#Churn is directly related to the profitability of a company.
#The more some can learn about customer behaviors, the more profit can be gained.
#This also helps identifying and improving areas or fields where customer service is lacking.
#This aims can be achieved by developing a propensity supervised model

#1. choosing a reference date in the past 
# reference date: 28 febbraio 2019 (next purchase curve df_7)

reference_date <- as.Date("28/02/2019", format = "%d/%m/%Y")

#2. imposing the length of an holdout period after each reference date. The length correspondes to the frequencey of the distribution and/or the purchase time scale 
# holdout period: 28 febbraio 2019 - 30 aprile 2019 of 60 days

holdout_period <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1) %>%
  filter(TIC_DATE >= reference_date) #all NO CHURN
holdout_period['CHURN'] <- 0

#3. choosing the lenght of a lookback period before the reference date 
# lookback period: 01 maggio 2018 - 28 febbraio 2019

lookback_period <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1) %>%
  filter(TIC_DATE < reference_date)
lookback_period['CHURN'] <- 1 #all CHURNERS

# 4. assigning to each customer a target 0/1 variable such that 1 is assigned to customers who churned in the holout period 

holdout_period_temp <- holdout_period %>%
  select(ID_CLI, CHURN)
setDT(lookback_period)[holdout_period_temp, CHURN := i.CHURN, on = .(ID_CLI)]
table(lookback_period$CHURN)

#5. defining a set of potentially relevant predictors variables to be computed within the lookback period 
churn_dataset <- lookback_period
churn_dataset <- churn_dataset %>%
  group_by(ID_CLI) %>%
  dplyr::summarize(FIRST_PURCHASE_DATE = min(TIC_DATE),
                   LAST_PURCHASE_DATE = max(TIC_DATE),
                   TOT_PURCHASE = sum(IMPORTO_LORDO),
                   TOT_SCONTO = sum(SCONTO),
                   TOT_SPESA = TOT_PURCHASE - TOT_SCONTO,
                   NUM_OF_PURCHASES = n_distinct(ID_SCONTRINO),
                   CHURN = names(which.max(table(CHURN)))
  ) 

churn_dataset$CHURN <- as.factor(churn_dataset$CHURN)
churn_dataset$RECENCY<-difftime(as.Date("30/04/2019", format="%d/%m/%Y"), 
                                churn_dataset$LAST_PURCHASE_DATE, units = "days")

churn_dataset <- churn_dataset %>%
  filter(NUM_OF_PURCHASES > 1) #more than one purchase
churn_dataset <- churn_dataset %>%
  filter(FIRST_PURCHASE_DATE < reference_date) #filter for client after 28/02/2019

# MORE FEATURES ADDEDD: 
#df1: LAST_COD_FID, FIRST_ID_NEG
#df2: TYP_CLI_ACCOUNT
#df3: REGION

churn_dataset <- left_join(churn_dataset, df_1_cli_fid_clean[,c("ID_CLI", "LAST_COD_FID", 
                                                                "NUM_FIDs", "FIRST_ID_NEG")], 
                           by = "ID_CLI")  # LAST_COD_FID, FIRST_ID_NEG, NUM_FIDs
churn_dataset$FIRST_ID_NEG <- as.factor(churn_dataset$FIRST_ID_NEG)

churn_dataset <- left_join(churn_dataset, df_2_cli_account_clean[,c("ID_CLI", 
                                                                    "TYP_CLI_ACCOUNT")], 
                           by = "ID_CLI")  # TYP_CLI_ACCOUNT


df_2_df_3 <- left_join(df_2_cli_account_clean, df_3_cli_address_clean, by = "ID_ADDRESS")
churn_dataset <- left_join(churn_dataset, df_2_df_3[,c("ID_CLI", "REGION")], by = "ID_CLI")  # REGION

# GROUP BY REGION
NORD <- c("VALLE D'AOSTA", "PIEMONTE", "LOMBARDIA", "LIGURIA", "FRIULI VENEZIA GIULIA", 
          "VENETO", "TRENTINO ALTO ADIGE", "EMILIA ROMAGNA")
CENTRO <- c("TOSCANA", "MARCHE", "LAZIO", "UMBRIA")
MEZZOGIORNO <- c("ABRUZZO", "MOLISE", "CAMPANIA", "BASILICATA", "PUGLIA", "CALABRIA", 
                 "SICILIA", "SARDEGNA")

nord_regioni <- churn_dataset %>%
  filter(REGION %in% NORD) %>%
  mutate(REGION = "NORD")

centro_regioni <- churn_dataset %>%
  filter(REGION %in% CENTRO) %>%
  mutate(REGION = "CENTRO")

mezzogiorno_regioni <- churn_dataset %>%
  filter(REGION %in% MEZZOGIORNO) %>%
  mutate(REGION = "MEZZOGIORNO")

#remove null
sum(is.na(churn_dataset$REGION))  
churn_dataset <- na.omit(churn_dataset)
sum(is.na(churn_dataset$REGION))  

churn_dataset <- rbind(nord_regioni, centro_regioni, mezzogiorno_regioni)
churn_dataset$REGION <- as.factor(churn_dataset$REGION )

str(churn_dataset)
summary(churn_dataset)
table(churn_dataset$CHURN) 
#0     1 
#39398 74940 

# DELETE NON RELEVANT VARIABLES
churn_dataset$ID_CLI <- NULL
churn_dataset$FIRST_PURCHASE_DATE <- NULL
churn_dataset$LAST_PURCHASE_DATE <- NULL

# PREDICTORS:
# TOT_PURCHASE + TOT_SCONTO + TOT_SPESA + NUM_OF_PURCHASES + RECENCY + REGION + LAST_COD_FID + FIRST_ID_NEG + TYP_CLI_ACCOUNT

var_num <- c("TOT_PURCHASE", "TOT_SCONTO", "TOT_SPESA", "NUM_OF_PURCHASES")
cor(churn_dataset[,var_num]) 
# high correletion between tot_purchase and tot_spesa --> delete tot_spesa

var_num <- c("TOT_PURCHASE", "TOT_SCONTO", "NUM_OF_PURCHASES")
cor(churn_dataset[,var_num])

var_num <- c("TOT_PURCHASE", "NUM_OF_PURCHASES")
cor(churn_dataset[,var_num])

# FINALS PREDICTORS: TOT_PURCHASE + NUM_OF_PURCHASES + RECENCY + REGION + LAST_COD_FID + TYP_CLI_ACCOUNT + FIRST_ID_NEG 

#6. Apply models

train_index <- createDataPartition(churn_dataset$CHURN, 
                                   p = .70, 
                                   list = FALSE, 
                                   times = 1)
train <- churn_dataset[train_index,]
test <- churn_dataset[-train_index,]



# Logistic Regression 
logistic.model <- glm(CHURN ~ TOT_PURCHASE + NUM_OF_PURCHASES + RECENCY + 
                        REGION + LAST_COD_FID + TYP_CLI_ACCOUNT + FIRST_ID_NEG,
                      data = train, family=binomial)
summary(logistic.model)

# Predictions
logistic.prob <- predict(logistic.model, test, type="response") 
qplot(x=logistic.prob, geom="histogram")
logistic.pred <- rep("0", length(logistic.prob))
logistic.pred[logistic.prob > 0.5] <- "1"
logistic.pred <- as.factor(logistic.pred)


# Evaluation
logistic.result <- confusionMatrix(logistic.pred, test$CHURN)

table(Predicted = logistic.pred, Actual = test$CHURN)
#     0     1
#0  4393  2301
#1  7426 20181

logistic.accuracy <- Accuracy(logistic.pred,test$CHURN) # 0.7164223
logistic.precision <- precision(logistic.pred, test$CHURN,relevant = '1') # 0.7310103
logistic.recall <- recall(logistic.pred, test$CHURN,relevant = '1') # 0.8976515
logistic.F1 <- F1_Score(logistic.pred, test$CHURN,positive = '1') # 0.8058057

# ROC
logistic.pr <- prediction(logistic.prob, test$CHURN)
logistic.prf <- performance(logistic.pr, measure = "tpr", x.measure = "fpr")
plot(logistic.prf, main = "ROC LOGISTIC REGRESSION")
abline(a = 0,b = 1,lwd = 2,lty = 3,col = "black")


# AUC 
logistic.auc <- performance(logistic.pr, measure = "auc")
logistic.auc <- logistic.auc@y.values[[1]]
logistic.auc  #0.7452261

#Neural Network Model ####
nm.model <- nnet(CHURN ~ TOT_PURCHASE + NUM_OF_PURCHASES + RECENCY + REGION + 
                   LAST_COD_FID + TYP_CLI_ACCOUNT + FIRST_ID_NEG, data = train, size = 3)

#Predictions
nm.prob <- predict(nm.model, test) 
qplot(x=nm.prob, geom="histogram")
nm.pred <- ifelse(nm.prob > 0.5, 1, 0)
nm.pred <- as.factor(nm.pred)

# Evaluation
nm.result <- confusionMatrix(nm.pred, test$CHURN)

table(Predicted = nm.pred, Actual = test$CHURN)
#  0     1
#0  5027  2834
#1  6792 19648

nm.accuracy <- Accuracy(nm.pred,test$CHURN) # 0.7193668
nm.precision <- precision(nm.pred, test$CHURN,relevant = '1') # 0.7431165
nm.recall <- recall(nm.pred, test$CHURN,relevant = '1') #0.8739436
nm.F1 <- F1_Score(nm.pred, test$CHURN,positive = '1') #  0.8032378


# ROC
nm.pr <- prediction(nm.prob, test$CHURN)
nm.prf <- performance(nm.pr, measure = "tpr", x.measure = "fpr")
plot(nm.prf, main = "ROC NEURAL NETWORK MODEL")
abline(a = 0,b = 1,lwd = 2,lty = 3,col = "black")


# AUC 
nm.auc <- performance(nm.pr, measure = "auc")
nm.auc <- nm.auc@y.values[[1]]
nm.auc  # 0.7456155

#Decision Trees 

#Fitting The Model

tree.model <- rpart(CHURN ~ TOT_PURCHASE + NUM_OF_PURCHASES + RECENCY + REGION + 
                      LAST_COD_FID + TYP_CLI_ACCOUNT + FIRST_ID_NEG,  
                    data = train, method = "class")

rpart.plot(tree.model, extra = 1)  #RECENCY MOST IMPORTANT
summary(tree.model) 

#Predictions
tree.pred <- predict(tree.model, test, type = "class")  # on test set
tree.prob <- predict(tree.model, test, type="prob")
qplot(x=tree.prob[, "1"], geom="histogram")

#Evaluation
tree.result <- confusionMatrix(tree.pred, test$CHURN)

table(Predicted = tree.pred, Actual = test$CHURN)
#0     1
#0  5625  3593
#1  6194 18889

tree.accuracy <- Accuracy(tree.pred, test$CHURN) # 0.714673
tree.precision <- precision(tree.pred, test$CHURN,relevant = '1') # 0.7530598
tree.recall <- recall(tree.pred, test$CHURN,relevant = '1') # 0.8401833
tree.F1 <- F1_Score(tree.pred, test$CHURN,positive = '1') # 0.7942395


# ROC
tree.pr <- prediction(tree.prob[,2], test$CHURN)
tree.prf <- performance(tree.pr, measure = "tpr", x.measure = "fpr")
plot(tree.prf, main = "ROC DECISION TREE")
abline(a = 0,b = 1,lwd = 2,lty = 3,col = "black")

# AUC value 
tree.auc <- performance(tree.pr, measure = "auc")
tree.auc <- tree.auc@y.values[[1]]
tree.auc  # 0.6905359


#Random Forest 
rf.model <- randomForest(CHURN ~  TOT_PURCHASE + NUM_OF_PURCHASES + RECENCY + 
                           REGION + LAST_COD_FID + TYP_CLI_ACCOUNT + FIRST_ID_NEG,
                         data = train , ntree = 100)
print(rf.model)
plot(rf.model)  
varImpPlot(rf.model, sort=T, n.var = 4, main = 'Features Importance') #RECENCY

#Predictions
rf.pred <- predict(rf.model, test, type = "class")  
rf.prob <- predict(rf.model, test, type="prob")
qplot(x=rf.prob[, "1"], geom="histogram")

# Evaluation
rf.result <- confusionMatrix(rf.pred, test$CHURN)

table(Predicted = rf.pred, Actual = test$CHURN)
#    0     1
#0  4543  2374
#1  7276 20108

rf.accuracy <- Accuracy(rf.pred,test$CHURN) # 0.7186671
rf.precision <- precision(rf.pred, test$CHURN,relevant = '1') # 0.7342974
rf.recall <- recall(rf.pred, test$CHURN,relevant = '1') # 0.8944044
rf.F1 <- F1_Score(rf.pred, test$CHURN,positive = '1') # 0.8064814

# ROC
rf.pr <- prediction(rf.prob[,2], test$CHURN)
rf.prf <- performance(rf.pr, measure = "tpr", x.measure = "fpr")
plot(rf.prf, main = "ROC RANDOM FOREST")
abline(a = 0,b = 1,lwd = 2,lty = 3,col = "black")


# AUC 
rf.auc <- performance(rf.pr, measure = "auc")
rf.auc <- rf.auc@y.values[[1]]
rf.auc  # 0.736105

# Naive Bayes 
nb.model <- naiveBayes(CHURN ~ TOT_PURCHASE + NUM_OF_PURCHASES + RECENCY + 
                         REGION + LAST_COD_FID + TYP_CLI_ACCOUNT + FIRST_ID_NEG, 
                       data = train)
summary(nb.model)

# Predictions
nb.pred <- predict(nb.model, test, type = "class")  
nb.prob <- predict(nb.model, test, type = "raw")
qplot(x=nb.prob[, "1"], geom="histogram")

# Evaluation
nb.result <- confusionMatrix(nb.pred, test$CHURN)

table(Predicted = nb.pred, Actual = test$CHURN)
#    0     1
#0  3265  1595
#1  8554 20887

nb.accuracy <- Accuracy(nb.pred,test$CHURN) 
nb.precision <- precision(nb.pred, test$CHURN,relevant = '1') 
nb.recall <- recall(nb.pred, test$CHURN,relevant = '1') 
nb.F1 <- F1_Score(nb.pred, test$CHURN,positive = '1') 


# ROC
nb.pr <- prediction(nb.prob[, "1"], test$CHURN)
nb.prf <- performance(nb.pr, measure = "tpr", x.measure = "fpr")
plot(nb.prf, main = "ROC NAIVE BAYES")
abline(a = 0,b = 1,lwd = 2,lty = 3,col = "black")


# AUC 
nb.auc <- performance(nb.pr, measure = "auc")
nb.auc <- nb.auc@y.values[[1]]
nb.auc  # 0.7339023


#### WHICH MODEL IS THE BEST?
Modello <- c("Decision Tree", "Random Forest", "Logistic Regression", 
             "Neural Network Model", "Naive Bayes")

# ACCURACY
tree.accuracy
rf.accuracy
logistic.accuracy
nm.accuracy
nb.accuracy

Accuracy <- c(tree.accuracy,
              rf.accuracy,
              logistic.accuracy,
              nm.accuracy,
              nb.accuracy)
Accuracy_results <- data.frame(Modello, Accuracy)

# PRECISION
tree.precision
rf.precision
logistic.precision
nb.precision

Precision <- c(tree.precision,
               rf.precision,
               logistic.precision,
               nm.precision,
               nb.precision)
Precision_results <- data.frame(Modello, Precision)

# RECALL
tree.recall
rf.recall
logistic.recall
nb.recall

Recall <- c(tree.recall,
            rf.recall,
            logistic.recall,
            nm.recall,
            nb.recall)
Recall_results <- data.frame(Modello, Recall)

# F1
tree.F1
rf.F1
logistic.F1
nb.F1

F1_score <- c(tree.F1,
              rf.F1,
              logistic.F1,
              nm.F1,
              nb.F1)
F1_score_results <- data.frame(Modello, F1_score)


# AUC
tree.auc
rf.auc
logistic.auc
nb.auc


AUC <- c(tree.auc,
         rf.auc,
         logistic.auc,
         nm.auc,
         nb.auc)
AUC_results <- data.frame(Modello, AUC)

# overview risultati
overview_results <- data.frame(Modello,  Accuracy_results$Accuracy, 
                               Precision_results$Precision, Recall_results$Recall, 
                               F1_score_results$F1_score, AUC_results$AUC)
colnames(overview_results) <- c("Modello", "Accuracy","Precision", "Recall", 
                                "F1_score", "AUC")
#View(overview_results)

#1	Decision Tree	0.7146730	0.7530598	0.8401833	0.7942395	0.6905359
#2	Random Forest	0.7186671	0.7342974	0.8944044	0.8064814	0.7361050
#3	Logistic Regression	0.7164223	0.7310103	0.8976515	0.8058057	0.7452261
#4	Neural Network Model	0.7193668	0.7431165	0.8739436	0.8032378	0.7456155
#5	Naive Bayes	0.7041194	0.7094528	0.9290544	0.8045375	0.7339023

hist(overview_results$Accuracy, col = "red")
ggplot(overview_results, aes(x = Modello, y = Accuracy, label = round(Accuracy, digits = 3))) +
  geom_bar(stat="identity") + 
  geom_col(position = 'dodge', fill="red") +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5, 
            size = 4)

# ROC
preds_list <- list(tree.prob[,2], logistic.prob, rf.prob[,2], nm.prob, nb.prob[,2])

m <- length(preds_list)
actuals_list <- rep(list(test$CHURN), m)

pred <- prediction(preds_list, actuals_list)
rocs <- performance(pred, "tpr", "fpr")

par(lwd= 4, lty= 6)
plot(rocs, col = as.list(1:m), main = "ROC Curves")
legend(x = "bottomright", 
       legend = c("Decision Tree", "Logistic", "Random Forest", "Neural Network Model", 
                  "Naive Bayes"),
       fill = 1:m)
abline(a=0, b= 1, col=c("grey"))


#BEST MODELS:
#RANDOM FOREST
#NN
#LOGISTIC