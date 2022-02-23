#################
#### RFM ANALYSIS

#Recency, Frequency and *Monetary* value is a marketing analysis tool used to identify a company's or an organization's best customers by using certain measures.
#The RFM model is based on three quantitative factors:
  
#Recency: How recently a customer has made a purchase;
#Frequency: How often a customer makes a purchase;
#Monetary Value: How much money a customer spends on purchases.

#The output of the RFM model is primarily used to differentiate the marketing actions accros the customer base. 
#Typical applications are:
  
#Investing more in marketing care actions for the best customers;
#Identifying the most cost efficcient marketing actions to increase the value of medium-value customers;
#Save budget on marketing actions for low-value customers.


# SOGLIA oltre la quale si classificano i clienti come ATTIVI
# si hanno dati dal 01/05/2018 al 30/04/2019
# reference date il 01/01/2019

rfm_data<-df_7_tic_clean_final  %>%
  filter(TIC_DATE > as.Date("01/01/2019", format = "%d/%m/%Y"))
rfm_data

#### RECENCY 

recency_data<-rfm_data %>%
  filter(DIREZIONE==1) %>%
  group_by(ID_CLI) %>%
  summarise(LAST_PURCHASE_DATE=max(TIC_DATE))

recency_data
recency_data$RECENCY_VALUE <- difftime(as.Date("30/04/2019",
                                               format="%d/%m/%Y"),
                                       recency_data$LAST_PURCHASE_DATE,
                                       units = "days")
recency_data$RECENCY_VALUE <- as.numeric(recency_data$RECENCY_VALUE, units="days")
recency_data

#### FREQUENCY
frequency_data <- rfm_data %>%
  filter(DIREZIONE == 1) %>% 
  group_by(ID_CLI)  %>% 
  summarise(FREQUENCY_VALUE = n_distinct(ID_SCONTRINO)) %>%
  arrange(desc(FREQUENCY_VALUE))
frequency_data$FREQUENCY_VALUE <- as.numeric(frequency_data$FREQUENCY_VALUE)

frequency_data

#### MONETARY 

monetary_data <- rfm_data %>%
  filter(DIREZIONE == 1) %>% 
  group_by(ID_CLI) %>% 
  dplyr::summarize(IMPORTO_LORDO = sum(IMPORTO_LORDO),
                   SCONTO = sum(SCONTO),
                   MONETARY_VALUE = IMPORTO_LORDO - SCONTO) %>%
  ungroup() %>%
  as.data.frame() %>%
  arrange(desc(IMPORTO_LORDO))
monetary_data$MONETARY_VALUE <- as.numeric(monetary_data$MONETARY_VALUE)

monetary_data


#### RFM

rfm_data_clean <- merge(frequency_data,
                        monetary_data,  
                        by = "ID_CLI") 

rfm_data_clean <- merge(rfm_data_clean,           
                        recency_data,  
                        by = "ID_CLI")

sum(is.na(rfm_data_clean)) 
rfm_data_clean <- rfm_data_clean[,c(1,2,5,7)]


hist(as.numeric(rfm_data_clean$RECENCY_VALUE), main = "Distribution RECENCY", col='orange', xlab="Recency")
hist(as.numeric(rfm_data_clean$FREQUENCY_VALUE), main = "Distribution FREQUENCY", col='orange', xlab="Frequency")
hist(as.numeric(rfm_data_clean$MONETARY_VALUE), main = "Distribution MONETARY", col='orange', xlab="Monetary")

#### RECENCY CLASSIFICATION

summary(rfm_data_clean$RECENCY_VALUE)  # si osservano i valori dei quantili per determinare i punti di taglio
quantile(rfm_data_clean$RECENCY_VALUE)
#0%  25%  50%  75% 100% 
#0   19   40   72  118 

rfm_data_clean$RECENCY_CLASS <- 0
rfm_data_clean$RECENCY_CLASS[rfm_data_clean$RECENCY_VALUE <= 19.00] <- "low"
rfm_data_clean$RECENCY_CLASS[rfm_data_clean$RECENCY_VALUE > 19.00 & rfm_data_clean$RECENCY_VALUE <= 72.00] <- "medium"
rfm_data_clean$RECENCY_CLASS[rfm_data_clean$RECENCY_VALUE > 72.00] <- "high"

rfm_data_clean %>% 
  group_by(RECENCY_CLASS) %>%
  summarise(Count = n())

recency_var <- as.data.frame(table(rfm_data_clean$RECENCY_CLASS))
target <- c("low", "medium", "high")
recency_var <- recency_var[match(target, recency_var$Var1),]
recency_var$Var1 <- factor(recency_var$Var1, levels = recency_var$Var1)

ggplot(data = recency_var,
       aes( x=Var1, y=Freq
       )) +                       
  geom_bar(stat = "identity", fill="orange") +                  
  labs(x     = "Recency classes",
       y     = "Total Purchase") +               
  theme_classic() +                             
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = function(x){paste0(x/1000, 'K')}) +
  scale_x_discrete(labels = c("Low", "Medium", "High")) + 
  guides(fill = FALSE) +
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.25)

#MEDIUM CLASS IS THE MOST PRESENT IN THE DATA


#### FREQUENCY CLASSIFICATION

summary(rfm_data_clean$FREQUENCY_VALUE)  
quantile(rfm_data_clean$FREQUENCY_VALUE)
#0%  25%  50%  75% 100% 
#1    1    2    3  101 

rfm_data_clean$FREQUENCY_CLASS <- 0
rfm_data_clean$FREQUENCY_CLASS[rfm_data_clean$RECENCY_VALUE <= 2] <- "low"
rfm_data_clean$FREQUENCY_CLASS[rfm_data_clean$RECENCY_VALUE > 2 & rfm_data_clean$RECENCY_VALUE <= 5] <- "medium"
rfm_data_clean$FREQUENCY_CLASS[rfm_data_clean$RECENCY_VALUE > 5] <- "high"

rfm_data_clean %>%
  group_by(FREQUENCY_CLASS) %>%
  summarise(Count = n())

frequency_var <- as.data.frame(table(rfm_data_clean$FREQUENCY_CLASS))
target <- c("low", "medium", "high")
frequency_var <- frequency_var[match(target, frequency_var$Var1),]
frequency_var$Var1 <- factor(frequency_var$Var1, levels = frequency_var$Var1)

ggplot(data = frequency_var,
       aes(x = Var1, y = Freq,
           fill = Freq)) +                       
  geom_bar(stat = "identity", fill="orange") +                 
  labs(x     = "Frequency Type",
       y     = "Total Purchases") +               
  theme_classic() +                             
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(labels = c("Low", "Medium", "High")) + 
  scale_y_continuous(labels = function(x){paste0(x/1000, 'K')}) +
  guides(fill = FALSE) +
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.25)

# MOST CLIENTS HAVE HIGH FREQUENCY



#### MONETARY CLASSIFICATION
summary(rfm_data_clean$MONETARY_VALUE)
quantile(rfm_data_clean$MONETARY_VALUE)
#0%         25%         50%         75%        100% 
#0.0000     39.3425    113.1300    319.7500 218480.2000 

rfm_data_clean$MONETARY_CLASS <- 0
rfm_data_clean$MONETARY_CLASS[rfm_data_clean$MONETARY_VALUE <= 39.34] <- "low"
rfm_data_clean$MONETARY_CLASS[rfm_data_clean$MONETARY_VALUE > 39.34 & rfm_data_clean$MONETARY_VALUE <= 319.75] <- "medium"
rfm_data_clean$MONETARY_CLASS[rfm_data_clean$MONETARY_VALUE > 319.75] <- "high"


rfm_data_clean %>% 
  group_by(MONETARY_CLASS) %>%
  summarise(Count = n())

monetary_var <- as.data.frame(table(rfm_data_clean$MONETARY_CLASS))
target <- c("low", "medium", "high")
monetary_var <- monetary_var[match(target, monetary_var$Var1),]
monetary_var$Var1 <- factor(monetary_var$Var1, levels = monetary_var$Var1)

ggplot(data = monetary_var,
       aes(x = Var1, y = Freq,
           fill = Freq)) +                       
  geom_bar(stat = "identity", fill="orange") +                  
  scale_colour_brewer(palette = "Spectral") +
  labs(x     = "Monetary Classes",
       y     = "Total Amount") +                  
  theme_classic() +                               
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(labels = c("Low", "Medium", "High")) +
  scale_y_continuous(labels = function(x){paste0(x/1000, 'K')}) +
  guides(fill = FALSE) +
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.25)

# MEDIUM IN MONETARY IS THE MOST PRESENT

#### RFM

rfm_data_clean$RECENCY_FREQUENCY <- NA
for(i in c(1:nrow(rfm_data_clean))){
  if(rfm_data_clean$RECENCY_CLASS[i] == "low" && rfm_data_clean$FREQUENCY_CLASS[i] == "low") rfm_data_clean$RECENCY_FREQUENCY[i] <- "One-Timer"
  if(rfm_data_clean$RECENCY_CLASS[i] == "medium" && rfm_data_clean$FREQUENCY_CLASS[i] == "low") rfm_data_clean$RECENCY_FREQUENCY[i] <- "One-Timer"
  if(rfm_data_clean$RECENCY_CLASS[i] == "high" && rfm_data_clean$FREQUENCY_CLASS[i] == "low") rfm_data_clean$RECENCY_FREQUENCY[i] <- "Leaving"
  if(rfm_data_clean$RECENCY_CLASS[i] == "low" && rfm_data_clean$FREQUENCY_CLASS[i] == "medium") rfm_data_clean$RECENCY_FREQUENCY[i] <- "Engaged"
  if(rfm_data_clean$RECENCY_CLASS[i] == "medium" && rfm_data_clean$FREQUENCY_CLASS[i] == "medium") rfm_data_clean$RECENCY_FREQUENCY[i] <- "Engaged"
  if(rfm_data_clean$RECENCY_CLASS[i] == "high" && rfm_data_clean$FREQUENCY_CLASS[i] == "medium") rfm_data_clean$RECENCY_FREQUENCY[i] <- "Leaving"
  if(rfm_data_clean$RECENCY_CLASS[i] == "low" && rfm_data_clean$FREQUENCY_CLASS[i] == "high") rfm_data_clean$RECENCY_FREQUENCY[i] <- "Top"
  if(rfm_data_clean$RECENCY_CLASS[i] == "medium" && rfm_data_clean$FREQUENCY_CLASS[i] == "high") rfm_data_clean$RECENCY_FREQUENCY[i] <- "Top"
  if(rfm_data_clean$RECENCY_CLASS[i] == "high" && rfm_data_clean$FREQUENCY_CLASS[i] == "high") rfm_data_clean$RECENCY_FREQUENCY[i] <- "Leaving Top"
}


low_low_count <- nrow(subset(rfm_data_clean,RECENCY_CLASS == "low" & FREQUENCY_CLASS == "low"))  # "One-Timer"
medium_low_count <- nrow(subset(rfm_data_clean,RECENCY_CLASS == "medium" & FREQUENCY_CLASS == "low")) # "One-Timer"
high_low_count <- nrow(subset(rfm_data_clean,RECENCY_CLASS == "high" & FREQUENCY_CLASS == "low"))  # "Leaving"
low_medium_count <- nrow(subset(rfm_data_clean,RECENCY_CLASS == "low" & FREQUENCY_CLASS == "medium"))  # "Engaged"
medium_medium_count <- nrow(subset(rfm_data_clean,RECENCY_CLASS == "medium" & FREQUENCY_CLASS == "medium"))  #"Engaged"
high_medium_count <- nrow(subset(rfm_data_clean,RECENCY_CLASS == "high" & FREQUENCY_CLASS == "medium"))  # "Leaving"
low_high_count <- nrow(subset(rfm_data_clean,RECENCY_CLASS == "low" & FREQUENCY_CLASS == "high"))  # "Top"
medium_high_count <- nrow(subset(rfm_data_clean,RECENCY_CLASS == "medium" & FREQUENCY_CLASS == "high"))  # "Top"
high_high_count <- nrow(subset(rfm_data_clean,RECENCY_CLASS == "high" & FREQUENCY_CLASS == "high"))  # "Leaving Top"

rfm_data_clean %>% 
  group_by(RECENCY_FREQUENCY) %>%
  summarise(Count = n())

table(rfm_data_clean$RECENCY_FREQUENCY)

recency_frequency_var <- as.data.frame(table(rfm_data_clean$RECENCY_FREQUENCY))
temp <- data.frame("Var1" = "Leaving", "Freq" = 0)  
recency_frequency_var <- rbind(recency_frequency_var[1,], temp, recency_frequency_var[2:4,])
rownames(recency_frequency_var) <- 1:nrow(recency_frequency_var)
recency_frequency_var <- recency_frequency_var[c(5,3,1,2,4),]
rownames(recency_frequency_var) <- 1:nrow(recency_frequency_var)
recency_frequency_var$Var1 <- factor(recency_frequency_var$Var1, levels = c("Top", "Leaving Top", "Engaged", "Leaving", "One-Timer"))

ggplot(data = recency_frequency_var,
       aes(x = Var1, y = Freq,
           fill = Freq)) +                       
  geom_bar(stat = "identity", fill="orange") +                  
  scale_colour_brewer(palette = "Spectral") +
  labs(x     = "Recency Frequency",
       y     = "Total Amount") +                  
  theme_classic() +                               
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(labels = c("Top", "Leaving Top", "Engaged", "Leaving", "One-Timer")) + 
  scale_y_continuous(labels = function(x){paste0(x/1000, 'K')}) +
  guides(fill = FALSE)+
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.25) #most clients are TOP


#DIVISION IN

# One-Timer 
# Leaving
# Engaged
# Top
# Leaving Top



recency_frequency_df <- as.data.frame(rbind(c("Top",         "High",   "Low",    low_high_count),
                                            c("Top",         "High",   "Medium", medium_high_count),
                                            c("Leaving Top", "High",   "High",   high_high_count),
                                            c("Engaged",     "Medium", "Low",    low_medium_count),
                                            c("Engaged",     "Medium", "Medium", medium_medium_count),
                                            c("Leaving",     "Medium", "High",   high_medium_count),
                                            c("One Timer",   "Low",    "Low",    low_low_count),
                                            c("One Timer",   "Low",    "Medium", medium_low_count),
                                            c("Leaving",     "Low",    "High",   high_low_count)))

colnames(recency_frequency_df) <-  c("Level", "Frequency", "Recency", "Value")

recency_frequency_df$Frequency <- factor(recency_frequency_df$Frequency,
                                         levels = c("High", "Medium", "Low"))

recency_frequency_df$Recency <- factor(recency_frequency_df$Recency,
                                       levels = c("High", "Medium", "Low"))

recency_frequency_df$Value <- as.numeric(recency_frequency_df$Value)

ggplot(recency_frequency_df, aes(x = Frequency, y = Recency, fill = Value)) + 
  geom_tile() +
  geom_text(aes(label = Level)) +
  scale_fill_distiller(palette = "Spectral") +
  theme_minimal()


#### RECENCY E FREQUENCY with MONETARY 

rfm_data_clean$RECENCY_FREQUENCY_MONETARY <- NA

for(i in c(1:nrow(rfm_data_clean))){
  if(rfm_data_clean$RECENCY_FREQUENCY[i] == "Top" && rfm_data_clean$MONETARY_CLASS[i] == "low") rfm_data_clean$RECENCY_FREQUENCY_MONETARY[i] <- "Silver"
  if(rfm_data_clean$RECENCY_FREQUENCY[i] == "Leaving Top" && rfm_data_clean$MONETARY_CLASS[i] == "low") rfm_data_clean$RECENCY_FREQUENCY_MONETARY[i] <- "Bronze"
  if(rfm_data_clean$RECENCY_FREQUENCY[i] == "Engaged" && rfm_data_clean$MONETARY_CLASS[i] == "low") rfm_data_clean$RECENCY_FREQUENCY_MONETARY[i] <- "Copper"
  if(rfm_data_clean$RECENCY_FREQUENCY[i] == "Leaving" && rfm_data_clean$MONETARY_CLASS[i] == "low") rfm_data_clean$RECENCY_FREQUENCY_MONETARY[i] <- "Tin"
  if(rfm_data_clean$RECENCY_FREQUENCY[i] == "One-Timer" && rfm_data_clean$MONETARY_CLASS[i] == "low") rfm_data_clean$RECENCY_FREQUENCY_MONETARY[i] <- "Cheap"
  
  
  if(rfm_data_clean$RECENCY_FREQUENCY[i] == "Top" && rfm_data_clean$MONETARY_CLASS[i] == "medium") rfm_data_clean$RECENCY_FREQUENCY_MONETARY[i] <- "Gold"
  if(rfm_data_clean$RECENCY_FREQUENCY[i] == "Leaving Top" && rfm_data_clean$MONETARY_CLASS[i] == "medium") rfm_data_clean$RECENCY_FREQUENCY_MONETARY[i] <- "Silver"
  if(rfm_data_clean$RECENCY_FREQUENCY[i] == "Engaged" && rfm_data_clean$MONETARY_CLASS[i] == "medium") rfm_data_clean$RECENCY_FREQUENCY_MONETARY[i] <- "Bronze"
  if(rfm_data_clean$RECENCY_FREQUENCY[i] == "Leaving" && rfm_data_clean$MONETARY_CLASS[i] == "medium") rfm_data_clean$RECENCY_FREQUENCY_MONETARY[i] <- "Copper"
  if(rfm_data_clean$RECENCY_FREQUENCY[i] == "One-Timer" && rfm_data_clean$MONETARY_CLASS[i] == "medium") rfm_data_clean$RECENCY_FREQUENCY_MONETARY[i] <- "Tin"
  
  
  if(rfm_data_clean$RECENCY_FREQUENCY[i] == "Top" && rfm_data_clean$MONETARY_CLASS[i] == "high") rfm_data_clean$RECENCY_FREQUENCY_MONETARY[i] <- "Diamond"
  if(rfm_data_clean$RECENCY_FREQUENCY[i] == "Leaving Top" && rfm_data_clean$MONETARY_CLASS[i] == "high") rfm_data_clean$RECENCY_FREQUENCY_MONETARY[i] <- "Gold"
  if(rfm_data_clean$RECENCY_FREQUENCY[i] == "Engaged" && rfm_data_clean$MONETARY_CLASS[i] == "high") rfm_data_clean$RECENCY_FREQUENCY_MONETARY[i] <- "Silver"
  if(rfm_data_clean$RECENCY_FREQUENCY[i] == "Leaving" && rfm_data_clean$MONETARY_CLASS[i] == "high") rfm_data_clean$RECENCY_FREQUENCY_MONETARY[i] <- "Bronze"
  if(rfm_data_clean$RECENCY_FREQUENCY[i] == "One-Timer" && rfm_data_clean$MONETARY_CLASS[i] == "high") rfm_data_clean$RECENCY_FREQUENCY_MONETARY[i] <- "Copper"
  
}


top_low_count <- nrow(subset(rfm_data_clean,RECENCY_FREQUENCY == "Top" & MONETARY_CLASS == "low"))  # "Silver"
leavingtop_low_count <- nrow(subset(rfm_data_clean,RECENCY_FREQUENCY == "Leaving Top" & MONETARY_CLASS == "low"))  # "Bronze"
engaged_low_count <- nrow(subset(rfm_data_clean,RECENCY_FREQUENCY == "Engaged" & MONETARY_CLASS == "low"))  # "Copper"
leaving_low_count <- nrow(subset(rfm_data_clean,RECENCY_FREQUENCY == "Leaving" & MONETARY_CLASS == "low"))  # "Tin"
onetimer_low_count <- nrow(subset(rfm_data_clean,RECENCY_FREQUENCY == "One-Timer" & MONETARY_CLASS == "low"))  # "Cheap"

top_medium_count <- nrow(subset(rfm_data_clean,RECENCY_FREQUENCY == "Top" & MONETARY_CLASS == "medium"))  # "Gold"
leavingtop_medium_count <- nrow(subset(rfm_data_clean,RECENCY_FREQUENCY == "Leaving Top" & MONETARY_CLASS == "medium"))  # "Silver"
engaged_medium_count <- nrow(subset(rfm_data_clean,RECENCY_FREQUENCY == "Engaged" & MONETARY_CLASS == "medium"))  # "Bronze"
leaving_medium_count <- nrow(subset(rfm_data_clean,RECENCY_FREQUENCY == "Leaving" & MONETARY_CLASS == "medium"))  # "Copper"
onetimer_medium_count <- nrow(subset(rfm_data_clean,RECENCY_FREQUENCY == "One-Timer" & MONETARY_CLASS == "medium"))  # "Tin"

top_high_count <- nrow(subset(rfm_data_clean,RECENCY_FREQUENCY == "Top" & MONETARY_CLASS == "high"))  # "Diamond"
leavingtop_high_count <- nrow(subset(rfm_data_clean,RECENCY_FREQUENCY == "Leaving Top" & MONETARY_CLASS == "high"))  # "Gold"
enaged_high_count <- nrow(subset(rfm_data_clean,RECENCY_FREQUENCY == "Engaged" & MONETARY_CLASS == "high"))  # "Silver"
leaving_high_count <- nrow(subset(rfm_data_clean,RECENCY_FREQUENCY == "Leaving" & MONETARY_CLASS == "high"))  # "Bronze"
onetimer_high_count <- nrow(subset(rfm_data_clean,RECENCY_FREQUENCY == "One-Timer" & MONETARY_CLASS == "high"))  # "Copper"

rfm_data_clean %>% 
  group_by(RECENCY_FREQUENCY_MONETARY) %>%
  summarise(Count = n())


recency_frequency_monetary_var <- as.data.frame(table(rfm_data_clean$RECENCY_FREQUENCY_MONETARY))

ggplot(data = recency_frequency_monetary_var,
       aes(x = Var1, y = Freq,
           fill = Freq)) +                       
  geom_bar(stat = "identity", fill="orange") +                  
  scale_colour_brewer(palette = "Spectral") +
  labs(x     = "Recency Frequency Monetary",
       y     = "Total Amount") +                  
  theme_classic() +                               
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(labels = c("Bronze", "Cheap", "Copper", "Diamond", "Gold", "Silver", "Tin")) + 
  scale_y_continuous(labels = function(x){paste0(x/1000, 'K')}) +
  guides(fill = FALSE)+
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.25)

# MOST FREQUENT ARE Gold or Silver

recency_frequency_monetary_df <- as.data.frame(rbind(c("Top", "High", "Diamond", top_high_count),
                                                     c("Top", "Medium", "Gold", top_medium_count),
                                                     c("Top", "Low", "Silver", top_low_count),
                                                     c("Leaving Top", "High", "Gold", leavingtop_high_count),
                                                     c("Leaving Top", "Medium", "Silver", leavingtop_medium_count),
                                                     c("Leaving Top", "Low", "Bronze", leavingtop_low_count),
                                                     c("Engaged", "High", "Silver", enaged_high_count),
                                                     c("Engaged", "Medium", "Bronze", engaged_medium_count),
                                                     c("Engaged", "Low", "Copper", engaged_low_count),
                                                     c("Leaving", "High", "Bronze", leaving_high_count),
                                                     c("Leaving", "Medium", "Copper", leaving_medium_count),
                                                     c("Leaving", "Low", "Tin", leaving_low_count),
                                                     c("One Timer", "High", "Copper", onetimer_high_count),
                                                     c("One Timer", "Medium", "Tin", onetimer_medium_count),
                                                     c("One Timer", "Low", "Cheap", onetimer_low_count)))

colnames(recency_frequency_monetary_df) <- c("RF", "Monetary", "Level", "Value")

recency_frequency_monetary_df$RF <- factor(recency_frequency_monetary_df$RF,
                                           levels = c("Top", "Leaving Top",
                                                      "Engaged", "Leaving", "One Timer"))

recency_frequency_monetary_df$Monetary <- factor(recency_frequency_monetary_df$Monetary,
                                                 levels = c("Low", "Medium", "High"))

recency_frequency_monetary_df$Value <- as.numeric(recency_frequency_monetary_df$Value)

ggplot(recency_frequency_monetary_df, aes(x = RF, y = Monetary, fill = Value)) + 
  geom_tile() +
  geom_text(aes(label = Level)) +
  scale_fill_distiller(palette = "Spectral") +
  theme_minimal()

slices <- c(14011,   780,  3544, 18808, 40065, 33490,  2408)
lbls <- c("Bronze",  "Cheap",   "Copper",  "Diamond", "Gold",    "Silver",  "Tin" )
slices_percent <- round(100*slices/sum(slices), 1)
slices_percent = paste(lbls, " ", slices_percent,"%")
pie3D(slices,labels=slices_percent,explode=0.1,
      main="Pie Chart of Countries ") #GOLD AND SILVER





#ALTERNATIVE APPROACH USING RFM LIBRARY


rfm_data_auto <- rfm_data %>%
  mutate(REVENUE = IMPORTO_LORDO - SCONTO)

rfm_result <- rfm_table_order(
  data = rfm_data_auto,
  customer_id = ID_CLI,
  revenue = REVENUE,
  order_date = TIC_DATE, 
  analysis_date = as.Date("2019-04-30") 
)

rfm_heatmap(rfm_result) 
rfm_bar_chart(rfm_result)
rfm_rm_plot(rfm_result) 
rfm_fm_plot(rfm_result)

# CATEGORIES
segment_names <- c("Champions", "Loyal Customers", "Potential Loyalist",
                   "New Customers", "Promising", "Need Attention", "About To Sleep",
                   "At Risk", "Can't Lose Them", "Lost")

recency_lower <- c(4, 2, 3, 4, 3, 2, 2, 1, 1, 1)
recency_upper <- c(5, 5, 5, 5, 4, 3, 3, 2, 1, 2)
frequency_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
frequency_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)
monetary_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
monetary_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)

segment <- rfm_segment(rfm_result,
                       segment_names,
                       recency_lower,
                       recency_upper,
                       frequency_lower, 
                       frequency_upper, 
                       monetary_lower,
                       monetary_upper)

segment_dist <- as.data.frame(table(segment$segment))


ggplot(segment_dist, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity") +  xlab("Customer Segments") + ylab("Count") +
  
  geom_col(position = 'dodge', fill="orange") +
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.25)

segment_dist <- table(segment$segment)
piepercent <- round(100*segment_dist/sum(segment_dist), 1)
lbls = paste(names(segment_dist), " ", piepercent,"%")
plotrix::pie3D(segment_dist, labels = lbls, main = "Pie chart for Customer Segments", explode = 0.1)


#RESULTS
rfm_plot_median_recency(segment)
rfm_plot_median_monetary(segment)
rfm_plot_median_frequency(segment)
