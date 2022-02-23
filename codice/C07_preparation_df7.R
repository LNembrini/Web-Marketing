# INGESTION df_7 purchase tickets #
#`raw_7_tic.csv` contiene le transazioni di purchase e refund di ogni customer:
#`ID_SCONTRINO`: identify the transaction (all products have same ID);
#`ID_CLI`: identify the client (*Foreign Key*);
#`ID_NEG`: identify the reference store (*Foreign Key*);
#`ID_ARTICOLO`: identify the purchased or refund item;
#`COD_REPARTO`: identify the business unit corresponding to the item;
#`DIREZIONE`: identify the purchase (1) or refund (-1);
#`IMPORTO_LORDO`: identify the gross amount as the sum of net amount and the discount applied;
#`SCONTO`: identify the discount applied (negative if refund);
#`DATETIME`: datetime of the transaction.

#### FIRST LOOK of df_7 ####

str(df_7_tic)
summary(df_7_tic)

#### START CLEANING df_7 ####

df_7_tic_clean <- df_7_tic

#### CLEANING DATA TYPES in df_7 ####

## formatting dates and times ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(TIC_DATETIME = as.POSIXct(DATETIME, format="%Y-%m-%dT%H%M%S")) %>%
  mutate(TIC_HOUR = hour(TIC_DATETIME)) %>%
  mutate(TIC_DATE = as.Date(TIC_DATETIME)) %>%
  select(-DATETIME)

## formatting boolean as factor ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(DIREZIONE = as.factor(DIREZIONE))

## formatting numerical categories as factor ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(COD_REPARTO = as.factor(COD_REPARTO))

#### CONSISTENCY CHECK ID_CLI in df_1/df_7 ####

cons_idcli_df1_df7 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  distinct() %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_7_tic_clean %>%
              select(ID_CLI) %>%
              distinct() %>%
              mutate(is_in_df_7 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_7) %>%
  dplyr::summarize(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df7

#!!! NOTE: all ID_CLI in df_7 are mapped in df_1, but not all ID_CLI in df_1 are mapped in df_7 !!!#  

#### RESHAPING df_7 ####

df_7_tic_clean_final <- df_7_tic_clean %>%
  ## adding day characterization ##
  mutate(TIC_DATE_WEEKDAY = wday(TIC_DATE)) %>%
  mutate(TIC_DATE_HOLIDAY = isHoliday("Italy", TIC_DATE)) %>%
  mutate(TIC_DATE_TYP = case_when(
    (TIC_DATE_WEEKDAY %in% c(6,7)) ~ "weekend"
    , (TIC_DATE_HOLIDAY == TRUE) ~ "holiday"
    , (TIC_DATE_WEEKDAY < 7) ~ "weekday"
    , TRUE ~ "other"
  )
  )

#### EXPLORE VARIABLES in df_7 ####
### GENERAL OVERVIEW ###

## compute aggregate
df7_overview <- df_7_tic_clean_final %>% 
  dplyr::summarize(MIN_DATE = min(TIC_DATE)
                   , MAX_DATE = max(TIC_DATE)
                   , TOT_TICs = n_distinct(ID_SCONTRINO)
                   , TOT_CLIs = n_distinct(ID_CLI))

df7_overview

### Variable DIREZIONE ###

## compute aggregate
df7_dist_direction <- df_7_tic_clean_final %>%
  group_by(DIREZIONE) %>%
  dplyr::summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
                   , TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT_TICs = TOT_TICs/df7_overview$TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/df7_overview$TOT_CLIs)

df7_dist_direction

### Variable TIC_HOURS ###

## compute aggregate
df7_dist_hour <- df_7_tic_clean_final %>%
  group_by(TIC_HOUR, DIREZIONE) %>%
  dplyr::summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
                   , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
  ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(-ALL_TOT_TICs, -ALL_TOT_CLIs)

df7_dist_hour

## plot aggregate
plot_df7_dist_hour <- (
  ggplot(data=df7_dist_hour
         , aes(fill=DIREZIONE, x=TIC_HOUR, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df7_dist_hour

## plot aggregate percent
plot_df7_dist_hour_percent <- (
  ggplot(data=df7_dist_hour
         , aes(fill=DIREZIONE, x=TIC_HOUR, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
)

plot_df7_dist_hour_percent


### Variable COD_REPARTO ###

## compute aggregate
df7_dist_dep <- df_7_tic_clean_final %>%
  group_by(COD_REPARTO, DIREZIONE) %>%
  dplyr::summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
                   , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
  ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(-ALL_TOT_TICs, -ALL_TOT_CLIs)

df7_dist_dep

## plot aggregate
plot_df7_dist_dep <- (
  ggplot(data=df7_dist_dep
         , aes(fill=DIREZIONE, x=COD_REPARTO, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df7_dist_dep

## plot aggregate percent
plot_df7_dist_dep_percent <- (
  ggplot(data=df7_dist_dep
         , aes(fill=DIREZIONE, x=COD_REPARTO, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
)

plot_df7_dist_dep_percent

### Variable TIC_DATE_TYP ###

## compute aggregate
df7_dist_datetyp <- df_7_tic_clean_final %>%
  group_by(TIC_DATE_TYP, DIREZIONE) %>%
  dplyr::summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
                   , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
  ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(-ALL_TOT_TICs, -ALL_TOT_CLIs)

df7_dist_datetyp

## plot aggregate
plot_df7_dist_datetyp <- (
  ggplot(data=df7_dist_datetyp
         , aes(fill=DIREZIONE, x=TIC_DATE_TYP, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df7_dist_datetyp

## plot aggregate percent
plot_df7_dist_datetyp_percent <- (
  ggplot(data=df7_dist_datetyp
         , aes(fill=DIREZIONE, x=TIC_DATE_TYP, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
)

plot_df7_dist_datetyp_percent

### Variable average IMPORTO_LORDO and average SCONTO per TICKET ###

## compute aggregate
df7_dist_importosconto <- df_7_tic_clean_final %>%
  group_by(ID_SCONTRINO, DIREZIONE) %>%
  dplyr::summarize(IMPORTO_LORDO = sum(IMPORTO_LORDO)
                   , SCONTO = sum(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

df7_dist_avgimportosconto <- df7_dist_importosconto %>%
  group_by(DIREZIONE) %>%
  dplyr::summarize(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
                   , AVG_SCONTO = mean(SCONTO))

df7_dist_avgimportosconto

## plot aggregate
plot_df7_dist_importo <- (
  ggplot(data=df7_dist_importosconto %>%
           filter((IMPORTO_LORDO > -1000) & (IMPORTO_LORDO < 1000))
         , aes(color=DIREZIONE, x=IMPORTO_LORDO)) +
    geom_histogram(binwidth=10, fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_importo

## plot aggregate
plot_df7_dist_sconto <- (
  ggplot(data=df7_dist_importosconto %>%
           filter((SCONTO > -250) & (IMPORTO_LORDO < 250))
         , aes(color=DIREZIONE, x=SCONTO)) +
    geom_histogram(binwidth=10, fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_sconto

#### TO DO df_7 ####
# EXPLORE average IMPORTO_LORDO and average SCONTO by COD_REPARTO


## compute aggregate
df7_dist_importosconto_reparto <- df_7_tic_clean_final %>%
  group_by(COD_REPARTO, DIREZIONE) %>%
  dplyr::summarize(IMPORTO_LORDO = sum(IMPORTO_LORDO)
                   , SCONTO = sum(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

df7_dist_avgimportosconto_reparto <- df7_dist_importosconto_reparto %>%
  group_by(DIREZIONE) %>%
  dplyr::summarize(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
                   , AVG_SCONTO = mean(SCONTO))

df7_dist_avgimportosconto_reparto

## plot aggregate
plot_df7_dist_importo_reparto <- (
  ggplot(data=df7_dist_importosconto_reparto %>%
           filter()
         , aes(color=DIREZIONE, x=IMPORTO_LORDO)) +
    geom_histogram(fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_importo_reparto

## plot aggregate
plot_df7_dist_sconto_reparto <- (
  ggplot(data=df7_dist_importosconto_reparto %>%
           filter()
         , aes(color=DIREZIONE, x=SCONTO)) +
    geom_histogram(fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_sconto_reparto

# EXPLORE ID_ARTICOLO DISTRIBUTIONS (i.e. num TICs by ID_ARTICOLO)

# number of tics per articolo
df7_dist_tics_articolo <- df_7_tic_clean_final %>%
  group_by(ID_ARTICOLO) %>%
  dplyr::summarize(NUM_TICs = sum(n_distinct(ID_SCONTRINO))) %>%
  ungroup()

df7_dist_tics_articolo

# distribution of TICs number
df7_dist_numtics_articolo <- df7_dist_tics_articolo %>%
  group_by(NUM_TICs) %>%
  dplyr::summarize(COUNT_ART = sum(n_distinct(ID_ARTICOLO))) %>%
  ungroup()

df7_dist_numtics_articolo

# plot aggregate
plot_df7_dist_numtics_articolo <- df7_dist_numtics_articolo %>%
  filter(NUM_TICs < 50) %>%
  ggplot(aes(x = NUM_TICs, y = COUNT_ART)) +
  geom_histogram(stat = "identity", fill = "#549900") + 
  ggtitle("Distribution of Numb TICs by ID_ARTICOLO") + 
  scale_x_continuous(breaks = seq(0, 50, 5)) +
  xlab("Numb of Articles") +
  ylab("Numb of transactions") +
  theme_grey() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold")) +
  theme(axis.text = element_text(size = 10, face = "italic")) +
  theme(axis.title = element_text(size = 13))

plot_df7_dist_numtics_articolo

# EXPLORE average IMPORTO_LORDO and average SCONTO per ID_CLI

## compute aggregate
df7_dist_importosconto_cli <- df_7_tic_clean_final %>%
  group_by(ID_CLI, DIREZIONE) %>%
  dplyr::summarize(IMPORTO_LORDO = sum(IMPORTO_LORDO)
                   , SCONTO = sum(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

df7_dist_avgimportosconto_cli <- df7_dist_importosconto_cli %>%
  group_by(DIREZIONE) %>%
  dplyr::summarize(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
                   , AVG_SCONTO = mean(SCONTO))

df7_dist_avgimportosconto_cli

## plot aggregate
plot_df7_dist_importo_cli <- (
  ggplot(data=df7_dist_importosconto_cli %>%
           filter()
         , aes(color=DIREZIONE, x=IMPORTO_LORDO)) +
    geom_histogram(fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_importo_cli

## plot aggregate
plot_df7_dist_sconto_cli <- (
  ggplot(data=df7_dist_importosconto_cli %>%
           filter()
         , aes(color=DIREZIONE, x=SCONTO)) +
    geom_histogram(fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_sconto_cli

# compute the distribution of customers by number of purchases 

df7_dist_total_purch <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1)                             %>% 
  group_by(ID_CLI)                                   %>% 
  summarise(TOT_PURCHASE = n_distinct(ID_SCONTRINO)) %>% 
  arrange(desc(TOT_PURCHASE))                           

df7_dist_total_purch

# compute the days for next purchase curve 

df_for_next_purchase_curve <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1) %>% 
  select(ID_CLI,
         ID_ARTICOLO,
         TIC_DATE,
         DIREZIONE)      %>%
  arrange(ID_CLI)

df_for_next_purchase_curve


df_date_diff <- df_for_next_purchase_curve %>%
  group_by(ID_CLI) %>%
  mutate(Days_difference = TIC_DATE - lag(TIC_DATE))

df_date_diff

df_days_curve <- as.data.frame(table(df_date_diff$Days_difference))
colnames(df_days_curve) <- c("Days_diff","Freq")
df_days_curve <- df_days_curve[-1, ]
df_days_curve$Perc <- df_days_curve$Freq/sum(df_days_curve$Freq)

df_days_curve


x <- as.data.frame(table(df_date_diff$Days_difference))
x <- x[-1, ]
x$Perc <- x$Freq/sum(x$Freq)

ggplot(x, 
       aes(x = as.numeric(Var1),
           y = cumsum(Perc))) +
  labs(title = "Next Purchase Curve",
       x = "Last Purchase Date (in Days)",
       y = "Cumulative Percent") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +    
  scale_x_continuous(breaks = seq(0, 400, 25)) +     
  geom_vline(xintercept = 75, linetype = "dotted") +
  geom_line(size = 1)


#### FINAL REVIEW df_7_clean ####

str(df_7_tic_clean_final)
summary(df_7_tic_clean_final)
