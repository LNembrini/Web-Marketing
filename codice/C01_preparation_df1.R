# Dataset 1
# INGESTION df_1 customers fidelity subscriptions #

#`raw_1_cli_fid.csv` contiene info sulle fidelity subcriptions di ogni customer account, in particolare:
#`ID_CLI`: identify client (Foreign Key);
#`ID_FID`: identify fidelty program (Key);
#`ID_NEG`: identify reference store;
#`TYP_CLI_FID`: identify the main account (Binomyal {1,0});
#`COD_FID`: identify the fidelty program type;
#`STATUS_FID`: identify if an account is active (Binomyal {1,0});
#`DT_ACTIVE`: identify the date of activation.

#### FIRST LOOK of df_2 ####

str(df_1_cli_fid)
summary(df_1_cli_fid)


#### START CLEANING df_1 ####

df_1_cli_fid_clean <- df_1_cli_fid

#### CLEANING DUPLICATE VALUES in df_1 ####

## check for duplicates
df_1_cli_fid_clean %>%
  dplyr::summarize(TOT_ID_CLIs = n_distinct(ID_CLI)
                   , TOT_ID_FIDs = n_distinct(ID_FID)
                   , TOT_ID_CLIFIDs = n_distinct(paste0(as.character(ID_CLI),"-",as.character(ID_FID)))
                   , TOT_ROWs = n())

#370135 osservazioni
#369472 valori unici per gli ID cliente, alcuni ID_CLI si ripetono
#367925 valori unici per gli ID fidelity, alcuni si ripetono
#370135 valori unici per coppie ciente-fidelity, non ci sono duplicati nelle coppie id_cli-id_fid)

#duplicati di ID_CLI
sum(duplicated(df_1_cli_fid_clean$ID_CLI))  #663
#df_1_cli_fid_clean$ID_CLI[duplicated(df_1_cli_fid_clean$ID_CLI)]  quali sono

#!!! NOTE:  no duplicates for combination CLI-FID !!!#

#### CLEANING DATA TYPES in df_1 ####

## formatting dates ##
df_1_cli_fid_clean <- df_1_cli_fid_clean %>%
  mutate(DT_ACTIVE = as.Date(DT_ACTIVE))

## formatting boolean as factor ##
df_1_cli_fid_clean <- df_1_cli_fid_clean %>%
  mutate(TYP_CLI_FID = as.factor(TYP_CLI_FID)) %>%
  mutate(STATUS_FID = as.factor(STATUS_FID))

#### CONSISTENCY CHECK on df1: number of fidelity subscriptions per client ####

str(df_1_cli_fid_clean)

## count the subscriptions for each client
num_fid_x_cli <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  dplyr::summarize(NUM_FIDs =  n_distinct(ID_FID)
                   , NUM_DATEs = n_distinct(DT_ACTIVE)
  )


tot_id_cli <- n_distinct(num_fid_x_cli$ID_CLI)

## compute the distribution of number of subscriptions
dist_num_fid_x_cli <- num_fid_x_cli %>%
  group_by(NUM_FIDs, NUM_DATEs) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT_CLIs = TOT_CLIs/tot_id_cli)

dist_num_fid_x_cli

#368833 clienti con 1 sottoscrizione in una sola data
#254 clienti con 2 sottoscrizioni in una sola data e 363 in due date diverse
#7 clienti con 3 sottoscrizioni in una sola data e 8 in due date diverse e 5 in tre
#2 clienti con 4 sottoscrizioni in una sola data

#!!! NOTE: there are clients with multiple fidelity subscriptions !!!#

## let examine in details clients with multiple subscriptions

num_fid_x_cli %>% filter(NUM_FIDs == 3)
num_fid_x_cli %>% filter(NUM_FIDs == 2)
num_fid_x_cli %>% filter(NUM_FIDs == 4)

# each subscription can have different dates
df_1_cli_fid %>% filter(ID_CLI == 621814)

# there could be subscriptions at the same dates [possibly for technical reasons]
df_1_cli_fid %>% filter(ID_CLI == 320880) #3 fidelity nello stesso negozio

#### RESHAPING df_1 ####
## combining information

# from first subscription  --> registration date, store for registration
# from last subscription   --> type of fidelity, status
# from subscriptions count --> number of subscriptions made

df_1_cli_fid_first <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  filter(DT_ACTIVE == min(DT_ACTIVE)) %>%
  arrange(ID_FID) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

df_1_cli_fid_last <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  filter(DT_ACTIVE == max(DT_ACTIVE)) %>%
  arrange(desc(ID_FID)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

df_1_cli_fid_clean <- df_1_cli_fid_last %>%
  select(ID_CLI
         , ID_FID
         , LAST_COD_FID = COD_FID
         , LAST_TYP_CLI_FID = TYP_CLI_FID
         , LAST_STATUS_FID = STATUS_FID
         , LAST_DT_ACTIVE = DT_ACTIVE) %>%
  left_join(df_1_cli_fid_first %>%
              select(ID_CLI
                     , FIRST_ID_NEG = ID_NEG
                     , FIRST_DT_ACTIVE = DT_ACTIVE)
            , by = 'ID_CLI') %>%
  left_join(num_fid_x_cli %>%
              select(ID_CLI
                     , NUM_FIDs) %>%
              mutate(NUM_FIDs = as.factor(NUM_FIDs))
            , by = 'ID_CLI')

#### EXPLORE COLUMNS of df_1 ####
### variable LAST_COD_FID ###

## compute distribution
df1_dist_codfid <- df_1_cli_fid_clean %>%
  group_by(LAST_COD_FID) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df1_dist_codfid

## plot distribution
plot_df1_dist_codfid <- (
  ggplot(data=df1_dist_codfid
         , aes(x=LAST_COD_FID, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df1_dist_codfid

df_1_cli_fid_clean %>% 
  count(LAST_STATUS_FID = factor(LAST_STATUS_FID )) %>% 
  mutate(count = prop.table(n)) %>% 
  ggplot(aes(x = LAST_STATUS_FID, y = count, label = scales::percent(count))) +
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5, 
            size = 3)+
  scale_y_continuous(labels = scales::percent)+
  scale_x_discrete(labels=c("0" = "unactive", "1" = "active"))+
  theme_classic()+
  labs(x = 'subscription status', y = 'Percentage')


#### TO DO df_1 ####
# EXPLORE the remaining df_1_cli_fid_clean relevant variables

### variable NUM_FIDS ###
## compute distribution
df1_dist_codfid <- df_1_cli_fid_clean %>%
  group_by(NUM_FIDs) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df1_dist_codfid

## plot distribution

plot_df1_dist_codfid <- (
  ggplot(data=df1_dist_codfid
         , aes(x=NUM_FIDs, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df1_dist_codfid #quasi tutti una sola sottoscrizione

### variable LAST_DT_ACTIVE ###

## compute distribution
df1_dist_codfid <- df_1_cli_fid_clean %>%
  group_by(substring(LAST_DT_ACTIVE,1,4)) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>% 
  rename(Year = `substring(LAST_DT_ACTIVE, 1, 4)`)

df1_dist_codfid

## plot distribution

plot_df1_dist_codfid <- (
  ggplot(data=df1_dist_codfid
         , aes(x=Year, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df1_dist_codfid #nel 2018 più attivazioni

### variable FIRST_DT_ACTIVE ###

## compute distribution
df1_dist_codfid <- df_1_cli_fid_clean %>%
  group_by(substring(FIRST_DT_ACTIVE,1,4)) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>% 
  rename(Year = `substring(FIRST_DT_ACTIVE, 1, 4)`)

df1_dist_codfid

## plot distribution

plot_df1_dist_codfid <- (
  ggplot(data=df1_dist_codfid
         , aes(x=Year, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df1_dist_codfid

### variable LAST_STATUS_FID ###

## compute distribution
df1_dist_codfid <- df_1_cli_fid_clean %>%
  group_by(LAST_STATUS_FID) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))


df1_dist_codfid

## plot distribution

plot_df1_dist_codfid <- (
  ggplot(data=df1_dist_codfid
         , aes(x=LAST_STATUS_FID, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df1_dist_codfid

#### FINAL REVIEW df_1_clean ####

str(df_1_cli_fid_clean)
summary(df_1_cli_fid_clean)
