# INGESTION df_2 customers accounts details #
#`raw_2_cli_account.csv` contiene info su ogni customer account, nello specifico:
#`ID_CLI`: identify the client (Key);
#`EMAIL_PROVIDER`: identify the email account provider;
#`W_PHONE`: identify if a phone number is added (Binomyal {0,1});
#`ID_ADDRESS`: identify the address (Foreign Key);
#`TYP_CLI_ACCOUNT`: identify the account type of the client;
#`TYP_JOB`: identify the client job.

#### FIRST LOOK of df_2 ####

str(df_2_cli_account)
summary(df_2_cli_account)

#### START CLEANING df_2 ####

df_2_cli_account_clean <- df_2_cli_account

#### CLEANING DUPLICATE VALUES in df_2 ####

## check for duplicates
df_2_cli_account_clean %>%
  dplyr::summarize(TOT_ID_CLIs = n_distinct(ID_CLI)
                   , TOT_ROWs = n())

#!!! NOTE:  no duplicates !!!#

#### CLEANING DATA TYPES in df_2 ####

## format boolean as factor ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(W_PHONE = as.factor(W_PHONE))

## format numerical categories as factor ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(TYP_CLI_ACCOUNT = as.factor(TYP_CLI_ACCOUNT))

#### CLEANING MISSING VALUES in df_2 ####

## MISSING VALUES mapped as natural values ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(W_PHONE = fct_explicit_na(W_PHONE, "0"))

## MISSING VALUES mapped as new level in categorical columns ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%  
  mutate(EMAIL_PROVIDER = fct_explicit_na(EMAIL_PROVIDER, "(missing)")) %>%
  mutate(TYP_JOB = fct_explicit_na(TYP_JOB, "(missing)"))

#### CONSISTENCY CHECK ID_CLI in df_1/df_2 ####

cons_idcli_df1_df2 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_2_cli_account_clean %>%
              select(ID_CLI) %>%
              mutate(is_in_df_2 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_2) %>%
  dplyr::summarize(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df2

#!!! NOTE: all ID_CLI in df_1 are also in df_2 and vice-versa !!!#

#### EXPLORE COLUMNS of df_2 ####

### Variable EMAIL_PROVIDER ###

## compute distribution
df_2_dist_emailprovider <- df_2_cli_account_clean %>%
  group_by(EMAIL_PROVIDER) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>%
  as.data.frame()

df_2_dist_emailprovider

tot_emailproviders <- n_distinct(df_2_dist_emailprovider$EMAIL_PROVIDER)

tot_emailproviders #20512 email provider diversi

#!!! NOTE: too many different values for EMAIL_PROVIDER to be an useful category !!!#

#### TO DO df_2 ####
# COMPUTE THE DISTRIBUTION for the remaining df_2_cli_fid_clean variables

###Variable W_PHONE ###
## compute distribution
df_2_dist_w_phone <- df_2_cli_account_clean %>%
  group_by(W_PHONE) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>% 
  arrange(desc(PERCENT)) %>%
  as.data.frame()

df_2_dist_w_phone

### Variable ID_ADDRESS ###
## compute distribution
df_2_dist_id_address <- df_2_cli_account_clean %>%
  group_by(ID_ADDRESS) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>%
  as.data.frame()

df_2_dist_id_address

##Variable TYP_CLI_ACCOUNT ###
df_2_dist_typ_cli_account <- df_2_cli_account_clean %>%
  group_by(TYP_CLI_ACCOUNT) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>%
  as.data.frame()

df_2_dist_typ_cli_account


### Variable TYP_JOB ###
df_2_dist_typ_job <- df_2_cli_account_clean %>%
  group_by(TYP_JOB) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>%
  as.data.frame()

df_2_dist_typ_job

#### RESHAPING df_2 ####

## keep the most frequent EMAIL_PROVIDER values and add a common factor level "OTHER" for the remaining ##
df_2_dist_emailprovider %>%
  arrange(desc(PERCENT)) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_CLIs)/sum(TOT_CLIs)) %>%
  as.data.frame() %>%
  head(20)

## always keep the (missing) level for technical reasons
## select levels that cover the 85% of the cases, the remaining 15% 
clean_email_providers <- df_2_dist_emailprovider %>%
  arrange(desc(PERCENT)) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_CLIs)/sum(TOT_CLIs)) %>%
  mutate(EMAIL_PROVIDER = as.character(EMAIL_PROVIDER)) %>%
  mutate(AUX = if_else(PERCENT_COVERED < 0.85 | (PERCENT_COVERED > 0.85 & lag(PERCENT_COVERED) < 0.85), 1,0)) %>%
  mutate(EMAIL_PROVIDER_CLEAN = if_else(AUX | EMAIL_PROVIDER == "(missing)", EMAIL_PROVIDER, "others"))

head(clean_email_providers, 20)

## add clean EMAIL_PROVIDER ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(EMAIL_PROVIDER = as.character(EMAIL_PROVIDER)) %>%
  left_join(clean_email_providers %>%
              select(EMAIL_PROVIDER, EMAIL_PROVIDER_CLEAN)
            , by = "EMAIL_PROVIDER") %>%
  select(-EMAIL_PROVIDER) %>%
  mutate(EMAIL_PROVIDER_CLEAN = as.factor(EMAIL_PROVIDER_CLEAN))


#### EXPLORE NEW COLUMNS EMAIL_PROVIDER_CLEAN in df_2 ####

## compute distribution
df2_dist_emailproviderclean <- df_2_cli_account_clean %>%
  group_by(EMAIL_PROVIDER_CLEAN) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df2_dist_emailproviderclean

## plot distribution
plot_df2_dist_emailproviderclean <- (
  ggplot(data=df2_dist_emailproviderclean
         , aes(x=EMAIL_PROVIDER_CLEAN, y=TOT_CLIs)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df2_dist_emailproviderclean #più usata è gmail

#### TO DO df_2 ####
# EXPLORE the remaining df_2_cli_account_clean relevant variables

# distribution of client job
df_2_dist_typJob <- df_2_cli_account_clean %>%
  group_by(TYP_JOB) %>%
  dplyr::summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  as.data.frame()

df_2_dist_typJob

#!!! NOTE 97,6% of type job is missing
# so include it in "Non Dichiarata"

df_2_typJob_miss <- df_2_dist_typJob %>% 
  filter(TYP_JOB == "(missing)")
# computes sum
df_2_typJob_nondic <- df_2_dist_typJob %>% 
  filter(TYP_JOB == "Non Dichiara") %>%
  mutate(TOT_CLIs = TOT_CLIs + df_2_typJob_miss[, "TOT_CLIs"])

df_2_dist_typJob <- df_2_dist_typJob %>% 
  filter(TYP_JOB != "Non Dichiara" & TYP_JOB != "(missing)") %>%
  bind_rows(df_2_typJob_nondic) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs))

df_2_dist_typJob

n_cli = sum((df_2_dist_typJob %>% filter(TYP_JOB != "Non Dichiara"))$TOT_CLIs)
# distribution of available data
plot_df_2_dist_typJob <- df_2_dist_typJob %>%
  filter(TYP_JOB != "Non Dichiara") %>%
  group_by(TYP_JOB) %>%
  dplyr::summarize(PERCENT = TOT_CLIs / n_cli) %>%
  ggplot(aes(PERCENT, TYP_JOB)) +
  ggtitle("Distribution of job Type") +
  geom_bar(stat = "identity", fill = "#549900", color = "black") +
  xlab("% clients") +
  ylab("Type of Job") +
  scale_x_continuous(breaks = seq(0,1,0.1), labels = scales::percent_format(scale = 100)) +
  theme_grey() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold")) +
  theme(axis.text = element_text(size = 10, face = "italic")) +
  theme(axis.title = element_text(size = 13))

plot_df_2_dist_typJob

### Variable W_PHONE ###
plot_df2_dist_w_phone <- (
  ggplot(data=df_2_dist_w_phone
         , aes(x=W_PHONE, y=TOT_CLIs)) +
    geom_bar(stat="identity"
             ) +
    theme_classic() + 
    xlab("Phone number") + ylab("Number of clients") +
    scale_x_discrete(labels=c("0" = "not added", "1" = "added")) +
    scale_y_continuous(labels = function(x){paste0(x/1000, 'K')}) +
    geom_text(aes(label=TOT_CLIs), position=position_dodge(width=0.9), vjust=-0.25)
)

plot_df2_dist_w_phone

### Variable TYP_CLI_ACCOUNT ###
plot_df2_dist_typ_cli_account <- (
  ggplot(data=df_2_dist_typ_cli_account
         , aes(x=TYP_CLI_ACCOUNT, y=TOT_CLIs)) +
    geom_bar(stat="identity"
             ) +
    theme_classic() + 
    xlab("Type account") + ylab("Number of clients") +
    scale_x_discrete(labels=c("2" = "2", "4" = "4")) +
    scale_y_continuous(labels = function(x){paste0(x/1000, 'K')}) +
    
    geom_text(aes(label=TOT_CLIs), position=position_dodge(width=0.9), vjust=-0.25)
)

plot_df2_dist_typ_cli_account

#### FINAL REVIEW df_2_clean ####

str(df_2_cli_account_clean)
summary(df_2_cli_account_clean)
