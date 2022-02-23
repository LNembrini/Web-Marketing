# INGESTION df_3 customers addresses #
#raw_3_cli_address.csv` contiene info sull'indirizzo corrispondente al customer account, nello specifico: 
#`ID_ADDRESS`: identify the address (Key);
#`CAP`: identify the postal code;
#`PRV`: identify the province;
#`REGION`: identify the region.


#### FIRST LOOK of df_3 ####

str(df_3_cli_address)
summary(df_3_cli_address)

#### START CLEANING df_3 ####

df_3_cli_address_clean <- df_3_cli_address

#### CLEANING DUPLICATE VALUES in df_3 ####

## check for duplicates
df_3_cli_address_clean %>%
  dplyr::summarize(TOT_ID_ADDRESSes = n_distinct(ID_ADDRESS)
                   , TOT_ROWs = n())

#!!! NOTE:  there are duplicates !!!#



df_3_cli_address_clean <- df_3_cli_address_clean %>%
  distinct()

#### CLEANING DATA TYPES in df_3 ####

## format string as factors ##
df_3_cli_address_clean <- df_3_cli_address_clean %>%
  mutate(CAP = as.character(CAP))



#### CLEANING MISSING VALUES in df_3 ####

df_3_cli_address_clean %>%
  group_by(w_CAP = !is.na(CAP)
           , w_PRV = !is.na(PRV)
           , w_REGION = !is.na(REGION)) %>%
  dplyr::summarize(TOT_ADDs = n_distinct(ID_ADDRESS))

## let examine in details some of these missing cases
df_3_cli_address_clean %>% filter(is.na(PRV) & !is.na(REGION))

## MISSING VALUES rows are removed ##
df_3_cli_address_clean <- df_3_cli_address_clean %>%  
  filter(!is.na(CAP) & !is.na(PRV) & !is.na(REGION))

#### CONSISTENCY CHECK ID_ADDRESS in df_2/df_3 ####

cons_idaddress_df2_df3 <- df_2_cli_account_clean %>%
  select(ID_ADDRESS) %>%
  mutate(is_in_df_2 = 1) %>%
  distinct() %>%
  full_join(df_3_cli_address_clean %>%
              select(ID_ADDRESS) %>%
              mutate(is_in_df_3 = 1) %>%
              distinct()
            , by = "ID_ADDRESS"
  ) %>%
  group_by(is_in_df_2, is_in_df_3) %>%
  dplyr::summarize(NUM_ID_ADDRESSes = n_distinct(ID_ADDRESS)) %>%
  as.data.frame()

cons_idaddress_df2_df3

#!!! NOTE:  there are ID_ADDRESSes actually not mapped in df_3 !!!#
#!!!        this issue should be taken into account in joining these two tables !!!#


#### EXPLORE COLUMNS of df_3 ####
#### TO DO df_3 ####
# EXPLORE the df_3_cli_address_clean relevant variables

# COMPUTE THE DISTRIBUTION OF REGION
## compute distribution
df_3_cli_address_clean_region_distrib <- df_3_cli_address_clean %>%
  group_by(REGION) %>%
  dplyr::summarize(TOT_IDs = n_distinct(ID_ADDRESS)) %>%
  mutate(PERCENT = TOT_IDs/sum(TOT_IDs)) %>%
  arrange(desc(PERCENT))

df_3_cli_address_clean_region_distrib

## plot distribution
plot_df_3_cli_address_clean_region_distrib <- (
  ggplot(data=df_3_cli_address_clean_region_distrib
         , aes(x=REGION, y=PERCENT)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df_3_cli_address_clean_region_distrib

# COMPUTE THE DISTRIBUTION OF PRV
## compute distribution
df_3_cli_address_clean_prv_distrib <- df_3_cli_address_clean %>%
  group_by(PRV) %>%
  dplyr::summarize(TOT_IDs = n_distinct(ID_ADDRESS)) %>%
  mutate(PERCENT = TOT_IDs/sum(TOT_IDs)) %>%
  arrange(desc(PERCENT))

df_3_cli_address_clean_prv_distrib

## plot distribution
plot_df_3_cli_address_clean_prv_distrib <- (
  ggplot(data=df_3_cli_address_clean_prv_distrib
         , aes(x=PRV, y=PERCENT)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df_3_cli_address_clean_prv_distrib

# COMPUTE THE DISTRIBUTION OF CAPs - NOT MEANINGFUL AS IT CALCULATES THE PERCENTAGE OF PEOPLE LIVING IN THE SAME CITY - TOO SMALL NUMBERS

## compute distribution
df_3_cli_address_clean_cap_distrib <- df_3_cli_address_clean %>%
  group_by(CAP) %>%
  dplyr::summarize(TOT_IDs = n_distinct(ID_ADDRESS)) %>%
  mutate(PERCENT = TOT_IDs/sum(TOT_IDs)) %>%
  arrange(desc(PERCENT))

df_3_cli_address_clean_cap_distrib

## plot distribution
plot_df_3_cli_address_clean_cap_distrib <- (
  ggplot(data=df_3_cli_address_clean_cap_distrib
         , aes(x=CAP, y=PERCENT)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df_3_cli_address_clean_cap_distrib

#### FINAL REVIEW df_3_clean ####

str(df_3_cli_address_clean)
summary(df_3_cli_address_clean)
