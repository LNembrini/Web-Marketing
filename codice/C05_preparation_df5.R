# INGESTION df_5 email campaign descriptions #
#`raw_5_camp_cat.csv` contiene la categorizzazione delle comunicazioni email di marketing:
#`ID_CAMP`: identify the email campaign (**Key**);
#`TYP_CAMP`: identify the type email campaign.


#### FIRST LOOK of df_5 ####

str(df_5_camp_cat)
summary(df_5_camp_cat)

#### START CLEANING df_5 ####

df_5_camp_cat_clean <- df_5_camp_cat

#### CLEANING LOW VARIANCE in df_5 ####

df_5_camp_cat_clean <- df_5_camp_cat_clean %>%
  select(-CHANNEL_CAMP)


df_5_dist_type_camp <- df_5_camp_cat_clean %>%
  group_by(TYP_CAMP) %>%
  dplyr::summarize(TOT_CAMPs = n_distinct(ID_CAMP)) %>%
  mutate(PERCENT = TOT_CAMPs/sum(TOT_CAMPs)) %>% 
  arrange(desc(PERCENT)) %>%
  as.data.frame()


plot_df5_dist_typ_camp <- (
  ggplot(data=df_5_dist_type_camp
         , aes(x=TYP_CAMP, y=TOT_CAMPs)) +
    geom_bar(stat="identity"
             ) +
    theme_classic() + 
    xlab("Type campaign") + ylab("Number of campaigns") + 
    geom_text(aes(label=TOT_CAMPs), position=position_dodge(width=0.9), vjust=-0.25))

plot_df5_dist_typ_camp

#### FINAL REVIEW df_5_clean ####

str(df_5_camp_cat_clean)
summary(df_5_camp_cat_clean)

