#### NAMES 
# Francesca De Cola 819343
# Laura Nembrini 819059
####

#### OPTIONS ####

options(scipen=999)
set.seed(123456)

#### LIBRARIES ####
packages <- c("dplyr",  "ggplot2", "forcats", "lubridate", "RQuantLib", "devtools",
              "data.table", "corrplot", "rpart", "rpart.plot", "MLmetrics", "caret",
              "rattle", "randomForest", "ROCR", "nnet", "e1071",
              "factoextra", "NbClust", "ggpubr", "cluster", 
              "dbscan", "rfm", "kableExtra", "scales", "plotrix") 
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
   install.packages(packages[!installed_packages])
}
invisible(lapply(packages, library, character.only = TRUE))

library(dplyr)
library(magrittr)
library(ggplot2)
library(forcats)
library(lubridate)
library(RQuantLib)
library(rfm)
library(caret)
library(rpart)
library(devtools)
library(data.table)
library(rpart.plot)
library(randomForest)
library(funModeling)
library(corrplot)
library(arules)
library(arulesViz)
library(tidyr)
library(MLmetrics)


#### DIRECTORIES ####
working_dir = "C:/Users/franc/OneDrive/Desktop/wbmkt"  # <path for scripts directory>
data_dir = "C:/Users/franc/OneDrive/Desktop/wbmkt/DMktg_DSLab_Data_1"  # <path for datasets directory>

setwd(working_dir)

#### EXECUTION FULL PIPELINE ####
# Uncomment to execute the entire pipeline of scripts
 PIPELINE_scripts <- c(
   'B01_ingestion.R'
   , 'C01_preparation_df1.R'
   , 'C02_preparation_df2.R'
   , 'C03_preparation_df3.R'
   , 'C04_preparation_df4.R'
   , 'C05_preparation_df5.R'
   , 'C06_preparation_df6.R'
   , 'C07_preparation_df7.R'
   , 'D2_churn.R'
   , 'D3_rfm.R'
   , 'D4_mba.R'
    ## add other scripts
   )
 
 for(i in PIPELINE_scripts){
   source(i, echo = TRUE)
 }









