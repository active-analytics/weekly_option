library(tidyverse)


#df_test <- read_csv("data_output/weekly/dia_weekly_2014_2018_managed_pnl.csv")


df_v1 <- read_csv("data_output/weekly/spy_weekly_2014_2018_managed_pnl.csv")
df_v2 <- read_csv("data_output/weekly/spy_weekly_2014_2018_managed_pnl_v2.csv")


df_v1$scaled_managed_pnl %>% sum()
df_v2$scaled_managed_pnl %>% sum()
