library(tidyverse)

###############
# chain desc ##
################
df_chain_desc_OLD <-
    read_csv("data_output/spy_weekly_2014_2018_chain_desc.csv")

df_chain_desc_NEW <-
    read_csv("data_output/spy_weekly_2014_2018_chain_desc_NEW.csv")


df_chain_desc_OLD %>% 
    summarize(
        d2x = sum(d2x)
        , num_opts = sum(num_opts)
        , exec_day_volume = sum(exec_day_volume)
        , realized_vol = sum(realized_vol)
        , return = sum(return)
    )

df_chain_desc_NEW %>% 
    summarize(
        d2x = sum(d2x)
        , num_opts = sum(num_opts)
        , exec_day_volume = sum(exec_day_volume)
        , realized_vol = sum(realized_vol)
        , return = sum(return)
    )



###############
# chain desc ##
###############
df_chain_hist_OLD <-
    read_csv("data_output/spy_weekly_2014_2018_chain_hist.csv")

df_chain_hist_NEW <-
    read_csv("data_output/spy_weekly_2014_2018_chain_hist_NEW.csv")


df_chain_hist_OLD %>%
    filter(trade_date != "2018-12-05") %>% 
    summarize(
        implied_forward = sum(implied_forward, na.rm = TRUE)
        , bid_swap_rate = sum(bid_swap_rate, na.rm = TRUE)
        , ask_swap_rate = sum(ask_swap_rate, na.rm = TRUE)
        , mid_swap_rate = sum(mid_swap_rate, na.rm = TRUE)
    )

df_chain_hist_NEW %>%
    filter(trade_date != "2018-12-05") %>% 
    summarize(
        implied_forward = sum(implied_forward, na.rm = TRUE)
        , bid_swap_rate = sum(bid_swap_rate, na.rm = TRUE)
        , ask_swap_rate = sum(ask_swap_rate, na.rm = TRUE)
        , mid_swap_rate = sum(mid_swap_rate, na.rm = TRUE)
    )



###############
# opt hist ##
###############
df_opt_hist_OLD <-
    read_csv("data_output/spy_weekly_2014_2018_opt_hist.csv")

df_opt_hist_NEW <-
    read_csv("data_output/spy_weekly_2014_2018_opt_hist_NEW.csv")


df_opt_hist_OLD %>%
    filter(data_date != "2018-12-05") %>%
    filter(data_date != "2015-04-02") %>%
    summarize(
        strike = sum(strike, na.rm = TRUE)
        , bid = sum(bid, na.rm = TRUE)
        , ask = sum(ask, na.rm = TRUE)
        , mid = sum(mid, na.rm = TRUE)
    )

df_opt_hist_NEW %>%
    filter(data_date != "2018-12-05") %>%
    filter(data_date != "2015-04-02") %>%
    summarize(
        strike = sum(strike, na.rm = TRUE)
        , bid = sum(bid, na.rm = TRUE)
        , ask = sum(ask, na.rm = TRUE)
        , mid = sum(mid, na.rm = TRUE)
    )














