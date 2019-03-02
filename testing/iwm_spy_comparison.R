df_spy <- read_csv("data_output/spy_weekly_2014_2018_pnl_master.csv")
df_iwm <- read_csv("data_output/iwm_weekly_2014_2018_pnl_master.csv")
df_qqq <- read_csv("data_output/qqq_weekly_2014_2018_pnl_master.csv")

df_spy_daily <-
    df_spy %>% 
        filter(variation == 0.1) %>% 
        group_by(data_date) %>% 
        summarize(pnl = sum(dly_tot_pnl)) %>% 
        mutate(
            ttd_pnl = cumsum(pnl)
            , underlying = "SPY"
        )


df_iwm_daily <-
    df_iwm %>% 
    filter(variation == 0.1) %>% 
    group_by(data_date) %>% 
    summarize(pnl = sum(dly_tot_pnl)) %>% 
    mutate(
        ttd_pnl = cumsum(pnl)
        , underlying = "IWM"
    )

df_qqq_daily <-
    df_qqq %>% 
    filter(variation == 0.1) %>% 
    group_by(data_date) %>% 
    summarize(pnl = sum(dly_tot_pnl)) %>% 
    mutate(
        ttd_pnl = cumsum(pnl)
        , underlying = "QQQ"
    )


(mean(df_spy_daily$pnl) / sd(df_spy_daily$pnl)) * sqrt(252)
(mean(df_iwm_daily$pnl) / sd(df_iwm_daily$pnl)) * sqrt(252)
(mean(df_qqq_daily$pnl) / sd(df_qqq_daily$pnl)) * sqrt(252)


df_all_daily <- 
    bind_rows(
        df_spy_daily
        , df_iwm_daily
        , df_qqq_daily
    )


df_all_daily %>% 
    ggplot() +
    geom_line(aes(x=data_date, y = ttd_pnl, color = underlying))
