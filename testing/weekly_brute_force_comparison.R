library(tidyverse)


#####################
## reading in data ##
#####################
df_spy <- read_csv("data_output/spy_weekly_2014_2018_pnl_master.csv")
df_iwm <- read_csv("data_output/iwm_weekly_2014_2018_pnl_master.csv")
df_qqq <- read_csv("data_output/qqq_weekly_2014_2018_pnl_master.csv")
df_dia <- read_csv("data_output/dia_weekly_2014_2018_pnl_master.csv")
df_eem <- read_csv("data_output/eem_weekly_2014_2018_pnl_master.csv")
df_fxi <- read_csv("data_output/fxi_weekly_2014_2018_pnl_master.csv")
df_ewz <- read_csv("data_output/ewz_weekly_2014_2018_pnl_master.csv")
df_tlt <- read_csv("data_output/tlt_weekly_2014_2018_pnl_master.csv")
df_uso <- read_csv("data_output/uso_weekly_2014_2018_pnl_master.csv")
df_gld <- read_csv("data_output/gld_weekly_2014_2018_pnl_master.csv")
df_slv <- read_csv("data_output/slv_weekly_2014_2018_pnl_master.csv")
df_gdx <- read_csv("data_output/gdx_weekly_2014_2018_pnl_master.csv")
df_xle <- read_csv("data_output/xle_weekly_2014_2018_pnl_master.csv")
df_fxe <- read_csv("data_output/fxe_weekly_2014_2018_pnl_master.csv")


############################
## calculating daily PNLs ##
############################
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


df_dia_daily <-
    df_dia %>% 
    filter(variation == 0.1) %>% 
    group_by(data_date) %>% 
    summarize(pnl = sum(dly_tot_pnl)) %>% 
    mutate(
        ttd_pnl = cumsum(pnl)
        , underlying = "DIA"
    )


df_eem_daily <-
    df_eem %>% 
    filter(variation == 0.1) %>% 
    group_by(data_date) %>% 
    summarize(pnl = sum(dly_tot_pnl)) %>% 
    mutate(
        ttd_pnl = cumsum(pnl)
        , underlying = "EEM"
    )

df_fxi_daily <-
    df_fxi %>% 
    filter(variation == 0.1) %>% 
    group_by(data_date) %>% 
    summarize(pnl = sum(dly_tot_pnl)) %>% 
    mutate(
        ttd_pnl = cumsum(pnl)
        , underlying = "FXI"
    )

df_ewz_daily <-
    df_ewz %>% 
    filter(variation == 0.1) %>% 
    group_by(data_date) %>% 
    summarize(pnl = sum(dly_tot_pnl)) %>% 
    mutate(
        ttd_pnl = cumsum(pnl)
        , underlying = "EWZ"
    )


df_tlt_daily <-
    df_tlt %>% 
    filter(variation == 0.1) %>% 
    group_by(data_date) %>% 
    summarize(pnl = sum(dly_tot_pnl)) %>% 
    mutate(
        ttd_pnl = cumsum(pnl)
        , underlying = "TLT"
    )

df_uso_daily <-
    df_uso %>% 
    filter(variation == 0.1) %>% 
    group_by(data_date) %>% 
    summarize(pnl = sum(dly_tot_pnl)) %>% 
    mutate(
        ttd_pnl = cumsum(pnl)
        , underlying = "USO"
    )


df_gld_daily <-
    df_gld %>% 
    filter(variation == 0.1) %>% 
    group_by(data_date) %>% 
    summarize(pnl = sum(dly_tot_pnl)) %>% 
    mutate(
        ttd_pnl = cumsum(pnl)
        , underlying = "GLD"
    )

df_slv_daily <-
    df_slv %>% 
    filter(variation == 0.1) %>% 
    group_by(data_date) %>% 
    summarize(pnl = sum(dly_tot_pnl)) %>% 
    mutate(
        ttd_pnl = cumsum(pnl)
        , underlying = "SLV"
    )


df_gdx_daily <-
    df_gdx %>% 
    filter(variation == 0.1) %>% 
    group_by(data_date) %>% 
    summarize(pnl = sum(dly_tot_pnl)) %>% 
    mutate(
        ttd_pnl = cumsum(pnl)
        , underlying = "GDX"
    )

df_xle_daily <-
    df_xle %>% 
    filter(variation == 0.1) %>% 
    group_by(data_date) %>% 
    summarize(pnl = sum(dly_tot_pnl)) %>% 
    mutate(
        ttd_pnl = cumsum(pnl)
        , underlying = "XLE"
    )

df_fxe_daily <-
    df_fxe %>% 
    filter(variation == 0.1) %>% 
    group_by(data_date) %>% 
    summarize(pnl = sum(dly_tot_pnl)) %>% 
    mutate(
        ttd_pnl = cumsum(pnl)
        , underlying = "FXE"
    )

##################
## sharpe ratio ##
##################
(mean(df_spy_daily$pnl) / sd(df_spy_daily$pnl)) * sqrt(252)
(mean(df_iwm_daily$pnl) / sd(df_iwm_daily$pnl)) * sqrt(252)
(mean(df_qqq_daily$pnl) / sd(df_qqq_daily$pnl)) * sqrt(252)
(mean(df_dia_daily$pnl) / sd(df_dia_daily$pnl)) * sqrt(252)
(mean(df_eem_daily$pnl) / sd(df_eem_daily$pnl)) * sqrt(252)
(mean(df_fxi_daily$pnl) / sd(df_fxi_daily$pnl)) * sqrt(252)
(mean(df_ewz_daily$pnl) / sd(df_ewz_daily$pnl)) * sqrt(252)
(mean(df_tlt_daily$pnl) / sd(df_tlt_daily$pnl)) * sqrt(252)
(mean(df_uso_daily$pnl) / sd(df_uso_daily$pnl)) * sqrt(252)
(mean(df_gld_daily$pnl) / sd(df_gld_daily$pnl)) * sqrt(252)
(mean(df_slv_daily$pnl) / sd(df_slv_daily$pnl)) * sqrt(252)
(mean(df_gdx_daily$pnl) / sd(df_gdx_daily$pnl)) * sqrt(252)
(mean(df_xle_daily$pnl) / sd(df_xle_daily$pnl)) * sqrt(252)
(mean(df_fxe_daily$pnl) / sd(df_fxe_daily$pnl)) * sqrt(252)

##################
## graphing TTD ##
##################
df_all_daily <- 
    bind_rows(
        df_spy_daily
        # , df_iwm_daily
        # , df_qqq_daily
        # , df_dia_daily
        # , df_fxi_daily
        # , df_ewz_daily
        # , df_tlt_daily
        # , df_uso_daily
        # , df_gld_daily
        # , df_slv_daily
        # , df_gdx_daily
        # , df_xle_daily
        , df_fxe_daily
    )


df_all_daily %>% 
    ggplot() +
    geom_line(aes(x=data_date, y = ttd_pnl, color = underlying))
