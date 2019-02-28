# clearing out session
rm(list = ls())
cat("\014")


# loading packages
library(tidyverse)
library(bizdays)
library(tidyquant)


# initializing bizdays libraries
load_rmetrics_calendars(2013:2018)
bizdays.options$set(default.calendar="Rmetrics/NYSE")


chr_strategy = "strangle"
dbl_variation = 0.1
dbl_sitout_ratio = 2

#####################
## reading-in data ##
#####################
df_managed_pnl <-
    read_csv("data_output/dia_weekly_2014_2018_managed_pnl_100_200.csv")

df_chain_desc <- 
    read_csv("data_output/dia_weekly_2014_2018_chain_desc.csv")

df_scaling_all <- 
    read_csv("data_output/dia_weekly_2014_2018_position_scaling.csv")


df_chain_hist <-
    read_csv("data_output/dia_weekly_2014_2018_chain_hist.csv")



#########################
## calculating sitouts ##
#########################
# grabbing the PNL sold
df_premium <- 
    df_scaling_all %>%
    filter(variation == dbl_variation) %>% 
    select(expiration, premium = strangle_prem_sold)


# here is the data that is used for the sit-out strategy
df_sit_out <- 
    df_managed_pnl %>% 
    filter(strategy == chr_strategy) %>% 
    filter(variation == dbl_variation) %>% 
    group_by(expiration) %>% 
    summarize(
        exp_pnl = sum(scaled_managed_pnl)
    ) %>%
    left_join(
        df_premium
        , by = "expiration"
    ) %>% 
    select(expiration, premium, exp_pnl) %>% 
    mutate(pnl_ratio = exp_pnl / premium)


# calculating sit-outs
df_sit_out$sitout <- NA
df_sit_out$sitout[1] <- FALSE
for (ix in 2:nrow(df_sit_out)){
    dbl_prev_ratio <- df_sit_out$pnl_ratio[ix - 1]
    if (dbl_prev_ratio < -dbl_sitout_ratio){
        df_sit_out$sitout[ix] <- TRUE
    } else {
        df_sit_out$sitout[ix] <- FALSE
    }
}



# adding sitout column to df_scaling by joining to df_sit_out
df_scaling <- 
    df_scaling_all %>% 
    filter(variation == dbl_variation) %>% 
    left_join(
        df_sit_out %>% select(expiration, sitout)
        , by = "expiration"
    ) %>% 
    select(
        underlying, expiration, execution, d2x, bid_put = bid.put
        , bid_call = bid.call, strangle_mult, strangle_prem_sold
        , sitout
    )



###############################
## setting sizing parameters ##
###############################
dbl_starting_aum = 1000000 # starting with a million
dbl_commission <- 1.0
dbl_size_param <- 0.0025 # daily prem sold per dollar of AUM


# isolating the strangle PNLs
df_strangle_pnl <-
    df_managed_pnl %>%
    filter(strategy == chr_strategy) %>% 
    filter(variation == dbl_variation)



# joining on the sitout mult and calculating the situout pnl
df_strangle_pnl <-
    df_strangle_pnl %>%
    left_join(
        df_sit_out %>% select(expiration, sitout)
        , by = "expiration"
    ) %>%
    mutate(
        sitout_mult = as.integer(!sitout)
    ) %>%
    mutate(
        sitout_pnl = scaled_managed_pnl * sitout_mult
    )


# calculating the daily pnl and the ttd pnl of the full strategy
df_sitout_daily <-
    df_strangle_pnl %>%
    group_by(data_date) %>%
    summarize(
        dly_pnl = sum(scaled_managed_pnl)
        , dly_pnl_sitout = sum(sitout_pnl)
    ) %>%
    mutate(
        ttd_pnl = cumsum(dly_pnl)
        , ttd_pnl_sitout = cumsum(dly_pnl_sitout)
    )



#####################
## position sizing ##
#####################
# Here is how I am thinking about this:
# 1) During my trading, I sold $8 of premium per trading day, on a $10K account.  
# 2) I believe that on a professional platform, with delta-hedging, 
#    I would have been able to sell $25 of premium per day.  
# 3) $25 dollars per day on a $10K account amounts to selling $0.0025 of premium
#    , per trade date, per dollar of AUM.

# calculating premium sold per d2x, on average: 0.04818. So just under five cents.
df_scaling <- 
    df_scaling %>% 
    mutate(
        prem_per_d2x = strangle_prem_sold / d2x
        , implied_aum = prem_per_d2x / dbl_size_param
    ) 

# calculating the per expiration PNL
df_pnl_by_exp <-
    df_strangle_pnl %>% 
    group_by(expiration) %>% 
    summarize(
        exp_sitout_pnl = sum(sitout_pnl)
    )


# adding the expiration PNL to df_scaling
df_scaling <- 
    df_scaling %>% 
    left_join(
        df_pnl_by_exp 
        , by = "expiration"
    )


df_scaling$aum_start <- NA_real_
df_scaling$aum_end <- NA_real_
df_scaling$aum_position_mult <- NA_real_
df_scaling$aum_prem_sold <- NA_real_
df_scaling$num_opt_sold <- NA_real_
df_scaling$commission <- NA_real_
df_scaling$aum_pnl <- NA_real_
# loop through and calculate pnl and new aum per expiration
for(ix in 1:nrow(df_scaling)){
    
    #ix <- 2
    # starting AUM
    if (ix == 1){
        df_scaling$aum_start[ix] <- dbl_starting_aum
    } else {
        df_scaling$aum_start[ix] <- df_scaling$aum_end[ix - 1]
    }
    
    
    # aum_position_mult - gets you from "scaled" position to AUM sized position
    df_scaling$aum_position_mult[ix] <-
        df_scaling$aum_start[ix] / df_scaling$implied_aum[ix]
    
    
    # total amount of premium sold
    df_scaling$aum_prem_sold[ix] <- 
        (!df_scaling$sitout[ix]) *
        df_scaling$aum_position_mult[ix] * 
        df_scaling$strangle_prem_sold[ix]
    
    
    # number of options sold
    dbl_prem_sold <- df_scaling$aum_prem_sold[ix]
    dbl_bid_put <- df_scaling$bid_put[ix]
    dbl_bid_call <- df_scaling$bid_call[ix]
    df_scaling$num_opt_sold[ix] <-
        2 * ((dbl_prem_sold / (dbl_bid_put + dbl_bid_call)) / 100)
    
    # commissions
    df_scaling$commission[ix] <-
        df_scaling$num_opt_sold[ix] * dbl_commission
    
    # PNL
    df_scaling$aum_pnl[ix] <-
        df_scaling$exp_sitout_pnl[ix] * df_scaling$aum_position_mult[ix]
    
    # ending AUM
    df_scaling$aum_end[ix] <-
        df_scaling$aum_start[ix]  +
        df_scaling$aum_pnl[ix] -
        (df_scaling$commission[ix] * 1)
}


# adding in implied_forward and notional to df_scaling - in the future
# turn this into close prices instead, shouldn't really matter
df_scaling <- 
    df_scaling %>% 
    left_join(
        df_chain_hist %>% select(expiration, trade_date, implied_forward)
        , by = c("expiration", "execution" = "trade_date")
    ) %>% 
    mutate(
        notional = implied_forward * num_opt_sold * 100
    )



# adding the aum_mult to df_strangle, to cacluate daily PNLs
df_strangle_pnl <- 
    df_strangle_pnl %>% 
    left_join(
        df_scaling %>% select(expiration, aum_position_mult)
        , by = "expiration"
    )


# calculating the daily pnl and the ttd pnl of the full strategy
dbl_total_commission <- df_scaling$commission %>% sum()
int_trade_days <- df_strangle_pnl %>% distinct(data_date) %>% nrow()
dbl_dly_commission <- dbl_total_commission / int_trade_days
df_aum_daily <-
    df_strangle_pnl %>%
    group_by(data_date) %>%
    summarize(
        aum_pnl_sitout = sum(sitout_pnl * aum_position_mult) 
    ) %>% 
    mutate(
        # accounting for commissions here
        aum_pnl_sitout = aum_pnl_sitout - dbl_dly_commission
    ) %>%
    mutate(
        aum_ttd_pnl = cumsum(aum_pnl_sitout)
        , eod_aum = dbl_starting_aum + aum_ttd_pnl
        , dly_ret = aum_pnl_sitout / lag(eod_aum) 
        , drawdown =  eod_aum - cummax(eod_aum) 
    ) 
df_aum_daily$dly_ret[1] <- 0   



#########################
## comparison with SPY ##
#########################
dt_min <- df_chain_hist$trade_date %>% min()
dt_max <- df_chain_hist$trade_date %>% max()
# getting data from yahoo finance using tidyquant
df_spy <- 
    tq_get(c("SPY")
           , get = "stock.prices"
           , from = dt_min     #"2013-12-20"
           , to = (dt_max + 1) #"2018-12-21"
    )

# calculating returns from adjusted prices
df_spy <- 
    df_spy %>% 
    mutate(
        return = (adjusted / lag(adjusted)) - 1
    )
df_spy$return[1] <- 0


# calculting pnl,ttd_pnl, eod_aum, drawdown
df_spy <- 
    df_spy %>% 
    mutate(
        pnl = return * dbl_starting_aum
        , ttd_pnl = cumsum(pnl)
        , eod_aum = ttd_pnl + dbl_starting_aum
        , drawdown =  eod_aum - cummax(eod_aum) 
    )


# joining up the two strategies (this is probably an extraneous step
# but it avoids issues when there are slightly different trade dates for 
# whatever reason)
df_comp <- 
    df_spy %>% 
    left_join(
        df_aum_daily
        , by = c("date" = "data_date")
    ) %>% 
    select(
        trade_date = date, close, adjusted
        , spy_ret = return, spy_pnl = pnl, spy_ttd = ttd_pnl
        , spy_aum = eod_aum.x, opt_pnl = aum_pnl_sitout
        , opt_ttd = aum_ttd_pnl, opt_aum = eod_aum.y, opt_ret = dly_ret
    )


# creating a "tidy" dataframe for easy comparison 
# of these trading the two trading strategies
df_strategy_comp <- 
    (
        df_comp %>% 
            mutate(
                strategy = "spy"
            ) %>% 
            select(
                strategy, trade_date, close, adjusted
                , dly_ret = spy_ret, dly_pnl = spy_pnl
                , ttd_pnl = spy_ttd, eod_aum = spy_aum
            )
    ) %>% 
    bind_rows(
        df_comp %>% 
            mutate(
                strategy = "option_selling"
            ) %>% 
            select(
                strategy, trade_date, close, adjusted
                , dly_ret = opt_ret, dly_pnl = opt_pnl
                , ttd_pnl = opt_ttd, eod_aum = opt_aum
            )  
    )


#################################
## writing data files to a CSV ##
#################################
#write_csv(df_scaling, "spy_weekly_2014_2018_aum_by_expiration.csv")
#write_csv(df_aum_daily, "spy_weekly_2014_2018_aum_daily.csv")
#write_csv(df_strategy_comp, "spy_weekly_2014_2018_strategy_comparison.csv")


###################
###################
## ANALYSIS CODE ##
###################
###################


# #######################################
# ## total PNL vs Max Profit - Unsized ##
# #######################################
# # comparison over the entire period
# df_sitout_daily$dly_pnl_sitout %>% sum()
# df_scaling %>% 
#     filter(!sitout) %>% 
#     .$strangle_prem_sold %>% 
#     sum()
# 
# 
# 
# 
# # comparison during my 2018 trading window
# df_strangle_pnl %>%
#     filter(expiration > "2018-03-16") %>% 
#     filter(expiration <= "2018-12-21") %>% 
#     group_by(data_date) %>%
#     summarize(
#         dly_pnl = sum(scaled_managed_pnl)
#         , dly_pnl_sitout = sum(sitout_pnl)
#     ) %>%
#     mutate(
#         ttd_pnl = cumsum(dly_pnl)
#         , ttd_pnl_sitout = cumsum(dly_pnl_sitout)
#     ) %>% 
#     .$dly_pnl_sitout %>% 
#     sum()
# 
# 
# df_scaling %>% 
#     filter(expiration > "2018-03-16") %>% 
#     filter(expiration <= "2018-12-21") %>% 
#     filter(!sitout) %>% 
#     .$strangle_prem_sold %>% 
#     sum()
# 
# 
# 
# # comparison during before 2018 - the good times
# df_strangle_pnl %>%
#     filter(expiration < "2018-01-01") %>%
#     group_by(data_date) %>%
#     summarize(
#         dly_pnl = sum(scaled_managed_pnl)
#         , dly_pnl_sitout = sum(sitout_pnl)
#     ) %>%
#     mutate(
#         ttd_pnl = cumsum(dly_pnl)
#         , ttd_pnl_sitout = cumsum(dly_pnl_sitout)
#     ) %>%
#     .$dly_pnl_sitout %>%
#     sum()
# 
# 
# df_scaling %>%
#     filter(expiration < "2018-01-01") %>%
#     filter(!sitout) %>%
#     .$strangle_prem_sold %>%
#     sum()



###############################################
## option-selling Sharpe-Ratio using Returns ##
###############################################
# ((df_aum_daily$dly_ret %>% mean(na.rm=TRUE)) / 
#     (df_aum_daily$dly_ret %>% sd(na.rm=TRUE)) ) * sqrt(252)
# 
# dbl_ending_aum <- df_aum_daily$eod_aum[nrow(df_aum_daily)]
# 
# dt_start <- as.Date("2013-12-20")
# dt_end <- as.Date("2018-12-28")
# int_td <- bizdays(dt_start, dt_end)
# (dbl_ending_aum / dbl_starting_aum) ^ (252 / int_td) - 1
# 
# 
# df_scaling$aum_prem_sold %>% sum()
# 
# df_scaling$num_opt_sold %>% sum()
# df_scaling$commission %>% sum()
# df_scaling$aum_end[nrow(df_scaling)]
# 
# df_scaling$num_opt_sold %>% mean()
# 
# average notional
# df_scaling %>% 
#     mutate(
#         notional = implied_forward * num_opt_sold * 100
#     ) %>% 
#     .$notional %>% 
#     mean()




#####################
## SPY performance ##
#####################
# dbl_ending_aum_spy <- df_spy$eod_aum[nrow(df_spy)]
# dt_start <- as.Date("2013-12-20")
# dt_end <- as.Date("2018-12-28")
# int_td <- bizdays(dt_start, dt_end)
# (dbl_ending_aum_spy / dbl_starting_aum) ^ (252 / int_td) - 1


##################
## correlations ##
##################
# cor(df_ret_comp$spy_ret, df_ret_comp$opt_ret, method = "pearson")
# cor(df_ret_comp$spy_ret, df_ret_comp$opt_ret, method = "kendall")
# cor(df_ret_comp$spy_ret, df_ret_comp$opt_ret, method = "spearman")

###################################
## comparing overall performance ##
###################################
# analysis code
df_strategy_comp %>%
    group_by(strategy) %>%
    summarize(
        total_pnl = sum(dly_pnl)
        , ann_ret =
            (((total_pnl + dbl_starting_aum) / dbl_starting_aum) ^
                 (252 / int_trade_days)) - 1
        , sharpe = (mean(dly_ret) / sd(dly_ret)) * sqrt(252)
    )


df_strategy_comp %>% filter(is.na(dly_pnl))
########################################
## visualizing cumulative performance ##
########################################
# # cummulative performance
df_strategy_comp %>%
    ggplot() +
    geom_line(aes(x = trade_date, y = eod_aum, color = strategy))

# 
# # return by year
# df_strategy_comp %>%
#     filter(trade_date >= "2014-01-01") %>%
#     mutate(
#         year = lubridate::year(trade_date)
#     ) %>%
#     group_by(strategy, year) %>%
#     summarize(
#         annual_return = prod(1 + dly_ret) - 1
#     ) %>%
#     ungroup() %>%
#     ggplot(aes(factor(year), annual_return, fill = strategy)) +
#     geom_bar(stat = "identity", position = "dodge") +
#     labs(
#         title = "annual returns"
#         , x = "year"
#         , y = "return"
# 
#     )
# 
# 
# # sharpe by year
# df_strategy_comp %>%
#     filter(trade_date >= "2014-01-01") %>%
#     mutate(
#         year = lubridate::year(trade_date)
#     ) %>%
#     group_by(strategy, year) %>%
#     summarize(
#         sharpe_ratio = (mean(dly_ret) / sd(dly_ret)) * sqrt(252)
#     ) %>%
#     ungroup() %>%
#     ggplot(aes(factor(year), sharpe_ratio, fill = strategy)) +
#     geom_bar(stat = "identity", position = "dodge") +
#     labs(
#         title = "annual sharpe-ratio"
#         , x = "year"
#         , y = "sharpe-ratio"
# 
#     )
# 
# 
# 
# # return by month
# df_strategy_comp %>%
#     filter(trade_date >= "2014-01-01") %>%
#     filter(strategy == "option_selling") %>%
#     mutate(
#         year = lubridate::year(trade_date)
#         , month = lubridate::month(trade_date)
#     ) %>%
#     group_by(strategy, year, month) %>%
#     summarize(
#         monthly_return = prod(1 + dly_ret) - 1
#     ) %>%
#     ungroup() %>%
#     ggplot(aes(factor(month), monthly_return, fill = strategy)) +
#     geom_bar(stat = "identity", position = "dodge") +
#     facet_wrap(~year, nrow = 5) +
#     labs(
#         title = "monthly returns"
#         , x = "month"
#         , y = "return"
# 
#     )














