#############
## purpose ##
#############
# The purpose of this script is to scale the PNLs correctly.


######################
## clearing session ##
######################
rm(list=ls())
cat("\014")


######################
## loading packages ##
######################
library(tidyverse)



#####################
## reading in data ##
#####################
df_chain <- 
    read_csv(
        "data_output/dia_weekly_2014_2018_chain_desc.csv"
        , col_types = cols()
    )

df_trade_master <- 
    read_csv(
        "data_output/dia_weekly_2014_2018_trade_master.csv"
        , col_types = cols()
    )

df_pnl_master <- 
    read_csv(
        "data_output/dia_weekly_2014_2018_pnl_master.csv"
        , col_types = cols()
    )



##################################
## creating df_position_scaling ##
##################################
df_call <- df_trade_master %>% dplyr::filter(type == "call")
df_put <- df_trade_master %>% dplyr::filter(type == "put")    

# The main idea behind this section is that we want to sell about $1 in premium
# every month to make sure that all the position sizes are similar. I decided
# to use the mid prices to scale the positions, rather than the bid, because
# I didn't want to take especially large positions in an option, just because
# it has a very wide bid/ask spread.
df_position_scaling <-
    df_chain %>% 
        left_join(
            df_put
            , by = c("underlying", "expiration", "execution")
            , suffix = c(".put", "put1")
        ) %>%
        select(
            variation, underlying, expiration, execution, d2x
            , bid, ask, mid
        ) %>% 
        inner_join(
            df_call
            , by = c("variation", "expiration", "underlying", "execution")
            , suffix = c(".put", ".call")
        ) %>% 
        select(
            variation, underlying, expiration, execution, d2x
            , bid.put, ask.put, mid.put
            , bid.call, ask.call, mid.call
        ) %>% 
        mutate(
            strangle_mult = 
                (d2x * 0.05) / (mid.put + mid.call)
            
            , strangle_prem_sold = 
                (bid.put + bid.call) * ((d2x * 0.05) / (mid.put + mid.call)) 
            
            , put_mult = (d2x * 0.05) / mid.put
            
            , put_prem_sold = 
                bid.put * ((d2x * 0.05) / mid.put)
            
            , call_mult = (d2x * 0.05) / mid.call
            
            , call_prem_sold = 
                bid.call * ((d2x * 0.05) / mid.call)
        )


# testing
df_position_scaling %>% 
    group_by(variation) %>% 
    summarize(
        tot_put = sum(put_prem_sold)
        , tot_all = sum(call_prem_sold)
        , tot_strangle = sum(strangle_prem_sold)
    )


#############################################################
## adding unity_mult position size scalar factor to df_pnl ##
#############################################################
# strangles
df_strangle_pnl_scaled <- 
    df_pnl_master %>% 
        left_join(
            df_position_scaling
            , by = 
                c("variation", "expiration", "underlying_symbol"= "underlying")
        ) %>% 
        select(
            underlying_symbol:dly_tot_mid_pnl, scale_mult = strangle_mult
        ) %>% 
        mutate(
        scaled_dly_opt_pnl = dly_opt_pnl * scale_mult
        , scaled_dly_tot_pnl = dly_tot_pnl * scale_mult
        , strategy = "strangle"
    )

# puts
df_put_pnl_scaled <-
    df_pnl_master %>% 
        dplyr::filter(type == "put") %>% 
        left_join(
            df_position_scaling
            , by = 
                c("variation", "expiration", "underlying_symbol"= "underlying")
        ) %>% 
        select(
            underlying_symbol:dly_tot_mid_pnl, scale_mult = put_mult
        ) %>% 
    mutate(
        scaled_dly_opt_pnl = dly_opt_pnl * scale_mult
        , scaled_dly_tot_pnl = dly_tot_pnl * scale_mult
        , strategy = "put"
    ) 

# calls
df_call_pnl_scaled <-
    df_pnl_master %>% 
        dplyr::filter(type == "call") %>% 
        left_join(
            df_position_scaling
            , by = 
                c("variation", "expiration", "underlying_symbol"= "underlying")
        ) %>% 
        select(
            underlying_symbol:dly_tot_mid_pnl, scale_mult = call_mult
        ) %>% 
        mutate(
            scaled_dly_opt_pnl = dly_opt_pnl * scale_mult
            , scaled_dly_tot_pnl = dly_tot_pnl * scale_mult
            , strategy = "call"
        )


# putting all scaled PNLs into a single dataframe
df_pnl_scaled <- 
    df_strangle_pnl_scaled %>% 
        bind_rows(df_put_pnl_scaled) %>% 
        bind_rows(df_call_pnl_scaled)


## adding signed-delta
df_pnl_scaled$signed_delta <- NA_real_
for (ix in 1:nrow(df_pnl_scaled)){
    chr_type <- df_pnl_scaled$type[ix]
    dbl_delta <- df_pnl_scaled$delta[ix]
    if (chr_type == "put"){
        df_pnl_scaled$signed_delta[ix] <- -dbl_delta
    } else {
        df_pnl_scaled$signed_delta[ix] <- dbl_delta
    }
}





#######################
## writing CSV files ##
#######################
#write_csv(df_position_scaling, "dia_weekly_2014_2018_position_scaling.csv")
#write_csv(df_pnl_scaled, "dia_weekly_2014_2018_pnl_scaled.csv")



#############
## TESTING ##
#############
# df_strangle_pnl_scaled  %>%
#     group_by(variation, data_date) %>%
#     summarize(
#         dly_naked = sum(scaled_dly_opt_pnl)
#         , dly_dh = sum(scaled_dly_tot_pnl)
#     ) %>%
#     group_by(variation) %>%
#     summarize(
#         #mean_naked = mean(dly_naked) * 252
#         #, mean_dh = mean(dly_dh) * 252
#          sharpe_naked = (mean(dly_naked) / sd(dly_naked)) * sqrt(252)
#         , sharpe_dh = (mean(dly_dh) / sd(dly_dh)) * sqrt(252)
#     )



# df_pnl_scaled %>% 
#     dplyr::filter(underlying %in% c("SPY", "IWM", "QQQ", "DIA")) %>% 
#     mutate(
#         scaled_naked = ttd_opt_pnl * unity_mult
#         , scaled_dh = ttd_tot_pnl * unity_mult
#     ) %>% 
#     dplyr::filter(variation == 0.3) %>%
#     group_by(trade_date) %>% 
#     summarize(
#         naked = sum(scaled_naked)
#         , delta_hedge = sum(scaled_dh)) %>% 
#     ggplot() +
#         geom_line(aes(x = trade_date, y = naked), color = 'green') +
#         geom_line(aes(x = trade_date, y = delta_hedge), color = 'blue')





# df_strangle_pnl_scaled  %>% 
#     filter(!expiration %in% c(as.Date("2015-04-02"), as.Date("2015-12-19"))) %>% 
#     filter(expiration <= "2018-11-30") %>% 
#     distinct(expiration)

# df_test <- read_csv("data_output/20181229_spy_weekly_pnl_strangle_5yr.csv")
# 
# 
# df_test %>%
#     filter(!expiration %in% c("2014-04-02", "2015-12-19")) %>%
#     filter(expiration <= "2018-11-30") %>%
#     mutate(
#         dly_naked = dly_opt_pnl * strangle_mult
#         , dly_dh = dly_tot_pnl * strangle_mult
#     ) %>%
#     group_by(variation, data_date) %>%
#     summarize(
#         dly_naked = sum(dly_naked)
#         , dly_dh = sum(dly_dh)
#     ) %>%
#     group_by(variation) %>%
#     summarize(
#         sharpe_naked = (mean(dly_naked) / sd(dly_naked)) * sqrt(252)
#         , sharpe_dh = (mean(dly_dh) / sd(dly_dh)) * sqrt(252)
#     )
# 
# 
# df_test %>% distinct(expiration) %>% View()
