############################
## clearing out workspace ##
############################
rm(list = ls())
cat("\014")


######################
## loading packages ##
######################
library(tidyverse)
library(tidyquant)


########################
## setting underlying ##
########################
chr_underlying <- "FXI"


#####################
## reading in data ##
#####################
# chain description
chr_path <- 
    paste0(
        "data_output/", str_to_lower(chr_underlying)
        , "_weekly_2014_2018_chain_desc.csv"
    )
df_chain_desc <- read_csv(chr_path)

# chain history
chr_path <- 
    paste0(
        "data_output/", str_to_lower(chr_underlying)
        , "_weekly_2014_2018_chain_hist.csv"
    )
df_chain_hist <-read_csv(chr_path)

# option history
chr_path <- 
    paste0(
        "data_output/", str_to_lower(chr_underlying)
        , "_weekly_2014_2018_opt_hist.csv"
    )
df_opt_hist <- read_csv(chr_path)

# pnl master
chr_path <- 
    paste0(
        "data_output/", str_to_lower(chr_underlying)
        , "_weekly_2014_2018_pnl_master.csv"
    )
df_pnl_master <- read_csv(chr_path)

# trade master
chr_path <- 
    paste0(
        "data_output/", str_to_lower(chr_underlying)
        , "_weekly_2014_2018_trade_master.csv"
    )
df_trade_master <- read_csv(chr_path)



##############
## df_chain ##
##############
df_chain <- 
    df_chain_desc %>% 
        left_join(
            df_chain_hist
            , by = c("underlying", "expiration", "execution" = "trade_date")
        ) %>% 
        select(
            underlying, expiration, last_trade_date = last_trade_date.x
            , execution, d2x, ret, realized_vol, implied_vol = mid_swap_rate
        )


############
## df_upx ##
############
df_upx <- 
    tq_get(chr_underlying, from = "2014-01-03", to = "2018-12-29")


##############
## df_trade ##
##############
df_trade <- 
    df_trade_master %>% 
        filter(variation == 0.3) %>% 
        select(
            underlying, expiration, execution, last_trade_date, type, strike
            , data_date, upx = underlying_price, bid, ask, delta
        )


############
## df_pnl ##
############
df_pnl <- 
    df_pnl_master %>% 
        filter(variation == 0.3) %>% 
        select(
            underlying = underlying_symbol, upx = underlying_price, expiration
            , type, strike, data_date, bid, ask, delta, dly_opt_pnl, dly_dh_pnl
            , dly_tot_pnl
        )




###########################
## writing the CSV files ##
###########################
chr_file_name <-
    paste0(chr_underlying, "_chain.csv")
write_csv(df_chain, chr_file_name)

chr_file_name <-
    paste0(chr_underlying, "_upx.csv")
write_csv(df_upx, chr_file_name)

chr_file_name <-
    paste0(chr_underlying, "_trade.csv")
write_csv(df_trade, chr_file_name)

chr_file_name <-
    paste0(chr_underlying, "_pnl.csv")
write_csv(df_pnl, chr_file_name)
