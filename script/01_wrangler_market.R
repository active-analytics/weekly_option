# clearing shit out
rm(list=ls())
cat("\014")

# loading packages
library(tidyverse)
library(lubridate)
library(bizdays)
library(tictoc)
library(backtestr)
library(tidyquant)

# sourcing functions
source("function/missing_data.R")
source("function/chain_weekly.R")
source("function/chain_monthly.R")

# initializing bizdays libraries
load_rmetrics_calendars(2000:2020)
bizdays.options$set(default.calendar="Rmetrics/NYSE")


# temp for testing
df_num_opt <-
    read_csv("data_output/weekly_etf_with_index_num_opt.csv")
    


# setting underlying
underlying <- "TLT" # don't call this chr_underlying

# creating df_chain
df_chain <- chain_weekly(underlying)
#df_chain <- chain_monthly(underlying)


# grab all close prices from Yahoo using tq_get()
df_upx <- 
    tq_get(underlying, get = "stock.prices", from = "2013-01-01")


tic()
#df_chain_hist = tibble()
#df_opt_hist = tibble()
lst_chain_hist <- list()
lst_opt_hist <- list()

# looping through all chains
for (ix_chn in 1:nrow(df_chain)){
#for (ix_chn in 74:74){    
    
    
    # grabbing the execution and expriation
    chr_underlying <- df_chain$underlying[ix_chn]
    dt_execution <- df_chain$execution[ix_chn]
    dt_expiration <- df_chain$expiration[ix_chn]
    dt_last_trade <- df_chain$last_trade_date[ix_chn]
    
    # calculuating days to expiration
    int_d2x = bizdays(dt_execution, dt_expiration)   
    
    # grabbing the option chain on execution from database
    df_opt_all <- 
        option_chain(
            #db_conn = db_connection()
             trade_date = dt_execution
            , underlying = chr_underlying
            , expiration = dt_expiration
            , exclude_zero_bid = TRUE
        )
    
    
    #------------------------#
    # wrangling calculations #
    #------------------------#
    # calculating implied forward        
    dbl_implied_forward <- implied_forward(df_opt_all)
    # all otm options relative to implied foward
    df_otm_all <- otm_all(df_opt_all, dbl_implied_forward)
    # removing low information options
    df_otm <- otm_clean(df_otm_all)
    # recalculating greeks
    df_otm <- greeks(df_otm, int_d2x, dbl_implied_forward)
    
    
    
    #---------------------------------------------------#
    # calculating realized volatility during expiration #
    #---------------------------------------------------#
    dbl_realized_vol <- 
        df_upx %>% 
            filter(date >= dt_execution) %>%  
            filter(date <= dt_expiration) %>% 
            mutate(ret = log(adjusted) - log(lag(adjusted))) %>% 
            .$ret %>% 
            sd(na.rm = TRUE) * sqrt(252)
    
    
    dbl_return <- 
        df_upx %>% 
        filter(date >= dt_execution) %>%  
        filter(date <= dt_expiration) %>% 
        mutate(ret = log(adjusted) - log(lag(adjusted))) %>% 
        .$ret %>% sum(na.rm = TRUE)
    
    
    #-------------------#
    # updating df_chain #
    #-------------------#
    # recording int_d2x 
    df_chain$d2x[ix_chn] <- int_d2x
    #recording execution day volume in df_chain
    df_chain$exec_day_volume[ix_chn] <- df_opt_all$volume %>% sum()
    # recording number of options
    df_chain$num_opts[ix_chn] <- df_otm %>% nrow()
    # recording realized volatility
    df_chain$realized_vol[ix_chn] <- dbl_realized_vol
    # recording return
    df_chain$ret[ix_chn] <- dbl_return
    
    
    
    #------------------------#
    # updating df_chain_hist #
    #------------------------#
    dbl_swap_rate <- swap_rate(df_otm, int_d2x) #change this to named vector
    lst_chain_hist[[length(lst_chain_hist) + 1]] <-
        tibble(
            underlying = chr_underlying
            , expiration = dt_expiration
            , trade_date = dt_execution
            , last_trade_date = dt_last_trade
            , implied_forward = dbl_implied_forward
            , bid_swap_rate = dbl_swap_rate[1]
            , ask_swap_rate = dbl_swap_rate[2]
            , mid_swap_rate = dbl_swap_rate[3]
        )

       
    #----------------------#
    # updating df_opt_hist #
    #----------------------#
    lst_opt_hist[[length(lst_opt_hist) + 1]] <- df_otm
    
    
    #------------------------------------------------------------------------#
    # looping through all trade days of this expiration and grabbing px info #
    #------------------------------------------------------------------------#
    # sequence of post-execution business days
    dt_post_exec_td <- 
        bizseq(add.bizdays(dt_execution, 1), dt_expiration)
    
    
    df_prev_opt_hist <- df_otm
    #loop through the trading days and grab
    #the price history for all the options in df_otm
    for (ix_td in 1:(length(dt_post_exec_td))){
        
       
        dt_trade <- dt_post_exec_td[ix_td]
        
        
        
        # calculuating days to expiration
        int_d2x = bizdays(dt_trade, dt_expiration)
        
        
        # grabbing all option prices for trade date
        df_opt_px_all <-
           option_chain(
                trade_date = dt_trade
               , underlying = chr_underlying
               , expiration = dt_expiration
               , exclude_zero_bid = FALSE
           )
        
        
        # testing
        # if (dt_trade == lubridate::ymd(20150604)){
        #     browser()
        # }
        
        #---------------------------------------#
        # calculating the implied forward price #
        #---------------------------------------#
        if(dt_trade == dt_last_trade){
            # dbl_implied_forward <- 
            #     df_upx %>% 
            #         filter(date == dt_trade) %>% 
            #         .$close %>% `[`(1)
            
            df_curr_upx <-
                df_upx %>% filter(date == dt_trade)
            if(nrow(df_curr_upx) == 1){
                dbl_implied_forward <- df_curr_upx$close[1]    
            } else {
                df_prev_upx <-
                    df_upx %>% filter(date == bizdays::add.bizdays(dt_trade, -1))
                dbl_implied_forward <- df_prev_upx$close[1]
            }
            
        } else {
            # this is not a very robust condition, but I'll use it
            # for now, ultimately need to make the implied forward
            # function more robust
            if (nrow(df_opt_px_all) >= nrow(df_otm)) {
                dbl_implied_forward <- implied_forward(df_opt_px_all)    
            } else {
                # dbl_implied_forward <-
                #     df_upx %>% 
                #     filter(date == dt_trade) %>% 
                #     .$close %>% `[`(1)
                
                df_curr_upx <-
                    df_upx %>% filter(date == dt_trade)
                if(nrow(df_curr_upx) == 1){
                    dbl_implied_forward <- df_curr_upx$close[1]    
                } else {
                    df_prev_upx <-
                        df_upx %>% filter(date == bizdays::add.bizdays(dt_trade, -1))
                    dbl_implied_forward <- df_prev_upx$close[1]
                }
            }
                
        }
        
        
        if (nrow(df_opt_px_all) >= nrow(df_otm)) {
            # all otm options relative to implied foward
            df_curr_otm_all <- otm_all(df_opt_px_all, dbl_implied_forward)
            # removing low information options
            df_curr_otm <- otm_clean(df_curr_otm_all)   
        }
        
        
        
        # calculating swap rates - set to zero on expiration
        if(dt_trade == dt_last_trade){
            dbl_swap_rate <- c(0, 0, 0)
        } else {
            if (nrow(df_opt_px_all) >= nrow(df_otm)){
                dbl_swap_rate <- swap_rate(df_curr_otm, int_d2x)  
            } else {
                dbl_swap_rate <- c(NA_real_, NA_real_, NA_real_)
            }
        }
        
        #---------------------#
        # updating chain hist #
        #---------------------#
        lst_chain_hist[[length(lst_chain_hist) + 1]] <-
            tibble(
                underlying = chr_underlying
                , expiration = dt_expiration
                , trade_date = dt_trade
                , last_trade_date = dt_last_trade
                , implied_forward = dbl_implied_forward
                , bid_swap_rate = dbl_swap_rate[1]
                , ask_swap_rate = dbl_swap_rate[2]
                , mid_swap_rate = dbl_swap_rate[3]
            )
        
        
        #------------------------------------------#
        # filtering for only the execution day otm #
        #------------------------------------------#
        df_opt_px <-
           df_otm %>%
               select(underlying_symbol, expiration, type, strike) %>%
               left_join(
                   df_opt_px_all
                   , by = c("underlying_symbol", "type", "strike", "expiration")
               )
        
        # filling in some missing data in case of empty prices
        df_opt_px <- 
            missing_data(
                 df_opt_px
                , dt_trade
                , int_d2x
                , df_upx
                , df_prev_opt_hist)
        
        
        
        
        
        # recalculating greeks
        if(dt_trade == dt_last_trade){
            df_opt_px <- greeks_exp(df_opt_px)
        } else {
            #if (nrow(df_opt_px_all) > 0){
                df_opt_px <- greeks(df_opt_px, int_d2x, dbl_implied_forward)    
            #}
                
        }
        
        #----------------------#
        # updating df_opt_hist #
        #----------------------#
        # update if it's either the final trading
        # or if there is actually price data 
        #if ((nrow(df_opt_px_all) > 0) | (dt_trade == dt_last_trade)){
            lst_opt_hist[[length(lst_opt_hist) + 1]] <- df_opt_px
        #}
        
        df_prev_opt_hist <- df_opt_px
        
    }
    
    print(paste0(chr_underlying, ": ", dt_expiration))
}
toc()


df_chain_hist <- bind_rows(lst_chain_hist)
df_opt_hist <- bind_rows(lst_opt_hist)

#-------------------#
# writing csv files #
#-------------------#
write_csv(df_chain, "tlt_weekly_2014_2018_chain_desc.csv")
write_csv(df_chain_hist, "tlt_weekly_2014_2018_chain_hist.csv")
write_csv(df_opt_hist, "tlt_weekly_2014_2018_opt_hist.csv")



