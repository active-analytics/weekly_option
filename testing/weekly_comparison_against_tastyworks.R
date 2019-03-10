# clearing out
rm(list = ls())
cat("\014")


# loading packages
library(tidyverse)
library(backtestr)
library(bizdays)



# initializing bizdays libraries
load_rmetrics_calendars(2000:2020)
bizdays.options$set(default.calendar="Rmetrics/NYSE")


chr_path <- "/Users/Pritam/Desktop/L3_20190301/L3_options_20190301.csv"
df_option_all <-
    backtestr::csv_option_all(chr_path)


chr_underlying_all <- 
    c("SPY", "GLD", "IWM", "QQQ", "DIA", "FXI", "EEM", "SLV", "GDX"
      , "USO", "EWZ", "TLT", "XLE")


dt_expiration <- as.Date("2019-03-08")
dt_analysis <- as.Date("2019-03-01")
dbl_strangle_delta <- 0.10

df_universe <-
    df_option_all %>% 
        filter(underlying_symbol %in% chr_underlying_all) %>% 
        filter(expiration == dt_expiration) %>% 
        distinct(underlying_symbol, expiration) %>% 
        mutate(
            analysis_date = dt_analysis
        )


# looping through and calculating some stuff for each underlying in
# df_universe
int_d2x <- bizdays(dt_analysis, dt_expiration)
df_universe$implied_forward <- NA_real_
df_universe$bid_swap_rate <- NA_real_
df_universe$ask_swap_rate <- NA_real_
df_universe$mid_swap_rate <- NA_real_
df_universe$put_delta <- NA_real_
df_universe$call_delta <- NA_real_
df_universe$put_strike <- NA_real_
df_universe$call_strike <- NA_real_
df_universe$put_bid <- NA_real_
df_universe$call_bid <- NA_real_
lst_opt_to_trade <- list()
lst_strangle <- list()

for (ix_und in 1:nrow(df_universe)){
    chr_underlying <-
        df_universe$underlying_symbol[ix_und]
    
    
    df_opt_all <- 
        df_option_all %>% 
        dplyr::filter(underlying_symbol == chr_underlying) %>% 
        dplyr::filter(expiration == dt_expiration) %>% 
        dplyr::filter(bid > 0)
    
    # calculating implied forward        
    dbl_implied_forward <- implied_forward(df_opt_all)
    # all otm options relative to implied foward
    df_otm_all <- otm_all(df_opt_all, dbl_implied_forward)
    # removing low information options
    df_otm <- otm_clean(df_otm_all)
    # recalculating greeks
    df_otm <- greeks(df_otm, int_d2x, dbl_implied_forward)
    # swap rates
    dbl_swap_rate <- swap_rate(df_otm, int_d2x)
    
    # select for 10-delta put 
    df_put <-
        df_otm %>% 
        dplyr::filter(type == "put")  %>% 
        dplyr::filter(
            abs(delta - dbl_strangle_delta) == min(abs(delta - dbl_strangle_delta))
        )
    
    # select for 10-delta call
    df_call <-
        df_otm %>% 
        dplyr::filter(type == "call")  %>% 
        dplyr::filter(
            abs(delta - dbl_strangle_delta) == min(abs(delta - dbl_strangle_delta))
        )
    
    
    # updating df_universe
    df_universe$implied_forward[ix_und] <- dbl_implied_forward
    df_universe$bid_swap_rate[ix_und] <- dbl_swap_rate[1]
    df_universe$ask_swap_rate[ix_und] <- dbl_swap_rate[2]
    df_universe$mid_swap_rate[ix_und] <- dbl_swap_rate[3]
    df_universe$put_delta[ix_und] <- df_put$delta[1]
    df_universe$call_delta[ix_und] <- df_call$delta[1]
    df_universe$put_strike[ix_und] <- df_put$strike[1]
    df_universe$call_strike[ix_und] <- df_call$strike[1]
    df_universe$put_bid[ix_und] <- df_put$bid[1]
    df_universe$call_bid[ix_und] <- df_call$bid[1]
    
    # collecting all otm options
    lst_opt_to_trade[[ix_und]] <- df_otm
    
    # strangles that will be traded
    lst_strangle[[ix_und]] <- bind_rows(df_put, df_call)
    
    # printing progress to screen
    print(
        paste0(chr_underlying, ": ", ix_und, " of ", nrow(df_universe))
    )
} 

# converting lists into dataframes
df_opt_to_trade <- bind_rows(lst_opt_to_trade)
df_strangle <- bind_rows(lst_strangle)



########################
## writing data files ##
########################
write_csv(df_universe, "tastyworks_comparison_universe.csv")
write_csv(df_opt_to_trade, "tastyworks_comparison_otm_opt.csv")
write_csv(df_strangle, "tastywrks_comparison_strangle.csv")
