chain_monthly <- function(chr_underlying){
    # sequency of dummy-dates, one per month, that will be used
    # to calculate the sequence of expiration dates
    dt_dummy <- 
        seq(lubridate::ymd(20131201), lubridate::ymd(20181201), "months")
    
    # data-frame that stores all the chain data
    df_exec_date <- 
        tibble::tibble(
              underlying = chr_underlying
            , dummy_date = dt_dummy
            , year = NA_integer_
            , month = NA_integer_
            , expiration = as.Date(NA)
            , last_trade_date = as.Date(NA)
            , execution = as.Date(NA)
        )
    
    # looping through all the chains and calcuating the dates
    for (ix in 1:nrow(df_exec_date)){
        int_year <- lubridate::year(df_exec_date$dummy_date[ix])
        int_month <- lubridate::month(df_exec_date$dummy_date[ix])
        df_exec_date$year[ix] <- int_year
        df_exec_date$month[ix] <- int_month
        df_exec_date$expiration[ix] <- 
            backtestr::monthly_expiration(int_year, int_month) 
        df_exec_date$last_trade_date[ix] <- 
            backtestr::monthly_last_td(int_year, int_month) 
    }
    
    df_exec_date$execution <- dplyr::lag(df_exec_date$last_trade_date)
    df_exec_date <- df_exec_date %>% dplyr::select(-dummy_date)
    df_exec_date <- df_exec_date[-1, ]
    df_exec_date$d2x = NA_integer_
    df_exec_date$num_opts = NA_integer_
    df_exec_date$exec_day_volume = NA_integer_
    
    
    if(chr_underlying == "SPY"){
        #this is a brute force hack to correct the 12/18/2015 expiration
        for(ix in 1:nrow(df_exec_date)){
            dt_expiration <- df_exec_date$expiration[ix]
            if (dt_expiration == as.Date("2015-12-18")){
                df_exec_date$expiration[ix] <- as.Date("2015-12-19")
            }
        }
    }
    
    
    
    df_exec_date    
}

