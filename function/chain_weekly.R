chain_weekly <- function(chr_underlying){
    
    df_expiration <-
        tibble::tibble(
            #friday = seq(ymd(20131213), ymd(20181221), "weeks")
            friday = seq(ymd(20140103), ymd(20181228), "weeks")
            , underlying = chr_underlying
            , monthly = NA 
            , expiration = as.Date(NA)
            , last_trade_date = as.Date(NA)
            , d2x = NA_integer_
            , num_opts = NA_integer_
            , exec_day_volume = NA_integer_
            , realized_vol = NA_real_
            , return = NA_real_
        )
    
    for (ix in 1:nrow(df_expiration)){
        
        dt_friday <- df_expiration$friday[ix]
        
        # is it third friday of the month
        bln_is_third <- backtestr::is_third_friday(dt_friday)
        
        # expiration
        if (bln_is_third == TRUE){
            dt_expiration <- 
                monthly_expiration(
                    lubridate::year(dt_friday)
                    , lubridate::month(dt_friday)
                ) 
        } else {
            dt_expiration <- dt_friday
            if(!bizdays::is.bizday(dt_friday)){
                dt_expiration <-
                    bizdays::add.bizdays(dt_friday, -1)
            }
        }
        
        # last trade-date 
        if (bln_is_third == TRUE){
            dt_last_td <- 
                monthly_last_td(
                    lubridate::year(dt_friday)
                    , lubridate::month(dt_friday)
                ) 
        } else {
            dt_last_td <- dt_expiration
        }
        
        # updating df_expiration
        df_expiration$monthly[ix] <- bln_is_third
        df_expiration$expiration[ix] <- dt_expiration
        df_expiration$last_trade_date[ix] <- dt_last_td
    }
    
    df_expiration <- 
        df_expiration %>% 
            dplyr::mutate(execution = lag(last_trade_date))
    
    df_expiration <- 
        df_expiration[-1, ] %>% 
        dplyr::select(
            underlying, monthly, expiration, last_trade_date
            , execution, d2x, num_opts, exec_day_volume
        )
    
    
    if(chr_underlying == "SPY"){
        #this is a brute force hack to correct the 12/18/2015 expiration
        for(ix in 1:nrow(df_expiration)){
            dt_expiration <- df_expiration$expiration[ix]
            if (dt_expiration == as.Date("2015-12-18")){
                df_expiration$expiration[ix] <- as.Date("2015-12-19")
            }
        }
    }
    
    

    df_expiration 
}


