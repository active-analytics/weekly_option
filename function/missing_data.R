missing_data <- function(df_opt_px, dt_trade, int_d2x, df_upx
                         , df_prev_opt_hist){
    
    
    
    # testing 
    # if (dt_trade == lubridate::ymd(20150604)){
    #     browser()    
    # }
    
    
    # grabbing underlying price from df_upx
    df_curr_upx <-
        df_upx %>% filter(date == dt_trade)
    if(nrow(df_curr_upx) == 1){
        dbl_underlying_price <- df_curr_upx$close[1]    
    } else {
        df_prev_upx <-
            df_upx %>% filter(date == bizdays::add.bizdays(dt_trade, -1))
        dbl_underlying_price <- df_prev_upx$close[1]
    }
    
    
    
    # adding previous implied vol to df_opt_px
    df_opt_px <- 
        df_opt_px %>%
            left_join(
                df_prev_opt_hist %>% 
                    select(
                        underlying_symbol, expiration
                        , type, strike, implied_vol
                    )
                , by = c("underlying_symbol", "expiration", "type", "strike")
            )
    
    
    # setting year length
    dbl_yrs = int_d2x / 252
    
    # looping through and replacing what needs to be replaced
    for (ix in 1:nrow(df_opt_px)){
        
       
        
        # underlying_price
        if(is.na(df_opt_px$underlying_price[ix])){
            df_opt_px$underlying_price[ix] <- dbl_underlying_price
        }
        # data_date
        if (is.na(df_opt_px$data_date[ix])){
            df_opt_px$data_date[ix] <- dt_trade
        }
        
        
        # if any of of the option prices are missing calculate the price
        if (is.na(df_opt_px$bid[ix]) | 
            is.na(df_opt_px$ask[ix]) | 
            is.na(df_opt_px$mid[ix]) ){
            chr_type = substr(df_opt_px$type[ix], 1, 1)
            dbl_strike =  df_opt_px$strike[ix]
            dbl_implied_vol = df_opt_px$implied_vol[ix]
            df_opt <-  
                fOptions::GBSCharacteristics(
                    TypeFlag = chr_type
                    , S = dbl_underlying_price
                    , X = dbl_strike
                    , Time = dbl_yrs
                    , r = 0
                    , b = 0
                    , sigma = dbl_implied_vol
                )
            
            # grabbing the premium we just calculated
            dbl_premium <- round(df_opt$premium, 2)
            
            rm(df_opt)   
        } 
        
        
        
        # bid price
        if(is.na(df_opt_px$bid[ix])){
            df_opt_px$bid[ix] <- dbl_premium
        }
        
        # ask price
        if(is.na(df_opt_px$ask[ix])){
            df_opt_px$ask[ix] <- max(dbl_premium, 0.01)
        }
        
        
        # mid price
        if(is.na(df_opt_px$mid[ix])){
            df_opt_px$mid[ix] <- 
                mean(c(df_opt_px$bid[ix], df_opt_px$ask[ix]))
        }
        
        
    }
    
    
    
    
    df_opt_px <-
        df_opt_px %>% select(-implied_vol)
}