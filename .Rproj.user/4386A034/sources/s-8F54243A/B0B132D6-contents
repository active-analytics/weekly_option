missing_data <- function(df_opt_px, df_opt_hist, dt_trade, df_spy_upx){
    
    # underlying price <- use stock-quote data and yahoo
    #dbl_underlying_price <- mean(df_opt_px$underlying_price, na.rm = TRUE)
    
    dbl_underlying_price <- 
        df_spy_upx %>% 
        filter(date == dt_trade) %>% 
        .$close %>% `[`(1)
                    
    # looping through and replacing what needs to be replaced
    for (ix in 1:nrow(df_opt_hist)){
        if(is.na(df_opt_hist$underlying_price[ix])){
            df_opt_hist$underlying_price[ix] <- dbl_underlying_price
        }
        if (is.na(df_opt_hist$data_date[ix])){
            df_opt_hist$data_date[ix] <- dt_trade
        }
    }
    
    df_opt_hist
}