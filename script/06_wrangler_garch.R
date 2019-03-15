# clearing shit out
rm(list=ls())
cat("\014")

# load packages
library(tidyverse)
library(backtestr)
library(rugarch)
library(bizdays)
library(tidyquant)


# initializing bizdays libraries
load_rmetrics_calendars(2000:2020)
bizdays.options$set(default.calendar="Rmetrics/NYSE")



df_underlying <- 
    tibble(
        symbol = c("DIA", "EEM", "EWZ", "FXI" ,"GDX", "GLD"
                   , "IWM", "QQQ", "SLV", "SPY", "USO","XLE")
    )


for (ix in 1:nrow(df_underlying)){
    
    chr_underlying <- df_underlying$symbol[ix]
    
    # reading in chain history
    chr_path <- 
        paste0(
            "data_output/monthly/"
            , chr_underlying
            , "_monthly_2014_2018_chain_desc.csv"
        )
    df_chain_desc <- read_csv(chr_path)
    
    
    df_chain_desc$garch_forecast <- NA_real_
    spec <- ugarchspec()
    for (ix_exp in 1:nrow(df_chain_desc)){
        
        
        #chr_underlying <- "SPY"
        dt_execution <- df_chain_desc$execution[ix_exp]
        int_d2x <- df_chain_desc$d2x[ix_exp]
        dt_garch_start <- add.bizdays(dt_execution, -2520)
        
        
        # using tidyquant to get the prices from yahoo
        if(int_d2x != 0){
            df_px <- 
                tryCatch(
                    tq_get(
                        chr_underlying
                        , get = "stock.prices"
                        , from = dt_garch_start
                        , to = dt_execution
                    ) 
                    , warning = function(cond) return(tibble(adjusted = NA_real_))
                    , error = function(cond) return(tibble(adjusted = NA_real_))
                )
            
            # if there are no missing prices then fitting the data
            if(sum(is.na(df_px$adjusted))==0){
                df_px <- df_px %>% mutate(ret = adjusted/lag(adjusted) - 1)
                
                # fitting the model
                fit <- ugarchfit(spec, df_px$ret[2:nrow(df_px)], solver = 'hybrid')
                
                # forcasting until expiration using the fit
                ugfore <- ugarchforecast(fit, n.ahead = int_d2x)
                
                # calculated volatility until maturity
                dbl_vol_forecast <- ugfore@forecast$sigmaFor %>% mean() * sqrt(252)
                
                df_chain_desc$garch_forecast[ix_exp] <- dbl_vol_forecast    
            }    
        }
        
        
        # printing progress to screen
        print(
            paste0(chr_underlying, ": ", ix_exp, " of ", nrow(df_chain_desc))
        )
    }   
    
    # write data to csv
    chr_path <- 
        paste0(
            str_to_lower(chr_underlying)
            , "_monthly_2014_2018_garch.csv"
        )
    write_csv(df_chain_desc, chr_path)
}
