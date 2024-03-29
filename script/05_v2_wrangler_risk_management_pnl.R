rm(list = ls())
cat("\014")

# loading packages
library(tidyverse)
library(tictoc)


df_underlying <-
    tibble(
        symbol = c("DIA", "EEM", "EWZ", "FXI" ,"GDX", "GLD"
                       , "IWM", "QQQ", "SLV", "SPY", "USO","XLE")
    )

#df_underlying <- tibble(symbol = c("SPY"))

for (ix in 1:nrow(df_underlying)){
    
    chr_underying <- df_underlying$symbol[ix]
    
    #####################
    ## reading in data ##
    #####################
    chr_path <- 
        paste0(
            "data_output/weekly/"
            , chr_underying
            , "_weekly_2014_2018_chain_pnl.csv"
        )
    df_chain_pnl_all <- read_csv(chr_path)
    
    
    
    df_chain_pnl_all %>% distinct(strategy, variation)
    
    df_chain_pnl <- 
        df_chain_pnl_all %>%
        filter(variation == 0.1) %>%
        filter(strategy == "strangle")
    
    
    df_strat_var_exp <- 
        df_chain_pnl %>%
        distinct(strategy, variation, expiration)
    
    lst_managed_pnl <- list()
    
    
    
    df_management_level <-
        crossing(
            dh_threshold = c(0, 1)
            , loss_trigger = c(seq(0.25, 10.00, 0.25), 1000)
            , win_trigger = c(seq(0.10, 1, 0.1), 1000)
            #, win_trigger = 1000
        )
    
    
    
    dt_start <- Sys.time()
    for(ix_level in 1:nrow(df_management_level)){
        
        tic()
        
        # setting risk management parameters
        dbl_loss_trigger <- df_management_level$loss_trigger[ix_level]
        dbl_win_trigger <- df_management_level$win_trigger[ix_level]
        dbl_dh_thresh <- df_management_level$dh_threshold[ix_level]
        
        
        for(ix_sve in 1:nrow(df_strat_var_exp)){
            
            chr_strategy <- df_strat_var_exp$strategy[]
            
            chr_strategy <- df_strat_var_exp$strategy[ix_sve]
            dbl_variation <- df_strat_var_exp$variation[ix_sve]
            dt_expiration <- df_strat_var_exp$expiration[ix_sve]
            
            
            df_sve_pnl <- 
                df_chain_pnl %>% 
                filter(strategy == chr_strategy) %>% 
                filter(variation == dbl_variation) %>% 
                filter(expiration == dt_expiration)
            
            
            #############################
            ## threshold delta-hedging ##
            #############################
            #initializing columns
            df_sve_pnl$thresh_hedge <- NA_real_  # the delta-hedge that is on EOD
            df_sve_pnl$thresh_dh_pnl <- NA_real_ # pnl from the delta-hedge
            df_sve_pnl$thresh_tot_pnl <- NA_real_ # opt_pnl + delta_hedge PNL
            dbl_hedge <- -df_sve_pnl$net_delta[1]
            for (ix_dt in 1:nrow(df_sve_pnl)){
                
                ## calculating delta-hedge
                dbl_delta_to_hedge <- df_sve_pnl$delta_to_hedge[ix_dt]
                if(ix_dt > 1){
                    # previous day delta
                    dbl_hedge <- df_sve_pnl$thresh_hedge[ix_dt - 1] 
                }
                # check if current delta after hedge is past threshold
                if (abs(dbl_delta_to_hedge + dbl_hedge) > dbl_dh_thresh){
                    # if so, then rebalance deltas
                    dbl_hedge <- -dbl_delta_to_hedge
                }
                df_sve_pnl$thresh_hedge[ix_dt] <- dbl_hedge 
                
                ## calculating pnl from delta-hedge
                if(ix_dt == 1){
                    df_sve_pnl$thresh_dh_pnl[ix_dt] <- 0
                } else {
                    dbl_prev_upx <- df_sve_pnl$upx[ix_dt - 1]
                    dbl_curr_upx <- df_sve_pnl$upx[ix_dt]
                    # use previous day delta to calculate pnl from delta
                    df_sve_pnl$thresh_dh_pnl[ix_dt] <-
                        df_sve_pnl$thresh_hedge[ix_dt - 1] * 
                        (dbl_curr_upx - dbl_prev_upx)
                }
                
                # total pnl: option + delta-hedge
                df_sve_pnl$thresh_tot_pnl[ix_dt] <-
                    df_sve_pnl$dly_opt_pnl[ix_dt] + df_sve_pnl$thresh_dh_pnl[ix_dt]
            }
            
            
            # calculatin the ttd pnl with threshold delta-hedging
            df_sve_pnl <- 
                df_sve_pnl %>% mutate(ttd_pnl = cumsum(thresh_tot_pnl))
            
            
            #####################
            ## position unwind ##
            #####################
            df_sve_pnl$breach_loss <- NA
            df_sve_pnl$breach_win <- NA
            df_sve_pnl$manage_mult <- NA_real_
            
            df_sve_pnl$breach_loss[1] <- FALSE
            df_sve_pnl$breach_win[1] <- FALSE
            df_sve_pnl$manage_mult[1] <- 1
            dbl_premium <- df_sve_pnl$bid[1]
            bln_breach_loss <- FALSE
            bln_breach_win <- FALSE
            for (ix_dt in 2:nrow(df_sve_pnl)){
                dbl_ttd <- df_sve_pnl$ttd_pnl[ix_dt]
                #dbl_prev_ttd <- df_sve_pnl$ttd_pnl[ix_dt - 1]
                
                # LOSS - if its not the final trading day 
                # and there hasn't been a breach yet, check for a breach
                if((ix_dt != nrow(df_sve_pnl)) & (!bln_breach_loss)){
                    # loss-breach
                    if ((dbl_ttd <  -(dbl_loss_trigger * dbl_premium))) {
                        bln_breach_loss <- TRUE 
                    }
                    
                }
                
                # WIN - if its not the final trading day 
                # and there hasn't been a breach yet, check for a breach
                if((ix_dt != nrow(df_sve_pnl)) & (!bln_breach_win)){
                    # win breach
                    if ((dbl_ttd > (dbl_win_trigger * dbl_premium))) {
                        bln_breach_win <- TRUE 
                    }
                }
                
                
                # updating the breach column
                df_sve_pnl$breach_loss[ix_dt] <- bln_breach_loss
                df_sve_pnl$breach_win[ix_dt] <- bln_breach_win
                
                # this is the logic for how it's going to affect subsequent PNLs
                # the position is unwound the day of the breach, so all subsequent
                # pnls after the breach are zeroed out
                df_sve_pnl$manage_mult[ix_dt] <-
                    as.integer(
                        !(df_sve_pnl$breach_loss[ix_dt - 1] | 
                          df_sve_pnl$breach_win[ix_dt - 1])
                    )
                
            }
            
            df_sve_pnl <- 
                df_sve_pnl %>% 
                mutate(
                    scaled_managed_pnl = scale_mult * manage_mult * thresh_tot_pnl
                    , dh_threshold = dbl_dh_thresh
                    , loss_trigger = dbl_loss_trigger
                    , win_trigger = dbl_win_trigger
                ) %>% 
                select(
                    strategy, variation, dh_threshold, loss_trigger, win_trigger
                    , expiration:scaled_managed_pnl
                    
                )
            
            
            
            
            lst_managed_pnl[[length(lst_managed_pnl) + 1]] <- df_sve_pnl    
        }
        
        toc() 
        chr_message <-
            paste0(
                chr_underying, ": "
                , ix_level, " of ", nrow(df_management_level), " complete.")
        print(chr_message)
        
    }
    
    
    # combining managed pnls in to a single dataframe
    df_managed_pnl <- bind_rows(lst_managed_pnl)
    
    dt_end <- Sys.time()
    
    #print("DONE!")
    #print(paste0("Start time: ", dt_start))
    #print(paste0("End time: ", dt_end))
    
    # write data to csv
    chr_path <- 
        paste0(
            str_to_lower(chr_underying)
            , "_weekly_2014_2018_managed_pnl_V2.csv"
        )
    write_csv(df_managed_pnl, chr_path)
    
    
    
}

    