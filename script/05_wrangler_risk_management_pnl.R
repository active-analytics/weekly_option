rm(list = ls())
cat("\014")

# loading packages
library(tidyverse)
library(tictoc)

#####################
## reading in data ##
#####################
df_chain_pnl_all <- 
    read_csv("data_output/dia_weekly_2014_2018_chain_pnl.csv")




df_chain_pnl_all %>% distinct(strategy, variation)

df_chain_pnl <- 
    df_chain_pnl_all #%>%
        # filter(variation == 0.1) %>% 
        # filter(strategy == "strangle")


df_strat_var_exp <- 
    df_chain_pnl %>%
        distinct(strategy, variation, expiration)

lst_managed_pnl <- list()


# df_management_level <-
#     crossing(
#         loss_trigger = seq(0.25, 10, .25)
#         , dh_threshold = seq(0, 1, 0.05)
#     )


df_management_level <-
    crossing(
          dh_threshold = 0.10
        , loss_trigger = 3.00
    )



dt_start <- Sys.time()
for(ix_level in 1:nrow(df_management_level)){
    
    tic()
    
    # setting risk management parameters
    dbl_loss_trigger <- df_management_level$loss_trigger[ix_level]
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
        df_sve_pnl$breach <- NA
        df_sve_pnl$manage_mult <- NA_real_
        
        df_sve_pnl$breach[1] <- FALSE
        df_sve_pnl$manage_mult[1] <- 1
        dbl_premium <- df_sve_pnl$bid[1]
        bln_breach <- FALSE
        for (ix_dt in 2:nrow(df_sve_pnl)){
            dbl_ttd <- df_sve_pnl$ttd_pnl[ix_dt]
            dbl_prev_ttd <- df_sve_pnl$ttd_pnl[ix_dt - 1]
            
            if((ix_dt != nrow(df_sve_pnl)) & (!bln_breach)){
                # if there hasn't been a breach yet, check for a breach
                if ((dbl_ttd <  -(dbl_loss_trigger * dbl_premium))) {
                    bln_breach <- TRUE 
                }
            }
            
            # updating the breach column
            df_sve_pnl$breach[ix_dt] <- bln_breach
            
            # this is the logic for how it's going to affect subsequent PNLs
            # the position is unwound the day of the breach, so all subsequent
            # pnls after the breach are zeroed out
            df_sve_pnl$manage_mult[ix_dt] <-
                as.integer(!df_sve_pnl$breach[ix_dt - 1])
            
        }
        
        df_sve_pnl <- 
            df_sve_pnl %>% 
            mutate(
                scaled_managed_pnl = scale_mult * manage_mult * thresh_tot_pnl
                , dh_threshold = dbl_dh_thresh
                , loss_trigger = dbl_loss_trigger
            ) %>% 
            select(
                strategy, variation, dh_threshold, loss_trigger
                , expiration:scaled_managed_pnl
                   
            )
        
        
        
        
        lst_managed_pnl[[length(lst_managed_pnl) + 1]] <- df_sve_pnl    
    }
    
    toc() 
    chr_message <-
        paste0(ix_level, " of ", nrow(df_management_level), " complete.")
    print(chr_message)
       
}


# combining managed pnls in to a single dataframe
df_managed_pnl <- bind_rows(lst_managed_pnl)

dt_end <- Sys.time()

print("DONE!")
print(paste0("Start time: ", dt_start))
print(paste0("End time: ", dt_end))

# write data to csv
#write_csv(df_managed_pnl, "dia_weekly_2014_2018_managed_pnl_010_300.csv")






#############
## testing ##
#############
# df_daily_pnl <-
#     df_managed_pnl %>% 
#         filter(strategy == "put") %>% 
#         filter(!expiration %in% c(as.Date("2015-04-02"), as.Date("2015-12-19"))) %>% 
#         filter(expiration <= as.Date("2018-11-30")) %>% 
#         group_by(data_date) %>% 
#         summarize(
#             dly_pnl = sum(scaled_managed_pnl)
#         )
#         
# 
# (mean(df_daily_pnl$dly_pnl) / sd(df_daily_pnl$dly_pnl)) * sqrt(252)
    