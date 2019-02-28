##########################
## clearing out session ##
##########################
rm(list = ls())
cat("\014")



######################
## loading packages ##
######################
library(tidyverse)
library(tictoc)


#####################
## reading-in data ##
#####################
df_chain_desc <- 
    read_csv("data_output/dia_weekly_2014_2018_chain_desc.csv")

df_scaled_pnl <- 
    read_csv("data_output/dia_weekly_2014_2018_pnl_scaled.csv")


# create the dataframe consisting of all strategy, variation, expiration
df_strat_var_exp <-
    df_scaled_pnl %>% 
        distinct(strategy, variation, expiration) %>% 
        select(strategy, variation, expiration)


tic()
lst_chain_pnl <- list()
for (ix in 1:nrow(df_strat_var_exp)){
    #ix <- 1
    
    chr_strategy <- df_strat_var_exp$strategy[ix]
    dbl_variation <- df_strat_var_exp$variation[ix]
    dt_expiration <- df_strat_var_exp$expiration[ix]
    
    
    df_trade_pnl <- 
        df_scaled_pnl %>% 
            filter(strategy == chr_strategy) %>% 
            filter(variation == dbl_variation) %>% 
            filter(expiration == dt_expiration)
    
    
    df_sve_pnl <- 
        df_trade_pnl %>% 
        group_by(data_date) %>% 
        summarize(
              upx = mean(underlying_price)
            , scale_mult = mean(scale_mult)
            , bid = sum(bid)
            , ask = sum(ask)
            , dly_opt_pnl = sum(dly_opt_pnl)
            , dly_tot_pnl = sum(dly_tot_pnl)
            , net_delta = -sum(signed_delta)
        ) 
        
    
        # delta to hedge is slightly different than net delta
        # the only difference is that on expiration, you don't hedge 
        # the delta; it's either going to be zero or one
        # I just force it to zero for everything
        df_sve_pnl <-     
            df_sve_pnl %>% 
            mutate(
                delta_to_hedge = net_delta
            )
        df_sve_pnl$delta_to_hedge[nrow(df_sve_pnl)] <- 0
        
        
        # adding columns to identify
        df_sve_pnl$strategy <- chr_strategy
        df_sve_pnl$variation <- dbl_variation
        df_sve_pnl$expiration <- dt_expiration
        # moving added columns to the beginning of dataframe
        df_sve_pnl <- 
            df_sve_pnl %>% 
                select(strategy:expiration, data_date:delta_to_hedge)
            
        
        # saving the sve pnl
        lst_chain_pnl[[length(lst_chain_pnl) + 1]] <- df_sve_pnl
}
toc()


# creating a single dataframe for all the data
df_chain_pnl <- bind_rows(lst_chain_pnl)



#######################
## writing csv files ##
#######################
#write_csv(df_chain_pnl, "dia_weekly_2014_2018_chain_pnl.csv")
