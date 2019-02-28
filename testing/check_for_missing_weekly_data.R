# this script goes through all weekly expirations and checks 
# for missing data on execution date

######################
## loading packages ##
######################
library(tidyverse)
library(backtestr)
library(bizdays)
library(tictoc)




# sourcing functions
source("function/missing_data.R")
source("function/chain_weekly.R")
source("function/chain_monthly.R")

# initializing bizdays libraries
load_rmetrics_calendars(2000:2020)
bizdays.options$set(default.calendar="Rmetrics/NYSE")

# grabbing the list of ETFs that have weekly options
df_underlying <- read_csv("data_input/etf_with_vol_index.csv")

# initializing the list that will hold the data
lst_num_opt <- list() 
for (ix_und in 1:nrow(df_underlying)){
    
    # grabbing the current underlying
    chr_underlying <- df_underlying$ticker[ix_und]
    
    # calculating all the weekly expirations
    df_chain <- chain_weekly(chr_underlying)
    
    # removing the columns that aren't needed
    df_chain <- 
        df_chain %>% select(-d2x, -exec_day_volume, -num_opts)
    
    tic()
    # looping through all the expirations
    for (ix in 1:nrow(df_chain)){
        
        # grabbing expiration date and execution date
        dt_expiration <- df_chain$expiration[ix]
        dt_execution <- df_chain$execution[ix]
        
        # calculating all the trade dates for the expiration
        dt_all_td <- bizseq(dt_execution, dt_expiration)
        
        for(ix_td in 1:length(dt_all_td)){
            dt_curr_td <- dt_all_td[ix_td]
            
            df_all_opt_px <- 
                backtestr::option_chain(
                    trade_date = dt_curr_td
                    , underlying = chr_underlying
                    , expiration = dt_expiration
                )
            
            int_num_opt <- nrow(df_all_opt_px)
            
            lst_num_opt[[length(lst_num_opt) + 1]] <-
                tibble(
                    underlying = chr_underlying
                    , expiration = dt_expiration
                    , execution = dt_execution
                    , trade_date = dt_curr_td
                    , num_opt = int_num_opt
                )
        }
    }
    
    print(
        paste0(ix_und, " of ", nrow(df_underlying), ": ", chr_underlying)
    )
    toc()
}

df_num_opt <- bind_rows(lst_num_opt)

write_csv(df_num_opt, "weekly_etf_with_index_num_opt.csv")

#############
## testing ##
#############
df_num_opt %>% 
    filter()




