vol_index <- function(underlying){
    
    if (underlying == "DIA"){
        return("data_input/vxdohlcprices.csv")
    }
    
    if (underlying == "EEM"){
        return("data_input/vxeemdailyprices.csv")
        
    }
    
    
    if (underlying == "EFA"){
        return("data_input/vxefadailydata.csv")
    }
    
    if (underlying == "EWZ"){
        return("data_input/vxewzdailyprices.csv")
    }
    
    if (underlying == "FXE"){
        return("data_input/missing")
    }
    
    
    if (underlying == "FXI"){
        return("data_input/vxfxidailyprices.csv")
    }
    
    
    if (underlying == "FXY"){
        return("data_input/missing")
    }
    
    if (underlying == "GDX"){
        return("data_input/vxgdxdailyprices.csv")
    }
    
    if (underlying == "GLD"){
        return("data_input/gvzhistory.csv")
    }
    
    
    if (underlying == "IWM"){
        return("data_input/rvxdailyprices.csv")
    }
    
    
    if (underlying == "QQQ"){
        return("data_input/vxncurrent.csv")
    }
    
    
    if (underlying == "SLV"){
        return("data_input/vxslvdailyprices.csv")
    }
    
    
    if (underlying == "SPY"){
        return("data_input/vix9ddailyprices.csv")
    }
    
    
    if (underlying == "TLT"){
        return("data_input/tyvixdailyprices.csv")
    }
    
    
    if (underlying == "USO"){
        return("data_input/ovxhistory.csv")
    }
    
    
    if (underlying == "XLE"){
        return("data_input/ovxhistory.csv")
    }
    
}