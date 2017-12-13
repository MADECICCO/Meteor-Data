UKMON_Merge_OLD_NEW <<- function() {
        ##
        ## This function reads in a consolidated .csv file of UKMON data and creates a table of the number
        ## of records each camera has submitted, per month
        ## Depending on whether or not this is run on the same day as the data download, lines 26,30/27,31 should be flipped
        
        
        ## Set libraries
        library(dplyr)
        library(lubridate)
        

      
        #############################################################################################################
        ## Start of Main processing
        #############################################################################################################
        
        ## Extract Current Date #####################################################################################
        Curr_Day <<- as.numeric(day(today()))
        Curr_Month <<- as.numeric(month(today()))
        Curr_Year <<- as.numeric(year(today()))
        ## end Extract Current Date #################################################################################
        
        ## Set Working Directory
        setwd("D:/Meteor Watch/Data Drops/#Output/UKMON OLD plus AWS/Analysis/ALL")
        
        #OLD_Data <<- read.csv(paste("UKMON OLD Consolidated Data_",Curr_Year,Curr_Month,Curr_Day, ".csv",sep=""))
        OLD_Data <<- read.csv(paste("UKMON OLD Consolidated Data_20161012.csv",sep=""))
        No_of_OLD_Meteors <<- dim(OLD_Data)[1]
        
        ## Reset Working Directory
        setwd("D:/R Libraries/MeteorData/R UKMON Reports")
        
        NEW_Data <<- read.csv(paste("UKMON AWS Consolidated Data_",Curr_Year,Curr_Month,Curr_Day, ".csv",sep=""))
        #NEW_Data <<- read.csv(paste("UKMON AWS Consolidated Data_2016826.csv",sep=""))
        No_of_NEW_Meteors <<- dim(NEW_Data)[1]
        
        ALL_Data <<- rbind(OLD_Data,NEW_Data)
        Acount <<- dim(ALL_Data)[1]
        
        ## Deduplicate
        Unique_Data <<- distinct(ALL_Data,Loc_Cam,LocalTime,Ra1,Dec1, .keep_all = TRUE)
        Dcount <<- dim(Unique_Data)[1]
        
        print(paste("Records input from OLD UKMON file = ",No_of_OLD_Meteors,sep=""))
        print(paste("Records input from NEW UKMON file = ",No_of_NEW_Meteors,sep=""))
        print(paste("Total Records Input              = ",Acount,sep=""))
        print(paste("Unique Records written to file   = ",Dcount,sep=""))
        
        ## Reset Working Directory
        setwd("D:/R Libraries/MeteorData/R UKMON Reports")
  
        write.csv(Unique_Data,file = paste("UKMON Unique Consolidated Data_",Curr_Year,Curr_Month,Curr_Day,".csv",sep=""), quote=FALSE,row.names=FALSE)
        
        
        #############################################################################################################
        ## End of Main processing
        #############################################################################################################        
        

        #######################################################################################################
        ## CHANGE LOG
        ## 30 Mar 2016
        ## Initial version  
        ########################################################################################################
        
}