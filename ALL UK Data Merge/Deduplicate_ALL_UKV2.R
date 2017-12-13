Deduplicate_ALL_UK <<- function() {
        ##
        ## This function reads in the consolidated UKMON and NEMETODE csv files created by
        ## Read_UKMON("ALL") and Read_NEMETODE(""), deduplicates across the two files
        ## and writes out a single ALL UK .csv file for use in UFO Orbit
        ##
        
        ## Set libraries
        library(dplyr)
        library(lubridate)
        
        ############################################################################################################
        ## Read in the two Network data files
        ############################################################################################################
        
        ## Extract Current Date ####################################################################################
        Curr_Day <<- as.numeric(day(today()))
        Curr_Month <<- as.numeric(month(today()))
        Curr_Year <<- as.numeric(year(today()))
        ## end Extract Current Date ################################################################################
        
        ## Set Working Directory
        setwd("D:/R Libraries/MeteorData/R UKMON Reports")
        File_U <<- paste("UKMON Unique Consolidated Data_",Curr_Year,Curr_Month,Curr_Day,".csv",sep="")
        #File_U <<- paste("UKMON Unique Consolidated Data_2017216.csv",sep="")
        UKMON <<- read.csv(File_U)
        
        ## Set Working Directory
        setwd("D:/R Libraries/MeteorData/R NEMETODE Reports")
        File_N <<- paste("NEMETODE Consolidated Data_",Curr_Year,Curr_Month,Curr_Day,".csv",sep="")
        #File_N <<- paste("NEMETODE Consolidated Data_2017216.csv",sep="")
        NEMETODE <<- read.csv(File_N)
        
        ALL_UK <<- rbind(UKMON,NEMETODE)
        
        ## Deduplicate
        xx <<- distinct(ALL_UK,Loc_Cam,LocalTime,Ra1,Dec1, .keep_all = TRUE)
        
        ## Display record counts
        Ucount <<- dim(UKMON)[1]
        Ncount <<- dim(NEMETODE)[1]
        Acount <<- dim(ALL_UK)[1]
        Dcount <<- dim(xx)[1]
        
        print(paste("Records input from UKMON file    = ",Ucount,sep=""))
        print(paste("Records input from NEMETODE file = ",Ncount,sep=""))
        print(paste("Total Records Input              = ",Acount,sep=""))
        print(paste("Unique Records written to file   = ",Dcount,sep=""))
        
        ## Set Working Directory
        setwd("D:/R Libraries/MeteorData/R ALLUK Reports")
        
        write.csv(xx, file = paste("ALL_UK Consolidated Data_",Curr_Year,Curr_Month,Curr_Day, ".csv",sep=""), quote=FALSE,row.names=FALSE)
        
        
}        
        #######################################################################################################
        ## CHANGE LOG
        ## V1: 19 Aug 2016
        ## Initial version  
        ########################################################################################################
