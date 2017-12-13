Read_UKMON_AWS <<- function(U_Station="ALL") {
        ##
        ## This function reads in all available csv files from the downloaded UKMON Server data
        ## and consolidates this data into a single .csv file for use in UFO Orbit
        ##
        
        ## Set Working Directory
        setwd("D:/R Libraries/MeteorData")
        
        ## Set libraries
        library(dplyr)
        library(lubridate)
        
        
        ############################################################################################################
        Find_Data <<- function(data_path,UKMON_DATA,U_Station) {
        ## Read in Data ############################################################################################
                ##UKMON_DATA <<- data.frame()        
                ## first setup 'listlen2' as an integer vector indicating the number of files to be read in
                Station_path <<- data_path        
                Station_DATA <<- data.frame()
                csv_count <<- 0
                if (U_Station != "ALL") {
                        Station_list_long <<- paste(Station_path,"/",U_Station,sep="")
                        Station_list_short <<- U_Station
                        no_of_Stations <<- 1
                }
                else {
                        Station_list_long <<- list.files(path = Station_path, full.names = TRUE)       ## obtain full names of files to be read
                        Station_list_short <<- list.files(path = Station_path)                         ## obtain short names of files to be read
                        no_of_Stations <<- length(Station_list_short)
                } 
                if (no_of_Stations == 0) {
                        print(paste("WARNING: No data available for path: ",data_path))
                        rframe <<- data.frame()
                }
                else {
                        ## first check if there are files to be read from this folder
                        Level_1_DATA <<- data.frame()
                        Read_Data_Files(Station_path,Station_list_short,no_of_Stations,UKMON_DATA)
                        Level_1_DATA <<- rbind(Level_1_DATA,LEVEL_DATA)
                        
                        ## Now read next level
                        camera_list <<- NULL
                        for (s in 1: no_of_Stations) { 
                                
                                Level_2_DATA <<- data.frame()
                                Level_3_DATA <<- data.frame()
                                Level_4_DATA <<- data.frame()
                                Level_5_DATA <<- data.frame()
                                
                                camera_path <<- paste(Station_path,"/",Station_list_short[s],sep="")
                                camera_list <<- list.files(path = camera_path)
                                no_of_cameras <<- length(camera_list)
                                if (no_of_cameras == 0) {
                                        print(paste("WARNING: No data available for station: ",camera_path))
                                }
                                else {
                                        ## first check if there are files to be read from this folder
                                        Read_Data_Files(camera_path,camera_list,no_of_cameras,UKMON_DATA)
                                        Level_2_DATA <<- rbind(Level_2_DATA,LEVEL_DATA)
                                        
                                        year_list <<- NULL
                                        for (y in 1: no_of_cameras) { 
                                                year_path <<- paste(camera_path,"/",camera_list[y],sep="")
                                                year_list <<- list.files(path = year_path)
                                                no_of_years <<- length(year_list)
                                                if (no_of_years == 0) {
                                                        print(paste("WARNING: No data available for camera: ",year_path))
                                                }
                                                else {
                                                        ## first check if there are files to be read from this folder
                                                        Read_Data_Files(year_path,year_list,no_of_years,UKMON_DATA)
                                                        Level_3_DATA <<- rbind(Level_3_DATA,LEVEL_DATA)
                                                        
                                                        month_list <<- NULL
                                                        for (m in 1: no_of_years) { 
                                                                month_path <<- paste(year_path,"/",year_list[m],sep="")
                                                                month_list <<- list.files(path = month_path)
                                                                no_of_months <<- length(month_list)
                                                                if (no_of_months == 0) {
                                                                        print(paste("WARNING: No data available for year: ",month_path))
                                                                }
                                                                else {
                                                                        ## first check if there are files to be read from this folder
                                                                        Read_Data_Files(month_path,month_list,no_of_months,UKMON_DATA)
                                                                        Level_4_DATA <<- rbind(Level_4_DATA,LEVEL_DATA)
                                                                        
                                                                        day_list <<- NULL
                                                                        for (d in 1: no_of_months) { 
                                                                                day_path <<- paste(month_path,"/",month_list[d],sep="")
                                                                                day_list <<- list.files(path = day_path)
                                                                                no_of_days <<- length(day_list)
                                                                                if (no_of_days == 0) {} # there won't always be data for every month
                                                                                
                                                                                else {
                                                                                        Read_Data_Files(day_path,day_list,no_of_days,UKMON_DATA)
                                                                                        Level_5_DATA <<- rbind(Level_5_DATA,LEVEL_DATA)
                                                                                        
                                                                                }
                                                                                
                                                                        }
                                                                        
                                                                } 
                                                                
                                                                
                                                        }
                                                        
                                                } 
                                                
                                        }
                                        
                                }
                                Station_DATA <<- rbind(Station_DATA,Level_1_DATA)
                                Station_DATA <<- rbind(Station_DATA,Level_2_DATA)
                                Station_DATA <<- rbind(Station_DATA,Level_3_DATA)
                                Station_DATA <<- rbind(Station_DATA,Level_4_DATA)
                                Station_DATA <<- rbind(Station_DATA,Level_5_DATA)
                                UKMON_DATA <<- Station_DATA
                                
                        }
                        
                } ## end of else Find Data ##################################################################################
        }        
        
        
        ############################################################################################################
        Read_Data_Files <<- function(data_path,data_list,f_end,LEVEL_DATA) {
        ## Read files in list provided #############################################################################        
                UKMONX_DATA <<- data.frame()
                
                for (f in 1: f_end) {
                        if (grepl("^M", data_list[f], ignore.case=TRUE) |
                            grepl("^DL1_2", data_list[f], ignore.case=TRUE)) 
                        { 
                                file_name <<- data_list[f]
                                print(paste("Reading File: ",file_name,sep=""))
                                summary_data <<- data.frame()
                                setwd(data_path)
                                summary_data <<- read.csv(file_name)
                                csv_count <<- csv_count + 1
                                UKMONX_DATA <<- rbind(UKMONX_DATA,summary_data)
                                urows <<- dim(summary_data)[1]
                                ucount <<- ucount + urows
                                
                        }
                }
                LEVEL_DATA <<- rbind(LEVEL_DATA,UKMONX_DATA)
                ##print("End of f loop")
                return(LEVEL_DATA)
                
        } ## end of function Read_Data_Files #######################################################################
        
        
        
        #############################################################################################################
        ## Start of Main processing
        #############################################################################################################
        ## Setup paths required to read data
        
        ## start with main path
        UKMON_path <<- "D:/Meteor Watch/Data Drops/2016/UKMON AWS Server Copy/"
        
        ucount <<- 0
        
        if(U_Station == "") {
                U_Station <<- "ALL"
        }
        
        UKMON_DATA <<- data.frame()
        
        Find_Data(UKMON_path,UKMON_DATA,U_Station)
        
        ## Deduplicate
        if (dim(UKMON_DATA)[1] != 0) {
                yy <<- distinct(UKMON_DATA,Loc_Cam,LocalTime,Ra1,Dec1, .keep_all = TRUE)
        }        
        
        ##Invoke_Read(UKMON_path,uframe) ## read data for each month
        
        setwd("D:/R Libraries/MeteorData/R UKMON Reports/")
        
        ## Extract Current Date #####################################################################################
        Curr_Day <<- as.numeric(day(today()))
        Curr_Month <<- as.numeric(month(today()))
        Curr_Year <<- as.numeric(year(today()))
        ## end Extract Current Date #################################################################################
        
        write.csv(yy, file = paste("UKMON AWS Consolidated Data_",Curr_Year,Curr_Month,Curr_Day, ".csv",sep=""), quote=FALSE,row.names=FALSE)
        #############################################################################################################
        ## End of Main processing
        #############################################################################################################        
        

        #######################################################################################################
        ## CHANGE LOG
        ## 30 Mar 2016
        ## Initial version  
        ########################################################################################################
        
}