Read_NEMETODE <<- function(N_Station="ALL") {
        ##
        ## This function reads in all available csv files from the downloaded NEMETODE Google Drive data
        ## and consolidates this data into a single .csv file for use in UFO Orbit
        ##
        ############################################
        ## NB NEED TO UPDATE FILENAME ON LINE 152 ##
        ############################################
        
        ## Set Working Directory
        #setwd("D:/R Libraries/MeteorData")
        setwd("C:/Users/Steve/Google Drive/NEMETODE/")
        
        ## Set libraries
        library(dplyr)
        library(lubridate)
        
        
        ############################################################################################################
        Find_Data <<- function(data_path,NEMETODE_DATA,N_Station) {
        ## Read in Data ############################################################################################
        ##browser()        
        ##print(paste("Find_Data called for",data_path,N_Station,sep=" "))
        ##full_summary <<- data.frame()        
        ## first setup 'listlen2' as an integer vector indicating the number of files to be read in
        Station_path <<- data_path
        Station_DATA <<- data.frame()
        if (N_Station != "ALL") {
                Station_list_long <<- paste(Station_path,"/",N_Station,sep="")
                Station_list_short <<- N_Station
                no_of_Stations <<- 1
        }
        else {
                Station_list_long <<- list.files(path = Station_path, full.names = TRUE)       ## obtain full names of files to be read
                Station_list_short <<- list.files(path = Station_path)                         ## obtain short names of files to be read
                no_of_Stations <<- length(Station_list_short)
        }        
        if (no_of_Stations == 0) {
                print(paste("WARNING: No data available for path: ",data_path))
        }
        else {
                ## first check if there are files to be read from this folder
                ##print(paste("Call L1 Read_Data_Files for",Station_path,Station_list_short,no_of_Stations,sep=" "))
                Read_Data_Files(Station_path,Station_list_short,no_of_Stations,NEMETODE_DATA)
                
                ## Now read next level
                Level_2_list <<- NULL
                ##Level_2_DATA <<- data.frame()
                for (s in 1: no_of_Stations) {
                        ## Initialise data frames to capture data from each folder level for this station
                        Level_1_DATA <<- data.frame()
                        Level_2_DATA <<- data.frame()
                        Level_3_DATA <<- data.frame()
                        Level_4_DATA <<- data.frame()
                        Level_5_DATA <<- data.frame()
                        StationCount <<- 0
                        
                        ## Setup search path
                        Level_2_path <<- paste(Station_path,"/",Station_list_short[s],sep="")
                        Level_2_list <<- list.files(path = Level_2_path)
                        no_of_Level_2s <<- length(Level_2_list)
                        if (no_of_Level_2s == 0) {
                        ##      print(paste("WARNING: No data available for Station: ",Level_2_path))
                        }
                        else {
                                ## first check if there are files to be read from this folder
                                ##print(paste("Call L2 Read_Data_Files for",Level_2_path,Level_2_list,no_of_Level_2s,sep=" * "))
                                Read_Data_Files(Level_2_path,Level_2_list,no_of_Level_2s,NEMETODE_DATA)
                                
                                Level_3_list <<- NULL
                                ##Level_3_DATA <<- data.frame()
                                for (y in 1: no_of_Level_2s) {
                                        Level_3_path <<- paste(Level_2_path,"/",Level_2_list[y],sep="")
                                        Level_3_list <<- list.files(path = Level_3_path)
                                        ##print(paste("Level3List: ",Level_3_list,sep=" "))
                                        no_of_Level_3s <<- length(Level_3_list)
                                        if (no_of_Level_3s == 0) {
                                              if (Level_2_list[y] != "desktop.ini") {
                                                      print(paste("WARNING: No data available for Level_2: ",Level_3_path))
                                              }
                                        }
                                        else {
                                                ## first check if there are files to be read from this folder
                                                ##print(paste("Call L3 Read_Data_Files for",Level_3_path,Level_3_list,no_of_Level_3s,sep=" "))
                                                Read_Data_Files(Level_3_path,Level_3_list,no_of_Level_3s,LEVEL_DATA)
                                                ##print("L3 NEMETODEX Data Retrieved ")
                                                ##stop()
                                                ##print(LEVEL_DATA)
                                                Level_3_DATA <<- rbind(Level_3_DATA,LEVEL_DATA)
                                                ##print("Level_3_Data bound ")
                                                ##print(Level_3_DATA)
                                                
                                                Level_4_list <<- NULL
                                                ##Level_4_DATA <<- data.frame()
                                                for (m in 1: no_of_Level_3s) { 
                                                        Level_4_path <<- paste(Level_3_path,"/",Level_3_list[m],sep="")
                                                        Level_4_list <<- list.files(path = Level_4_path)
                                                        no_of_Level_4s <<- length(Level_4_list)
                                                        if (no_of_Level_4s == 0) {
                                                              if (Level_3_list[m] != "desktop.ini" &
                                                                  StationCount == 0) {
                                                                        print(paste("WARNING: No data available for Level_3: ",Level_4_path))
                                                              }
                                                        }
                                                        else { ## start of Level 4 Processing
                                                                ## first check if there are files to be read from this folder
                                                                ##print(paste("Call L4 Read_Data_Files for",Level_4_path,Level_4_list,no_of_Level_4s,sep=" "))
                                                                Read_Data_Files(Level_4_path,Level_4_list,no_of_Level_4s,LEVEL_DATA)
                                                                ##print("NEMETODEX Data ")
                                                                ##print(NEMETODEX_DATA)
                                                                Level_4_DATA <<- rbind(Level_4_DATA,LEVEL_DATA)
                                                                ##print(Level_4_DATA)
                                                                
                                                                Level_5_list <<- NULL
                                                                ##Level_5_DATA <<- data.frame()
                                                                for (d in 1: no_of_Level_4s) { 
                                                                        Level_5_path <<- paste(Level_4_path,"/",Level_4_list[d],sep="")
                                                                        Level_5_list <<- list.files(path = Level_5_path)
                                                                        no_of_Level_5s <<- length(Level_5_list)
                                                                        if (no_of_Level_5s == 0) {} # there won't always be data for every Level_4
                                                                        
                                                                        else { ## start of Level 5 Processing
                                                                                ##print(paste("Call L5 Read_Data_Files for",Level_5_path,Level_5_list,no_of_Level_5s,sep=" "))
                                                                                Read_Data_Files(Level_5_path,Level_5_list,no_of_Level_5s,LEVEL_DATA)
                                                                                Level_5_DATA <<- rbind(Level_5_DATA,LEVEL_DATA)
                                                                                                                                                             
                                                                        } ## end of level 5 processing
                                                                        
                                                                } ## end of Level 4 Loop
                                                                
                                                                ##print("Level_4_Data bound to NEMETODE Data ")
                                                                ##print(NEMETODE_DATA)
                                                                ##stop()
                                                                
                                                        } ## end of Level 4 Processing
                                                        
                                                        
                                                 } ## end of Level 3 Loop
                                                
                                        } ## end of Level 3 Processing
                                        ##NEMETODE_DATA <<- rbind(NEMETODE_DATA,Level_3_DATA)
                                        ##print("Level_3_Data bound to NEMETODE Data ")
                                        ##print("Correct to Here")
                                        ##stop()
                                        
                                } ## end of Level 2 Loop
                                
                        } ## end of Level 2 Processing
                        Station_DATA <<- rbind(Station_DATA,LEVEL_DATA)
                        NEMETODE_DATA <<- Station_DATA
                        
                } ## end of loop for no of stations
                
                
        }## end of else Find Data ##################################################################################
        ##NEMETODE_DATA <<- LEVEL_DATA
        return(NEMETODE_DATA)
        
        }        
 
        
        ############################################################################################################
        Read_Data_Files <<- function(data_path,data_list,f_end,LEVEL_DATA) {
        ## Read files in list provided #############################################################################
                ##print("Reading Data Files")
                
                NEMETODEX_DATA <<- data.frame()
                for (f in 1: f_end) {
                        
                        ##print(paste(f,f_end,sep= " - "))
                        
                        if (grepl("^M2", data_list[f], ignore.case=TRUE) |
                            grepl("^201", data_list[f], ignore.case=TRUE) |
                            grepl("^Basingstoke", data_list[f], ignore.case=TRUE) |
                            grepl("^Cherry", data_list[f], ignore.case=TRUE) |
                            grepl("^MY20", data_list[f], ignore.case=TRUE) |
                            grepl("^Jan", data_list[f], ignore.case=TRUE) |
                            grepl("^Feb", data_list[f], ignore.case=TRUE) |
                            grepl("^Mar", data_list[f], ignore.case=TRUE) |
                            grepl("^Apr", data_list[f], ignore.case=TRUE) |
                            grepl("^May", data_list[f], ignore.case=TRUE) |
                            grepl("^Jun", data_list[f], ignore.case=TRUE) |
                            grepl("^Jul", data_list[f], ignore.case=TRUE) |
                            grepl("^Aug", data_list[f], ignore.case=TRUE) |
                            grepl("^Sep", data_list[f], ignore.case=TRUE) |
                            grepl("^Oct", data_list[f], ignore.case=TRUE) |
                            grepl("^Nov", data_list[f], ignore.case=TRUE) |
                            grepl("^Dec", data_list[f], ignore.case=TRUE) |
                            grepl("^RM 201", data_list[f], ignore.case=TRUE) |
                            grepl("^Ravensmoor", data_list[f], ignore.case=TRUE) |
                            grepl("^DL1_2", data_list[f], ignore.case=TRUE)) 
                        {
                                if (grepl(".csv$", data_list[f], ignore.case=TRUE)) {
                                        
                                        file_name <<- data_list[f]
                                        ##print(paste("Reading File: ",file_name,sep=""))
                                        ## Tempfix to drop 2 corrupt files in G Reineke 2016 folder
                                        #if (file_name == "2016_Oct_az160_8_Newbridge_6mm.csv" |
                                        #    file_name == "2016_Sept_az160_8_Newbridge_6mm.csv") {
                                        #  print(paste("Corrupt File Dropped: ",file_name,sep=""))
                                        #}
                                        #else { # start of temporary else clause
                                        
                                        summary_data <<- data.frame()
                                        pathx <<- paste(data_path,"/",file_name,sep="")
                                        summary_data <<- read.csv(pathx)
                                        
                                        ##print(summary_data)
                                        
                                        nrows <<- dim(summary_data)[1]
                                        ##print(paste(nrows," rows to be added to existing total of ",rowstotal,sep=""))
                                        
                                        if(Next_Action == "copy") {
                                                ##print("copying data")
                                                NEMETODEX_DATA <<- summary_data
                                                Next_Action <<- "bind"
                                        }
                                        else {
                                                ##print("binding data")
                                                NEMETODEX_DATA <<- rbind(NEMETODEX_DATA,summary_data)
                                        }
                                        rowstotal <<- rowstotal + nrows
                                        StationCount <<- StationCount + nrows
                                        ##print(NEMETODEX_DATA)
                                        
                                        #} #end of temporary else clause
                                        
                                }
                                
                        }
                        
                }
                LEVEL_DATA <<- rbind(LEVEL_DATA,NEMETODEX_DATA)
                ##print("End of f loop")
                return(LEVEL_DATA)
                
        } ## end of function Read_Data_Files #######################################################################

        
        #############################################################################################################
        ## Start of Main processing
        #############################################################################################################
        ##print("Start Processing")
        ## Setup paths required to read data
        
        ## start with main path
        #NEMETODE_path <<- "D:/Meteor Watch/Data Drops/2016/NEMETODE Server Copy/NEMETODE-ALL Sept 2016/"
        NEMETODE_path <<-"C:/Users/Steve/Google Drive/NEMETODE/"
        
        rowstotal <<- 0
        Next_Action <<- "copy"
        
        if(N_Station == "") {
                N_Station <<- "ALL"
        }
        
        ##print(paste("Find_Data for",NEMETODE_path,N_Station,sep=" "))
        NEMETODE_DATA <<- data.frame()
        
        Find_Data(NEMETODE_path,NEMETODE_DATA,N_Station)
        
        ## Deduplicate
        xx <<- distinct(NEMETODE_DATA,Loc_Cam,LocalTime,Ra1,Dec1, .keep_all = TRUE)
        
        #setwd("D:/Meteor Watch/Data Drops/#Output/NEMETODE/")
        setwd("D:/R Libraries/MeteorData/R NEMETODE Reports/")
        
        ## Extract Current Date #####################################################################################
        Curr_Day <<- as.numeric(day(today()))
        Curr_Month <<- as.numeric(month(today()))
        Curr_Year <<- as.numeric(year(today()))
        ## end Extract Current Date #################################################################################
        
        write.csv(xx, file = paste("NEMETODE Consolidated Data_",Curr_Year,Curr_Month,Curr_Day, ".csv",sep=""), quote=FALSE,row.names=FALSE)
        #############################################################################################################
        ## End of Main processing
        #############################################################################################################        
        
        
}        
        #######################################################################################################
        ## CHANGE LOG
        ## 30 Mar 2016
        ## Initial version  
        ########################################################################################################
