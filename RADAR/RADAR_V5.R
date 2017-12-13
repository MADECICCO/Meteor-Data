RADAR <- function() {
        ## NB this version modified to take in .csv radar data !!!
        ## Set Working Directory
        ## NB two lines need update (flagged) before this is run
        setwd("D:/R Libraries/MeteorData/R RADAR Reports")
        
        ## Set libraries
        library(dplyr)
        library(lubridate)
        ##library(xlsx)
        ##library(readxl)
        
        ##############################################################################################################
        ## Read in RADAR Data
        print ("Reading in clanfield RADAR Data")
        #dsnr <<- "Radar Camera Comparison V03 - 2017YTD.csv" # ensure this is updated and copied into the correct folder
        dsnr <<- "Radar Camera Comparison V04 - Jan17-Nov17.csv" # ensure this is updated and copied into the correct folder
        ##Consolidated_RADAR <<- read.xlsx2(dsnr,1)
        Consolidated_RADAR <<- read.csv(dsnr,header = TRUE)
        
        No_of_RADAR_Records <<- (dim(Consolidated_RADAR))[1]
        print (paste(No_of_RADAR_Records, " RADAR records read",sep=""))
        
        Consolidated_RADAR <<- filter(Consolidated_RADAR,as.character(LocalTime) >= "20170101_000000")
        No_of_RADAR_Records <<- (dim(Consolidated_RADAR))[1]
        print (paste(No_of_RADAR_Records, " RADAR records remain after filtering",sep=""))
        
        ## Read in Video Data #######################################################################################
        ## NB.this version has multiple records per event so full file match needs to cope
        ##
        print ("Reading in UKALL Video Data")
        
        UKALLdata_path <<- "D:/R Libraries/MeteorData/R ALLUK Reports/Analysis/2017124/" # update this
        dsnu <<- "ALL_UK Consolidated Data_2017124.csv"                                  # update this too
        Consolidated_Video <<- read.csv(paste(UKALLdata_path,dsnu,sep=""))
        No_of_Video_Records <<- (dim(Consolidated_Video))[1]
        print (paste(No_of_Video_Records, " Video records read",sep=""))
        
        Consolidated_Video <<- filter(Consolidated_Video,as.character(LocalTime) >= "20170101_000000")
        
        No_of_Video_Records <<- (dim(Consolidated_Video))[1]
        print (paste(No_of_Video_Records, " Video records remain after filtering",sep=""))
        # Sort both data frames into the same order to permit comparison
        print ("Sorting Data into common sequence, by date")
        Consolidated_RADAR <<- arrange(Consolidated_RADAR,as.numeric(LocalTime)) ##Date),as.numeric("Camera Time"))
        Consolidated_Video <<- arrange(Consolidated_Video,as.numeric(LocalTime))
        
        
        #Now compare both files and write out a file of the matched video records for input to UFO Orbit
        # Drive compare using RADAR records because they should all have matching video records
        Orbit_Data <<- data.frame()
        vstart <<- 1
        vend <<- No_of_Video_Records
        vbreak <<- FALSE
        print("Matching Video and RADAR data")
        for (i in 1:No_of_RADAR_Records) {
                ##print(paste("For i = ",i))
                RADAR <<- Consolidated_RADAR[i,]
                ##RDate <<- parse_date_time(paste(RADAR$Date,substring(RADAR$"Camera Time",12),sep="_"),"y-m-d_h:m:s")
                RDate <<- parse_date_time(RADAR$LocalTime,"Ymd_HMS")
                for (j in vstart:vend) {
                        #print(paste("For j = ",j))
                        Video <<- Consolidated_Video[j,]
                        VDate <<- parse_date_time(Video$LocalTime,"Ymd_HMS")
                        #print(paste(VDate,RDate,seconds(1),sep=" "))
                        if (VDate >= RDate - seconds(1) & VDate <= RDate + seconds(1)) {
                                print(paste("Match found for ",RDate))
                                Orbit_Data <<- rbind(Orbit_Data,Video)
                        }
                        else {  #print("else")
                                if (VDate > RDate + seconds(1)) {
                                        vstart <<- j
                                        vbreak <<- TRUE
                                        break
                                }
                        }
                        if (vbreak == TRUE) {
                                vbreak <<- FALSE
                                break
                        }
                }
                
                
        }
        No_of_Orbit_Records <<- (dim(Orbit_Data))[1]
        print (paste(No_of_Video_Records, " matching Analysis records extracted",sep=""))
        write.csv(Orbit_Data, file = "Matched RADAR Data.csv", row.names=FALSE)
}